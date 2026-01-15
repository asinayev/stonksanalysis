import csv
import os
import json
import logging
import time
from google import genai  # 

# --- Configuration ---

# PLEASE CONFIGURE THESE VALUES
GOOGLE_API_KEY = os.environ["GOOGLEKEY"]  # Set your Google AI API key
INPUT_CSV_FILE = '/tmp/etf_list.csv'           # The name of your input CSV file
OUTPUT_CSV_FILE = '/tmp/etf_analysis.csv'  # The name of the file to save results
TICKER_COLUMN_NAME = 'symbol'         # The exact column name in your CSV that contains the ETF ticker
NAME_COLUMN_NAME = 'name'             # The exact column name in your CSV that contains the ETF name/description
MODEL_NAME = 'gemini-2.5-flash'             # The model you want to use

# Configure logging
log_file_path = '/tmp/etf_analysis.log'
logging.basicConfig(filename=log_file_path, level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

# --- LLM Prompt Template ---

# This prompt now includes both the ticker and the name for better analysis.
PROMPT_TEMPLATE = """
You are an expert financial analyst. Analyze the ETF provided below.
Use both the ticker and the name to make an accurate assessment.

ETF Ticker: {etf_ticker}
ETF Name/Description: {etf_name}

Please provide your analysis strictly in a single, valid JSON object. Do not include any text or markdown formatting (like ```json) before or after the JSON object.

The JSON object must contain the following keys:

**1. category:** Classify the ETF into ONE of the following, based on the asset(s) it tracks (regardless of how):
'equity basket' -- tracks price of  combination of companies that produce goods or services, including indirectly (like swaps) 
'single equity'  -- tracks a single equity, including indirectly (like swaps) 
'debt' -- includes derivatives that track debt like leveraged treasuries
'physical commodities' -- tracks commodities like gold or oil, including indirectly (like gold miners)
'cryptocurrency' -- tracks cryptocurrencies including, including indirectly (like MARA Holdings or Strategy Inc)
'risky strategies' -- financial derivatives intended only to hedge or for speculation (like volatility, buying puts, credit default swaps etc.) and excluding derivatives for tracking the other categories above like cryptocurrency and equity', 
'safe strategies' -- financial derivatives intended to be safe or produce income (like collared trades, income) and excluding derivatives for tracking the other categories aboce like cryptocurrency and equity', 
'other'

**2. leverage:** Specify the leverage factor as a number.
Use 1 for no leverage.
Use positive numbers for long leverage (e.g., 2 for 2x, 3 for 3x).
Use negative numbers for inverse leverage (e.g., -1 for -1x, -2 for -2x).
Use 1.5, 2.5, etc., for fractional leverage.

---
**EXAMPLES**
---

**Example 1:**
* ETF Ticker: TQQQ
* ETF Name/Description: ProShares UltraPro QQQ
* JSON Output:
{{
  "category": "equity basket",
  "leverage": 3,
}}

**Example 2:**
* ETF Ticker: VXX
* ETF Name/Description: iPath Series B S&P 500 VIX Short-Term Futures ETN
* JSON Output:
{{
  "category": "risky strategies",
  "leverage": 1,
}}

---
**TASK**
---

Now, analyze the following ETF:
ETF Ticker:{etf_ticker}
ETF Name/Description:{etf_name}
JSON Output:
"""

def read_input_csv(filename, ticker_column, name_column):
    """Reads the input CSV and returns a list of dictionaries."""
    etfs = []
    try:
        with open(filename, mode='r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            fieldnames = reader.fieldnames
            
            if fieldnames is None:
                logger.error(f"Could not read fieldnames from {filename}. Is the file empty?")
                print(f"Error: Could not read headers from {filename}. Is the file empty?")
                return None

            if ticker_column not in fieldnames:
                logger.error(f"Column '{ticker_column}' not found in {filename}.")
                print(f"Error: Column '{ticker_column}' not found in {filename}.")
                print(f"Available columns are: {fieldnames}")
                return None

            if name_column not in fieldnames:
                logger.error(f"Column '{name_column}' not found in {filename}.")
                print(f"Error: Column '{name_column}' not found in {filename}.")
                print(f"Available columns are: {fieldnames}")
                return None
            
            for row in reader:
                etfs.append(row)
        logger.info(f"Successfully read {len(etfs)} ETFs from {filename}.")
        return etfs
    except FileNotFoundError:
        logger.error(f"Input file not found: {filename}")
        print(f"Error: Input file not found: {filename}")
        return None
    except Exception as e:
        logger.error(f"Error reading CSV {filename}: {e}")
        print(f"Error reading CSV: {e}")
        return None

def create_prompt(etf_data, ticker_column, name_column):
    """Create a prompt from the template and ETF data."""
    ticker = etf_data.get(ticker_column)
    name = etf_data.get(name_column)
    
    if not ticker:
        logger.warning(f"No ticker found in row: {etf_data}")
        return None, "No ticker found in row"
    
    # If name is missing, use an empty string. The ticker is the critical part.
    if not name:
        logger.warning(f"No name found for ticker {ticker}. Proceeding with ticker only.")
        name = ""
        
    prompt = PROMPT_TEMPLATE.format(etf_ticker=ticker, etf_name=name)
    return prompt, ""

def parse_response(response, etf_data, ticker_column):
    """Parse the response from the model and extract the JSON data."""
    try:
        # CHANGED: The new SDK allows direct access to .text
        text_content = response.text
        
        json_start = text_content.find("{")
        json_end = text_content.rfind("}") + 1
        
        if json_start == -1 or json_end == -1:
            raise json.JSONDecodeError("No JSON object found", text_content, 0)
            
        json_text = text_content[json_start:json_end]
        response_dict = json.loads(json_text)
        
        etf_data.update(response_dict)
        etf_data['llm_message'] = 'Success'
        return etf_data
        
    except Exception as e:
        ticker = etf_data.get(ticker_column, 'UNKNOWN_TICKER')
        logger.error(f"Issue parsing result for {ticker}: {e}")
        etf_data['llm_message'] = f"Failed to parse: {e}"
        return etf_data

def analyze_etfs(etf_list, ticker_column, name_column, client): # CHANGED: pass client
    processed_etfs = []
    
    for i, etf_data in enumerate(etf_list):
        ticker = etf_data.get(ticker_column, f"ROW_{i}")
        logger.info(f"Processing {i+1}/{len(etf_list)}: {ticker}")
        
        prompt, error_msg = create_prompt(etf_data, ticker_column, name_column)
        if not prompt:
            etf_data['llm_message'] = error_msg
            processed_etfs.append(etf_data)
            continue

        try:
            time.sleep(1) 
            # CHANGED: Use client.models.generate_content
            response = client.models.generate_content(
                model=MODEL_NAME, 
                contents=prompt
            )
            
            analysis_result = parse_response(response, etf_data, ticker_column)
            processed_etfs.append(analysis_result)

        except Exception as e:
            logger.error(f"Model generation failed for {ticker}: {e}")
            etf_data['llm_message'] = f"Model generation failed: {e}"
            processed_etfs.append(etf_data)
    return processed_etfs

def write_output_csv(filename, analysis_results):
    """Writes the combined results to a new CSV file."""
    if not analysis_results:
        logger.warning("No analysis results to write.")
        print("No analysis results to write.")
        return

    # Dynamically get all fieldnames from the results
    # Ensure all original and new keys are included
    fieldnames = set()
    for row in analysis_results:
        fieldnames.update(row.keys())
    
    # Define a preferred order, putting new fields at the end
    original_fields = list(analysis_results[0].keys())
    new_fields = ['category', 'leverage', 'income_producing', 'llm_message']
    
    # Remove new fields from original list in case they were already there
    # (e.g., if 'category' was already a column)
    for f in new_fields:
        if f in original_fields:
            original_fields.remove(f)
            
    # Re-add them to ensure they appear at the end
    final_fieldnames = original_fields + [f for f in new_fields if f in fieldnames]
    
    # Add any other new fields that weren't in the original or new_fields list
    # (This is a safeguard in case the JSON parsing adds unexpected keys)
    other_new_fields = [f for f in fieldnames if f not in final_fieldnames]
    final_fieldnames.extend(other_new_fields)
    
    try:
        with open(filename, mode='w', encoding='utf-8', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=final_fieldnames, extrasaction='ignore')
            writer.writeheader()
            writer.writerows(analysis_results)
        logger.info(f"Successfully wrote {len(analysis_results)} rows to {filename}.")
        print(f"Successfully wrote {len(analysis_results)} rows to {filename}.")
    except Exception as e:
        logger.error(f"Error writing to CSV {filename}: {e}")
        print(f"Error writing to CSV: {e}")

def main():
    print(f"Starting ETF analysis...")
    if not GOOGLE_API_KEY:
        print("Error: GOOGLEKEY environment variable not set.")
        return
        
    try:
        # CHANGED: Initialize Client instead of genai.configure
        client = genai.Client(api_key=GOOGLE_API_KEY)
        logger.info(f"Google AI Client initialized for model '{MODEL_NAME}'.")
    except Exception as e:
        print(f"Error: Failed to configure Google AI. {e}")
        return

    etf_list = read_input_csv(INPUT_CSV_FILE, TICKER_COLUMN_NAME, NAME_COLUMN_NAME)
    if etf_list:
        # CHANGED: Pass client to analyze_etfs
        analysis_results = analyze_etfs(etf_list, TICKER_COLUMN_NAME, NAME_COLUMN_NAME, client)
        write_output_csv(OUTPUT_CSV_FILE, analysis_results)
    
    print("Analysis complete.")

if __name__ == "__main__":
    main()
