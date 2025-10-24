import csv
import os
import json
import logging
import time
import google.generativeai as genai

# --- Configuration ---

# PLEASE CONFIGURE THESE VALUES
GOOGLE_API_KEY = os.environ["GOOGLEKEY"]  # Set your Google AI API key
INPUT_CSV_FILE = '/tmp/etf_list_short.csv'           # The name of your input CSV file
OUTPUT_CSV_FILE = '/tmp/etf_analysis.csv'  # The name of the file to save results
TICKER_COLUMN_NAME = 'ticker'         # The exact column name in your CSV that contains the ETF ticker
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

The JSON object must contain the following six keys:

**1. category:** Classify the ETF into ONE of the following specific categories:
'traditional equities', 'speculative equities', 'thematic or geographic equities', 'debt', 'physical commodities', 'currencies', 'cryptocurrencies', 'real estate', 'financial derivatives (like volatility)', 'funds of funds', 'strategies (like following a hedge fund or momentum or events)', 'futures tied to the real world (e.g., carbon credits or shipping routes)', 'other'

**2. leverage:** Specify the leverage factor as a number.
Use 1 for no leverage.
Use positive numbers for long leverage (e.g., 2 for 2x, 3 for 3x).
Use negative numbers for inverse leverage (e.g., -1 for -1x, -2 for -2x).
Use 1.5, 2.5, etc., for fractional leverage.

**3. income_producing:** A string, either 'Yes' or 'No'.
'Yes' if the ETF's underlying assets typically produce income (e.g., stock dividends, bond interest).
'No' if they typically do not (e.g., physical commodities, cryptocurrencies).

**4. underlying_mechanism:** Classify the *primary* method the ETF uses to get its exposure.
'equity_basket': Holds the actual stocks (e.g., SPY).
'debt_instruments': Holds the actual bonds (e.g., TLT).
'physical_assets': Holds the actual commodity (e.g., GLD).
'futures_contracts': Holds futures to track an index or commodity (e.g., VXX, USO).
'swap_agreements': Uses derivatives (swaps) to get exposure, common for leveraged/inverse (e.g., TQQQ).
'other_derivative': Uses options, forwards, or other non-swap derivatives.

**5. reset_frequency:** Specify how often the ETF's stated exposure is reset.
'none': The fund is not leveraged/inverse and simply holds assets (e.g., SPY, GLD).
'daily': The fund rebalances every day to meet its target (e.g., 2x, -1x).
'monthly': The fund rebalances monthly (less common, but exists for some ETPs).
'other': Any other non-standard reset period.

**6. primary_price_pattern_driver:** Identify the *main structural factor* (not market opinion) that influences its price behavior *beyond* the underlying asset itself.
'underlying_price': The ETF price is driven purely by the value of its holdings (e.g., SPY).
'daily_compounding_math': The price pattern is heavily affected by daily resetting and volatility (e.g., TQQQ, SQQQ). This is the "volatility decay" or "beta slippage" effect.
'futures_curve_structure': The price pattern is heavily affected by "roll yield" (contango or backwardation) from its futures holdings (e.g., VXX, USO).
'interest_rate_sensitivity': The price is primarily driven by changes in interest rates (e.g., TLT).
'credit_risk': The price is primarily driven by default risk (e.g., HYG).
'strategy_rules': The price is driven by the execution of a specific rules-based strategy (e.g., a momentum or factor ETF).

---
**EXAMPLES**
---

**Example 1:**
* ETF Ticker: SPY
* ETF Name/Description: SPDR S&P 500 ETF Trust
* JSON Output:
{{
  "category": "traditional equities",
  "leverage": 1,
  "income_producing": "Yes",
  "underlying_mechanism": "equity_basket",
  "reset_frequency": "none",
  "primary_price_pattern_driver": "underlying_price"
}}

**Example 2:**
* ETF Ticker: TQQQ
* ETF Name/Description: ProShares UltraPro QQQ
* JSON Output:
{{
  "category": "speculative equities",
  "leverage": 3,
  "income_producing": "Yes",
  "underlying_mechanism": "swap_agreements",
  "reset_frequency": "daily",
  "primary_price_pattern_driver": "daily_compounding_math"
}}

**Example 3:**
* ETF Ticker: VXX
* ETF Name/Description: iPath Series B S&P 500 VIX Short-Term Futures ETN
* JSON Output:
{{
  "category": "financial derivatives (like volatility)",
  "leverage": 1,
  "income_producing": "No",
  "underlying_mechanism": "futures_contracts",
  "reset_frequency": "none",
  "primary_price_pattern_driver": "futures_curve_structure"
}}

**Example 4:**
* ETF Ticker: TLT
* ETF Name/Description: iShares 20+ Year Treasury Bond ETF
* JSON Output:
{{
  "category": "debt",
  "leverage": 1,
  "income_producing": "Yes",
  "underlying_mechanism": "debt_instruments",
  "reset_frequency": "none",
  "primary_price_pattern_driver": "interest_rate_sensitivity"
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
        # Find the JSON block in the response text
        json_start = response.text.find("{")
        json_end = response.text.rfind("}") + 1
        
        if json_start == -1 or json_end == -1:
            raise json.JSONDecodeError("No JSON object found in response.", response.text, 0)
            
        json_text = response.text[json_start:json_end]
        response_dict = json.loads(json_text)
        
        # Merge the original ETF data with the new analysis data
        etf_data.update(response_dict)
        etf_data['llm_message'] = 'Success'
        return etf_data
        
    except (json.JSONDecodeError, AttributeError, TypeError) as e:
        ticker = etf_data.get(ticker_column, 'UNKNOWN_TICKER')
        logger.error(f"Issue parsing result for ticker: {ticker}. Error: {e}")
        logger.debug(f"Raw response text: {response.text if hasattr(response, 'text') else 'No response text'}")
        
        etf_data['llm_message'] = f"Failed to parse LLM response: {e}"
        return etf_data

def analyze_etfs(etf_list, ticker_column, name_column, model):
    """Read and process AI-generated content for all ETFs."""
    processed_etfs = []
    
    for i, etf_data in enumerate(etf_list):
        ticker = etf_data.get(ticker_column, f"ROW_{i}")
        logger.info(f"Processing ETF {i+1}/{len(etf_list)}: {ticker}")
        
        prompt, error_msg = create_prompt(etf_data, ticker_column, name_column)
        if not prompt:
            etf_data['llm_message'] = error_msg
            processed_etfs.append(etf_data)
            continue

        try:
            # Add a small delay to avoid hitting rate limits
            time.sleep(1) 
            response = model.generate_content(prompt, request_options=genai.types.RequestOptions(timeout=30))
            
            analysis_result = parse_response(response, etf_data, ticker_column)
            processed_etfs.append(analysis_result)

        except Exception as e:
            logger.error(f"Model generation failed for {ticker}: {e}")
            etf_data['llm_message'] = f"Model generation failed: {e}"
            processed_etfs.append(etf_data)
            # Optional: Add a longer sleep if an API error occurs
            time.sleep(5) 

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
    """Main function to run the ETF analysis."""
    print(f"Starting ETF analysis...")
    logger.info("--- Starting new ETF analysis run ---")
    
    # Configure the Generative AI model
    if GOOGLE_API_KEY == 'YOUR_API_KEY_HERE':
        logger.error("GOOGLE_API_KEY is not set.")
        print("Error: Please set your GOOGLE_API_KEY at the top of the script.")
        return
        
    try:
        genai.configure(api_key=GOOGLE_API_KEY)
        model = genai.GenerativeModel(MODEL_NAME)
        logger.info(f"Google AI Model '{MODEL_NAME}' configured.")
    except Exception as e:
        logger.error(f"Failed to configure Google AI: {e}")
        print(f"Error: Failed to configure Google AI. Check your API key and permissions. {e}")
        return

    # 1. Read input CSV
    print(f"Reading ETFs from {INPUT_CSV_FILE}...")
    etf_list = read_input_csv(INPUT_CSV_FILE, TICKER_COLUMN_NAME, NAME_COLUMN_NAME)
    
    if etf_list is None:
        print("Exiting due to CSV read error.")
        return

    # 2. Analyze ETFs
    print(f"Analyzing {len(etf_list)} ETFs using model '{MODEL_NAME}'. This may take a while...")
    analysis_results = analyze_etfs(etf_list, TICKER_COLUMN_NAME, NAME_COLUMN_NAME, model)

    # 3. Write output CSV
    print(f"Writing results to {OUTPUT_CSV_FILE}...")
    write_output_csv(OUTPUT_CSV_FILE, analysis_results)
    
    print("Analysis complete.")
    logger.info("--- ETF analysis run finished ---")

if __name__ == "__main__":
    main()
