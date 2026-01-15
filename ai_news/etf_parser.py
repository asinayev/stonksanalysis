import csv
import os
import json
import logging
import time
import google.generativeai as genai

# --- Configuration ---

# PLEASE CONFIGURE THESE VALUES
GOOGLE_API_KEY = os.environ.get("GOOGLEKEY")  # Set your Google AI API key
INPUT_CSV_FILE = '/tmp/etf_list.csv'           # The name of your input CSV file
OUTPUT_CSV_FILE = '/tmp/etf_analysis.csv'  # The name of the file to save results
TICKER_COLUMN_NAME = 'ticker'         # The exact column name in your CSV
NAME_COLUMN_NAME = 'name'             # The exact column name in your CSV
MODEL_NAME = 'gemini-1.5-flash'       # Current recommended model for speed/cost

# Configure logging
log_file_path = '/tmp/etf_analysis.log'
logging.basicConfig(filename=log_file_path, level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

# --- LLM Prompt Template ---

PROMPT_TEMPLATE = """
You are an expert financial analyst. Analyze the ETF provided below.
Use both the ticker and the name to make an accurate assessment.

ETF Ticker: {etf_ticker}
ETF Name/Description: {etf_name}

The output must be a JSON object with exactly these keys:
1. "category": One of ['equity basket', 'single equity', 'debt', 'physical commodities', 'cryptocurrency', 'risky strategies', 'safe strategies', 'other']
2. "leverage": A number (1 for no leverage, 2/3 for long, -1/-2 for inverse).
"""

def read_input_csv(filename, ticker_column, name_column):
    etfs = []
    try:
        with open(filename, mode='r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            fieldnames = reader.fieldnames
            
            if fieldnames is None or ticker_column not in fieldnames or name_column not in fieldnames:
                logger.error(f"Required columns missing in {filename}")
                return None
            
            for row in reader:
                etfs.append(row)
        return etfs
    except Exception as e:
        logger.error(f"Error reading CSV: {e}")
        return None

def parse_response(response, etf_data):
    """Parse the JSON response from the model."""
    try:
        # The SDK now handles JSON formatting if configured in generation_config
        response_dict = json.loads(response.text)
        etf_data.update(response_dict)
        etf_data['llm_message'] = 'Success'
        return etf_data
    except Exception as e:
        etf_data['llm_message'] = f"Parsing Error: {str(e)}"
        return etf_data

def analyze_etfs(etf_list, ticker_column, name_column, model):
    processed_etfs = []
    
    for i, etf_data in enumerate(etf_list):
        ticker = etf_data.get(ticker_column, "UNKNOWN")
        name = etf_data.get(name_column, "")
        logger.info(f"Processing {i+1}/{len(etf_list)}: {ticker}")

        prompt = PROMPT_TEMPLATE.format(etf_ticker=ticker, etf_name=name)

        try:
            # Respecting Rate Limits (Free tier is usually 15 RPM)
            time.sleep(4) 
            
            response = model.generate_content(
                prompt,
                # Force the model to output valid JSON
                generation_config={"response_mime_type": "application/json"}
            )
            
            result = parse_response(response, etf_data)
            processed_etfs.append(result)

        except Exception as e:
            logger.error(f"API Error for {ticker}: {e}")
            etf_data['llm_message'] = f"API Error: {e}"
            processed_etfs.append(etf_data)

    return processed_etfs

def write_output_csv(filename, analysis_results):
    if not analysis_results: return

    # Gather all potential headers
    fieldnames = list(analysis_results[0].keys())
    # Ensure our specific keys exist in fieldnames if they weren't in the input
    for key in ['category', 'leverage', 'llm_message']:
        if key not in fieldnames:
            fieldnames.append(key)

    try:
        with open(filename, mode='w', encoding='utf-8', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames, extrasaction='ignore')
            writer.writeheader()
            writer.writerows(analysis_results)
        print(f"Results saved to {filename}")
    except Exception as e:
        print(f"Write Error: {e}")

def main():
    if not GOOGLE_API_KEY:
        print("Error: GOOGLEKEY environment variable not set.")
        return

    genai.configure(api_key=GOOGLE_API_KEY)
    model = genai.GenerativeModel(MODEL_NAME)

    etf_list = read_input_csv(INPUT_CSV_FILE, TICKER_COLUMN_NAME, NAME_COLUMN_NAME)
    if etf_list:
        results = analyze_etfs(etf_list, TICKER_COLUMN_NAME, NAME_COLUMN_NAME, model)
        write_output_csv(OUTPUT_CSV_FILE, results)

if __name__ == "__main__":
    main()
