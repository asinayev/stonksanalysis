import os
import sys
import logging
from google import genai
from google.genai import types

# Add the directory to sys.path to import local modules
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

import scrape_news
import ai_reads_search
import call_ai

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

def main():
    # 1. Setup Keys and Client
    google_key = os.environ.get("GOOGLEKEY")
    polygon_key = os.environ.get("POLYGONKEY")
    
    if not google_key or not polygon_key:
        logger.error("Missing GOOGLEKEY or POLYGONKEY environment variables.")
        return

    client = genai.Client(api_key=google_key)
    model_id = 'gemini-2.0-flash'
    
    safety_settings = [
        types.SafetySetting(
            category='HARM_CATEGORY_HATE_SPEECH',
            threshold='BLOCK_NONE'
        ),
        types.SafetySetting(
            category='HARM_CATEGORY_HARASSMENT',
            threshold='BLOCK_NONE'
        ),
        types.SafetySetting(
            category='HARM_CATEGORY_SEXUALLY_EXPLICIT',
            threshold='BLOCK_NONE'
        ),
        types.SafetySetting(
            category='HARM_CATEGORY_DANGEROUS_CONTENT',
            threshold='BLOCK_NONE'
        ),
    ]

    config = types.GenerateContentConfig(
        safety_settings=safety_settings,
        response_mime_type='application/json'
    )

    # 2. Define Prompt Template (Copied from buybacks.py)
    prompt_template="""
Determine if the following search result includes a NEW announcement about a {}  Answer 'yes' or 'no'.

For your response, also extract (or infer) the following:
- Company Full Name
- Stock Ticker Symbol
- Publication Date and Time
- A short quote from the search result that supports your 'yes'/'no' determination.

1. Locate the title. If the title doesn't refer to the company at all, RESPOND NO (not a new program).  
2. Locate the publication time in the search result's snippet, body or metadata, also try searching inside the URL for the date. It may appear in any format like 'days ago' or a datetime with timezone like '1999-07-01T23:21:10-5:00' or within the URL like /20200105... . Reformat to YYYY-MM-DD HH:MM using military time (put 24:00 if no time is provided). Do not convert timezones, so if it says +3:00, that refers to the timezone and you can ignore that.
3. Read the rest of the announcement for the rest of the details. Pay special attention to the link and title and pay secondary attention to the actual text. 
4. If you did not find the ticker anywhere, but are sure that you know the ticker for this company, use what you know.

If any of these details cannot be determined, use "UNKNOWN".

Respond in JSON format as shown in this example:
{{"newProgram":"No","companyName":"Microsoft Corporation","ticker":"MSFT","timePublished":"2024-08-12 15:30","quote":"In June, the board announced plans for a new shareholder incentive..."}}

Search result:
"""

    buyback_description="an announcement of a share buyback or share repurchase program. If the announcement does not say that stock will be repurchased or bought back IN THE FUTURE, (e.g., is simply an update about stock repurchases that already happened, or reminds shareholders of a previously announced repurchase), respond no."
    
    parameters = {
        'search_query': 'buyback|repurchase share|stock', # Used for filename/logging
        'prompt_template': prompt_template.format(buyback_description),
        'min_close': 5, 
        'min_volume': 20000, 
        'max_market_cap': 10000000000, 
        'min_overnight_pchange': -2.2, 
        'max_overnight_pchange': 9
    }

    # 3. Fetch News directly from websites
    logger.info("Fetching news from PR Newswire, Business Wire, GlobeNewswire...")
    raw_results = scrape_news.get_latest_buyback_news()
    
    if not raw_results:
        logger.warning("No news found from scrapers.")
        return

    logger.info(f"Found {len(raw_results)} articles to process.")

    # 4. Process with AI (Gemini)
    # Using process_results_with_ai from ai_reads_search module
    # Note: ai_reads_search.process_results_with_ai expects a list of dicts with 'link' and 'title' keys, which scrape_news provides.
    processed_results = ai_reads_search.process_results_with_ai(
        raw_results,
        prompt_template=parameters['prompt_template'],
        client=client,
        model_id=model_id,
        config=config
    )

    if not processed_results:
        logger.warning("No valid results after AI processing.")
        return

    # 5. Enrich with Polygon Data
    enriched_results = ai_reads_search.enrich_with_financial_data(
        processed_results, 
        polygon_key, 
        parameters
    )

    # 6. Save Results
    output_dir = '/tmp/stonksanalysis/'
    filename = 'steves_buybacks.csv' # Or match the query based name if preferred
    
    # We'll use the existing save function but force the filename if possible, 
    # or just let it use the query name and then rename/copy it.
    # The existing function: format_and_save_results(enriched_results, query, write_to_dir)
    # It saves as write_to_dir + query.replace('|','_') + '.csv'
    
    ai_reads_search.format_and_save_results(enriched_results, "steves_buybacks", output_dir)
    logger.info(f"Done. Saved to {output_dir}steves_buybacks.csv")

    # 7. Generate Summary Text File
    summary_path = os.path.join(output_dir, "steves_buybacks.txt")
    try:
        with open(summary_path, "w") as f:
            f.write("STEVE'S BUYBACK SCANNER - SUMMARY\n")
            f.write("=================================\n\n")
            
            # Filter for matches
            matches = [r for r in enriched_results if r.get('match') and r.get('newProgram') == 'yes']
            
            f.write(f"Total Articles Scanned: {len(raw_results)}\n")
            f.write(f"Valid Buyback Mentions (after AI): {len([r for r in processed_results if r.get('newProgram') == 'yes'])}\n")
            f.write(f"Passing Financial Criteria: {len(matches)}\n\n")
            
            if matches:
                f.write("MATCHING OPPORTUNITIES:\n")
                f.write("-----------------------\n")
                
                # Deduplicate by ticker/company
                unique_matches = {}
                for m in matches:
                    ticker = m.get('ticker', 'UNKNOWN')
                    # Use ticker as primary key if available, else company name
                    if ticker and ticker not in ['UNKNOWN', '_UNKNOWN']:
                        key = ticker
                    else:
                        key = m.get('companyName', 'Unknown Company')
                    
                    if key not in unique_matches:
                        unique_matches[key] = {
                            'info': m,
                            'links': set(),
                            'quotes': []
                        }
                    
                    if m.get('link'):
                        unique_matches[key]['links'].add(m.get('link'))
                    
                    q = m.get('quote', '').strip()
                    if q:
                        unique_matches[key]['quotes'].append(q)

                for key, data in unique_matches.items():
                    m = data['info']
                    ticker = m.get('ticker', 'UNKNOWN')
                    company = m.get('companyName', 'Unknown Company')
                    date = m.get('timePublished', 'Unknown Date')
                    
                    # Pick the longest quote as representative
                    quotes = data['quotes']
                    quote = max(quotes, key=len) if quotes else ""
                    
                    f.write(f"[{ticker}] {company} ({date})\n")
                    if quote:
                        f.write(f"  \"...{quote}...\"\n")
                    
                    f.write(f"  Sources:\n")
                    for link in sorted(list(data['links'])):
                        f.write(f"  - {link}\n")
                    f.write("\n")
            else:
                f.write("No matching opportunities found this run.\n")
                
        logger.info(f"Summary saved to {summary_path}")
    except Exception as e:
        logger.error(f"Error writing summary file: {e}")

if __name__ == "__main__":
    main()
