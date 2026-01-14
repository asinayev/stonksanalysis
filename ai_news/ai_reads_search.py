import logging
from googleapiclient.discovery import build
from polygon import RESTClient
import pandas as pd
import search_web
import call_ai
from google.genai import types

# Configure logging
logging.basicConfig(filename='/tmp/read_search.log', level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

def fetch_search_results(search_service, search_id, query, n_results, **kwargs):
    """Fetches search results from Google Custom Search."""
    try:
        all_results = search_web.all_search_pages(
            service=search_service,
            cse_id=search_id,
            n_results=n_results,
            q=query,
            sort="date",
            num=10,
            **kwargs
        )
        logging.info(f"{len(all_results)} total results found for query: {query}")
        return all_results
    except Exception as e:
        logging.error(f"Error fetching search results: {e}")
        return []

def process_results_with_ai(all_results, prompt_template, client, model_id, config):
    """
    Processes search results using the AI model.
    UPDATED: Passes client, model_id, and config to call_ai.read_results
    """
    try:
        valid_results = call_ai.read_results(
            all_results, 
            prompt_template, 
            client=client, 
            model_id=model_id, 
            config=config
        )
        logging.info(f"{len(valid_results)} valid results after AI processing.")
        return valid_results
    except Exception as e:
        logging.error(f"Error processing results with AI: {e}")
        return []

def enrich_with_financial_data(valid_results, polygon_key, parameters):
    """Enriches results with data from Polygon API."""
    enriched_results = []
    poly_client = RESTClient(api_key=polygon_key) 
    for r in valid_results:
        try:
            enriched_result = call_ai.enrich_result(r, poly_client, parameters)
            enriched_results.append(enriched_result) 
        except Exception as e:
            logging.error(f"Error enriching result: {r} - Error: {e}")
    logging.info(f"{len(enriched_results)} results enriched with Polygon data.")
    return enriched_results

def format_and_save_results(enriched_results, query, write_to_dir):
    """Formats the results and saves them to a CSV file."""
    try:
        all_data = []
        for r in enriched_results:
            try:
                r['newProgram'] = r['newProgram'].lower()
                r['quote'] = r['quote'].lower()
            except Exception as e:
                logging.error(f"Error processing program reasoning: {e}")
            all_data.append(r)
        
        if not all_data:
            logging.info("No data to save.")
            return

        already_tracked = pd.DataFrame(all_data).reindex(columns=['timePublished','ticker','match','message','newProgram','quote','title','link','companyName','current','volume','market_cap_ok','liquidity_ok','overnight_in_range'])
        
        # Handle empty DataFrame or missing columns gracefully
        if not already_tracked.empty and 'newProgram' in already_tracked.columns:
            already_tracked.sort_values(["newProgram","timePublished"], ascending=False, inplace=True)

        # --- Logging ---
        logging.info("############################ MATCHING RESULTS: " + query)
        matching_count = len([x for x in enriched_results if x.get('match')])
        logging.info(f"{matching_count} matching enriched results")
        
        pd.set_option('display.max_colwidth', None)
        if not already_tracked.empty:
            logging.info(already_tracked[(already_tracked.newProgram=='yes')&(already_tracked.match)][['ticker', 'timePublished', 'link']])
            already_tracked.to_csv(write_to_dir + query.replace('|','_') + '.csv')
    except Exception as e:
        logging.error(f"Error formatting and saving results: {e}")

def read_search(google_key: str, polygon_key: str, search_id: str, 
                client, model_id: str, config,  # <--- UPDATED SIGNATURE
                write_to_dir: str, parameters:dict, n_results=90, **kwargs):
    """
    Performs a search, processes results, enriches them with financial data, 
    and saves the output to a CSV file.
    
    Args:
        client: The google.genai.Client object.
        model_id: String, e.g. "gemini-2.0-flash".
        config: The GenerateContentConfig object containing safety settings.
    """
    search_service = build("customsearch", "v1", developerKey=google_key).cse()

    search_results = fetch_search_results(search_service, search_id, query=parameters['search_query'], n_results=n_results, **kwargs)
    
    # Pass the unpacked components down the chain
    processed_results = process_results_with_ai(
        search_results, 
        prompt_template=parameters['prompt_template'], 
        client=client, 
        model_id=model_id, 
        config=config
    )
    
    enriched_results = enrich_with_financial_data(processed_results, polygon_key, parameters)
    format_and_save_results(enriched_results, parameters['search_query'], write_to_dir)
