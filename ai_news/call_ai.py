import json
import logging
import datetime
import requests
import google.generativeai as genai

# Configure logging
log_file_path = '/tmp/read_search.log'
logging.basicConfig(filename=log_file_path, level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')  # Log at INFO level
logger = logging.getLogger(__name__)

def fetch_article(url):
    """Fetches content from a URL and extracts the main text."""
    try:
        # Add headers to mimic a browser, some sites block simple requests
        headers = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
        }
        response = requests.get(url, headers=headers, timeout=15) # Increased timeout
        response.raise_for_status() # Raise HTTPError for bad responses (4xx or 5xx)
        return response.text
    except requests.exceptions.RequestException as e:
        logger.error(f"Error fetching URL {url}: {e}")
        return None
    except Exception as e:
        logger.error(f"Error parsing URL {url}: {e}")
        return None

def read_results(all_results, prompt_template, model):
    """Read and process AI-generated content for all results."""
    valid_summaries = []
    
    for result in all_results:
        prompt = create_prompt(prompt_template, result)
        response = model.generate_content(prompt, request_options=genai.types.RequestOptions(timeout=5))
        summary = parse_response(response, result)
        if summary:
            valid_summaries.append(summary)
    return valid_summaries

def create_prompt(prompt_template, result):
    """Create a prompt from the template and result data."""
    prompt = prompt_template
    if 'title' in result:
        prompt += f"\n Title: {result['title']}"
    if 'snippet' in result:
        prompt += f"\n Snippet from article: {result['snippet']}"
    if 'link' in result:
        prompt += f"\n Full article HTML: {fetch_article(result['link'])}" 
    else:
        prompt += f"\n Full article metadata: {result}"
    return prompt

def parse_response(response, result):
    """Parse the response from the model and extract relevant data."""
    try:
        json_text = response.text[response.text.find("{"):response.text.find("}")+1]
        response_dict = json.loads(json_text)
        response_dict['link'] = result['link']
        response_dict['title'] = result['title']
        return response_dict
    except (json.JSONDecodeError, AttributeError, TypeError) as e: # More specific exceptions
        title = result.get('title', 'No Title')
        logger.error(f"Issue parsing result for title: {title}. Error: {e}")
        logger.debug(f"Response text: {response.text if hasattr(response, 'text') else 'No response text'}") # Log the full response text at debug level
        return None

def enrich_result(result, poly_client):
    """Enrich the result with financial data and match criteria."""
    result['match'] = False
    result['message'] = ''
    if not result['ticker'] or result['ticker']=='UNKNOWN':
        logger.exception(f"No ticker") 
        result['message'] += f'No ticker'
        if 'companyName' in result: result['ticker']="_"+str(result['companyName'])
        return result
    try:
        first_match=""
        names_to_try=[result['companyName'],
            ''.join(ch for ch in result['companyName'] if ch.isalnum() or ch==" "), #remove non letter number whitespace
            result['companyName'].rsplit(' ', 1)[0],
            result['companyName'].split(' ')[0]
                        ]
        for n in names_to_try:
            matches = poly_client.list_tickers(search=n, active=True, type='CS')
            try:
                first_match=next(matches)
                assert(first_match.ticker == result['ticker'])
                break
            except:
                continue
        if not first_match or first_match.ticker != result['ticker']:
            result['message'] += f'No good match in polygon for ticker'
        market_cap = poly_client.get_ticker_details(ticker=result['ticker']).market_cap
        snap = poly_client.get_snapshot_ticker(ticker=result['ticker'], market_type='stocks')
    except TimeoutError as e:
        logger.exception(f"Ticker timed out. {result['companyName']}: {result['ticker']}") 
        result['message'] += f'Ticker timed out'
    except Exception as e:
        logger.exception(f"Ticker not found. {result['companyName']}: {result['ticker']}") 
        result['message'] += f'Issue getting match in Polygon'
        return result
    try:
        result.update({
            'market_cap_ok': market_cap < 10000000000,
            'volume': snap.prev_day.volume,
            'current': snap.prev_day.close + snap.todays_change,
            'liquidity_ok': snap.prev_day.close > 5 and snap.prev_day.volume > 10000,
            'overnight_in_range': -1.75 < snap.todays_change_percent < 9,
        })
        result['message'] += generate_message(result)
        result['match']=first_match and first_match.ticker == result['ticker'] and result['liquidity_ok'] and result['market_cap_ok'] and result['overnight_in_range']
        if result['message']=="No good match in polygon for ticker":
            result['message']="DOUBLE CHECK THIS TICKER"
            result['match']=True
    except Exception as e:
        logger.exception(f"Issue getting polygon data for ticker: {result['ticker']}") # Use logger.exception to log stack trace
        result['message'] += f'Issue getting ticker data'
    return result

def generate_message(result):
    """Generate a message based on result criteria checks."""
    issues = ['liquidity_ok', 'market_cap_ok', 'overnight_in_range']
    messages = [f'failed {issue}' for issue in issues if not result[issue]]
    return ' '.join(messages)
