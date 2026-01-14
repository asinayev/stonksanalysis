import re
import json
import html
import logging
import datetime
import requests

# Configure logging
log_file_path = '/tmp/read_search.log'
logging.basicConfig(filename=log_file_path, level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')  # Log at INFO level
logger = logging.getLogger(__name__)

def fetch_article(url):
    """
    Fetches content and extracts main text using Standard Library tools only.
    No BeautifulSoup or Trafilatura required.
    """
    try:
        headers = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
        }
        response = requests.get(url, headers=headers, timeout=15)
        response.raise_for_status()
        raw_html = response.text
        
        # 1. Remove JavaScript and CSS
        # This regex looks for <script>...</script> and <style>...</style> blocks
        # flags=re.DOTALL allows the dot (.) to match newlines
        clean_html = re.sub(r'<(script|style|head|title|noscript)[^>]*>.*?</\1>', ' ', raw_html, flags=re.DOTALL | re.IGNORECASE)
        
        # 2. Remove HTML Comments clean_html = re.sub(r'', ' ', clean_html, flags=re.DOTALL)
        
        # 3. Strip all remaining HTML tags
        # Replaces <any_tag> with a newline to prevent words merging
        text_only = re.sub(r'<[^>]+>', '\n', clean_html)
        
        # 4. Unescape HTML entities (e.g., "&amp;" -> "&", "&nbsp;" -> " ")
        text_only = html.unescape(text_only)
        
        # 5. The "News Heuristic": Filter for content density
        # - Split into lines
        # - Strip whitespace
        # - KEEP only lines that are long enough to be a sentence (e.g., > 50 chars)
        #   or lines that end in punctuation (to catch short dialogue or headers)
        lines = []
        for line in text_only.splitlines():
            clean_line = line.strip()
            # Threshold: 50 chars is a good balance for financial news
            if len(clean_line) > 50: 
                lines.append(clean_line)
        return '\n'.join(lines)
    except Exception as e:
        logger.error(f"Error extracting content from {url}: {e}")
        return None


def read_results(all_results, prompt_template, client, model_id, config):
    """
    Read and process AI-generated content for all results.
    
    Args:
        all_results (list): List of search result dictionaries.
        prompt_template (str): The prompt string with placeholders.
        client (genai.Client): The initialized Google GenAI Client.
        model_id (str): The model ID string (e.g., 'gemini-2.0-flash').
        config (types.GenerateContentConfig): Configuration object with safety settings.
    """
    valid_summaries = []
    
    for result in all_results:
        result['message'] = ''
        
        # Ensure your create_prompt function exists in this module or is imported
        prompt = create_prompt(prompt_template, result)
        
        try:
            # NEW SDK CALL
            response = client.models.generate_content(
                model=model_id,
                contents=prompt,
                config=config
            )
        except Exception as e:
            # The new SDK raises google.genai.errors.ClientError or similar
            logger.error(f"model did not respond: {e}")
            continue
            
        # Ensure your parse_response function handles the new response object
        # (response.text is still the standard way to access content)
        summary = parse_response(response, result)
        
        if summary:
            valid_summaries.append(summary)
            
    return valid_summaries

def create_prompt(prompt_template, result):
    """Create a prompt from the template and result data."""
    
    if 'link' in result:
        full_article = fetch_article(result['link'])
    else: full_article = None
    
    if full_article:
        search_data = (
            f"\n\nSearch Result Details:\n"
            f"Title: {result.get('title', 'Unknown')}\n"
            f"Snippet: {result.get('snippet', 'Unknown')}\n"
            f"Link: {result.get('link', 'Unknown')}\n"
            f"Full Article: {full_article}\n"
        )
    else:
        search_data = (
            f"\n\nSearch Result Details:\n"
            f"Title: {result.get('title', 'Unknown')}\n"
            f"Link: {result.get('link', 'Unknown')}\n"
    return prompt_template + search_data


def parse_response(response, result):
    """Parse the response from the model and extract relevant data."""
    try:
        raw_text = response.text
        if not raw_text:
            return None

        clean_text = re.sub(r"```json\s*|\s*```", "", raw_text).strip()
        ai_data = json.loads(clean_text)
        result.update(ai_data)
        return result
        
    except json.JSONDecodeError:
        logger.error(f"Failed to parse JSON. Raw text: {raw_text[:100]}...")
        return None
    except Exception as e:
        logger.error(f"Error processing response: {e}")
        return None

def enrich_result(result, poly_client, parameters):
    """Enrich the result with financial data and match criteria."""
    result['match'] = False
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
            'market_cap_ok': market_cap < parameters['max_market_cap'],
            'volume': snap.prev_day.volume,
            'current': snap.prev_day.close + snap.todays_change,
            'liquidity_ok': snap.prev_day.close > parameters['min_close'] and snap.prev_day.volume > parameters['min_volume'],
            'overnight_in_range': parameters['min_overnight_pchange'] < snap.todays_change_percent < parameters['max_overnight_pchange'],
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
