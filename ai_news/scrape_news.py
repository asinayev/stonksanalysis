import requests
import re
import logging
from urllib.parse import urljoin, quote

# Configure logging
logger = logging.getLogger(__name__)

def fetch_html(url):
    try:
        # Use a realistic User-Agent to avoid 403s
        headers = {
            'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
            'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8',
            'Accept-Language': 'en-US,en;q=0.9'
        }
        response = requests.get(url, headers=headers, timeout=15)
        response.raise_for_status()
        return response.text
    except Exception as e:
        logger.error(f"Error fetching {url}: {e}")
        return None

def scrape_prnewswire(keyword):
    """Scrapes PR Newswire search results."""
    # Keyword needs to be URL encoded if it contains spaces or special chars, but PRNewswire handles simple spaces well usually.
    # The existing URL structure is: https://www.prnewswire.com/search/all/?keyword=buyback
    
    # Prnewswire seems to use "+" for spaces in some contexts or just url encoded. 
    # Let's simple use requests param encoding or just quote it.
    
    base_url = "https://www.prnewswire.com/search/all/"
    # We construct the query string manually or via params to ensure control
    # Using ?keyword=...&pagesize=25&sortby=Date
    
    url = f"{base_url}?keyword={quote(keyword)}&pagesize=25&sortby=Date"
    
    html_content = fetch_html(url)
    if not html_content:
        return []

    results = []
    # Pattern: <a href="/news-releases/..." ...>Title</a>
    matches = re.findall(r'<a\s+[^>]*href="(/news-releases/[^"]+)"[^>]*>(.*?)</a>', html_content, re.IGNORECASE | re.DOTALL)
    
    for link, title_raw in matches:
        title = re.sub(r'<[^>]+>', '', title_raw).strip() # Remove any internal tags like <span>
        if not title: continue
        full_link = urljoin("https://www.prnewswire.com", link)
        results.append({'title': title, 'link': full_link, 'source': 'PR Newswire'})
        
    logger.info(f"Found {len(results)} results from PR Newswire for '{keyword}'")
    return results

def scrape_businesswire(keyword):
    """Scrapes Business Wire search results."""
    # https://www.businesswire.com/portal/site/home/search/?searchType=all&searchTerm=buyback&searchPage=1
    url = f"https://www.businesswire.com/portal/site/home/search/?searchType=all&searchTerm={quote(keyword)}&searchPage=1"
    
    html_content = fetch_html(url)
    if not html_content:
        return []

    results = []
    # Business Wire: <a class="bwTitleLink" href="..."> <span itemprop="headline">Title</span> </a>
    matches = re.findall(r'<a\s+[^>]*href="([^"]+)"[^>]*class="bwTitleLink"[^>]*>.*?<span itemprop="headline">([^<]+)</span>', html_content, re.DOTALL | re.IGNORECASE)
    
    if not matches:
        matches = re.findall(r'<a\s+class="bwTitleLink"\s+href="([^"]+)">\s*<span itemprop="headline">(.*?)</span>', html_content, re.DOTALL | re.IGNORECASE)

    for link, title in matches:
        title = title.strip()
        results.append({'title': title, 'link': link, 'source': 'Business Wire'})

    logger.info(f"Found {len(results)} results from Business Wire for '{keyword}'")
    return results

def scrape_globenewswire(keyword):
    """Scrapes GlobeNewswire search results."""
    # https://www.globenewswire.com/en/search/keyword/repurchase%20share%20buyback?pageSize=25
    # Globe uses path-like or query-like structure? The original was: /en/search/keyword/repurchase%20share%20buyback
    # Let's follow that pattern.
    
    encoded_keyword = quote(keyword)
    url = f"https://www.globenewswire.com/en/search/keyword/{encoded_keyword}?pageSize=25"
    
    html_content = fetch_html(url)
    if not html_content:
        return []

    results = []
    # GlobeNewswire: <a data-autid="article-url" href="/news-release/..." ...>Title</a>
    matches = re.findall(r'<a\s+[^>]*href="(/news-release/[^"]+)"[^>]*data-autid="article-url"[^>]*>(.*?)</a>', html_content, re.IGNORECASE | re.DOTALL)
    
    for link, title_raw in matches:
        title = re.sub(r'<[^>]+>', '', title_raw).strip()
        full_link = urljoin("https://www.globenewswire.com", link)
        results.append({'title': title, 'link': full_link, 'source': 'GlobeNewswire'})

    logger.info(f"Found {len(results)} results from GlobeNewswire for '{keyword}'")
    return results

def get_latest_news(keyword):
    """Aggregates news from all sources for a given keyword."""
    all_news = []
    all_news.extend(scrape_prnewswire(keyword))
    all_news.extend(scrape_businesswire(keyword))
    all_news.extend(scrape_globenewswire(keyword))
    
    # Deduplicate by link
    unique_news = {item['link']: item for item in all_news}.values()
    return list(unique_news)

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    # Test with a simple keyword
    news = get_latest_news("buyback")
    for item in news:
        print(f"[{item['source']}] {item['title']} - {item['link']}")
