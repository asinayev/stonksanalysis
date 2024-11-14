from googleapiclient.discovery import build
from polygon import RESTClient
import search_web
import call_ai

def read_search(google_key, polygon_key, search_id, query, prompt_template, model, **kwargs):
  search_service = build("customsearch", "v1", developerKey=google_key).cse()
  
  all_results=search_web.all_search_pages(
        service=search_service, 
        cse_id=search_id, 
        q=query,
        sort="date",
        num=10,
        **kwargs)
  
  valid_results = call_ai.read_results(all_results, prompt_template, model)
  poly_client = RESTClient(api_key=polygon_key)

  print("############################ MATCHING RESULTS: "+query)
  enriched_results = [call_ai.enrich_result(r, poly_client) for r in valid_results]
  already_tracked = []
  for r in enriched_results:
    if r['match'] and r['companyName'] not in already_tracked:
      
      r['timePublished'] = model.generate_content("Reformat the following time as YYYY-MM-DD HH:MM using military time:"+ r['timePublished'])
      print(r['timePublished'],r['ticker'],r['link'],r['title'])
      already_tracked.append(r['companyName'])
  
  print("############################ OTHER RESULTS: "+query)
  for r in enriched_results:
    if not r['match'] and r['companyName'] not in already_tracked:
      print(r['timePublished'],r['ticker'],r['message'])
      already_tracked.append(r['companyName'])
