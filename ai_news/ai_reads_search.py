from googleapiclient.discovery import build
from polygon import RESTClient
import pandas as pd
import search_web
import call_ai

def read_search(google_key, polygon_key, search_id, query, prompt_template, model, write_to_dir, **kwargs):
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
  default_data={
    'date': '03/12/2024',
    'symbol': '',
    'strike_price': '',
    'action': 'BUY',
    'order_type': 'MKT',
    'time_in_force': 'OPG',
    'asset_type': 'liquid'
  }
  print("############################ MATCHING RESULTS: "+query)
  enriched_results = [call_ai.enrich_result(r, poly_client) for r in valid_results]
  already_tracked = pd.DataFrame([default_data])
  for r in enriched_results:
    if r['match'] and not (already_tracked['companyName']==r['companyName']).any():
      model_out=model.generate_content("Reformat the following time as YYYY-MM-DD HH:MM using military time:"+ r['timePublished'])
      r['timePublished'] = model_out.text.strip()
      print(r['timePublished'],r['ticker'],r['link'],r['title'])
      already_tracked=pd.concat([pandas_results, pd.DataFrame([r])], ignore_index=True)
  pandas_results.to_csv(write_to_dir+query.replace('|','_')+'.csv'
  
  print("############################ OTHER RESULTS: "+query)
  for r in enriched_results:
    model_out=model.generate_content("Reformat the following time as YYYY-MM-DD HH:MM using military time:"+ r['timePublished'])
    r['timePublished'] = model_out.text.strip()
    if not r['match'] and r['companyName'] not in already_tracked:
      print(r['timePublished'],r['ticker'],r['message'])
      already_tracked.append(r['companyName'])
