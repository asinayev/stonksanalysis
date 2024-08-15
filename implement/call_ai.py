from googleapiclient.discovery import build
import google.generativeai as genai
from polygon import RESTClient
import os
import time, datetime
import json

my_cse_id = os.environ["SEARCHID"]
google_key = os.environ["GOOGLEKEY"]
polygon_key = os.environ["POLYGONKEY"]

genai.configure(api_key=google_key)
model = genai.GenerativeModel('gemini-1.5-flash-latest')
client = RESTClient(api_key=polygon_key)

def google_search(api_key, cse_id, **kwargs):
    service = build("customsearch", "v1", developerKey=api_key)
    res = service.cse().list(cx=cse_id, **kwargs).execute()
    return res

def all_search_pages(**kwargs):
  all_results=[]
  results=['items']
  search_i=1
  while 'items' in results and search_i<90:
    results = google_search(start=search_i,**kwargs)
    if 'items' in results:
      all_results+=results['items']
      search_i+=10
  return(all_results)

def read_results(all_results, prompt_template):
  overtime=0
  valid_summaries=[]
  for result in all_results:
    prompt = prompt_template
    if 'title' in result:
      prompt+="\n Title: " + result['title']
    if 'snippet' in result:
      prompt+="\n Snippet from article: " + result['snippet']
    prompt+="\n Full article metadata: " + str(result)
    response = model.generate_content(prompt)
    try:
      json_text = response.text[response.text.find("{"):response.text.find("}")+1]
      response_dict = json.loads(json_text)
      if(response_dict['newProgram']=='Yes'):
        valid_summaries.append(response_dict)
      pub_time=datetime.datetime.strptime( response_dict['timePublished'], '%m/%d/%Y %I:%M:%S %p' )
      if (datetime.datetime.now()-pub_time) > datetime.timedelta(days=1):
        overtime+=1
      else: overtime=0
    except:
      print("Issue parsing result")
    if overtime==3: break
  return valid_summaries

def enrich_result(result):
  matches= client.list_tickers(search=result['companyName'], active=True, type='CS')
  first_match=next(matches)
  if first_match.ticker==result['ticker']:
    details= client.get_ticker_details(ticker=result['ticker'])
    result['market_cap_ok']=details.market_cap<10000000000
    snap= client.get_snapshot_ticker(ticker=result['ticker'], market_type='stocks')
    result['liquidity_ok']=snap.prev_day.close>5 and snap.prev_day.volume>10000
    result['volume']=snap.prev_day.volume
    result['overnight_in_range']=snap.todays_change_percent> -1.75 and snap.todays_change_percent< 9
    result['match']= result['liquidity_ok'] and result['market_cap_ok'] and result['overnight_in_range']
  else: result['match']=False
  return(result)

prompt_template="The time now is "
prompt_template+=datetime.datetime.now().strftime("%I:%M%p on %B %d, %Y")

prompt_template+=""". Based on the following search result, please answer yes or no about whether this constitues a NEW announcement about a share buyback or share repurchase program which says more stocks will be repurchased in the future. If the announcement is not within 23 hours of the current time or simply an update about stock repurchases that already happened, respond no. Please also answer the full name of the company doing the announcement, the ticker and the time it was published. 
Respond in a format like this: {"newProgram":"No","companyName":"Microsoft Corporation","ticker":"MSFT",timePublished:"8/12/2024 3:30:00 PM"}

Here is the search result: 
"""

all_results=all_search_pages(
        api_key=google_key, 
        cse_id=my_cse_id, 
        q='stock',
        orTerms='buyback repurchase',
        sort="date",
        num=10)

print("############################ MATCHING RESULTS")
valid_results = read_results(all_results, prompt_template)
enriched_results = [enrich_result(r) for r in valid_results]
for r in enriched_results:
  if r['match']:
    print(r)

print("############################ OTHER RESULTS")
for r in enriched_results:
  if not r['match']:
    print(r)
