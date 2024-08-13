from googleapiclient.discovery import build
import google.generativeai as genai
import os
import time

my_cse_id = os.environ["SEARCHID"]
google_key = os.environ["GOOGLEKEY"]

genai.configure(api_key=google_key)
model = genai.GenerativeModel('gemini-1.5-flash-latest')

def google_search(search_term, api_key, cse_id, **kwargs):
    service = build("customsearch", "v1", developerKey=api_key)
    res = service.cse().list(q=search_term, cx=cse_id, **kwargs).execute()
    return res

def all_search_pages(**kwargs):
  all_results=[]
  result_log={}
  results=['items']
  search_i=1
  while 'items' in results:
    results = google_search(start=search_i,**kwargs)
    result_log[search_i]=results
    if 'items' in results:
      all_results+=results['items']
      search_i+=10
  return(all_results,result_log)

all_results, logs=all_search_pages(
        search_term='announces stock buyback repurchase', 
        api_key=google_key, 
        cse_id=my_cse_id, 
        dateRestrict='d1',
        num=10)

prompt_template = \
"""Based on the following search result, please answer yes or no about whether this constitues an announcement of a NEW share buyback or share repurchase program. Please also answer the full name of the company doing the announcement and the ticker. For example: {"announcedBuyback":"Yes","companyName":"Microsoft Corporation","ticker":"MSFT"}

Here is the search result:
"""

for result in all_results:
  response = model.generate_content(prompt_template+str(result))
  print(response.text)
