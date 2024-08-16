import google.generativeai as genai
from polygon import RESTClient
import search_web
import call_ai
import os
import time, datetime

my_cse_id = os.environ["SEARCHID"]
google_key = os.environ["GOOGLEKEY"]
polygon_key = os.environ["POLYGONKEY"]

genai.configure(api_key=google_key)
model = genai.GenerativeModel('gemini-1.5-flash-latest')
client = RESTClient(api_key=polygon_key)

prompt_template="The time now is "
prompt_template+=datetime.datetime.now().strftime("%I:%M%p on %B %d, %Y")

prompt_template+=""". Based on the following search result, please answer yes or no about whether this constitues a NEW announcement about a share buyback or share repurchase program which says more stocks will be repurchased in the future. If the announcement is not within 23 hours of the current time or simply an update about stock repurchases that already happened, respond no. Please also answer the full name of the company doing the announcement, the ticker and the time it was published. 
Respond in a format like this: {"newProgram":"No","companyName":"Microsoft Corporation","ticker":"MSFT",timePublished:"8/12/2024 3:30:00 PM"}

Here is the search result: 
"""

all_results=search_web.all_search_pages(
        api_key=google_key, 
        cse_id=my_cse_id, 
        q='stock',
        orTerms='buyback repurchase',
        sort="date",
        num=10)

valid_results = call_ai.read_results(all_results, prompt_template, model)
print("############################ MATCHING RESULTS: BUYBACKS")
enriched_results = [call_ai.enrich_result(r, client) for r in valid_results]
for r in enriched_results:
  if r['match']:
    print(r)

print("############################ OTHER RESULTS")
for r in enriched_results:
  if not r['match']:
    print(r)



prompt_template="The time now is "
prompt_template+=datetime.datetime.now().strftime("%I:%M%p on %B %d, %Y")

prompt_template+=""". Based on the following search result, please answer yes or no about whether this constitues a NEW dividend increase announcement which says a larger dividend will be paid than previously planned. If the announcement is not within 23 hours of the current time or simply an update about plans to pay a dividend of an already-known size, respond no. Please also answer the full name of the company doing the announcement, the ticker and the time it was published. 
Respond in a format like this: {"newProgram":"No","companyName":"Microsoft Corporation","ticker":"MSFT",timePublished:"8/12/2024 3:30:00 PM"}

Here is the search result: 
"""

all_results=search_web.all_search_pages(
        api_key=google_key, 
        cse_id=my_cse_id, 
        q='dividend increase',
        orTerms='buyback repurchase',
        sort="date",
        num=10)

valid_results = call_ai.read_results(all_results, prompt_template, model)
print("############################ MATCHING RESULTS: DIVIDEND INCREASE")
enriched_results = [call_ai.enrich_result(r, client) for r in valid_results]
for r in enriched_results:
  if r['match']:
    print(r)

print("############################ OTHER RESULTS")
for r in enriched_results:
  if not r['match']:
    print(r)
