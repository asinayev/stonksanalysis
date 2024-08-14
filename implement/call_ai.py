from googleapiclient.discovery import build
import google.generativeai as genai
import os
import time, datetime
import json

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
  while 'items' in results and search_i<90:
    results = google_search(start=search_i,**kwargs)
    result_log[search_i]=results
    if 'items' in results:
      all_results+=results['items']
      search_i+=10
  return(all_results,result_log)

all_results, logs=all_search_pages(
        search_term='stock buyback', 
        api_key=google_key, 
        cse_id=my_cse_id, 
        sort="date",
        num=10)

prompt_template="The time now is "
prompt_template+=datetime.datetime.now().strftime("%I:%M%p on %B %d, %Y")

prompt_template+=""". Based on the following search result, please answer yes or no about whether this constitues an announcement of a NEW share buyback or share repurchase program in which stocks will be repurchased in the future. If the announcement is not within 24 hours of the current time or simply an update about stock repurchases that already happened, respond no. Please also answer the full name of the company doing the announcement, the ticker and the time it was published. 
Respond in a format like this: {"newProgram":"No","companyName":"Microsoft Corporation","ticker":"MSFT",timePublished:"8/12/2024 3:30:00 PM"}

Here is the search result: 
"""

overtime=0
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
      print(response_dict)
    pub_time=datetime.datetime.strptime( response_dict['timePublished'], '%m/%d/%Y %I:%M:%S %p' )
    if (datetime.datetime.now()-pub_time) > datetime.timedelta(days=1):
      overtime+=1
    else: overtime=0
  except:
    print("Issue parsing result")
  if overtime==3: break
