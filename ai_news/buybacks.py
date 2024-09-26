import google.generativeai as genai
import os
import time, datetime
import ai_reads_search

my_cse_id = os.environ["SEARCHID"]
google_key = os.environ["GOOGLEKEY"]
polygon_key = os.environ["POLYGONKEY"]

genai.configure(api_key=google_key)
model = genai.GenerativeModel('gemini-1.5-flash-latest')

prompt_template="The time now is "
prompt_template+=datetime.datetime.now().strftime("%I:%M%p on %B %d, %Y")

prompt_template+=""". Based on the following search result, please answer yes or no about whether this constitues a NEW announcement about a share buyback or share repurchase program which says more stocks will be repurchased in the future. If the announcement is older than 24 hours or simply an update about stock repurchases that already happened, respond no. Please also answer the full name of the company doing the announcement, the ticker and the time it was published. 
Respond in a format like this: {"newProgram":"No","companyName":"Microsoft Corporation","ticker":"MSFT","timePublished":"8/12/2024 3:30:00 PM"}

Here is the search result: 
"""

ai_reads_search.read_search(google_key, polygon_key, my_cse_id, query='stock buyback', \
  orTerms='share repurchase', prompt_template=prompt_template, model=model)


prompt_template="The time now is "
prompt_template+=datetime.datetime.now().strftime("%I:%M%p on %B %d, %Y")

prompt_template+=""" UTC. Based on the following search result, please answer yes or no about whether this constitues a NEW announcement of improved guidance, The announcement should say that guidance for an important metric is now higher than previously expected. If the announcement is from before the close of the previous trading day or simply an update about progress on existing guidance, respond no. Please also answer the full name of the company doing the announcement, the ticker and the time it was published. 
Respond in this JSON format with time formatted as follows: {"newProgram":"No","companyName":"Microsoft Corporation","ticker":"MSFT","timePublished":"8/12/2024 3:30:00 PM"}
p
Here is the search result: 
"""

ai_reads_search.read_search(google_key, polygon_key, my_cse_id, query='guidance', \
  orTerms='improves raises increases', prompt_template=prompt_template, model=model)
