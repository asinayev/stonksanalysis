import os
import time, datetime
import ai_reads_search
import google.generativeai as genai

my_cse_id = os.environ["SEARCHID"]
zacks_search_id = os.environ["ZACKSSEARCHID"]
google_key = os.environ["GOOGLEKEY"]
polygon_key = os.environ["POLYGONKEY"]

genai.configure(api_key=google_key)

model = genai.GenerativeModel(
  model_name='gemini-1.5-flash-latest', 
  safety_settings=[
          {"category": "HARM_CATEGORY_HATE_SPEECH", "threshold": genai.types.HarmBlockThreshold.BLOCK_NONE},
          {"category": "HARM_CATEGORY_HARASSMENT", "threshold": genai.types.HarmBlockThreshold.BLOCK_NONE},
          {"category": "HARM_CATEGORY_SEXUALLY_EXPLICIT", "threshold": genai.types.HarmBlockThreshold.BLOCK_NONE},
          {"category": "HARM_CATEGORY_DANGEROUS_CONTENT", "threshold": genai.types.HarmBlockThreshold.BLOCK_NONE},
      ]
  )

prompt_template="""Based on the following search result, please answer yes or no about whether this constitues a NEW announcement about a {}
Please also answer the full name of the company doing the announcement, the ticker and the time it was published. 
Respond in this JSON format: {{"newProgram":"No","companyName":"Microsoft Corporation","ticker":"MSFT","timePublished":"8/12/2024 3:30:00 PM"}}. 
If any of the fields cannot be determined, write UNKNOWN, for example: {{"newProgram":"No","companyName":"Microsoft Corporation","ticker":"MSFT","timePublished":"UNKNOWN"}}

Here is the search result: 
"""

buyback_description="share buyback or share repurchase program which says more stocks will be repurchased in the future. If the announcement is simply an update about stock repurchases that already happened, respond no. "

ai_reads_search.read_search(google_key, polygon_key, my_cse_id, query='buyback|repurchase share|stock', \
  orTerms='', prompt_template=prompt_template.format(buyback_description), model=model, write_to_dir='/tmp/stonksanalysis/')

guidance_description="improved guidance in the newProgram field, The announcement should say that guidance for an important metric is now higher (better) than previously expected. If the announcement is simply an update about progress on existing guidance, respond no."

ai_reads_search.read_search(google_key, polygon_key, my_cse_id, query='improves|raises|increases guidance', \
  orTerms='', prompt_template=prompt_template.format(guidance_description), model=model, write_to_dir='/tmp/stonksanalysis/')

guidance_description="earnings and revenue change for a specific stock."

ai_reads_search.read_search(google_key, polygon_key, zacks_search_id, query='allintitle: revenue|revenues earnings|earning', \
  orTerms='', prompt_template=prompt_template.format(guidance_description), model=model, write_to_dir='/tmp/stonksanalysis/')
