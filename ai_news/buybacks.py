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
  model_name='gemini-2.0-flash', 
  safety_settings=[
          {"category": "HARM_CATEGORY_HATE_SPEECH", "threshold": genai.types.HarmBlockThreshold.BLOCK_NONE},
          {"category": "HARM_CATEGORY_HARASSMENT", "threshold": genai.types.HarmBlockThreshold.BLOCK_NONE},
          {"category": "HARM_CATEGORY_SEXUALLY_EXPLICIT", "threshold": genai.types.HarmBlockThreshold.BLOCK_NONE},
          {"category": "HARM_CATEGORY_DANGEROUS_CONTENT", "threshold": genai.types.HarmBlockThreshold.BLOCK_NONE},
      ]
  )

prompt_template="""
Determine if the following search result is a NEW announcement about a {}  If the title doesn't refer to the company at all, respond no. Answer 'yes' or 'no'.

For your response, also extract the following (except the quote, provide info if it is known even if it is not in the announcement):
- Company Full Name
- Stock Ticker Symbol
- Publication Date and Time
- If available, a key quote from the search result that supports your 'yes'/'no' determination.

Locate the publication time in the search result's snippet, body or metadata. It may appear in any format like 'days ago' or a datetime with timezone like '1999-07-01T23:21:10-5:00' or other. Reformat to YYYY-MM-DD HH:MM using military time (put 00:00 if no time is provided). Do not convert timezones, so if it says +3:00, that refers to the timezone and you can ignore that.

If any of these details cannot be found, use "UNKNOWN".

Respond in JSON format as shown in this example:
{{"newProgram":"No","companyName":"Microsoft Corporation","ticker":"MSFT","timePublished":"2024-08-12 15:30","quote":"In June, the board announced plans for a new shareholder incentive..."}}

Search result:
"""

buyback_description="share buyback or share repurchase program which says more stocks will be repurchased in the future. If the announcement is simply an update about stock repurchases that already happened, respond no."

ai_reads_search.read_search(google_key, polygon_key, my_cse_id, query='buyback|repurchase share|stock', \
  orTerms='', prompt_template=prompt_template.format(buyback_description), model=model, write_to_dir='/tmp/stonksanalysis/')

guidance_description="improved guidance in the newProgram field, The announcement should say that guidance for an important metric is now higher (better) than previously expected. If the announcement is simply an update about progress on existing guidance, respond no."

ai_reads_search.read_search(google_key, polygon_key, my_cse_id, query='improves|raises|increases guidance|outlook', \
  orTerms='', prompt_template=prompt_template.format(guidance_description), model=model, write_to_dir='/tmp/stonksanalysis/')
