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
Determine if the following search result includes a NEW announcement about a {}  Answer 'yes' or 'no'.

For your response, also extract the following (except the quote, provide info if it is known even if it is not in the announcement):
- Company Full Name
- Stock Ticker Symbol
- Publication Date and Time
- If available, a short quote from the search result that supports your 'yes'/'no' determination.

1. Locate the title. If the title doesn't refer to the company at all, respond no (this is not a new program). If the full text is not available, use the title, not the snippet to determine if you should respond yes or no. 
2. Locate the publication time in the search result's snippet, body or metadata. It may appear in any format like 'days ago' or a datetime with timezone like '1999-07-01T23:21:10-5:00' or other. Reformat to YYYY-MM-DD HH:MM using military time (put 00:00 if no time is provided). Do not convert timezones, so if it says +3:00, that refers to the timezone and you can ignore that.
3. Read the rest of the announcement for the rest of the details. Note that businesswire articles include previous announcements at the bottom, which may show up in the snippet. If the full text is not available, use the title to determine your yes or no answer.

If any of these details cannot be determined, use "UNKNOWN".

Respond in JSON format as shown in this example:
{{"newProgram":"No","companyName":"Microsoft Corporation","ticker":"MSFT","timePublished":"2024-08-12 15:30","quote":"In June, the board announced plans for a new shareholder incentive..."}}

Search result:
"""

buyback_description="an announcement of a share buyback or share repurchase program. If the announcement does not say that stock will be repurchased or bought back IN THE FUTURE, (e.g., is simply an update about stock repurchases that already happened, or reminds shareholders of a previously announced repurchase), respond no."
buyback_parameters={'search_query':'buyback|repurchase share|stock',
                    'prompt_template':prompt_template.format(buyback_description), 
                    'min_close':5, 'min_volume':20000, 'max_market_cap':10000000000, 'min_overnight_pchange':-2.2, 'max_overnight_pchange':9}

ai_reads_search.read_search(google_key, polygon_key, my_cse_id, orTerms='', model=model, write_to_dir='/tmp/stonksanalysis/', \
                           parameters=buyback_parameters)

guidance_description="improved guidance in the newProgram field, The announcement should say that guidance for an important metric is now higher (better) than previously expected. If the announcement is simply an update about progress on existing guidance, respond no."
guidance_parameters={'search_query':'improves|raises|increases guidance|outlook',
                     'prompt_template':prompt_template.format(guidance_description), 
                     'min_close':5, 'min_volume':20000, 'max_market_cap':100000000000, 'min_overnight_pchange':-0.2, 'max_overnight_pchange':20}

ai_reads_search.read_search(google_key, polygon_key, my_cse_id, orTerms='', model=model, write_to_dir='/tmp/stonksanalysis/', \
                           parameters=guidance_parameters)
