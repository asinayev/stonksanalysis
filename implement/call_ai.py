from googleapiclient.discovery import build
import google.generativeai as genai
import os

my_cse_id = os.environ["SEARCHID"]
genai.configure(api_key=os.environ["GOOGLEKEY"])
model = genai.GenerativeModel('gemini-1.5-flash-latest')

def google_search(search_term, api_key, cse_id, **kwargs):
    service = build("customsearch", "v1", developerKey=os.environ["GOOGLEKEY"])
    res = service.cse().list(q=search_term, cx=cse_id, **kwargs).execute()
    return res['items']

results = google_search(
    'stackoverflow site:en.wikipedia.org', my_api_key, my_cse_id, num=10, dateRestrict='d1')

def gemini_call()

response = model.generate_content("The opposite of hot is")
print(response.text)
