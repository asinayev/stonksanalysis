from googleapiclient.discovery import build

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
