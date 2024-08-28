def all_search_pages(service, cse_id, **kwargs):
  all_results=[]
  results=['items']
  search_i=1
  while 'items' in results and search_i<90:
    results = service.list(cx=cse_id, start=search_i, **kwargs).execute()
    if 'items' in results:
      all_results+=results['items']
      search_i+=10
    else: break
  return(all_results)
