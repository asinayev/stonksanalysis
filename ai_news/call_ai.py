import json
import datetime

def read_results(all_results, prompt_template, model):
  valid_summaries=[]
  for result in all_results:
    prompt = prompt_template
    if 'title' in result:
      prompt+="\n Title: " + result['title']
    if 'snippet' in result:
      prompt+="\n Snippet from article: " + result['snippet']
    short_prompt=prompt
    prompt+="\n Full article metadata: " + str(result)
    response = model.generate_content(prompt)
    try:
      json_text = response.text[response.text.find("{"):response.text.find("}")+1]
      response_dict = json.loads(json_text)
      response_dict['link']=result['link']
      response_dict['snippet']=result['snippet']
      if(response_dict['newProgram']=='Yes'):
        valid_summaries.append(response_dict)
    except Exception as e:
      print("Issue parsing result: "+ short_prompt)
      print(e)
  return valid_summaries

def enrich_result(result, poly_client):
  result['match']=False
  try:
    matches= poly_client.list_tickers(search=result['companyName'], active=True, type='CS')
    first_match=next(matches)
    details= poly_client.get_ticker_details(ticker=result['ticker'])
    result['market_cap_ok']=details.market_cap<10000000000
    snap= poly_client.get_snapshot_ticker(ticker=result['ticker'], market_type='stocks')
    result['liquidity_ok']=snap.prev_day.close>5 and snap.prev_day.volume>10000
    result['volume']=snap.prev_day.volume
    result['overnight_in_range']=snap.todays_change_percent> -1.75 and snap.todays_change_percent< 9
    result['match']= first_match.ticker==result['ticker'] and result['liquidity_ok'] and result['market_cap_ok'] and result['overnight_in_range']
    for issue in ['liquidity_ok','market_cap_ok','overnight_in_range']:
      result['message']+=['failed '+issue,''][result[issue]
  except Exception as e:
    result['message']='issue getting ticker data'
  return(result)
