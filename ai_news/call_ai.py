import json
import datetime

def read_results(all_results, prompt_template, model):
  overtime=0
  valid_summaries=[]
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
        valid_summaries.append(response_dict)
      pub_time=datetime.datetime.strptime( response_dict['timePublished'], '%m/%d/%Y %I:%M:%S %p' )
      if (datetime.datetime.now()-pub_time) > datetime.timedelta(days=1):
        overtime+=1
      else: overtime=0
    except Exception as e:
      print("Issue parsing result")
      print(e)
    if overtime==3: break
  return valid_summaries

def enrich_result(result, poly_client):
  matches= poly_client.list_tickers(search=result['companyName'], active=True, type='CS')
  first_match=next(matches)
  if first_match.ticker==result['ticker']:
    details= poly_client.get_ticker_details(ticker=result['ticker'])
    result['market_cap_ok']=details.market_cap<10000000000
    snap= poly_client.get_snapshot_ticker(ticker=result['ticker'], market_type='stocks')
    result['liquidity_ok']=snap.prev_day.close>5 and snap.prev_day.volume>10000
    result['volume']=snap.prev_day.volume
    result['overnight_in_range']=snap.todays_change_percent> -1.75 and snap.todays_change_percent< 9
    result['match']= result['liquidity_ok'] and result['market_cap_ok'] and result['overnight_in_range']
  else: result['match']=False
  return(result)
