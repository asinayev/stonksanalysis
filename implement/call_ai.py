import google.generativeai as genai
import os

genai.configure(api_key=os.environ["GOOGLEKEY"])

model = genai.GenerativeModel('gemini-1.5-flash-latest')
response = model.generate_content("The opposite of hot is")
print(response.text)
