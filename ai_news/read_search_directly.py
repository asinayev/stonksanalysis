import requests
import sys
import os
import json # Import json for parsing API responses

def fetch_url_content(url):
    """
    Fetches the content of a given URL.

    Args:
        url (str): The URL of the webpage to fetch.

    Returns:
        str: The content of the webpage, or an error message if fetching fails.
    """
    try:
        # Send a GET request to the specified URL with a timeout
        response = requests.get(url, timeout=15)
        response.raise_for_status() # Raise an HTTPError for bad responses (4xx or 5xx)
        return response.text
    except requests.exceptions.HTTPError as http_err:
        return f"Error fetching URL: HTTP error occurred: {http_err}"
    except requests.exceptions.ConnectionError as conn_err:
        return f"Error fetching URL: Connection error occurred: {conn_err}"
    except requests.exceptions.Timeout as timeout_err:
        return f"Error fetching URL: Timeout occurred while waiting for server: {timeout_err}"
    except requests.exceptions.RequestException as req_err:
        return f"Error fetching URL: An unexpected request error occurred: {req_err}"
    except Exception as e:
        return f"Error fetching URL: An unknown error occurred: {e}"

def get_gemini_response(prompt_with_content):
    """
    Sends the combined prompt and URL content to a Gemini model and returns the response.

    Args:
        prompt_with_content (str): The complete prompt string including URL content.

    Returns:
        str: The text response from the Gemini model, or an error message.
    """
    try:
        api_key = os.environ.get("GOOGLEKEY")
        if not api_key:
            return "Error: GOOGLEKEY environment variable not set. Please set your Gemini API key."

        # chat_history is a list that will hold message objects for the 'contents' array
        chat_history = []
        # Corrected: Use .append() instead of .push() for Python lists
        chat_history.append({
            "role": "user",
            "parts": [{"text": prompt_with_content}]
        })

        payload = {
            "contents": chat_history
        }

        # Using gemini-2.5-flash-preview-04-17 for a fast model, as requested
        api_url = f"https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-preview-04-17:generateContent?key={api_key}"

        headers = {
            'Content-Type': 'application/json'
        }

        print("Sending request to Gemini API...")
        # Send the POST request to the Gemini API with a timeout
        response = requests.post(api_url, headers=headers, data=json.dumps(payload), timeout=90)
        response.raise_for_status() # Raise an HTTPError for bad responses (4xx or 5xx)

        result = response.json()

        # Check for the expected structure of the response
        if result.get("candidates") and len(result["candidates"]) > 0 and \
           result["candidates"][0].get("content") and result["candidates"][0]["content"].get("parts") and \
           len(result["candidates"][0]["content"]["parts"]) > 0:
            return result["candidates"][0]["content"]["parts"][0]["text"]
        else:
            # If the structure is unexpected, print the full response for debugging
            return f"Error: Unexpected response structure from Gemini API. Full response: {json.dumps(result, indent=2)}"

    except requests.exceptions.HTTPError as http_err:
        # Include response text for more detailed HTTP error debugging
        error_detail = f"Response text: {response.text}" if 'response' in locals() else "No response text available."
        return f"Gemini API Error: HTTP error occurred: {http_err}. {error_detail}"
    except requests.exceptions.ConnectionError as conn_err:
        return f"Gemini API Error: Connection error occurred: {conn_err}"
    except requests.exceptions.Timeout as timeout_err:
        return f"Gemini API Error: Timeout occurred while waiting for Gemini: {timeout_err}"
    except requests.exceptions.RequestException as req_err:
        return f"Gemini API Error: An unexpected request error occurred: {req_err}"
    except json.JSONDecodeError as json_err:
        # Catch JSON decoding errors to help diagnose malformed responses
        raw_response = response.text if 'response' in locals() else "N/A"
        return f"Gemini API Error: Could not parse JSON response: {json_err}. Raw response: {raw_response}"
    except Exception as e:
        return f"An unexpected error occurred during Gemini API call: {e}"

if __name__ == "__main__":
    # Check if exactly two command-line arguments are provided
    if len(sys.argv) != 3:
        print("Usage: python your_script_name.py <URL> <Prompt>")
        print("Example: python your_script_name.py https://www.example.com 'Summarize the following content:'")
        sys.exit(1)

    url_to_fetch = sys.argv[1]
    initial_prompt = """Determine if the following search results include NEW announcement about a buyback or repurchase  . Only include results that match this criterion.

For your response, extract the following (except the quote, provide info if it is known even if it is not in the announcement):
- Publication Date and Time
- Stock Ticker Symbol
- The link where the full result can be found
- If available, a short quote from the search result that supports your 'yes'/'no' determination.

1. Locate the title. If the title doesn't refer to the company at all, or has nothing to do with the announcement respond no (this is not a new program). 
2. Locate the publication time in the search result's snippet, body or metadata. It may appear in any format like 'days ago' or a datetime with timezone like '1999-07-01T23:21:10-5:00' or other. Reformat to YYYY-MM-DD HH:MM using military time (put 00:00 if no time is provided). Do not convert timezones, so if it says +3:00, that refers to the timezone and you can ignore that.
3. Read the rest of the announcement for the rest of the details.

If any of these details cannot be determined, use ‘UNKNOWN’.

Respond in CSV format showing all the matches as shown in this example:

timePublished,ticker,link,quote
2025-05-01 08:37,MSFT,https://www.prnewswire.com/news-releases/work-programs-microsoft-302491329.html, "Microsoft approved a share repurchase program for the benefit of its shareholders"

Search results:


"""

    print(f"Fetching content from: {url_to_fetch}")
    url_content = fetch_url_content(url_to_fetch)

    # Exit if there was an error fetching the URL content
    if url_content.startswith("Error fetching URL:"):
        print(url_content)
        sys.exit(1)

    # Combine the initial prompt with the fetched URL content
    combined_prompt = f"{initial_prompt}\n\n---\n\n{url_content}"

    print("\nSending combined prompt to Gemini model...")
    gemini_response = get_gemini_response(combined_prompt)

    print("\n--- Gemini Response ---")
    print(gemini_response)
    print("-----------------------")

