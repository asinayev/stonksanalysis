import os
import sys
import requests
import google.generativeai as genai
import csv
import json
import io

def fetch_url_content(url: str) -> str:
    """
    Fetches and returns the text content of a URL.
    If fetching fails, it prints a warning and returns None.
    """
    try:
        response = requests.get(url, timeout=15)
        response.raise_for_status()  # Raise an exception for bad status codes (4xx or 5xx)
        return response.text
    except requests.exceptions.RequestException as e:
        # Log the error for the specific URL and continue to the next
        print(f"Warning: Could not fetch {url}. Error: {e}", file=sys.stderr)
        return None

def get_gemini_response(prompt_with_content: str) -> list:
    """
    Sends a prompt to the Gemini API requesting JSON and returns the response
    as a list of dictionaries.
    """
    try:
        model = genai.GenerativeModel('gemini-1.5-flash')
        response = model.generate_content(prompt_with_content)
        
        # Clean the response text from markdown code blocks
        clean_text = response.text.strip()
        if clean_text.startswith("```json"):
            clean_text = clean_text[7:]
        if clean_text.endswith("```"):
            clean_text = clean_text[:-3]
        clean_text = clean_text.strip()

        # Parse the JSON string into a Python list of dictionaries
        parsed_json = json.loads(clean_text)
        
        # The prompt asks for a list of objects, so we expect a list
        if isinstance(parsed_json, list):
            return parsed_json
        # If the API returns a single object not in a list for some reason, wrap it
        elif isinstance(parsed_json, dict):
            return [parsed_json]
        else:
            return []


    except json.JSONDecodeError as e:
        print(f"Warning: Failed to decode JSON from Gemini response. Error: {e}\nResponse text: '{response.text}'", file=sys.stderr)
        return []
    except Exception as e:
        # Log other errors from the API call and return an empty list
        print(f"Warning: Gemini API call failed or response parsing failed. Error: {e}", file=sys.stderr)
        return []

def main():
    """
    Main function to fetch content from multiple URLs, process it with Gemini
    requesting JSON, parse the JSON, sort the results by date, and save
    them to a single CSV file.
    """
    try:
        # Configure the API key once at the start
        genai.configure(api_key=os.environ["GOOGLEKEY"])
    except KeyError:
        sys.exit("Error: GOOGLEKEY environment variable not set. Please set your Gemini API key.")

    # Hardcoded list of URLs to process
    urls_to_process = [
        "https://www.example-news.com/article123",
        "https://www.finance-press.com/news-story-456",
        "https://www.market-update.com/789"
    ]

    initial_prompt = """
    Determine if the following search results include a NEW announcement about a buyback or repurchase.
    Only include results that match this criterion.

    For your response, extract the following for each match:
    - "timePublished": The publication date and time.
    - "ticker": The stock ticker symbol.
    - "link": The link where the full result can be found.
    - "quote": A short quote from the search result that supports your 'yes'/'no' determination.

    1. Locate the title. If the title doesn't refer to the company or the announcement, respond 'no'.
    2. Format the publication time to "YYYY-MM-DD HH:MM" (use "00:00" if no time is provided). Do not convert timezones.
    3. Read the announcement for the remaining details.

    If any details cannot be determined, use "UNKNOWN".

    Respond ONLY with the data in a valid JSON array of objects. Do not add any other text, explanations, or markdown formatting.
    Example of the expected JSON format:
    [
        {
            "timePublished": "2025-05-01 08:37",
            "ticker": "MSFT",
            "link": "https://www.example.com",
            "quote": "Microsoft approved a share repurchase program..."
        },
        {
            "timePublished": "2025-04-29 16:30",
            "ticker": "GOOG",
            "link": "https://www.otherexample.com",
            "quote": "The board authorized a repurchase of an additional $70 billion of its Class A and Class C shares."
        }
    ]

    Search results:
    """

    all_data_rows = []

    for url in urls_to_process:
        print(f"Processing URL: {url}")
        url_content = fetch_url_content(url)

        if not url_content:
            print(f"Skipping {url} due to fetch error.")
            continue

        combined_prompt = f"{initial_prompt}\n\n---\n\n{url_content}"

        print(f"Sending prompt for {url} to Gemini...")
        # The function now returns a list of dictionaries
        gemini_json_response = get_gemini_response(combined_prompt)

        if gemini_json_response:
            all_data_rows.extend(gemini_json_response)
        
        print("-" * 25)

    # Sort the collected data by the 'timePublished' key in each dictionary
    try:
        # The YYYY-MM-DD HH:MM format is lexicographically sortable
        all_data_rows.sort(key=lambda row: row.get('timePublished', ''))
    except (IndexError, AttributeError) as e:
        print(f"Warning: Could not sort data, some rows may be malformed. Error: {e}", file=sys.stderr)


    # Define the output directory and file path
    output_dir = "/tmp/stonksanalysis"
    output_path = os.path.join(output_dir, "buybacks2.csv")

    os.makedirs(output_dir, exist_ok=True)

    # Write the sorted results to the CSV file using csv.DictWriter
    try:
        with open(output_path, "w", newline="", encoding='utf-8') as f:
            # Define the header based on the JSON keys
            header = ['timePublished', 'ticker', 'link', 'quote']
            writer = csv.DictWriter(f, fieldnames=header)
            
            # Write the header first
            writer.writeheader()
            # Write all the sorted data rows
            writer.writerows(all_data_rows)
            
        print(f"\n✅ Analysis complete. Results sorted and saved to: {output_path}")
    except IOError as e:
        print(f"\n❌ Critical Error: Could not write to file {output_path}. Error: {e}", file=sys.stderr)


if __name__ == "__main__":
    main()
