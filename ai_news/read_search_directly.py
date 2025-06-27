import os
import sys
import requests
import google.generativeai as genai
import csv
import json

def fetch_url_content(url: str) -> str | None:
    """
    Fetches and returns the text content of a URL.

    Args:
        url: The URL to fetch.

    Returns:
        The text content of the URL, or None if fetching fails.
    """
    try:
        response = requests.get(url, timeout=15)
        response.raise_for_status()  # Raise an exception for bad status codes
        return response.text
    except requests.exceptions.RequestException as e:
        print(f"Warning: Could not fetch {url}. Error: {e}", file=sys.stderr)
        return None

def get_gemini_response(prompt_with_content: str) -> list:
    """
    Sends a prompt to the Gemini API and parses the JSON response.

    Args:
        prompt_with_content: The full prompt including the fetched content.

    Returns:
        A list of dictionaries from the JSON response.
    """
    try:
        model = genai.GenerativeModel('gemini-1.5-flash')
        response = model.generate_content(prompt_with_content)

        # Clean markdown code blocks from the response text
        clean_text = response.text.strip()
        clean_text = clean_text.removeprefix("```json").removesuffix("```").strip()

        # Parse the JSON string into a Python list of dictionaries
        parsed_json = json.loads(clean_text)

        # Ensure the output is always a list of dictionaries
        if isinstance(parsed_json, dict):
            return [parsed_json]
        if isinstance(parsed_json, list):
            return parsed_json
        return []

    except json.JSONDecodeError as e:
        print(f"Warning: Failed to decode JSON from Gemini. Error: {e}\nResponse: '{response.text}'", file=sys.stderr)
        return []
    except Exception as e:
        print(f"Warning: Gemini API call failed. Error: {e}", file=sys.stderr)
        return []

def main():
    """
    Main function to fetch data, process with Gemini, and save to a CSV file.
    """
    api_key = os.environ.get("GOOGLEKEY")
    if not api_key:
        sys.exit("Error: GOOGLEKEY environment variable is not set. Please provide your API key.")
    genai.configure(api_key=api_key)

    urls_to_process = [
        "https://www.prnewswire.com/search/all/?keyword=repurchase",
        "https://www.prnewswire.com/search/all/?keyword=buyback",
        "https://www.globenewswire.com/en/search/keyword/repurchase,buyback/exchange/Nasdaq,NYSE?pageSize=30&page=1"
    ]

    initial_prompt = """
    Determine if the following search results include a NEW announcement about a buyback or repurchase.
    Only include results that match this criterion.

    For your response, extract the following for each match:
    - "timePublished": The publication date and time.
    - "ticker": The stock ticker symbol.
    - "link": The link where the full result can be found.
    - "quote": A short quote from the search result that supports your determination.

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
        gemini_json_response = get_gemini_response(combined_prompt)

        if gemini_json_response:
            all_data_rows.extend(gemini_json_response)
        
        print("-" * 25)

    # Sort data by publication time in descending order
    all_data_rows.sort(key=lambda row: row.get('timePublished', ''), reverse=True)

    # Define the output directory and file path
    output_dir = "/tmp/stonksanalysis"
    output_path = os.path.join(output_dir, "buybacks2.csv")
    os.makedirs(output_dir, exist_ok=True)

    # Write the sorted results to a CSV file
    try:
        with open(output_path, "w", newline="", encoding='utf-8') as f:
            header = ['timePublished', 'ticker', 'link', 'quote']
            writer = csv.DictWriter(f, fieldnames=header)
            writer.writeheader()
            writer.writerows(all_data_rows)
        print(f"\n✅ Analysis complete. Results sorted and saved to: {output_path}")
    except IOError as e:
        print(f"\n❌ Critical Error: Could not write to file {output_path}. Error: {e}", file=sys.stderr)

if __name__ == "__main__":
    main()
