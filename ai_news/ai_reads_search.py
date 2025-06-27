import os
import io
import csv
import sys
import requests
import google.generativeai as genai

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
    Sends a prompt to the Gemini API and returns the response as a list of CSV rows.
    It cleans markdown formatting and removes headers from the model's response.
    """
    try:
        model = genai.GenerativeModel('gemini-1.5-flash')
        response = model.generate_content(prompt_with_content)
        
        # Clean the response text from markdown code blocks
        clean_text = response.text.strip()
        if clean_text.startswith("```csv"):
            clean_text = clean_text[5:]
        if clean_text.endswith("```"):
            clean_text = clean_text[:-3]
        clean_text = clean_text.strip()

        # Use the csv module to parse the string
        # io.StringIO treats the string as a file
        f = io.StringIO(clean_text)
        reader = csv.reader(f)
        
        all_rows = list(reader)

        # Filter out any header rows from the response
        header = ['timePublished', 'ticker', 'link', 'quote']
        data_rows = [row for row in all_rows if row and row[0].strip().lower() != 'timepublished']
        
        return data_rows

    except Exception as e:
        # Log errors from the API call and return an empty list
        print(f"Warning: Gemini API call failed or response parsing failed. Error: {e}", file=sys.stderr)
        return []

def main():
    """
    Main function to fetch content from multiple URLs, process it with Gemini,
    sort the results by date, and save them to a single CSV file.
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

    For your response, extract the following:
    - Publication Date and Time
    - Stock Ticker Symbol
    - The link where the full result can be found
    - If available, a short quote from the search result that supports your 'yes'/'no' determination.

    1. Locate the title. If the title doesn't refer to the company or the announcement, respond 'no'.
    2. Format the publication time to YYYY-MM-DD HH:MM (use 00:00 if no time is provided). Do not convert timezones.
    3. Read the announcement for the remaining details.

    If any details are unknown, use 'UNKNOWN'.

    Respond ONLY with the data in CSV format with a header, do not add any other text or formatting:
    timePublished,ticker,link,quote
    2025-05-01 08:37,MSFT,https://www.example.com,"Microsoft approved a share repurchase program..."

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
        # The function now returns a list of lists (rows)
        gemini_rows = get_gemini_response(combined_prompt)

        if gemini_rows:
            all_data_rows.extend(gemini_rows)
        
        print("-" * 25)

    # Sort the collected data by the first column (timePublished)
    # The YYYY-MM-DD HH:MM format is lexicographically sortable
    try:
        all_data_rows.sort(key=lambda row: row[0])
    except IndexError:
        print("Warning: Could not sort data, some rows may be malformed.", file=sys.stderr)


    # Define the output directory and file path
    output_dir = "/tmp/stonksanalysis"
    output_path = os.path.join(output_dir, "buybacks2.csv")

    os.makedirs(output_dir, exist_ok=True)

    # Write the sorted results to the CSV file using the csv module
    try:
        with open(output_path, "w", newline="", encoding='utf-8') as f:
            writer = csv.writer(f)
            # Write the header first
            writer.writerow(['timePublished', 'ticker', 'link', 'quote'])
            # Write all the sorted data rows
            writer.writerows(all_data_rows)
        print(f"\n✅ Analysis complete. Results sorted and saved to: {output_path}")
    except IOError as e:
        print(f"\n❌ Critical Error: Could not write to file {output_path}. Error: {e}", file=sys.stderr)


if __name__ == "__main__":
    main()
