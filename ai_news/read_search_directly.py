import os
import sys
import requests
import google.generativeai as genai

def fetch_url_content(url: str) -> str:
    """Fetches and returns the text content of a URL."""
    try:
        response = requests.get(url, timeout=15)
        response.raise_for_status()
        return response.text
    except requests.exceptions.RequestException as e:
        # Log the error for the specific URL and continue to the next
        print(f"Warning: Could not fetch {url}. Error: {e}", file=sys.stderr)
        return None

def get_gemini_response(prompt_with_content: str) -> str:
    """Sends a prompt to the Gemini API and returns the response."""
    try:
        model = genai.GenerativeModel('gemini-2.0-flash')
        response = model.generate_content(prompt_with_content)
        # We'll strip the header from individual responses to manage it ourselves
        # This assumes the model's response includes the header as requested
        response_lines = response.text.strip().split('\n')
        if len(response_lines) > 1 and response_lines[0].strip() == "timePublished,ticker,link,quote":
            return '\n'.join(response_lines[1:])
        return response.text.strip()
    except KeyError:
        sys.exit("Error: GOOGLEKEY environment variable not set.")
    except Exception as e:
        print(f"Warning: Gemini API call failed. Error: {e}", file=sys.stderr)
        return None

def main():
    """Main function to run the script."""
    # Hardcoded list of URLs to process
    urls_to_process = [
        "https://www.prnewswire.com/search/all/?keyword=repurchase",
        "https://www.prnewswire.com/search/all/?keyword=buyback",
        "https://www.globenewswire.com/en/search/keyword/repurchase,buyback/exchange/Nasdaq,NYSE?pageSize=30&page=1"
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

    Respond in CSV format with a header:
    timePublished,ticker,link,quote
    2025-05-01 08:37,MSFT,https://www.example.com,"Microsoft approved a share repurchase program..."

    Search results:
    """

    all_csv_rows = []

    for url in urls_to_process:
        print(f"Fetching content from: {url}")
        url_content = fetch_url_content(url)

        if not url_content:
            continue # Skip to the next URL if fetching failed

        combined_prompt = f"{initial_prompt}\n\n---\n\n{url_content}"

        print(f"Sending prompt for {url} to Gemini...")
        gemini_response = get_gemini_response(combined_prompt)

        if gemini_response:
            all_csv_rows.append(gemini_response)
        print("-" * 25)


    # Define the output directory and file path
    output_dir = "/tmp/stonksanalysis"
    output_path = os.path.join(output_dir, "buybacks2.csv")

    # Ensure the output directory exists, creating it if necessary
    os.makedirs(output_dir, exist_ok=True)

    # Write the combined results to the CSV file
    try:
        with open(output_path, "w", newline="") as f:
            f.write("timePublished,ticker,link,quote\n")
            for row in all_csv_rows:
                f.write(row + "\n")
        print(f"\n✅ Analysis complete. Results saved to: {output_path}")
    except IOError as e:
        print(f"\n❌ Critical Error: Could not write to file {output_path}. Error: {e}", file=sys.stderr)

if __name__ == "__main__":
    main()
