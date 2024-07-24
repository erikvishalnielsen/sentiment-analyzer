# PYTHON ML SENTIMENT ANALYSIS MODEL
import requests
import json
import sys

def my_api(ticker, start, end):
    """
    Description of what the function does.

    Returns:
    type: Description of the return value.
    """
    # Function body goes here
    # You can perform operations, calculations, etc.

    # Example:
    response = requests.get("https://api.findl.com/v1.0/data/stocks?format=json&apiKey=b88ce70b-7d65-4d39-a92c-8ce68da91965&fields=date,open,close,volume&ticker=" + ticker + "&from=" + start + "&to=" + end) 
    print(response.status_code)
    return(response.json())

if len(sys.argv) != 4:
    print("Usage: python sentiment_ml.py [ticker] [starting_date] [ending_date]")
    sys.exit(1)

ticker = sys.argv[1]
start = sys.argv[2]
end = sys.argv[3]

jsonFile = my_api(ticker, start, end)
file_path = ticker + '_fundamentals.json'  # specify your file path here

with open(file_path, 'w') as file:
    json.dump(jsonFile, file, indent=4)

print(f'JSON data has been written to {file_path}')
