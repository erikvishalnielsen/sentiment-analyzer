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

    newsApiToken = "66a11a43014e19.64423066"

    # Example:
    response = requests.get("https://api.findl.com/v1.0/data/stocks?format=json&apiKey=b88ce70b-7d65-4d39-a92c-8ce68da91965&fields=date,open,close,volume&ticker=" + ticker + "&from=" + start + "&to=" + end) 
    newsData = requests.get("https://eodhd.com/api/news?s=" + ticker + ".US&offset=0&limit=1000&api_token=" + newsApiToken + "&from=" + start + "&to=" + end + "&fmt=json") 
    print(response.status_code)
    print(newsData.status_code)
    return(response.json(),newsData.json())

if len(sys.argv) != 4:
    print("Usage: python sentiment_ml.py [ticker] [starting_date] [ending_date]")
    sys.exit(1)

ticker = sys.argv[1]
start = sys.argv[2]
end = sys.argv[3]

try: 
    open(ticker + "_fundamentals.json", "r")
except IOError:
    jsonFileFund, jsonFileNews = my_api(ticker, start, end)
    file_pathFUND = "data/" + ticker + '_fundamentals.json'  # specify your file path here
    file_pathNEWS = "data/" + ticker + '_news.json'

    with open(file_pathFUND, 'w') as file:
        json.dump(jsonFileFund, file, indent=4)
    print(f'JSON data has been written to {file_pathFUND}')

    with open(file_pathNEWS, 'w') as file:
        json.dump(jsonFileNews, file, indent=4)
    print(f'JSON data has been written to {file_pathNEWS}')


# FILE OPENING
#file = open(ticker + "_fundamentals.json")
#data = json.load(file) # returns list of dicts
