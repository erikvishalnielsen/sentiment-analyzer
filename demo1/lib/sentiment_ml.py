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
    priceData = requests.get("https://api.polygon.io/v2/aggs/ticker/" + ticker + "/range/1/day/" + start + "/" + end + "?adjusted=true&sort=asc&apiKey=SKIoSufYtDsfh8YitV2Ue5ozoWDgERT") 
    newsData = requests.get("https://eodhd.com/api/news?s=" + ticker + ".US&offset=0&limit=1000&api_token=" + newsApiToken + "&from=" + start + "&to=" + end + "&fmt=json") 
    print(priceData.status_code)
    print(newsData.status_code)
    return(priceData.json(),newsData.json())

if len(sys.argv) != 5:
    print("Usage: python sentiment_ml.py [ticker] [starting_date] [ending_date] [max_search]")
    sys.exit(1)

ticker = sys.argv[1]
start = sys.argv[2]
end = sys.argv[3]
max_search = sys.argv[4]

try: 
    open("data/" + ticker + "_fundamentals.json", "r")
except IOError:
    jsonFileFund, jsonFileNews = my_api(ticker, max_search, end)
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
