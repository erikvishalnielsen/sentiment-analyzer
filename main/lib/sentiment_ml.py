# PYTHON ML SENTIMENT ANALYSIS MODEL
import requests
import json
import sys
import os
import re
from dotenv import load_dotenv, dotenv_values
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
from datetime import datetime, timedelta
import pathlib
import textwrap

import google.generativeai as genai

from IPython.display import display
from IPython.display import Markdown


def to_markdown(text):
  text = text.replace('•', '  *')
  return Markdown(textwrap.indent(text, '> ', predicate=lambda _: True))
  
load_dotenv()
apiKey = os.getenv("GOOGLE_API_KEY")
genai.configure(api_key=apiKey)
model = genai.GenerativeModel('gemini-1.5-flash')

def previous_business_day(date):
    # Define the number of days to subtract to go back one day
    one_day = timedelta(days=1)
    # Initialize a variable to keep track of the number of days subtracted
    days_subtracted = 0
    # Loop until we find a business day (not Saturday or Sunday)
    while True:
        # Subtract one day from the current date
        date -= one_day
        # Increment the counter
        days_subtracted += 1
        # Check if the current date is a weekday (Monday to Friday)
        if date.weekday() < 5:
            # Return the date if it's a weekday
            return date

def my_api(ticker, start, end):
    """
    Description of what the function does.

    Returns:
    type: Description of the return value.
    """
    # Function body goes here
    # You can perform operations, calculations, etc.
    ticker = ticker.upper()

    newsApiTokenErik = "66aa4b2f971169.77002381"
    newsApiTokenSam = "66a11a43014e19.64423066"
    # Example:
    priceData = requests.get("https://api.polygon.io/v2/aggs/ticker/" + ticker + "/range/1/day/" + start + "/" + end + "?adjusted=true&sort=desc&apiKey=SKIoSufYtDsfh8YitV2Ue5ozoWDgERT_") 
    newsData = requests.get("https://eodhd.com/api/news?s=" + ticker + ".US&offset=0&limit=1000&api_token=" + newsApiTokenErik + "&from=" + start + "&to=" + end + "&fmt=json") 
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
    open(f"data/{ticker}_{end}_fundamentals.json", "r")
except IOError:
        directory_path = 'data/'
        # Iterate through all files and directories in the specified directory
        for filename in os.listdir(directory_path):
            # Create the full file path
            file_path = os.path.join(directory_path, filename)
    
            # Check if it's a file (not a directory)
            if os.path.isfile(file_path):
                filenameParts = re.split(r'[/_]', file_path)
                if filenameParts[1] == ticker:
                    os.remove(file_path)
                    print(f"The file {file_path} has been deleted.")

        jsonFileFund, jsonFileNews = my_api(ticker, max_search, end)

        if jsonFileFund["resultsCount"] != 0:
            file_pathFUND = "data/" + ticker + '_' + end + '_fundamentals.json'  # specify your file path here
            file_pathNEWS = "data/" + ticker + '_' + end + '_news.json'

        with open(file_pathFUND, 'w') as file:
            json.dump(jsonFileFund, file, indent=4)
        print(f'JSON data has been written to {file_pathFUND}')

        with open(file_pathNEWS, 'w') as file:
            json.dump(jsonFileNews, file, indent=4)
        print(f'JSON data has been written to {file_pathNEWS}')


# FILE OPENING

def get_sentiment_of_article(headline, content):
    analyzer = SentimentIntensityAnalyzer()

    headline_scores = analyzer.polarity_scores(headline)
    content_scores = analyzer.polarity_scores(content)

    return (headline_scores.get("compound") + content_scores.get("compound")) / 2


def create_news_datapoints(data):
    for news_dict in data:
        sentiment = get_sentiment_of_article(news_dict.get("title"), news_dict.get("content"))
        if news_dict.get("date")[0:10] in news_sentiments.keys():
            news_sentiments[news_dict.get("date")[0:10]].append(sentiment)
        else:
            news_sentiments[news_dict.get("date")[0:10]] = [sentiment]

questionList = ["Give me a 1 sentence summary of " + ticker + " (stock ticker) stock", "Give me a 1 sentence summary of where " + ticker + " (stock ticker) company makes its money", "Give me a short list of companies who compete with " + ticker + " (stock ticker) stock in one sentence"]
answerList = []
for question in questionList:
    answerList.append((model.generate_content(question)).text)

for answer in answerList:
    print(answer)

try: 
    open("data/" + ticker +  '_' + end + "_sentiment_price.json", "r")
except IOError:
    jsonFileFund, jsonFileNews = my_api(ticker, max_search, end)
    if jsonFileFund["resultsCount"] != 0:
        # Price info
        financefile = open("data/" + ticker + '_' + end + "_fundamentals.json")
        financedata = json.load(financefile) # returns list of dicts
        financepricedata = financedata["results"]
        financedict = {}

        for dict in financepricedata:
            timestamp = dict["t"] / 1000
            currdate = (datetime.fromtimestamp(timestamp)).strftime("%Y-%m-%d")
            financedict[currdate] = [dict["o"], dict["c"], dict["v"]]

        # Sentiment info
        file = open("data/" + ticker + '_' + end + "_news.json")
        news_data = json.load(file) # returns list of dicts
        news_sentiments = {}

        # Create json
        create_news_datapoints(news_data)
        json_sentiment_price = json.dumps([news_sentiments, financedict, answerList], indent = 4)
        with open("data/" + ticker +  '_' + end + "_sentiment_price.json", "w") as outfile:
            outfile.write(json_sentiment_price)


