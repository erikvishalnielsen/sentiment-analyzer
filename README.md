# Public Stock Sentiment Analyzer

## Authors

Erik Nielsen and Sam Meddin

## Highlights
- Analyzes real-time sentiment of live earnings calls utilizing asynchronous frameworks to simultaneously download new audio data while transcribing and analyzing previously collected data.
- Collects financial news data about a given stock ticker in the specified time frame to create a visualization of changes in stock sentiment relative to changes in price and volume.
- Calculates four separate correlations between price and sentiment to determine if sentiment is lagging or leading price in the specified time frame (one or two days) and assesses the strength of these correlations.
- Predicts the closing price of a stock ticker using a linear regression model that analyzes sentiment data from the specified time frame.
- Provides detailed information about a given stock ticker, including how the company generates revenue, company sector, major competitors, and recent growth trends using GeminiAI.

## Description
The Public Stock Sentiment Analyzer is an OCaml-based GUI that enables users to see a live visualization of sentiment for the provided stock ticker and time frame. Users are provided with two different modes. The first feature allows users to see an updated graph in real-time that takes all data from a live earnings call, plotting the change in sentiment as the call progresses, producing important trade signals during one of a stock's most volatile times. The second feature allows users to see the sentiment of the specified stock ticker over a user-defined number of days. After hitting calculate, users are presented with a graph depicting sentiment, price, and volume together. This graph also includes data regarding the correlation of sentiment and price, indicating whether one lags behind the other, as well as a future forecast of the next closing stock price.

The feature to view changes in sentiment during a live earnings call utilizes yt-dlp to in real-time get and download audio data of an earnings call, and afterwards, uses subprocesses to process this data into a readable .wav file. After processing, OpenAI-Whisper is used to transcribe the audio data into text via speech recognition. At this point, the data is formatted to calculate a sentiment score using Vader NLTK's NLP model. To ensure simultaneous collection of future audio data while processing current data to derive a sentiment score, asyncio, an asynchronous framework, is utilized. With the sentiment data, this Python script transmits the data to our sentiment analysis dashboard in OCaml. As new points are received, a new graph is drawn with the real-time data from the earnings call.

Our second feature provides sentiment changes for the ticker specified within the given timeframe. Utilizing Polygon and EODHD APIs, financial news, price, and volume data are derived and used to generate our graph. Using jsonaf, all of this data is stored in files, and Python's json is used to parse this data into a format usable by OCaml. Before creating the new JSON file for OCaml, the Vader NLTK NLP model is used to create a sentiment score for the financial news data for each day in the dataset. With the newly created JSON file containing all relevant information, OCaml reads the data to visualize it in the GUI. The Gemini AI model provides information about the stock in the sentiment analyzer interface, allowing users to learn more about the company.

## Languages Used
- OCaml
- Python

## Getting Started

### How to use
1) Run the Program using the following:

```cd main```
```dune exec bin/main.exe```

2) Once the graphical interface is open, you can enter a stock ticker and a number of days by clicking on those fields. The boxes will light up, allowing you to enter and delete text. When you are satisfied, press the green "CALCULATE" button. Additionally, the live option allows you to view real-time sentiment during an earnings call by entering the URL for the earnings call when the link is active.

3) After the data is fetched from APIs, analyzed using Vader NLTK, processed into data structures, and plotted, you will see a comprehensive graph with green representing price, blue for % change in volume, and red for sentiment levels from -1 to 1. This will cover the last N amount of days you specified in the "DAYS" textbox before clicking calculate.

4) You will also see metrics on the side that correspond to linear correlation coefficients based on the delay of price movement after sentiment. The program takes the best correlation and creates a linear equation using linear regression to predict the price for the following day. If the highest correlation is 0 or -1, the program will not return an equation as a sentiment predictive model is not useful when the price is the leading variable. Added functionality allows you to toggle lines graphed on and off from the sidebar.

5) The bottom of the interface provides an overview of the stock from the Google Gemini API, which includes a quick overview, information on how the company makes money, and tickers of major competitors.

6) The program accounts for various errors, including incorrect stock tickers, too long or short timeframes entered, data issues with the APIs, and more.