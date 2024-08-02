PUBLIC STOCK SENTIMENT ANALYZER

By Erik Nielsen and Sam Meddin

LANGUAGES USED
- OCaml
- Python

HOW TO USE
1) Run the Program using the following:

dune exec bin/main.exe

2) Now that you have the graphical interface open you can enter a stock ticker and a number of days by clicking on those fields. The boxes will light up, allowing you to enter and delete text from them. When you are satisfied, press the green "CALCULATE" button.

3) After the data is fetched from APIs, analyzed using Vader NLTK, processed into data structures, and plotted for your convenience, you will see a comprehensive graph with green corresponding to price, blue to % change in volume, and red to sentiment levels from -1 to 1. This will be complete for the last N amout of days you put in the "DAYS" textbox before clicking calculate. 

4) You will also notice some metrics on the side, which correspond to linear correlation coefficients depending on the delay of price movement after sentiment. The program takes the best correlation and creates a linear equation using linear regression to predict the price of the following day. If the highest correlation is on 0 or -1, the program will not return an equation because a sentiment predictive model is not useful when the price is the leading variable. 

5) The bottom of the interface has an overview of the stock provided by Google Gemini API which provides a quick overview, how the company makes money, and the tickers of major competitors to the business.

6) The program should acconut for a number of errors, including incorrect stock tickers, too long or short of a timeframe entered, data issues with the APIs, and more.