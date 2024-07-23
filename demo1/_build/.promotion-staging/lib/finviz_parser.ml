open! Core

module Stock_date = struct
  type t = {
    date : Date.t;
    days_from_beginning : int
  }
  [@@deriving sexp_of]
end

module Finviz_parser = struct
  type t = {
    stock_ticker : string;
    time_period : int;
    link : string; 
    headlines : (Stock_date.t * string) list
  }
end

module Total_book = struct
  type t = {
    bookName : string;
    stockTable : (string, Finviz_parser.t) Hashtbl.t 
  }
end

let get_finviz_link ticker = 
  "https://finviz.com/quote.ashx?t=" ^ (String.uppercase ticker) ^ "&p=d";
;;

let create_finviz_parser ticker time = 
  let newlink = get_finviz_link ticker in
  let newParser = {Finviz_parser.stock_ticker = ticker ; time_period = time ; link = newlink ; headlines = []} in
  newParser
;;

let getMonth (month : string) : Month.t = 
  match month with 
  | "Jan" -> Jan
  | "Feb" -> Feb
  | "Mar" -> Mar
  | "Apr" -> Apr
  | "May" -> May
  | "Jun" -> Jun
  | "Jul" -> Jul
  | "Aug" -> Aug
  | "Sep" -> Sep
  | "Oct" -> Oct
  | "Nov" -> Nov
  | "Dec" -> Dec
  | _ -> failwith "Not a valid month!"
;;

let get_date (date : string) : Stock_date.t = 
  let dateList = String.split ~on:'-' date in
  let currDate = Date.today ~zone:(Timezone.utc) in
  let date : Date.t = Date.create_exn ~y:(Int.of_string ("20" ^ (List.nth_exn dateList 2))) ~m:(getMonth (List.nth_exn dateList 0)) ~d:(Int.of_string (List.nth_exn dateList 1)) in
  {date = date ; days_from_beginning = Date.diff currDate date}
;;

let get_relevant_info (url : string) : (Stock_date.t * string) list =
  let data_with_times = Lambda_soup.get_list_items (Lambda_soup.Curl.get_exn url) in
  let date = 
  ref (if ((String.length (fst (List.hd_exn data_with_times))) > 8) then {Stock_date.date = (Date.today ~zone:(Timezone.utc)) ; days_from_beginning = 0} else get_date (String.slice (fst (List.hd_exn data_with_times)) 0 9)) in
  List.map data_with_times ~f:(fun a -> if (String.length (fst a) > 8) && not (String.equal (fst a) (fst (List.hd_exn data_with_times))) then date := get_date (String.slice (fst a) 0 9); (!date, snd a))
;;

let%expect_test "web scraper - relevant info test" =
  print_s
    [%sexp
      (get_relevant_info "https://finviz.com/quote.ashx?t=AMZN&p=d" : (Stock_date.t * string) list)];
  [%expect {|
    ((((date 2024-07-23) (days_from_beginning 0))
      "Alphabet earnings top estimates as cloud business gains steam, AI losses grow")
     (((date 2024-07-23) (days_from_beginning 0))
      "Alphabet beats analysts' expectations on top and bottom line as cloud business picks up steam")
     (((date 2024-07-23) (days_from_beginning 0))
      "Why It's Time to Buy the Dip in Tech Stocks")
     (((date 2024-07-23) (days_from_beginning 0))
      "Amazon Talks Responsible AI One Year After Agreeing to Biden Admins Voluntary\194\160Commitments")
     (((date 2024-07-23) (days_from_beginning 0))
      "Meta Unveils New AI Model That Rivals Google and OpenAI Models")
     (((date 2024-07-23) (days_from_beginning 0))
      "Spotify Keeps the Profit Tune Humming")
     (((date 2024-07-23) (days_from_beginning 0))
      "Alexa is losing Amazon billions of dollars")
     (((date 2024-07-23) (days_from_beginning 0))
      "This Undervalued Stock Could Join Amazon in the $2 Trillion Club")
     (((date 2024-07-23) (days_from_beginning 0))
      "7 Stocks That Are Capitalizing on the Digital World")
     (((date 2024-07-23) (days_from_beginning 0))
      "Google Talks To Buy Wiz for $23B Reportedly End")
     (((date 2024-07-23) (days_from_beginning 0))
      "Whole Foods settles ex-worker's lawsuit over Black Lives Matter masks")
     (((date 2024-07-23) (days_from_beginning 0))
      "S&P 500 Earnings Shrivel Without Magnificent Seven Names Like Nvidia, Meta")
     (((date 2024-07-23) (days_from_beginning 0))
      "Could Earnings Stop the Small-Cap Stocks Rotation in Its Tracks?")
     (((date 2024-07-23) (days_from_beginning 0))
      "Jim Cramer Thinks Amazon.com Inc (NASDAQ:AMZN) Can Benefit if Donald Trump Wins Election 2024")
     (((date 2024-07-23) (days_from_beginning 0))
      "Italy seizes $131 million from Amazon over alleged tax, labour offences")
     (((date 2024-07-23) (days_from_beginning 0))
      "Nvidia vs. Amazon: Which \"Magnificent Seven\" Stock Could Be the Better Buy Over the Next 5 Years?")
     (((date 2024-07-23) (days_from_beginning 0)) "Is Rivian Stock a Buy?")
     (((date 2024-07-23) (days_from_beginning 0))
      "iRobot Is a Falling Knife That's Too Risky for Most")
     (((date 2024-07-23) (days_from_beginning 0))
      "2 Stocks That Can Help You to Get Richer in 2024")
     (((date 2024-07-22) (days_from_beginning 1))
      "Amazon Stock (NASDAQ:AMZN) Q2 Earnings Preview: Expect Another Exceptional Quarter")
     (((date 2024-07-22) (days_from_beginning 1))
      "Alexa Is in Millions of Householdsand Amazon Is Losing Billions")
     (((date 2024-07-22) (days_from_beginning 1))
      "Should You Buy Amazon Stock Before Aug. 1?")
     (((date 2024-07-22) (days_from_beginning 1))
      "EV demand dips, TNT matches NBA bid: Market Domination Overtime")
     (((date 2024-07-22) (days_from_beginning 1))
      "Amazon (AMZN) Stock Drops Despite Market Gains: Important Facts to Note")
     (((date 2024-07-22) (days_from_beginning 1))
      "Warner Bros. Discovery offers to match Amazon offer for NBA rights")
     (((date 2024-07-22) (days_from_beginning 1))
      "Warners TNT Makes Matching Offer for Amazons NBA Package")
     (((date 2024-07-22) (days_from_beginning 1))
      "Warner Bros. Discovery's TNT Network matches bid for NBA rights")
     (((date 2024-07-22) (days_from_beginning 1))
      "Can Rivian Automotive Inc (NASDAQ:RIVN) Become the Next Tesla (TSLA) Due to AI? Analyst Answers")
     (((date 2024-07-22) (days_from_beginning 1))
      "Warner Bros. Discovery says it matched a bid for NBA rights")
     (((date 2024-07-22) (days_from_beginning 1))
      "Google parent Alphabet to report Q2 earnings Tuesday with AI, ad spending front and center")
     (((date 2024-07-22) (days_from_beginning 1))
      "Top 3 tech picks ahead of Q2 earnings: Evercore's Mahaney")
     (((date 2024-07-22) (days_from_beginning 1))
      "VP Harris's impact on markets, earnings this week: Catalysts")
     (((date 2024-07-22) (days_from_beginning 1))
      "Google search could bring 'double-barreled' revenue boost")
     (((date 2024-07-22) (days_from_beginning 1))
      "Jeff Bezos To Sell Another $5 Billion In Amazon Shares: How Could He Spend It?")
     (((date 2024-07-22) (days_from_beginning 1))
      "Amazon Stock Gets Pair Of Price Target Increases With Q2 Earnings Due")
     (((date 2024-07-22) (days_from_beginning 1))
      "2 Top Stocks That Could Outperform for the Rest of 2024")
     (((date 2024-07-22) (days_from_beginning 1))
      "Amazon (AMZN) is a Top-Ranked Growth Stock: Should You Buy?")
     (((date 2024-07-22) (days_from_beginning 1))
      "CrowdStrike Falls Further While Rivals Extend Gains After Outage")
     (((date 2024-07-22) (days_from_beginning 1))
      "Amazon, Better Business Bureau file joint lawsuit in fight over fake reviews")
     (((date 2024-07-22) (days_from_beginning 1))
      "Beyond Market Price: Uncovering Amazon.com Inc's Intrinsic Value")
     (((date 2024-07-22) (days_from_beginning 1))
      "Prediction: 2 Artificial Intelligence Stocks That Could Be Worth More Than Nvidia 5 Years From Now")
     (((date 2024-07-22) (days_from_beginning 1))
      "Jeff Bezos Is Selling $5B of Amazon Stock. Why You Should Sell Some, Too.")
     (((date 2024-07-22) (days_from_beginning 1))
      "2 Millionaire-Maker Artificial Intelligence (AI) Stocks")
     (((date 2024-07-22) (days_from_beginning 1))
      "Plug Power Inc. (PLUG): Do Redditors Think That It Is a Good Undervalued Stock to Buy Now?")
     (((date 2024-07-22) (days_from_beginning 1))
      "A Once-in-a-Generation Investment Opportunity: Why I Think This Warren Buffett and Cathie Wood Artificial Intelligence (AI) Stock Will Be the Best \"Magnificent Seven\" Opportunity for Decades to Come")
     (((date 2024-07-22) (days_from_beginning 1))
      "Generative AI Seen as Inflection Point' for Amazon.com Inc (NASDAQ:AMZN)")
     (((date 2024-07-22) (days_from_beginning 1))
      "Jim Cramer's Top 11 Trump Trades: Winners and Losers")
     (((date 2024-07-22) (days_from_beginning 1))
      "2 No-Brainer Stocks I'd Buy Right Now Without Hesitation")
     (((date 2024-07-22) (days_from_beginning 1))
      "Wells Fargo: Amazon most favored long among mega-caps going into earnings")
     (((date 2024-07-22) (days_from_beginning 1))
      "Amazon.coms (AMZN) Q2 Results Exceeded Expectations")
     (((date 2024-07-22) (days_from_beginning 1))
      "A Stock-Market Rotation of Historic Proportions Is Taking Shape")
     (((date 2024-07-22) (days_from_beginning 1))
      "Why Are Hedge Funds Bullish on CrowdStrike Holdings, Inc. (CRWD) Now?")
     (((date 2024-07-21) (days_from_beginning 2))
      "The Hershey Company (HSY): Hedge Funds Are Bullish on This Stock Right Now")
     (((date 2024-07-21) (days_from_beginning 2))
      "MSFT, AMZN, or GOOGL: Which Cloud Computing Player is the Best AI Stock?")
     (((date 2024-07-21) (days_from_beginning 2))
      "10 Best Undervalued Stocks to Buy According to Reddit")
     (((date 2024-07-21) (days_from_beginning 2))
      "Up 145% So Far This Year, Here's What It Will Take for Nvidia Stock to Drop, and Why I Think It Could Happen Sooner Rather Than Later")
     (((date 2024-07-21) (days_from_beginning 2))
      "Chips and Taiwan Are a New Cloud for Tech Earnings")
     (((date 2024-07-21) (days_from_beginning 2))
      "It smacks of a potential grand plan: How Royal Mail is poised to create the Amazon of Europe")
     (((date 2024-07-20) (days_from_beginning 3))
      "2 Growth Stocks That Could Skyrocket in the Back Half of 2024 and Beyond")
     (((date 2024-07-20) (days_from_beginning 3))
      "As Amazon Prime Day Hits Records, Is Now a Great Opportunity to Buy Amazon Stock?")
     (((date 2024-07-20) (days_from_beginning 3))
      "Amazon.com, Inc. (AMZN): Hedge Funds Are Bullish on This Profitable Stock")
     (((date 2024-07-20) (days_from_beginning 3))
      "3 Reasons to Buy Amazon Stock Like There's No Tomorrow")
     (((date 2024-07-20) (days_from_beginning 3))
      "Amazon Putting Pieces in Place to Offer Satellite Internet Service")
     (((date 2024-07-20) (days_from_beginning 3))
      "What a List of the Best-Performing Stocks of the Past Century Tells Us")
     (((date 2024-07-20) (days_from_beginning 3))
      "Forget Costco: Buy This Unstoppable Growth Stock Instead")
     (((date 2024-07-20) (days_from_beginning 3))
      "Forget Nike: These Stocks Have Greater Long-Term Potential")
     (((date 2024-07-19) (days_from_beginning 4))
      "These Chipmakers Could Benefit From Record 2024 Data Center Spending, BofA Says")
     (((date 2024-07-19) (days_from_beginning 4))
      "Explainer: How CrowdStrike knocked the world offline")
     (((date 2024-07-19) (days_from_beginning 4))
      "Netflix is doubling down on India as its competitors back away")
     (((date 2024-07-19) (days_from_beginning 4))
      "The top-selling Amazon Prime Day picks: Household must-haves dominate U.S. sales")
     (((date 2024-07-19) (days_from_beginning 4))
      "What Is CrowdStrike and How Did Its Update Cause a Global Tech Outage?")
     (((date 2024-07-19) (days_from_beginning 4))
      "Are Retail-Wholesale Stocks Lagging  Amazon.com (AMZN) This Year?")
     (((date 2024-07-19) (days_from_beginning 4))
      "Three lessons from the success of Amazon Prime Day")
     (((date 2024-07-19) (days_from_beginning 4))
      "Amazon.com, Inc. (AMZN) Is a Trending Stock: Facts to Know Before Betting on It")
     (((date 2024-07-19) (days_from_beginning 4))
      "Be Resilient By Accepting Help From Others")
     (((date 2024-07-19) (days_from_beginning 4))
      "Amazon is cracking down on RTO holdouts and speaking directly to employees who havent spent enough time in the office")
     (((date 2024-07-19) (days_from_beginning 4))
      "Retailers' early US back-to-school sales hasten peak ocean shipping season")
     (((date 2024-07-19) (days_from_beginning 4))
      "This Billionaire Founder Sold Over $13 Billion of His Company's Stock This Year Alone. Should Investors Be Worried?")
     (((date 2024-07-19) (days_from_beginning 4))
      "The Friday Checkout: Amazon drives grocery purchases during Prime Day")
     (((date 2024-07-19) (days_from_beginning 4))
      "Amazon's Prime Day, Dollar General beats Walmart and Target, Bud Light falls: Retail news roundup")
     (((date 2024-07-19) (days_from_beginning 4))
      "Is This One Number From Nvidia a Red Flag for Investors?")
     (((date 2024-07-18) (days_from_beginning 5))
      "TikTok Faces Challenges in Advertising Upfronts")
     (((date 2024-07-18) (days_from_beginning 5))
      "MELI vs. AMZN: Which E-Commerce Stock Is the Better Buy?")
     (((date 2024-07-18) (days_from_beginning 5))
      "Spending More Time Shopping For the Best Price? You're Not Alone")
     (((date 2024-07-18) (days_from_beginning 5))
      "How Microsoft-Backed OpenAI's 'Mini' AI Model Is Heating Up the AI Competition")
     (((date 2024-07-18) (days_from_beginning 5))
      "How the FTC Could Complicate the Saks-Neimans Merger")
     (((date 2024-07-18) (days_from_beginning 5))
      "Amazon.com to Webcast Second Quarter 2024 Financial Results Conference Call")
     (((date 2024-07-18) (days_from_beginning 5))
      "Whole Foods Market launches food waste initiative with Too Good To Go")
     (((date 2024-07-18) (days_from_beginning 5))
      "Amazon Says Prime Day 2024 Set Records")
     (((date 2024-07-18) (days_from_beginning 5))
      "Whole Foods launches food waste initiative with Too Good To Go")
     (((date 2024-07-18) (days_from_beginning 5))
      "Amazon says this year's Prime Day was its biggest ever")
     (((date 2024-07-18) (days_from_beginning 5))
      "Amazon Prime Day crushes records with $14.2 billion in sales")
     (((date 2024-07-18) (days_from_beginning 5))
      "Amazon Says Prime Day Brought 'Record-Breaking' Sales")
     (((date 2024-07-18) (days_from_beginning 5))
      "UK Amazon Workers Vote Against\194\160Unionization")
     (((date 2024-07-18) (days_from_beginning 5))
      "Amazon (AMZN) is a Top-Ranked Momentum Stock: Should You Buy?")
     (((date 2024-07-18) (days_from_beginning 5))
      "Amazon Prime Day boosts US online sales to record $14.2 billion, Adobe says")
     (((date 2024-07-18) (days_from_beginning 5))
      "Amazon Announces Record-Breaking Sales for 2024 Prime Day Event")
     (((date 2024-07-18) (days_from_beginning 5))
      "The Magnificent Seven Stocks Suffer Their Worst Market Cap Loss In Two Years")
     (((date 2024-07-18) (days_from_beginning 5))
      "Amazon is sitting on a Gen-Z goldmine that could be worth $46 billion  and its not movies or Prime")
     (((date 2024-07-18) (days_from_beginning 5))
      "Forget Apple: These Unstoppable Stocks Are Better Buys"))|}] 
;;