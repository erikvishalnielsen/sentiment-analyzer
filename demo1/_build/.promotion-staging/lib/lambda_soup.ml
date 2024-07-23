open! Core

module Curl = struct
  let writer accum data =
    Buffer.add_string accum data;
    String.length data
  ;;

  let get_exn url =
    let error_buffer = ref "" in
    let result = Buffer.create 16384 in
    let fail error = failwithf "Curl failed on %s: %s" url error () in
    try
      let connection = Curl.init () in
      Curl.set_errorbuffer connection error_buffer;
      Curl.set_writefunction connection (writer result);
      Curl.set_followlocation connection true;
      Curl.set_url connection url;
      Curl.perform connection;
      let result = Buffer.contents result in
      Curl.cleanup connection;
      result
    with
    | Curl.CurlException (_reason, _code, _str) -> fail !error_buffer
    | Failure s -> fail s
  ;;
end

(* Gets the "title" node of an HTML page. *)
let get_title contents : string =
  let open Soup in
  parse contents $ "title" |> R.leaf_text
;;

(* Gets all of the list items contained in an HTML page. *)
let get_list_items contents : (string * string) list =
  let open Soup in
  parse contents
  $$ "table" |> to_list |> List.filter ~f:(fun table -> List.exists (classes table) ~f:(fun a -> String.equal a "news-table")) |> List.hd_exn
  $$ "tr"
  |> to_list
  |> List.filter ~f:(fun tr -> List.exists (classes tr) ~f:(fun a -> String.equal a "has-label"))
  |> List.map ~f:(fun li -> let tds = li $$ "td" |> to_list in 
    match tds with 
    | first::second::[] -> (String.strip (String.concat (texts first)), String.strip(String.concat (texts second)))
    | thing -> print_s [%message (String.concat (List.map thing ~f:(fun a -> String.concat (texts a))) : string)]; failwith "Something's wrong"
  )


    (* texts li |> String.concat ~sep:"" |> String.strip) *)
;;

(* Gets the first item of all unordered lists contained in an HTML page. *)
let get_first_item_of_all_unordered_lists contents : string list =
  let open Soup in
  parse contents
  $$ "ul"
  |> to_list
  |> List.map ~f:(fun ul -> (ul $$ "li") |> to_list |> List.hd_exn |> texts |> String.concat ~sep:"" |> String.strip)
;;

(* Gets the first item of the second unordered list in an HTML page. *)
let get_first_item_of_second_unordered_list contents : string =
  let list = get_first_item_of_all_unordered_lists contents in
  List.nth_exn list 1
;; 

(* Gets all bolded text from an HTML page. *)
let get_bolded_text contents : string list =
  let open Soup in
  parse contents
  $$ "b"
  |> to_list
  |> List.map ~f:(fun b -> texts b |> String.concat ~sep:"" |> String.strip)
;;

let%expect_test "web scraper" =
  
  print_s
    [%sexp
      (get_list_items (Curl.get_exn "https://finviz.com/quote.ashx?t=AMZN&p=d") : (string * string) list)];
  [%expect {|
    (("Today 10:30AM"
       "Alexa is losing Amazon billions of dollars\
      \n                \
      \n                \
      \n                    (Quartz)")
     (09:57AM
       "This Undervalued Stock Could Join Amazon in the $2 Trillion Club\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (09:30AM
       "7 Stocks That Are Capitalizing on the Digital World\
      \n                \
      \n                \
      \n                    (InvestorPlace)")
     (08:54AM
       "Google Talks To Buy Wiz for $23B Reportedly End\
      \n                \
      \n                \
      \n                    (Investopedia)")
     (08:47AM
       "Whole Foods settles ex-worker's lawsuit over Black Lives Matter masks\
      \n                \
      \n                \
      \n                    (Reuters)")
     (08:00AM
       "S&P 500 Earnings Shrivel Without Magnificent Seven Names Like Nvidia, Meta\
      \n                \
      \n                \
      \n                    (Investor's Business Daily)")
     (07:51AM
       "Could Earnings Stop the Small-Cap Stocks Rotation in Its Tracks?\
      \n                \
      \n                \
      \n                    (Investopedia)")
     (07:50AM
       "Jim Cramer Thinks Amazon.com Inc (NASDAQ:AMZN) Can Benefit if Donald Trump Wins Election 2024\
      \n                \
      \n                \
      \n                    (Insider Monkey)")
     (07:35AM
       "Italy seizes $131 million from Amazon over alleged tax, labour offences\
      \n                \
      \n                \
      \n                    (Reuters)")
     (07:17AM
       "Nvidia vs. Amazon: Which \"Magnificent Seven\" Stock Could Be the Better Buy Over the Next 5 Years?\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (04:26AM
       "Is Rivian Stock a Buy?\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (04:20AM
       "iRobot Is a Falling Knife That's Too Risky for Most\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (04:03AM
       "2 Stocks That Can Help You to Get Richer in 2024\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     ("Jul-22-24 09:41PM"
       "Amazon Stock (NASDAQ:AMZN) Q2 Earnings Preview: Expect Another Exceptional Quarter\
      \n                \
      \n                \
      \n                    (TipRanks)")
     (09:00PM
       "Alexa Is in Millions of Householdsand Amazon Is Losing Billions\
      \n                \
      \n                \
      \n                    (The Wall Street Journal)")
     (08:15PM
       "Should You Buy Amazon Stock Before Aug. 1?\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (05:55PM
       "EV demand dips, TNT matches NBA bid: Market Domination Overtime\
      \n                \
      \n                \
      \n                    (Yahoo Finance Video)")
     (05:45PM
       "Amazon (AMZN) Stock Drops Despite Market Gains: Important Facts to Note\
      \n                \
      \n                \
      \n                    (Zacks)")
     (05:15PM
       "Warner Bros. Discovery offers to match Amazon offer for NBA rights\
      \n                \
      \n                \
      \n                    (CNBC TV)")
     (05:07PM
       "Warners TNT Makes Matching Offer for Amazons NBA Package\
      \n                \
      \n                \
      \n                    (The Wall Street Journal)")
     (05:06PM
       "Warner Bros. Discovery's TNT Network matches bid for NBA rights\
      \n                \
      \n                \
      \n                    (Yahoo Finance)")
     (05:06PM
       "Can Rivian Automotive Inc (NASDAQ:RIVN) Become the Next Tesla (TSLA) Due to AI? Analyst Answers\
      \n                \
      \n                \
      \n                    (Insider Monkey)")
     (04:32PM
       "Warner Bros. Discovery says it matched a bid for NBA rights\
      \n                \
      \n                \
      \n                    (Yahoo Finance Video)")
     (03:16PM
       "Google parent Alphabet to report Q2 earnings Tuesday with AI, ad spending front and center\
      \n                \
      \n                \
      \n                    (Yahoo Finance)")
     (02:30PM
       "Top 3 tech picks ahead of Q2 earnings: Evercore's Mahaney\
      \n                \
      \n                \
      \n                    (Yahoo Finance Video)")
     (01:20PM
       "VP Harris's impact on markets, earnings this week: Catalysts\
      \n                \
      \n                \
      \n                    (Yahoo Finance Video)")
     (12:04PM
       "Google search could bring 'double-barreled' revenue boost\
      \n                \
      \n                \
      \n                    (Yahoo Finance Video)")
     (11:00AM
       "Jeff Bezos To Sell Another $5 Billion In Amazon Shares: How Could He Spend It?\
      \n                \
      \n                \
      \n                    (Benzinga)")
     (10:30AM
       "Amazon Stock Gets Pair Of Price Target Increases With Q2 Earnings Due\
      \n                \
      \n                \
      \n                    (Investor's Business Daily)")
     (10:15AM
       "2 Top Stocks That Could Outperform for the Rest of 2024\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (09:45AM
       "Amazon (AMZN) is a Top-Ranked Growth Stock: Should You Buy?\
      \n                \
      \n                \
      \n                    (Zacks)")
     (08:54AM
       "CrowdStrike Falls Further While Rivals Extend Gains After Outage\
      \n                \
      \n                \
      \n                    (Investopedia)")
     (08:23AM
       "Amazon, Better Business Bureau file joint lawsuit in fight over fake reviews\
      \n                \
      \n                \
      \n                    (Retail Dive)")
     (08:00AM
       "Beyond Market Price: Uncovering Amazon.com Inc's Intrinsic Value\
      \n                \
      \n                \
      \n                    (GuruFocus.com)")
     (07:30AM
       "Prediction: 2 Artificial Intelligence Stocks That Could Be Worth More Than Nvidia 5 Years From Now\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (06:30AM
       "Jeff Bezos Is Selling $5B of Amazon Stock. Why You Should Sell Some, Too.\
      \n                \
      \n                \
      \n                    (InvestorPlace)")
     (06:15AM
       "2 Millionaire-Maker Artificial Intelligence (AI) Stocks\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (06:10AM
       "Plug Power Inc. (PLUG): Do Redditors Think That It Is a Good Undervalued Stock to Buy Now?\
      \n                \
      \n                \
      \n                    (Insider Monkey)")
     (05:18AM
       "A Once-in-a-Generation Investment Opportunity: Why I Think This Warren Buffett and Cathie Wood Artificial Intelligence (AI) Stock Will Be the Best \"Magnificent Seven\" Opportunity for Decades to Come\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (05:13AM
       "Generative AI Seen as Inflection Point' for Amazon.com Inc (NASDAQ:AMZN)\
      \n                \
      \n                \
      \n                    (Insider Monkey)")
     (05:05AM
       "Jim Cramer's Top 11 Trump Trades: Winners and Losers\
      \n                \
      \n                \
      \n                    (Insider Monkey)")
     (04:20AM
       "2 No-Brainer Stocks I'd Buy Right Now Without Hesitation\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (03:32AM
       "Wells Fargo: Amazon most favored long among mega-caps going into earnings\
      \n                \
      \n                \
      \n                    (Investing.com)")
     (02:25AM
       "Amazon.coms (AMZN) Q2 Results Exceeded Expectations\
      \n                \
      \n                \
      \n                    (Insider Monkey)")
     (12:54AM
       "A Stock-Market Rotation of Historic Proportions Is Taking Shape\
      \n                \
      \n                \
      \n                    (The Wall Street Journal)")
     (12:05AM
       "Why Are Hedge Funds Bullish on CrowdStrike Holdings, Inc. (CRWD) Now?\
      \n                \
      \n                \
      \n                    (Insider Monkey)")
     ("Jul-21-24 11:57PM"
       "The Hershey Company (HSY): Hedge Funds Are Bullish on This Stock Right Now\
      \n                \
      \n                \
      \n                    (Insider Monkey)")
     (07:57PM
       "MSFT, AMZN, or GOOGL: Which Cloud Computing Player is the Best AI Stock?\
      \n                \
      \n                \
      \n                    (TipRanks)")
     (06:06PM
       "10 Best Undervalued Stocks to Buy According to Reddit\
      \n                \
      \n                \
      \n                    (Insider Monkey)")
     (06:00PM
       "Up 145% So Far This Year, Here's What It Will Take for Nvidia Stock to Drop, and Why I Think It Could Happen Sooner Rather Than Later\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (09:30AM
       "Chips and Taiwan Are a New Cloud for Tech Earnings\
      \n                \
      \n                \
      \n                    (The Wall Street Journal)")
     (09:05AM
       "It smacks of a potential grand plan: How Royal Mail is poised to create the Amazon of Europe\
      \n                \
      \n                \
      \n                    (The Telegraph)")
     ("Jul-20-24 06:30PM"
       "2 Growth Stocks That Could Skyrocket in the Back Half of 2024 and Beyond\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (06:00PM
       "As Amazon Prime Day Hits Records, Is Now a Great Opportunity to Buy Amazon Stock?\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (09:28AM
       "Amazon.com, Inc. (AMZN): Hedge Funds Are Bullish on This Profitable Stock\
      \n                \
      \n                \
      \n                    (Insider Monkey)")
     (08:30AM
       "3 Reasons to Buy Amazon Stock Like There's No Tomorrow\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (07:07AM
       "Amazon Putting Pieces in Place to Offer Satellite Internet Service\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (07:00AM
       "What a List of the Best-Performing Stocks of the Past Century Tells Us\
      \n                \
      \n                \
      \n                    (Investopedia)")
     (05:53AM
       "Forget Costco: Buy This Unstoppable Growth Stock Instead\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (05:05AM
       "Forget Nike: These Stocks Have Greater Long-Term Potential\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     ("Jul-19-24 04:41PM"
       "These Chipmakers Could Benefit From Record 2024 Data Center Spending, BofA Says\
      \n                \
      \n                \
      \n                    (Investopedia)")
     (01:53PM
       "Explainer: How CrowdStrike knocked the world offline\
      \n                \
      \n                \
      \n                    (Yahoo Finance)")
     (12:11PM
       "Netflix is doubling down on India as its competitors back away\
      \n                \
      \n                \
      \n                    (Yahoo Finance)")
     (12:05PM
       "The top-selling Amazon Prime Day picks: Household must-haves dominate U.S. sales\
      \n                \
      \n                \
      \n                    (Quartz)")
     (10:44AM
       "What Is CrowdStrike and How Did Its Update Cause a Global Tech Outage?\
      \n                \
      \n                \
      \n                    (Investopedia)")
     (09:40AM
       "Are Retail-Wholesale Stocks Lagging  Amazon.com (AMZN) This Year?\
      \n                \
      \n                \
      \n                    (Zacks)")
     (09:02AM
       "Three lessons from the success of Amazon Prime Day\
      \n                \
      \n                \
      \n                    (Chain Store Age)")
     (09:00AM
       "Amazon.com, Inc. (AMZN) Is a Trending Stock: Facts to Know Before Betting on It\
      \n                \
      \n                \
      \n                    (Zacks)")
     (07:00AM
       "Be Resilient By Accepting Help From Others\
      \n                \
      \n                \
      \n                    (Investor's Business Daily)")
     (06:47AM
       "Amazon is cracking down on RTO holdouts and speaking directly to employees who havent spent enough time in the office\
      \n                \
      \n                \
      \n                    (Fortune)")
     (06:01AM
       "Retailers' early US back-to-school sales hasten peak ocean shipping season\
      \n                \
      \n                \
      \n                    (Reuters)")
     (06:00AM
       "This Billionaire Founder Sold Over $13 Billion of His Company's Stock This Year Alone. Should Investors Be Worried?\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (06:00AM
       "The Friday Checkout: Amazon drives grocery purchases during Prime Day\
      \n                \
      \n                \
      \n                    (Grocery Dive)")
     (05:00AM
       "Amazon's Prime Day, Dollar General beats Walmart and Target, Bud Light falls: Retail news roundup\
      \n                \
      \n                \
      \n                    (Quartz)")
     (04:15AM
       "Is This One Number From Nvidia a Red Flag for Investors?\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     ("Jul-18-24 06:36PM"
       "TikTok Faces Challenges in Advertising Upfronts\
      \n                \
      \n                \
      \n                    (The Information)")
     (06:04PM
       "MELI vs. AMZN: Which E-Commerce Stock Is the Better Buy?\
      \n                \
      \n                \
      \n                    (TipRanks)")
     (05:14PM
       "Spending More Time Shopping For the Best Price? You're Not Alone\
      \n                \
      \n                \
      \n                    (Investopedia)")
     (04:03PM
       "How Microsoft-Backed OpenAI's 'Mini' AI Model Is Heating Up the AI Competition\
      \n                \
      \n                \
      \n                    (Investopedia)")
     (04:02PM
       "How the FTC Could Complicate the Saks-Neimans Merger\
      \n                \
      \n                \
      \n                    (WWD)")
     (04:01PM
       "Amazon.com to Webcast Second Quarter 2024 Financial Results Conference Call\
      \n                \
      \n                \
      \n                    (Business Wire)")
     (03:45PM
       "Whole Foods Market launches food waste initiative with Too Good To Go\
      \n                \
      \n                \
      \n                    (Chain Store Age)")
     (01:13PM
       "Amazon Says Prime Day 2024 Set Records\
      \n                \
      \n                \
      \n                    (Investopedia)")
     (11:48AM
       "Whole Foods launches food waste initiative with Too Good To Go\
      \n                \
      \n                \
      \n                    (Chain Store Age)")
     (11:11AM
       "Amazon says this year's Prime Day was its biggest ever\
      \n                \
      \n                \
      \n                    (Associated Press Finance)")
     (10:48AM
       "Amazon Prime Day crushes records with $14.2 billion in sales\
      \n                \
      \n                \
      \n                    (Chain Store Age)")
     (10:37AM
       "Amazon Says Prime Day Brought 'Record-Breaking' Sales\
      \n                \
      \n                \
      \n                    (Investor's Business Daily)")
     (10:16AM
       "UK Amazon Workers Vote Against\194\160Unionization\
      \n                \
      \n                \
      \n                    (Sourcing Journal)")
     (09:50AM
       "Amazon (AMZN) is a Top-Ranked Momentum Stock: Should You Buy?\
      \n                \
      \n                \
      \n                    (Zacks)")
     (09:17AM
       "Amazon Prime Day boosts US online sales to record $14.2 billion, Adobe says\
      \n                \
      \n                \
      \n                    (Reuters)")
     (09:00AM
       "Amazon Announces Record-Breaking Sales for 2024 Prime Day Event\
      \n                \
      \n                \
      \n                    (Business Wire)")
     (08:27AM
       "The Magnificent Seven Stocks Suffer Their Worst Market Cap Loss In Two Years\
      \n                \
      \n                \
      \n                    (Investor's Business Daily)")
     (05:45AM
       "Amazon is sitting on a Gen-Z goldmine that could be worth $46 billion  and its not movies or Prime\
      \n                \
      \n                \
      \n                    (Fortune)")
     (05:15AM
       "Forget Apple: These Unstoppable Stocks Are Better Buys\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (05:00AM
       "Amazon pushes savings with back-to-school ad, Dorm Roomz series\
      \n                \
      \n                \
      \n                    (Marketing Dive)")
     (04:15AM
       "Missed Out on Nvidia? Billionaires Are Buying These 2 Artificial Intelligence (AI) Stocks Hand Over Fist.\
      \n                \
      \n                \
      \n                    (Motley Fool)")
     (03:17AM
       "Amazon, Apple, Eli Lilly: Here's Morgan Stanley's earnings playbook\
      \n                \
      \n                \
      \n                    (Investing.com)")
     ("Jul-17-24 06:16PM"
       "Amazon-Backed Anthropic and Menlo Ventures Launch Fund for AI Startups Here's Why\
      \n                \
      \n                \
      \n                    (Investopedia)")
     (06:15PM
       "The Trump trade, CDK cyberattack: Asking for a Trend\
      \n                \
      \n                \
      \n                    (Yahoo Finance Video)")
     (06:12PM
       "NBA media rights: WBD may lose out, WNBA to bring in $2.2B\
      \n                \
      \n                \
      \n                    (Yahoo Finance Video)"))|}] 
;;