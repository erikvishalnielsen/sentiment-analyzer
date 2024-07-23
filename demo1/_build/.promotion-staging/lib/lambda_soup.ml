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
  let all_tables = parse contents $$ "table" |> to_list in
  let news_table = List.find_exn all_tables ~f:(fun table -> List.exists (classes table) ~f:(fun a -> String.equal a "news-table")) in
  let table_rows = news_table $$ "tr" |> to_list |> List.filter ~f:(fun tr -> List.exists (classes tr) ~f:(fun a -> String.equal a "has-label")) in
  List.map table_rows ~f:(fun li -> let tds = li $$ "td" |> to_list in 
    match tds with 
    | first::second::[] -> 
      let date = String.strip (String.concat (texts first)) in
      let actual_name = List.find_exn (second $$ "a" |> to_list) ~f:(fun table -> List.exists (classes table) ~f:(fun a -> String.equal a "tab-link-news")) in
      (date, String.strip (String.concat (texts actual_name)))
    | _thing -> failwith "Something's wrong"
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
    (("Today 04:10PM"
      "Alphabet earnings top estimates as cloud business gains steam, AI losses grow")
     (04:10PM
      "Alphabet beats analysts' expectations on top and bottom line as cloud business picks up steam")
     (02:36PM "Why It's Time to Buy the Dip in Tech Stocks")
     (02:30PM
      "Amazon Talks Responsible AI One Year After Agreeing to Biden Admins Voluntary\194\160Commitments")
     (01:36PM "Meta Unveils New AI Model That Rivals Google and OpenAI Models")
     (12:32PM "Spotify Keeps the Profit Tune Humming")
     (10:30AM "Alexa is losing Amazon billions of dollars")
     (09:57AM "This Undervalued Stock Could Join Amazon in the $2 Trillion Club")
     (09:30AM "7 Stocks That Are Capitalizing on the Digital World")
     (08:54AM "Google Talks To Buy Wiz for $23B Reportedly End")
     (08:47AM
      "Whole Foods settles ex-worker's lawsuit over Black Lives Matter masks")
     (08:00AM
      "S&P 500 Earnings Shrivel Without Magnificent Seven Names Like Nvidia, Meta")
     (07:51AM "Could Earnings Stop the Small-Cap Stocks Rotation in Its Tracks?")
     (07:50AM
      "Jim Cramer Thinks Amazon.com Inc (NASDAQ:AMZN) Can Benefit if Donald Trump Wins Election 2024")
     (07:35AM
      "Italy seizes $131 million from Amazon over alleged tax, labour offences")
     (07:17AM
      "Nvidia vs. Amazon: Which \"Magnificent Seven\" Stock Could Be the Better Buy Over the Next 5 Years?")
     (04:26AM "Is Rivian Stock a Buy?")
     (04:20AM "iRobot Is a Falling Knife That's Too Risky for Most")
     (04:03AM "2 Stocks That Can Help You to Get Richer in 2024")
     ("Jul-22-24 09:41PM"
      "Amazon Stock (NASDAQ:AMZN) Q2 Earnings Preview: Expect Another Exceptional Quarter")
     (09:00PM "Alexa Is in Millions of Householdsand Amazon Is Losing Billions")
     (08:15PM "Should You Buy Amazon Stock Before Aug. 1?")
     (05:55PM "EV demand dips, TNT matches NBA bid: Market Domination Overtime")
     (05:45PM
      "Amazon (AMZN) Stock Drops Despite Market Gains: Important Facts to Note")
     (05:15PM
      "Warner Bros. Discovery offers to match Amazon offer for NBA rights")
     (05:07PM "Warners TNT Makes Matching Offer for Amazons NBA Package")
     (05:06PM "Warner Bros. Discovery's TNT Network matches bid for NBA rights")
     (05:06PM
      "Can Rivian Automotive Inc (NASDAQ:RIVN) Become the Next Tesla (TSLA) Due to AI? Analyst Answers")
     (04:32PM "Warner Bros. Discovery says it matched a bid for NBA rights")
     (03:16PM
      "Google parent Alphabet to report Q2 earnings Tuesday with AI, ad spending front and center")
     (02:30PM "Top 3 tech picks ahead of Q2 earnings: Evercore's Mahaney")
     (01:20PM "VP Harris's impact on markets, earnings this week: Catalysts")
     (12:04PM "Google search could bring 'double-barreled' revenue boost")
     (11:00AM
      "Jeff Bezos To Sell Another $5 Billion In Amazon Shares: How Could He Spend It?")
     (10:30AM
      "Amazon Stock Gets Pair Of Price Target Increases With Q2 Earnings Due")
     (10:15AM "2 Top Stocks That Could Outperform for the Rest of 2024")
     (09:45AM "Amazon (AMZN) is a Top-Ranked Growth Stock: Should You Buy?")
     (08:54AM "CrowdStrike Falls Further While Rivals Extend Gains After Outage")
     (08:23AM
      "Amazon, Better Business Bureau file joint lawsuit in fight over fake reviews")
     (08:00AM "Beyond Market Price: Uncovering Amazon.com Inc's Intrinsic Value")
     (07:30AM
      "Prediction: 2 Artificial Intelligence Stocks That Could Be Worth More Than Nvidia 5 Years From Now")
     (06:30AM
      "Jeff Bezos Is Selling $5B of Amazon Stock. Why You Should Sell Some, Too.")
     (06:15AM "2 Millionaire-Maker Artificial Intelligence (AI) Stocks")
     (06:10AM
      "Plug Power Inc. (PLUG): Do Redditors Think That It Is a Good Undervalued Stock to Buy Now?")
     (05:18AM
      "A Once-in-a-Generation Investment Opportunity: Why I Think This Warren Buffett and Cathie Wood Artificial Intelligence (AI) Stock Will Be the Best \"Magnificent Seven\" Opportunity for Decades to Come")
     (05:13AM
      "Generative AI Seen as Inflection Point' for Amazon.com Inc (NASDAQ:AMZN)")
     (05:05AM "Jim Cramer's Top 11 Trump Trades: Winners and Losers")
     (04:20AM "2 No-Brainer Stocks I'd Buy Right Now Without Hesitation")
     (03:32AM
      "Wells Fargo: Amazon most favored long among mega-caps going into earnings")
     (02:25AM "Amazon.coms (AMZN) Q2 Results Exceeded Expectations")
     (12:54AM "A Stock-Market Rotation of Historic Proportions Is Taking Shape")
     (12:05AM
      "Why Are Hedge Funds Bullish on CrowdStrike Holdings, Inc. (CRWD) Now?")
     ("Jul-21-24 11:57PM"
      "The Hershey Company (HSY): Hedge Funds Are Bullish on This Stock Right Now")
     (07:57PM
      "MSFT, AMZN, or GOOGL: Which Cloud Computing Player is the Best AI Stock?")
     (06:06PM "10 Best Undervalued Stocks to Buy According to Reddit")
     (06:00PM
      "Up 145% So Far This Year, Here's What It Will Take for Nvidia Stock to Drop, and Why I Think It Could Happen Sooner Rather Than Later")
     (09:30AM "Chips and Taiwan Are a New Cloud for Tech Earnings")
     (09:05AM
      "It smacks of a potential grand plan: How Royal Mail is poised to create the Amazon of Europe")
     ("Jul-20-24 06:30PM"
      "2 Growth Stocks That Could Skyrocket in the Back Half of 2024 and Beyond")
     (06:00PM
      "As Amazon Prime Day Hits Records, Is Now a Great Opportunity to Buy Amazon Stock?")
     (09:28AM
      "Amazon.com, Inc. (AMZN): Hedge Funds Are Bullish on This Profitable Stock")
     (08:30AM "3 Reasons to Buy Amazon Stock Like There's No Tomorrow")
     (07:07AM
      "Amazon Putting Pieces in Place to Offer Satellite Internet Service")
     (07:00AM
      "What a List of the Best-Performing Stocks of the Past Century Tells Us")
     (05:53AM "Forget Costco: Buy This Unstoppable Growth Stock Instead")
     (05:05AM "Forget Nike: These Stocks Have Greater Long-Term Potential")
     ("Jul-19-24 04:41PM"
      "These Chipmakers Could Benefit From Record 2024 Data Center Spending, BofA Says")
     (01:53PM "Explainer: How CrowdStrike knocked the world offline")
     (12:11PM "Netflix is doubling down on India as its competitors back away")
     (12:05PM
      "The top-selling Amazon Prime Day picks: Household must-haves dominate U.S. sales")
     (10:44AM
      "What Is CrowdStrike and How Did Its Update Cause a Global Tech Outage?")
     (09:40AM
      "Are Retail-Wholesale Stocks Lagging  Amazon.com (AMZN) This Year?")
     (09:02AM "Three lessons from the success of Amazon Prime Day")
     (09:00AM
      "Amazon.com, Inc. (AMZN) Is a Trending Stock: Facts to Know Before Betting on It")
     (07:00AM "Be Resilient By Accepting Help From Others")
     (06:47AM
      "Amazon is cracking down on RTO holdouts and speaking directly to employees who havent spent enough time in the office")
     (06:01AM
      "Retailers' early US back-to-school sales hasten peak ocean shipping season")
     (06:00AM
      "This Billionaire Founder Sold Over $13 Billion of His Company's Stock This Year Alone. Should Investors Be Worried?")
     (06:00AM
      "The Friday Checkout: Amazon drives grocery purchases during Prime Day")
     (05:00AM
      "Amazon's Prime Day, Dollar General beats Walmart and Target, Bud Light falls: Retail news roundup")
     (04:15AM "Is This One Number From Nvidia a Red Flag for Investors?")
     ("Jul-18-24 06:36PM" "TikTok Faces Challenges in Advertising Upfronts")
     (06:04PM "MELI vs. AMZN: Which E-Commerce Stock Is the Better Buy?")
     (05:14PM "Spending More Time Shopping For the Best Price? You're Not Alone")
     (04:03PM
      "How Microsoft-Backed OpenAI's 'Mini' AI Model Is Heating Up the AI Competition")
     (04:02PM "How the FTC Could Complicate the Saks-Neimans Merger")
     (04:01PM
      "Amazon.com to Webcast Second Quarter 2024 Financial Results Conference Call")
     (03:45PM
      "Whole Foods Market launches food waste initiative with Too Good To Go")
     (01:13PM "Amazon Says Prime Day 2024 Set Records")
     (11:48AM "Whole Foods launches food waste initiative with Too Good To Go")
     (11:11AM "Amazon says this year's Prime Day was its biggest ever")
     (10:48AM "Amazon Prime Day crushes records with $14.2 billion in sales")
     (10:37AM "Amazon Says Prime Day Brought 'Record-Breaking' Sales")
     (10:16AM "UK Amazon Workers Vote Against\194\160Unionization")
     (09:50AM "Amazon (AMZN) is a Top-Ranked Momentum Stock: Should You Buy?")
     (09:17AM
      "Amazon Prime Day boosts US online sales to record $14.2 billion, Adobe says")
     (09:00AM "Amazon Announces Record-Breaking Sales for 2024 Prime Day Event")
     (08:27AM
      "The Magnificent Seven Stocks Suffer Their Worst Market Cap Loss In Two Years")
     (05:45AM
      "Amazon is sitting on a Gen-Z goldmine that could be worth $46 billion  and its not movies or Prime")
     (05:15AM "Forget Apple: These Unstoppable Stocks Are Better Buys"))|}] 
;;