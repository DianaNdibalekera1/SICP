#lang racket
;(require "./lexicons/AFINN-lexicon") ; Sentiment lexicon
(require net/url)
(require data-science-master)
(require plot)
(require math)

;; Step 1: Load and clean data

;; Abstraction for loading data
(define (load-data filepath)
  (read-csv filepath #:header? #t))

;; Abstraction for cleaning tweets (preprocessing)
(define (clean-tweet text)
  (string-normalize-spaces
   (remove-punctuation
    (remove-urls
     (string-downcase text)))))

;; Abstraction for cleaning all tweets
(define (clean-data data)
  (map clean-tweet ($ data 'text)))

;; Step 2: Sentiment Analysis Abstraction

;; Abstraction for analyzing sentiment using a specific lexicon
(define (analyze-sentiment text lexicon)
  (let* ([tokens (document->tokens text #:sort? #t)]
         [sentiment (list->sentiment tokens #:lexicon lexicon)])
    sentiment))  ; Returning the raw sentiment data

;; Step 3: Aggregate Sentiment Data

;; Aggregate sentiment frequencies by label
(define (aggregate-sentiment sentiment)
  (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq)))

;; Step 4: Plotting Abstraction

;; Abstraction for plotting sentiment distribution
(define (plot-sentiment-distribution aggregated-sentiment)
  ;; Display the aggregated sentiment data
  ;(displayln "Aggregated Sentiment Data:")
  ;(displayln aggregated-sentiment)  ;; Print the aggregated sentiment to check
  
  ;; Plotting the aggregated sentiment data
(parameterize ((plot-width 800))
  (plot (list
         (tick-grid)
         (discrete-histogram
          (sort aggregated-sentiment
                (λ (x y) (> (second x) (second y)))) ; Sorting by frequency
          #:color "MediumSlateBlue"
          #:line-color "MediumSlateBlue"))
    #:x-label "Affective Label"
    #:y-label "Frequency")))








;; Step 5: Extracting Month and Year from Date

;; Extract the month and year from the date (assumes format "YYYY-MM-DD")
(define (extract-month-year date)
  (let* ((date-parts (string-split date "-"))
         (year (first date-parts))
         (month (second date-parts)))
    (list year month)))  ;; Return a list containing year and month

;; Step 6: Grouping Tweets by Month and Year

;; Group tweets by their month and year
(define (group-by-month data)
  (define grouped
    (group-with (lambda (tweet)
                 (second (string-split (assoc 'date tweet) "-")))  ;; Extract the month (ignore year)
               data))
  ;; Debugging: Check the structure of grouped data
  (if (not (list? grouped)) 
      (error "group-by-month returned something other than a list" grouped)
      grouped))  ;; Return grouped data

;; Step 7: Aggregate Sentiment by Month

;; Aggregate sentiment by month
(define (aggregate-monthly-sentiment grouped-tweets lexicon)
  ;; Debugging: Check the structure of grouped-tweets before using map
  (if (not (list? grouped-tweets))
      (error "aggregate-monthly-sentiment received non-list data" grouped-tweets)
      (map (lambda (month-group)
             (let* ([tweets (map (lambda (tweet) (cdr (assoc 'text tweet))) month-group)]  ;; Get tweet text
                    [all-cleaned-text (string-join tweets " ")]  ;; Combine cleaned tweets
                    [sentiment (analyze-sentiment all-cleaned-text lexicon)]  ;; Analyze sentiment
                    [aggregated-sentiment (aggregate-sentiment sentiment)])  ;; Aggregate sentiment data
               (list (first month-group) aggregated-sentiment))  ;; Return month and sentiment
           grouped-tweets))))  ;; Continue only if grouped-tweets is a list

;; Step 8: Plotting Sentiment Distribution Over Time (Line Plot)

;; Abstraction for plotting sentiment distribution over time (12 months)
(define (plot-monthly-sentiment aggregated-sentiment)
  (parameterize ((plot-width 800))  ;; Set plot width
    (plot (list
           (tick-grid)  ;; Add grid for better readability
           (lines (map (lambda (month-data)
                         (list (string->number (first month-data))  ;; Convert month to number
                               (second month-data))) aggregated-sentiment))  ;; Month and aggregated sentiment
           #:color "MediumSlateBlue"
           #:line-color "MediumSlateBlue"
           #:x-label "Month"
           #:y-label "Sentiment Frequency"))))





;; Step 9: Main Execution Flow

;; Load the data
(define twitter-data (load-data "uganda.csv"))

;; Clean the data
(define cleaned-tweets (clean-data twitter-data))

;; Combine all cleaned tweets into a single string
(define all-cleaned-text (string-join cleaned-tweets " "))

;; Analyze sentiment (using NRC lexicon as an example)
(define sentiment (analyze-sentiment all-cleaned-text 'nrc))

;; Aggregate sentiment data
(define aggregated-sentiment (aggregate-sentiment sentiment))

;; Display the aggregated sentiment data
(displayln "Aggregated Sentiment Data:")
(displayln aggregated-sentiment)

;; Plot the sentiment distribution
(plot-sentiment-distribution aggregated-sentiment)


