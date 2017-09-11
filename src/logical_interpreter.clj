            ))
(require '[clojure.string :as str])

(defn lazy-contains? [col key]
  "Return true if key is include in col"
   (if (some #{key} col) true false)
)

(defn trim [string]
	 (str/trim string)
)

(defn parser-fact
    "Parser of facts"
  [lineas]
	(let [fkey (trim (subs lineas 0 (str/index-of lineas "(")))
		fargs (trim (subs lineas (+ (str/index-of lineas "(") 1) (str/index-of lineas ")")))]
		(let [args (map trim (str/split fargs #","))]
		     [fkey args]
    )
  )
)

(defn parser-rule
  "Parser of rules"
  [lineas]
  (let [rkey (trim (subs lineas 0 (str/index-of lineas ":")))
		rargs (trim (subs lineas (+ (str/index-of lineas "-") 1) (str/index-of lineas ".")))]
		     [rkey rargs]
  )
)

(defn parser 
  "Separate rules from facts"
  [linea]
  (if (str/includes? (trim linea) ":-")
      (parser-rule linea)
      (parser-fact linea)
      ) 
  )

(defn separate
   "Separate rules and facts"
  [listas]
  [(filter (fn [x] (str/includes? (first x) ",")) listas)(filter (fn [x] (not(str/includes? (first x) ","))) listas) ]
  )

(defn ev-rules [rule]
  "Evaluates if rules are false o true"
  (let [rkey (subs (first rule) 0 (str/index-of (first rule) "("))
		rargs (subs (first rule) (+ (str/index-of (first rule) "(") 1) (str/index-of (first rule) ")"))]
		(let [args (map trim (str/split rargs #","))]
		     (hash-map (keyword rkey) [args (second rule)])
    )
  )
)

(defn is-fact [query facts]
  "Return true if query is a true fact"
 (lazy-contains? facts query)
  )

(defn funcc [val [k v]]
  "Replace rules parameters by query parameters"
  (str/replace val k v)
  )

(defn is-rule [query rules facts]
   "Return true if query is a true rule"
  (def crux (zipmap (first rules) (second query)))
  (def pars (map trim (str/split (second rules) #"\*,")))
  (if (= () (filter false? (for [x2 (map parser-fact (for [x1 pars](reduce funcc x1 crux)))] (is-fact x2 facts)))) 
      true 
      false
    )
  )

(defn parser-query [query facts rules]
  "Separate rule-query from fact-query"
  (def queryp (parser-fact query))
  (if (contains? rules (keyword(first queryp)))
      (is-rule queryp (rules (keyword(first queryp))) facts)
      (is-fact queryp facts)
    )
  )

(defn is-database-complete?
   "Return true if database is complete"
   [lines]
   (def result
     (filter (fn [line] (not (str/includes? line "."))) lines))
   (empty? result)
   )
 
 (defn is-query-complete?
   "Return true if query is complete"
   [query]
   (and
     (not (empty? query))
     (str/includes? query "(")
     (str/includes? query ")")
     )
   )
 
 (defn ev-query [lines query]
   "Evaluate if query is true"
   (def listas (map parser lines))
  ((fn [x] (def rules (first x))(def facts (second x))) (separate listas))
  (def map-rules (reduce merge (map ev-rules rules)))
  (parser-query query facts map-rules)
   )
 
(defn evaluate-query [database query] 
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  (def database2 (str/replace database ")," ")*,"))
  (def lineas (remove empty?  (str/split database2 #"\n")))
  (if (and (is-query-complete? query) (is-database-complete? lineas))
     (ev-query lineas query)
     nil)
)
