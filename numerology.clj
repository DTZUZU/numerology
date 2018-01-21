(ns numerology.core
  "DTZUZU's numerology library")

(defn get-letter-value
  [letter]
  (case (clojure.string/upper-case letter)
    ("A" "J" "S" "1") 1
    ("B" "K" "T" "2") 2
    ("C" "L" "U" "3") 3
    ("D" "M" "V" "4") 4
    ("E" "N" "W" "5") 5
    ("F" "O" "X" "6") 6
    ("G" "P" "Y" "7") 7
    ("H" "Q" "Z" "8") 8
    ("I" "R" "9") 9))

(defn get-birthday-number
  [day]
  (case (str day)
    ("1" "10" "19" "28") 1
    ("2" "20" "29") 2
    ("3" "12" "21" "30") 3
    ("4" "13" "31") 4
    ("5" "14" "23") 5
    ("6" "15" "24") 6
    ("7" "16" "25") 7
    ("8" "17" "26") 8
    ("9" "18" "27") 9
    ("11") 11
    ("22") 22))

(def master-numbers '("11" "22" "33" "44" "55" "66" "77" "88" "99"))
(def numbers '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
(def consonants '("B" "C" "D" "F" "G" "H" "J" "K" "L" "M" "N" "P" "Q" "R" "S" "T" "V" "W" "X" "Y" "Z"))
(def vowels '("A", "E", "I", "O", "U", "Y"))

(defn parse-int [s] (Integer. (re-find  #"\d+" s )))
(defn transform-number [number] (apply + (into [] (map parse-int (clojure.string/split (str number) #""))))) 
(defn reduce-number [number]
  (if (= number 0)
    number         
    (do 
      (if (.contains master-numbers (str number))
        number
        (do 
          (let [x (transform-number number)]
            (if (.contains numbers (str x))
              x                        
              (recur (transform-number x)))))))))

(defn get-vowels
  [name]
  (let [a (clojure.string/split (clojure.string/upper-case name) #"")
        b consonants]
    (remove (set b) a)))

(defn get-consonants
  [name]
  (let [a (clojure.string/split (clojure.string/upper-case name) #"")
        b vowels]
    (remove (set b) a)))

(defn life-path-number
  [month day year]
  (reduce-number (+ (reduce-number month) (reduce-number day) (reduce-number year))))
     
(defn destiny-number
  [name]
  (if (.contains master-numbers name)
    (do
      (reduce-number (parse-int name)))
    (do
      (reduce-number (apply + (into [] (map get-letter-value (clojure.string/split (clojure.string/replace name #" " "") #""))))))))
                     
(defn inner-dream-number
  [name]
  (let [xyz (apply str (get-consonants name))]
    (if (> (count xyz) 0)
      (reduce-number (apply + (into [] (map get-letter-value (clojure.string/split (clojure.string/replace xyz #" " "") #"")))))
      (println "No consonants!"))))

(defn soul-urge-number
  [name]
  (let [xyz (apply str (get-vowels name))]
    (if (> (count xyz) 0)
      (reduce-number (apply + (into [] (map get-letter-value (clojure.string/split (clojure.string/replace xyz #" " "") #"")))))
      (println "No vowels!"))))

(defn rational-thought-number
  [name day]  
  (reduce-number 
    (+ (apply + (into [] (map get-letter-value (clojure.string/split (clojure.string/replace name #" " "") #"")))) (get-birthday-number day))))

(defn balance-number
  [names]
  (reduce-number (apply + (map get-letter-value (map first (clojure.string/split names #" "))))))

(defn subconscious-self-number
  [name]
  (count (distinct (into [] (map get-letter-value (clojure.string/split (clojure.string/replace name #" " "") #""))))))

(defn hidden-passion-number
  [name]
  (get-letter-value (get (first (sort-by val > (frequencies (clojure.string/split (clojure.string/replace (clojure.string/upper-case name) #" " "") #""))))0)))

(defn get-numbers
  [name]  
  (if (= "" name)
    (println "Please enter a name!")
    (do 
      (println "Destiny / Expression Number: " (str (destiny-number name)))
      (println "Soul Urge / Heart's Desire Number: " (str (soul-urge-number name)))
      (println "Inner Dream / Personality Number: " (str (inner-dream-number name)))
      (println "")
      (println "Balance Number: " (str (balance-number name)))
      (println "Hidden Passion Number: " (str (hidden-passion-number name)))
      (println "Subconscious Self Number: " (str (subconscious-self-number name))))))

