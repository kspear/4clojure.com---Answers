(letfn [(word-chain
         ([words]
          (let [words (for [w words]
                        (word-chain (disj words w) [w]))]
            (if (some true? (flatten words)) true false)))
         ([words chain]
          (if (not-empty words)
            (let [l (last chain)]
              (for [w words]
                (if (matcher [l w])
                  (word-chain (disj words w) (conj chain w)))))
            true)))
   
        (matcher
         [words]
         (let [[w1 w2] (sort-by count < words)
               min-matches (dec (apply max (map count words)))
               matches (for [l w1]
                         (re-seq (re-pattern (str l)) w2))]
           (= min-matches
              (->> matches
                   (remove nil?)
                   count))))]
  (partial word-chain))





