(fn word-chain
  ([words]
   (if (first
        (remove
         nil?
         (flatten
          (for [w words]
            (word-chain (disj words w) [w])))))
     true false))
  ([words chain]
   (if (not-empty words)
     (let [l (last chain)]
       (for [w words]
         (if ((fn [w1 w2]
                (let [[w1 w2] (sort-by count > [w1 w2])]
                  (cond
                   (= (count w1) (count w2))
                   (some #(not (nil? %))
                         ((fn match-sequential-letters [[w1 w2]]
                            (for [i (range (count w1))]
                              (let [s (str (apply str (take i w1))
                                           #"[\w]"
                                           (apply str (drop (inc i) w1)))
                                    pattern (re-pattern s)]
                                (when (re-find pattern w2) (str pattern)))))
                          [w1 w2]))

                   (= (dec (count w1)) (count w2))
                   ((fn match-sequential-letters [[w1 w2]]
                      (for [i (range (count w1))]
                        (let [s (str (apply str (take i w1))
                                     #"[\w]"
                                     (apply str (drop (inc i) w1)))
                              pattern (re-pattern s)]
                          (when (re-find pattern w2) (str pattern)))))
                    [w1 w2]))))
              l w)
           (word-chain (disj words w) (conj chain w)))))
     chain)))