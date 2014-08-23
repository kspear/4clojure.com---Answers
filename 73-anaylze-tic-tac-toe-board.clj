(fn check-win [board]
  (let [valid-wins [[[0 0] [0 1] [0 2]]
                    [[1 0] [1 1] [1 2]]
                    [[2 0] [2 1] [2 2]]

                    [[0 0] [1 0] [2 0]]
                    [[0 1] [1 1] [2 1]]
                    [[0 2] [1 2] [2 2]]

                    [[0 0] [1 1] [2 2]]
                    [[0 2] [1 1] [2 0]]]]
    (some
      (fn [win] (let [l (for [pos win] (get-in board pos))
                      p (first l)]
                  (when (and (apply = l)
                          (not= :e p))
                    p)))
      valid-wins)))