let rec helper acc n m =
    if n < m then
        helper (acc + (n + 1)) (n + 1) m
    else
        acc

let sum n m =
    helper 0 n m

#time
sum 0 100000000