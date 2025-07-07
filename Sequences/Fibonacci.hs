module Sequences.Fibonacci
( fibonacci 
) where


fibonacci :: Integer -> Integer
fibonacci n 
    | n < 20 = fibSeq n
    | n < 40 = fibSeq' n18 18 n19 19 n
    | n < 60 = fibSeq' n38 38 n39 39 n
    | n < 80 = fibSeq' n58 58 n59 59 n
    | n < 100 = fibSeq' n78 78 n79 79 n
    | n < 120 = fibSeq' n98 98 n99 99 n
    | n < 140 = fibSeq' n118 118 n119 119 n
    | n < 160 = fibSeq' n138 138 n139 139 n
    | n < 180 = fibSeq' n158 158 n159 159 n
    | n < 200 = fibSeq' n178 178 n179 179 n
    | n >= 200 = fibSeq' n198 198 n199 199 n
    where
        n18  = fibonacci 18;  n19  = fibonacci 19
        n38  = fibonacci 38;  n39  = fibonacci 39
        n58  = fibonacci 58;  n59  = fibonacci 59
        n78  = fibonacci 78;  n79  = fibonacci 79
        n98  = fibonacci 98;  n99  = fibonacci 99
        n118 = fibonacci 118; n119 = fibonacci 119
        n138 = fibonacci 138; n139 = fibonacci 139
        n158 = fibonacci 158; n159 = fibonacci 159
        n178 = fibonacci 178; n179 = fibonacci 179
        n198 = fibonacci 198; n199 = fibonacci 199
       

fibSeq' :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
fibSeq' nn8 xx8 nn9 xx9 n
    | n == xx8 = nn8
    | n == xx9 = nn9
fibSeq' nn8 xx8 nn9 xx9 n = (fibSeq' nn8 xx8 nn9 xx9 (n-1)) + (fibSeq' nn8 xx8 nn9 xx9 (n-2))
  
 
fibSeq :: Integer -> Integer
fibSeq 0 = 0
fibSeq 1 = 1
fibSeq n = fibSeq (n-1) + fibSeq (n-2)
