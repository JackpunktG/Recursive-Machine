module Sequences.Fibonacci
( fibboosted,
  fibArray,
  fibHashMap,
  fibSeq,
  fibZip
) where

import System.IO.Unsafe (unsafePerformIO)
import Data.Array
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Control.Monad.State




fibHashMap :: Int -> State (HashMap Int Integer) Integer
fibHashMap 0 = return 0
fibHashMap 1 = return 1
fibHashMap n = do
    memo <- get  --get gets the current state of the HashMap and binds it to memo
    case HM.lookup n memo of
        Just value -> return value  -- if value exists return it
        Nothing -> do
            a <- fibHashMap (n - 1)
            b <- fibHashMap (n - 2)
            let result = a + b
            modify (HM.insert n result)  -- update the HashMap in state
            return result
            
            
-- Array memoization of Fibonacci
fibArray :: Int -> Array Int Integer
fibArray n = arr
  where
    arr = array (0, n) [(i, fib i) | i <- [0..n]]
    fib 0 = 0
    fib 1 = 1
    fib i = arr ! (i - 1) + arr ! (i - 2)
    
    
    
-- Ziptail memoization 
fibZip :: [Integer]
fibZip = 0 : 1 : zipWith (+) fibZip (tail fibZip)

  
--most basic Fibonnaci recursive function
fibSeq :: Int -> Integer
fibSeq 0 = 0
fibSeq 1 = 1
fibSeq n = fibSeq (n-1) + fibSeq (n-2)


-- "boosted" helper function
fibSeq' :: Integer -> Int -> Integer -> Int -> Int -> Integer
fibSeq' nn8 xx8 nn9 xx9 n
    | n == xx8 = nn8
    | n == xx9 = nn9
fibSeq' nn8 xx8 nn9 xx9 n = (fibSeq' nn8 xx8 nn9 xx9 (n-1)) + (fibSeq' nn8 xx8 nn9 xx9 (n-2))


-- "boosted" self-recursive function - would love to find a ways to automatically generate this ;)
fibboosted :: Int -> Integer
fibboosted n
  | n < 0     = error "Negative index not allowed"
  | n < 2     = fromIntegral n
  | n < 20    = fibSeq n
  | n < 40  = fibSeq' n18 18 n19 19 n
  | n < 60  = fibSeq' n38 38 n39 39 n
  | n < 80  = fibSeq' n58 58 n59 59 n
  | n < 100  = fibSeq' n78 78 n79 79 n
  | n < 120  = fibSeq' n98 98 n99 99 n
  | n < 140  = fibSeq' n118 118 n119 119 n
  | n < 160  = fibSeq' n138 138 n139 139 n
  | n < 180  = fibSeq' n158 158 n159 159 n
  | n < 200  = fibSeq' n178 178 n179 179 n
  | n < 220  = fibSeq' n198 198 n199 199 n
  | n < 240  = fibSeq' n218 218 n219 219 n
  | n < 260  = fibSeq' n238 238 n239 239 n
  | n < 280  = fibSeq' n258 258 n259 259 n
  | n < 300  = fibSeq' n278 278 n279 279 n
  | n < 320  = fibSeq' n298 298 n299 299 n
  | n < 340  = fibSeq' n318 318 n319 319 n
  | n < 360  = fibSeq' n338 338 n339 339 n
  | n < 380  = fibSeq' n358 358 n359 359 n
  | n < 400  = fibSeq' n378 378 n379 379 n
  | n < 420  = fibSeq' n398 398 n399 399 n
  | n < 440  = fibSeq' n418 418 n419 419 n
  | n < 460  = fibSeq' n438 438 n439 439 n
  | n < 480  = fibSeq' n458 458 n459 459 n
  | n < 500  = fibSeq' n478 478 n479 479 n
  | n < 520  = fibSeq' n498 498 n499 499 n
  | n < 540  = fibSeq' n518 518 n519 519 n
  | n < 560  = fibSeq' n538 538 n539 539 n
  | n < 580  = fibSeq' n558 558 n559 559 n
  | n < 600  = fibSeq' n578 578 n579 579 n
  | n < 620  = fibSeq' n598 598 n599 599 n
  | n < 640  = fibSeq' n618 618 n619 619 n
  | n < 660  = fibSeq' n638 638 n639 639 n
  | n < 680  = fibSeq' n658 658 n659 659 n
  | n < 700  = fibSeq' n678 678 n679 679 n
  | n < 720  = fibSeq' n698 698 n699 699 n
  | n < 740  = fibSeq' n718 718 n719 719 n
  | n < 760  = fibSeq' n738 738 n739 739 n
  | n < 780  = fibSeq' n758 758 n759 759 n
  | n < 800  = fibSeq' n778 778 n779 779 n
  | n < 820  = fibSeq' n798 798 n799 799 n
  | n < 840  = fibSeq' n818 818 n819 819 n
  | n < 860  = fibSeq' n838 838 n839 839 n
  | n < 880  = fibSeq' n858 858 n859 859 n
  | n < 900  = fibSeq' n878 878 n879 879 n
  | n < 920  = fibSeq' n898 898 n899 899 n
  | n < 940  = fibSeq' n918 918 n919 919 n
  | n < 960  = fibSeq' n938 938 n939 939 n
  | n < 980  = fibSeq' n958 958 n959 959 n
  | n < 1000  = fibSeq' n978 978 n979 979 n
  | n < 1020  = fibSeq' n998 998 n999 999 n
  | n < 1040  = fibSeq' n1018 1018 n1019 1019 n
  | n < 1060  = fibSeq' n1038 1038 n1039 1039 n
  | n < 1080  = fibSeq' n1058 1058 n1059 1059 n
  | n < 1100  = fibSeq' n1078 1078 n1079 1079 n
  | n < 1120  = fibSeq' n1098 1098 n1099 1099 n
  | n < 1140  = fibSeq' n1118 1118 n1119 1119 n
  | n < 1160  = fibSeq' n1138 1138 n1139 1139 n
  | n < 1180  = fibSeq' n1158 1158 n1159 1159 n
  | n < 1200  = fibSeq' n1178 1178 n1179 1179 n
  | n < 1220  = fibSeq' n1198 1198 n1199 1199 n
  | n < 1240  = fibSeq' n1218 1218 n1219 1219 n
  | n < 1260  = fibSeq' n1238 1238 n1239 1239 n
  | n < 1280  = fibSeq' n1258 1258 n1259 1259 n
  | n < 1300  = fibSeq' n1278 1278 n1279 1279 n
  | n < 1320  = fibSeq' n1298 1298 n1299 1299 n
  | n < 1340  = fibSeq' n1318 1318 n1319 1319 n
  | n < 1360  = fibSeq' n1338 1338 n1339 1339 n
  | n < 1380  = fibSeq' n1358 1358 n1359 1359 n
  | n < 1400  = fibSeq' n1378 1378 n1379 1379 n
  | n < 1420  = fibSeq' n1398 1398 n1399 1399 n
  | n < 1440  = fibSeq' n1418 1418 n1419 1419 n
  | n < 1460  = fibSeq' n1438 1438 n1439 1439 n
  | n < 1480  = fibSeq' n1458 1458 n1459 1459 n
  | n < 1500  = fibSeq' n1478 1478 n1479 1479 n
  | n < 1520  = fibSeq' n1498 1498 n1499 1499 n
  | n < 1540  = fibSeq' n1518 1518 n1519 1519 n
  | n < 1560  = fibSeq' n1538 1538 n1539 1539 n
  | n < 1580  = fibSeq' n1558 1558 n1559 1559 n
  | n < 1600  = fibSeq' n1578 1578 n1579 1579 n
  | n < 1620  = fibSeq' n1598 1598 n1599 1599 n
  | n < 1640  = fibSeq' n1618 1618 n1619 1619 n
  | n < 1660  = fibSeq' n1638 1638 n1639 1639 n
  | n < 1680  = fibSeq' n1658 1658 n1659 1659 n
  | n < 1700  = fibSeq' n1678 1678 n1679 1679 n
  | n < 1720  = fibSeq' n1698 1698 n1699 1699 n
  | n < 1740  = fibSeq' n1718 1718 n1719 1719 n
  | n < 1760  = fibSeq' n1738 1738 n1739 1739 n
  | n < 1780  = fibSeq' n1758 1758 n1759 1759 n
  | n < 1800  = fibSeq' n1778 1778 n1779 1779 n
  | n < 1820  = fibSeq' n1798 1798 n1799 1799 n
  | n < 1840  = fibSeq' n1818 1818 n1819 1819 n
  | n < 1860  = fibSeq' n1838 1838 n1839 1839 n
  | n < 1880  = fibSeq' n1858 1858 n1859 1859 n
  | n < 1900  = fibSeq' n1878 1878 n1879 1879 n
  | n < 1920  = fibSeq' n1898 1898 n1899 1899 n
  | n < 1940  = fibSeq' n1918 1918 n1919 1919 n
  | n < 1960  = fibSeq' n1938 1938 n1939 1939 n
  | n < 1980  = fibSeq' n1958 1958 n1959 1959 n
  | n < 2000  = fibSeq' n1978 1978 n1979 1979 n
  | n < 2020  = fibSeq' n1998 1998 n1999 1999 n
  | n < 2040  = fibSeq' n2018 2018 n2019 2019 n
  | n < 2060  = fibSeq' n2038 2038 n2039 2039 n
  | n < 2080  = fibSeq' n2058 2058 n2059 2059 n
  | n < 2100  = fibSeq' n2078 2078 n2079 2079 n
  | n < 2120  = fibSeq' n2098 2098 n2099 2099 n
  | n < 2140  = fibSeq' n2118 2118 n2119 2119 n
  | n < 2160  = fibSeq' n2138 2138 n2139 2139 n
  | n < 2180  = fibSeq' n2158 2158 n2159 2159 n
  | n < 2200  = fibSeq' n2178 2178 n2179 2179 n
  | n < 2220  = fibSeq' n2198 2198 n2199 2199 n
  | n < 2240  = fibSeq' n2218 2218 n2219 2219 n
  | n < 2260  = fibSeq' n2238 2238 n2239 2239 n
  | n < 2280  = fibSeq' n2258 2258 n2259 2259 n
  | n < 2300  = fibSeq' n2278 2278 n2279 2279 n
  | n < 2320  = fibSeq' n2298 2298 n2299 2299 n
  | n < 2340  = fibSeq' n2318 2318 n2319 2319 n
  | n < 2360  = fibSeq' n2338 2338 n2339 2339 n
  | n < 2380  = fibSeq' n2358 2358 n2359 2359 n
  | n < 2400  = fibSeq' n2378 2378 n2379 2379 n
  | n < 2420  = fibSeq' n2398 2398 n2399 2399 n
  | n < 2440  = fibSeq' n2418 2418 n2419 2419 n
  | n < 2460  = fibSeq' n2438 2438 n2439 2439 n
  | n < 2480  = fibSeq' n2458 2458 n2459 2459 n
  | n < 2500  = fibSeq' n2478 2478 n2479 2479 n
  | n < 2520  = fibSeq' n2498 2498 n2499 2499 n
  | n < 2540  = fibSeq' n2518 2518 n2519 2519 n
  | n < 2560  = fibSeq' n2538 2538 n2539 2539 n
  | n < 2580  = fibSeq' n2558 2558 n2559 2559 n
  | n < 2600  = fibSeq' n2578 2578 n2579 2579 n
  | n < 2620  = fibSeq' n2598 2598 n2599 2599 n
  | n < 2640  = fibSeq' n2618 2618 n2619 2619 n
  | n < 2660  = fibSeq' n2638 2638 n2639 2639 n
  | n < 2680  = fibSeq' n2658 2658 n2659 2659 n
  | n < 2700  = fibSeq' n2678 2678 n2679 2679 n
  | n < 2720  = fibSeq' n2698 2698 n2699 2699 n
  | n < 2740  = fibSeq' n2718 2718 n2719 2719 n
  | n < 2760  = fibSeq' n2738 2738 n2739 2739 n
  | n < 2780  = fibSeq' n2758 2758 n2759 2759 n
  | n < 2800  = fibSeq' n2778 2778 n2779 2779 n
  | n < 2820  = fibSeq' n2798 2798 n2799 2799 n
  | n < 2840  = fibSeq' n2818 2818 n2819 2819 n
  | n < 2860  = fibSeq' n2838 2838 n2839 2839 n
  | n < 2880  = fibSeq' n2858 2858 n2859 2859 n
  | n < 2900  = fibSeq' n2878 2878 n2879 2879 n
  | n < 2920  = fibSeq' n2898 2898 n2899 2899 n
  | n < 2940  = fibSeq' n2918 2918 n2919 2919 n
  | n < 2960  = fibSeq' n2938 2938 n2939 2939 n
  | n < 2980  = fibSeq' n2958 2958 n2959 2959 n
  | n < 3000  = fibSeq' n2978 2978 n2979 2979 n
  | n < 3020  = fibSeq' n2998 2998 n2999 2999 n
  | n < 3040  = fibSeq' n3018 3018 n3019 3019 n
  | n < 3060  = fibSeq' n3038 3038 n3039 3039 n
  | n < 3080  = fibSeq' n3058 3058 n3059 3059 n
  | n < 3100  = fibSeq' n3078 3078 n3079 3079 n
  | n < 3120  = fibSeq' n3098 3098 n3099 3099 n
  | n < 3140  = fibSeq' n3118 3118 n3119 3119 n
  | n < 3160  = fibSeq' n3138 3138 n3139 3139 n
  | n < 3180  = fibSeq' n3158 3158 n3159 3159 n
  | n < 3200  = fibSeq' n3178 3178 n3179 3179 n
  | n < 3220  = fibSeq' n3198 3198 n3199 3199 n
  | n < 3240  = fibSeq' n3218 3218 n3219 3219 n
  | n < 3260  = fibSeq' n3238 3238 n3239 3239 n
  | n < 3280  = fibSeq' n3258 3258 n3259 3259 n
  | n < 3300  = fibSeq' n3278 3278 n3279 3279 n
  | n < 3320  = fibSeq' n3298 3298 n3299 3299 n
  | n < 3340  = fibSeq' n3318 3318 n3319 3319 n
  | n < 3360  = fibSeq' n3338 3338 n3339 3339 n
  | n < 3380  = fibSeq' n3358 3358 n3359 3359 n
  | n < 3400  = fibSeq' n3378 3378 n3379 3379 n
  | n < 3420  = fibSeq' n3398 3398 n3399 3399 n
  | n < 3440  = fibSeq' n3418 3418 n3419 3419 n
  | n < 3460  = fibSeq' n3438 3438 n3439 3439 n
  | n < 3480  = fibSeq' n3458 3458 n3459 3459 n
  | n < 3500  = fibSeq' n3478 3478 n3479 3479 n
  | n < 3520  = fibSeq' n3498 3498 n3499 3499 n
  | n < 3540  = fibSeq' n3518 3518 n3519 3519 n
  | n < 3560  = fibSeq' n3538 3538 n3539 3539 n
  | n < 3580  = fibSeq' n3558 3558 n3559 3559 n
  | n < 3600  = fibSeq' n3578 3578 n3579 3579 n
  | n < 3620  = fibSeq' n3598 3598 n3599 3599 n
  | n < 3640  = fibSeq' n3618 3618 n3619 3619 n
  | n < 3660  = fibSeq' n3638 3638 n3639 3639 n
  | n < 3680  = fibSeq' n3658 3658 n3659 3659 n
  | n < 3700  = fibSeq' n3678 3678 n3679 3679 n
  | n < 3720  = fibSeq' n3698 3698 n3699 3699 n
  | n < 3740  = fibSeq' n3718 3718 n3719 3719 n
  | n < 3760  = fibSeq' n3738 3738 n3739 3739 n
  | n < 3780  = fibSeq' n3758 3758 n3759 3759 n
  | n < 3800  = fibSeq' n3778 3778 n3779 3779 n
  | n < 3820  = fibSeq' n3798 3798 n3799 3799 n
  | n < 3840  = fibSeq' n3818 3818 n3819 3819 n
  | n < 3860  = fibSeq' n3838 3838 n3839 3839 n
  | n < 3880  = fibSeq' n3858 3858 n3859 3859 n
  | n < 3900  = fibSeq' n3878 3878 n3879 3879 n
  | n < 3920  = fibSeq' n3898 3898 n3899 3899 n
  | n < 3940  = fibSeq' n3918 3918 n3919 3919 n
  | n < 3960  = fibSeq' n3938 3938 n3939 3939 n
  | n < 3980  = fibSeq' n3958 3958 n3959 3959 n
  | n < 4000  = fibSeq' n3978 3978 n3979 3979 n
  | n < 4020  = fibSeq' n3998 3998 n3999 3999 n
  | n < 4040  = fibSeq' n4018 4018 n4019 4019 n
  | n < 4060  = fibSeq' n4038 4038 n4039 4039 n
  | n < 4080  = fibSeq' n4058 4058 n4059 4059 n
  | n < 4100  = fibSeq' n4078 4078 n4079 4079 n
  | n < 4120  = fibSeq' n4098 4098 n4099 4099 n
  | n < 4140  = fibSeq' n4118 4118 n4119 4119 n
  | n < 4160  = fibSeq' n4138 4138 n4139 4139 n
  | n < 4180  = fibSeq' n4158 4158 n4159 4159 n
  | n < 4200  = fibSeq' n4178 4178 n4179 4179 n
  | n < 4220  = fibSeq' n4198 4198 n4199 4199 n
  | n < 4240  = fibSeq' n4218 4218 n4219 4219 n
  | n < 4260  = fibSeq' n4238 4238 n4239 4239 n
  | n < 4280  = fibSeq' n4258 4258 n4259 4259 n
  | n < 4300  = fibSeq' n4278 4278 n4279 4279 n
  | n < 4320  = fibSeq' n4298 4298 n4299 4299 n
  | n < 4340  = fibSeq' n4318 4318 n4319 4319 n
  | n < 4360  = fibSeq' n4338 4338 n4339 4339 n
  | n < 4380  = fibSeq' n4358 4358 n4359 4359 n
  | n < 4400  = fibSeq' n4378 4378 n4379 4379 n
  | n < 4420  = fibSeq' n4398 4398 n4399 4399 n
  | n < 4440  = fibSeq' n4418 4418 n4419 4419 n
  | n < 4460  = fibSeq' n4438 4438 n4439 4439 n
  | n < 4480  = fibSeq' n4458 4458 n4459 4459 n
  | n < 4500  = fibSeq' n4478 4478 n4479 4479 n
  | n < 4520  = fibSeq' n4498 4498 n4499 4499 n
  | n < 4540  = fibSeq' n4518 4518 n4519 4519 n
  | n < 4560  = fibSeq' n4538 4538 n4539 4539 n
  | n < 4580  = fibSeq' n4558 4558 n4559 4559 n
  | n < 4600  = fibSeq' n4578 4578 n4579 4579 n
  | n < 4620  = fibSeq' n4598 4598 n4599 4599 n
  | n < 4640  = fibSeq' n4618 4618 n4619 4619 n
  | n < 4660  = fibSeq' n4638 4638 n4639 4639 n
  | n < 4680  = fibSeq' n4658 4658 n4659 4659 n
  | n < 4700  = fibSeq' n4678 4678 n4679 4679 n
  | n < 4720  = fibSeq' n4698 4698 n4699 4699 n
  | n < 4740  = fibSeq' n4718 4718 n4719 4719 n
  | n < 4760  = fibSeq' n4738 4738 n4739 4739 n
  | n < 4780  = fibSeq' n4758 4758 n4759 4759 n
  | n < 4800  = fibSeq' n4778 4778 n4779 4779 n
  | n < 4820  = fibSeq' n4798 4798 n4799 4799 n
  | n < 4840  = fibSeq' n4818 4818 n4819 4819 n
  | n < 4860  = fibSeq' n4838 4838 n4839 4839 n
  | n < 4880  = fibSeq' n4858 4858 n4859 4859 n
  | n < 4900  = fibSeq' n4878 4878 n4879 4879 n
  | n < 4920  = fibSeq' n4898 4898 n4899 4899 n
  | n < 4940  = fibSeq' n4918 4918 n4919 4919 n
  | n < 4960  = fibSeq' n4938 4938 n4939 4939 n
  | n < 4980  = fibSeq' n4958 4958 n4959 4959 n
  | n < 5000  = fibSeq' n4978 4978 n4979 4979 n
  | n < 5020  = fibSeq' n4998 4998 n4999 4999 n
  | n < 5040  = fibSeq' n5018 5018 n5019 5019 n
  | n < 5060  = fibSeq' n5038 5038 n5039 5039 n
  | n < 5080  = fibSeq' n5058 5058 n5059 5059 n
  | n < 5100  = fibSeq' n5078 5078 n5079 5079 n
  | n < 5120  = fibSeq' n5098 5098 n5099 5099 n
  | n < 5140  = fibSeq' n5118 5118 n5119 5119 n
  | n < 5160  = fibSeq' n5138 5138 n5139 5139 n
  | n < 5180  = fibSeq' n5158 5158 n5159 5159 n
  | n < 5200  = fibSeq' n5178 5178 n5179 5179 n
  | n < 5220  = fibSeq' n5198 5198 n5199 5199 n
  | n < 5240  = fibSeq' n5218 5218 n5219 5219 n
  | n < 5260  = fibSeq' n5238 5238 n5239 5239 n
  | n < 5280  = fibSeq' n5258 5258 n5259 5259 n
  | n < 5300  = fibSeq' n5278 5278 n5279 5279 n
  | n < 5320  = fibSeq' n5298 5298 n5299 5299 n
  | n < 5340  = fibSeq' n5318 5318 n5319 5319 n
  | n < 5360  = fibSeq' n5338 5338 n5339 5339 n
  | n < 5380  = fibSeq' n5358 5358 n5359 5359 n
  | n < 5400  = fibSeq' n5378 5378 n5379 5379 n
  | n < 5420  = fibSeq' n5398 5398 n5399 5399 n
  | n < 5440  = fibSeq' n5418 5418 n5419 5419 n
  | n < 5460  = fibSeq' n5438 5438 n5439 5439 n
  | n < 5480  = fibSeq' n5458 5458 n5459 5459 n
  | n < 5500  = fibSeq' n5478 5478 n5479 5479 n
  | n < 5520  = fibSeq' n5498 5498 n5499 5499 n
  | n < 5540  = fibSeq' n5518 5518 n5519 5519 n
  | n < 5560  = fibSeq' n5538 5538 n5539 5539 n
  | n < 5580  = fibSeq' n5558 5558 n5559 5559 n
  | n < 5600  = fibSeq' n5578 5578 n5579 5579 n
  | n < 5620  = fibSeq' n5598 5598 n5599 5599 n
  | n < 5640  = fibSeq' n5618 5618 n5619 5619 n
  | n < 5660  = fibSeq' n5638 5638 n5639 5639 n
  | n < 5680  = fibSeq' n5658 5658 n5659 5659 n
  | n < 5700  = fibSeq' n5678 5678 n5679 5679 n
  | n < 5720  = fibSeq' n5698 5698 n5699 5699 n
  | n < 5740  = fibSeq' n5718 5718 n5719 5719 n
  | n < 5760  = fibSeq' n5738 5738 n5739 5739 n
  | n < 5780  = fibSeq' n5758 5758 n5759 5759 n
  | n < 5800  = fibSeq' n5778 5778 n5779 5779 n
  | n < 5820  = fibSeq' n5798 5798 n5799 5799 n
  | n < 5840  = fibSeq' n5818 5818 n5819 5819 n
  | n < 5860  = fibSeq' n5838 5838 n5839 5839 n
  | n < 5880  = fibSeq' n5858 5858 n5859 5859 n
  | n < 5900  = fibSeq' n5878 5878 n5879 5879 n
  | n < 5920  = fibSeq' n5898 5898 n5899 5899 n
  | n < 5940  = fibSeq' n5918 5918 n5919 5919 n
  | n < 5960  = fibSeq' n5938 5938 n5939 5939 n
  | n < 5980  = fibSeq' n5958 5958 n5959 5959 n
  | n < 6000  = fibSeq' n5978 5978 n5979 5979 n
  | n < 6020  = fibSeq' n5998 5998 n5999 5999 n
  | n < 6040  = fibSeq' n6018 6018 n6019 6019 n
  | n < 6060  = fibSeq' n6038 6038 n6039 6039 n
  | n < 6080  = fibSeq' n6058 6058 n6059 6059 n
  | n < 6100  = fibSeq' n6078 6078 n6079 6079 n
  | n < 6120  = fibSeq' n6098 6098 n6099 6099 n
  | n < 6140  = fibSeq' n6118 6118 n6119 6119 n
  | n < 6160  = fibSeq' n6138 6138 n6139 6139 n
  | n < 6180  = fibSeq' n6158 6158 n6159 6159 n
  | n < 6200  = fibSeq' n6178 6178 n6179 6179 n
  | n < 6220  = fibSeq' n6198 6198 n6199 6199 n
  | n < 6240  = fibSeq' n6218 6218 n6219 6219 n
  | n < 6260  = fibSeq' n6238 6238 n6239 6239 n
  | n < 6280  = fibSeq' n6258 6258 n6259 6259 n
  | n < 6300  = fibSeq' n6278 6278 n6279 6279 n
  | n < 6320  = fibSeq' n6298 6298 n6299 6299 n
  | n < 6340  = fibSeq' n6318 6318 n6319 6319 n
  | n < 6360  = fibSeq' n6338 6338 n6339 6339 n
  | n < 6380  = fibSeq' n6358 6358 n6359 6359 n
  | n < 6400  = fibSeq' n6378 6378 n6379 6379 n
  | n < 6420  = fibSeq' n6398 6398 n6399 6399 n
  | n < 6440  = fibSeq' n6418 6418 n6419 6419 n
  | n < 6460  = fibSeq' n6438 6438 n6439 6439 n
  | n < 6480  = fibSeq' n6458 6458 n6459 6459 n
  | n < 6500  = fibSeq' n6478 6478 n6479 6479 n
  | n < 6520  = fibSeq' n6498 6498 n6499 6499 n
  | n < 6540  = fibSeq' n6518 6518 n6519 6519 n
  | n < 6560  = fibSeq' n6538 6538 n6539 6539 n
  | n < 6580  = fibSeq' n6558 6558 n6559 6559 n
  | n < 6600  = fibSeq' n6578 6578 n6579 6579 n
  | n < 6620  = fibSeq' n6598 6598 n6599 6599 n
  | n < 6640  = fibSeq' n6618 6618 n6619 6619 n
  | n < 6660  = fibSeq' n6638 6638 n6639 6639 n
  | n < 6680  = fibSeq' n6658 6658 n6659 6659 n
  | n < 6700  = fibSeq' n6678 6678 n6679 6679 n
  | n < 6720  = fibSeq' n6698 6698 n6699 6699 n
  | n < 6740  = fibSeq' n6718 6718 n6719 6719 n
  | n < 6760  = fibSeq' n6738 6738 n6739 6739 n
  | n < 6780  = fibSeq' n6758 6758 n6759 6759 n
  | n < 6800  = fibSeq' n6778 6778 n6779 6779 n
  | n < 6820  = fibSeq' n6798 6798 n6799 6799 n
  | n < 6840  = fibSeq' n6818 6818 n6819 6819 n
  | n < 6860  = fibSeq' n6838 6838 n6839 6839 n
  | n < 6880  = fibSeq' n6858 6858 n6859 6859 n
  | n < 6900  = fibSeq' n6878 6878 n6879 6879 n
  | n < 6920  = fibSeq' n6898 6898 n6899 6899 n
  | n < 6940  = fibSeq' n6918 6918 n6919 6919 n
  | n < 6960  = fibSeq' n6938 6938 n6939 6939 n
  | n < 6980  = fibSeq' n6958 6958 n6959 6959 n
  | n < 7000  = fibSeq' n6978 6978 n6979 6979 n
  | n < 7020  = fibSeq' n6998 6998 n6999 6999 n
  | n < 7040  = fibSeq' n7018 7018 n7019 7019 n
  | n < 7060  = fibSeq' n7038 7038 n7039 7039 n
  | n < 7080  = fibSeq' n7058 7058 n7059 7059 n
  | n < 7100  = fibSeq' n7078 7078 n7079 7079 n
  | n < 7120  = fibSeq' n7098 7098 n7099 7099 n
  | n < 7140  = fibSeq' n7118 7118 n7119 7119 n
  | n < 7160  = fibSeq' n7138 7138 n7139 7139 n
  | n < 7180  = fibSeq' n7158 7158 n7159 7159 n
  | n < 7200  = fibSeq' n7178 7178 n7179 7179 n
  | n < 7220  = fibSeq' n7198 7198 n7199 7199 n
  | n < 7240  = fibSeq' n7218 7218 n7219 7219 n
  | n < 7260  = fibSeq' n7238 7238 n7239 7239 n
  | n < 7280  = fibSeq' n7258 7258 n7259 7259 n
  | n < 7300  = fibSeq' n7278 7278 n7279 7279 n
  | n < 7320  = fibSeq' n7298 7298 n7299 7299 n
  | n < 7340  = fibSeq' n7318 7318 n7319 7319 n
  | n < 7360  = fibSeq' n7338 7338 n7339 7339 n
  | n < 7380  = fibSeq' n7358 7358 n7359 7359 n
  | n < 7400  = fibSeq' n7378 7378 n7379 7379 n
  | n < 7420  = fibSeq' n7398 7398 n7399 7399 n
  | n < 7440  = fibSeq' n7418 7418 n7419 7419 n
  | n < 7460  = fibSeq' n7438 7438 n7439 7439 n
  | n < 7480  = fibSeq' n7458 7458 n7459 7459 n
  | n < 7500  = fibSeq' n7478 7478 n7479 7479 n
  | n < 7520  = fibSeq' n7498 7498 n7499 7499 n
  | n < 7540  = fibSeq' n7518 7518 n7519 7519 n
  | n < 7560  = fibSeq' n7538 7538 n7539 7539 n
  | n < 7580  = fibSeq' n7558 7558 n7559 7559 n
  | n < 7600  = fibSeq' n7578 7578 n7579 7579 n
  | n < 7620  = fibSeq' n7598 7598 n7599 7599 n
  | n < 7640  = fibSeq' n7618 7618 n7619 7619 n
  | n < 7660  = fibSeq' n7638 7638 n7639 7639 n
  | n < 7680  = fibSeq' n7658 7658 n7659 7659 n
  | n < 7700  = fibSeq' n7678 7678 n7679 7679 n
  | n < 7720  = fibSeq' n7698 7698 n7699 7699 n
  | n < 7740  = fibSeq' n7718 7718 n7719 7719 n
  | n < 7760  = fibSeq' n7738 7738 n7739 7739 n
  | n < 7780  = fibSeq' n7758 7758 n7759 7759 n
  | n < 7800  = fibSeq' n7778 7778 n7779 7779 n
  | n < 7820  = fibSeq' n7798 7798 n7799 7799 n
  | n < 7840  = fibSeq' n7818 7818 n7819 7819 n
  | n < 7860  = fibSeq' n7838 7838 n7839 7839 n
  | n < 7880  = fibSeq' n7858 7858 n7859 7859 n
  | n < 7900  = fibSeq' n7878 7878 n7879 7879 n
  | n < 7920  = fibSeq' n7898 7898 n7899 7899 n
  | n < 7940  = fibSeq' n7918 7918 n7919 7919 n
  | n < 7960  = fibSeq' n7938 7938 n7939 7939 n
  | n < 7980  = fibSeq' n7958 7958 n7959 7959 n
  | n < 8000  = fibSeq' n7978 7978 n7979 7979 n
  | n < 8020  = fibSeq' n7998 7998 n7999 7999 n
  | n < 8040  = fibSeq' n8018 8018 n8019 8019 n
  | n < 8060  = fibSeq' n8038 8038 n8039 8039 n
  | n < 8080  = fibSeq' n8058 8058 n8059 8059 n
  | n < 8100  = fibSeq' n8078 8078 n8079 8079 n
  | n < 8120  = fibSeq' n8098 8098 n8099 8099 n
  | n < 8140  = fibSeq' n8118 8118 n8119 8119 n
  | n < 8160  = fibSeq' n8138 8138 n8139 8139 n
  | n < 8180  = fibSeq' n8158 8158 n8159 8159 n
  | n < 8200  = fibSeq' n8178 8178 n8179 8179 n
  | n < 8220  = fibSeq' n8198 8198 n8199 8199 n
  | n < 8240  = fibSeq' n8218 8218 n8219 8219 n
  | n < 8260  = fibSeq' n8238 8238 n8239 8239 n
  | n < 8280  = fibSeq' n8258 8258 n8259 8259 n
  | n < 8300  = fibSeq' n8278 8278 n8279 8279 n
  | n < 8320  = fibSeq' n8298 8298 n8299 8299 n
  | n < 8340  = fibSeq' n8318 8318 n8319 8319 n
  | n < 8360  = fibSeq' n8338 8338 n8339 8339 n
  | n < 8380  = fibSeq' n8358 8358 n8359 8359 n
  | n < 8400  = fibSeq' n8378 8378 n8379 8379 n
  | n < 8420  = fibSeq' n8398 8398 n8399 8399 n
  | n < 8440  = fibSeq' n8418 8418 n8419 8419 n
  | n < 8460  = fibSeq' n8438 8438 n8439 8439 n
  | n < 8480  = fibSeq' n8458 8458 n8459 8459 n
  | n < 8500  = fibSeq' n8478 8478 n8479 8479 n
  | n < 8520  = fibSeq' n8498 8498 n8499 8499 n
  | n < 8540  = fibSeq' n8518 8518 n8519 8519 n
  | n < 8560  = fibSeq' n8538 8538 n8539 8539 n
  | n < 8580  = fibSeq' n8558 8558 n8559 8559 n
  | n < 8600  = fibSeq' n8578 8578 n8579 8579 n
  | n < 8620  = fibSeq' n8598 8598 n8599 8599 n
  | n < 8640  = fibSeq' n8618 8618 n8619 8619 n
  | n < 8660  = fibSeq' n8638 8638 n8639 8639 n
  | n < 8680  = fibSeq' n8658 8658 n8659 8659 n
  | n < 8700  = fibSeq' n8678 8678 n8679 8679 n
  | n < 8720  = fibSeq' n8698 8698 n8699 8699 n
  | n < 8740  = fibSeq' n8718 8718 n8719 8719 n
  | n < 8760  = fibSeq' n8738 8738 n8739 8739 n
  | n < 8780  = fibSeq' n8758 8758 n8759 8759 n
  | n < 8800  = fibSeq' n8778 8778 n8779 8779 n
  | n < 8820  = fibSeq' n8798 8798 n8799 8799 n
  | n < 8840  = fibSeq' n8818 8818 n8819 8819 n
  | n < 8860  = fibSeq' n8838 8838 n8839 8839 n
  | n < 8880  = fibSeq' n8858 8858 n8859 8859 n
  | n < 8900  = fibSeq' n8878 8878 n8879 8879 n
  | n < 8920  = fibSeq' n8898 8898 n8899 8899 n
  | n < 8940  = fibSeq' n8918 8918 n8919 8919 n
  | n < 8960  = fibSeq' n8938 8938 n8939 8939 n
  | n < 8980  = fibSeq' n8958 8958 n8959 8959 n
  | n < 9000  = fibSeq' n8978 8978 n8979 8979 n
  | n < 9020  = fibSeq' n8998 8998 n8999 8999 n
  | n < 9040  = fibSeq' n9018 9018 n9019 9019 n
  | n < 9060  = fibSeq' n9038 9038 n9039 9039 n
  | n < 9080  = fibSeq' n9058 9058 n9059 9059 n
  | n < 9100  = fibSeq' n9078 9078 n9079 9079 n
  | n < 9120  = fibSeq' n9098 9098 n9099 9099 n
  | n < 9140  = fibSeq' n9118 9118 n9119 9119 n
  | n < 9160  = fibSeq' n9138 9138 n9139 9139 n
  | n < 9180  = fibSeq' n9158 9158 n9159 9159 n
  | n < 9200  = fibSeq' n9178 9178 n9179 9179 n
  | n < 9220  = fibSeq' n9198 9198 n9199 9199 n
  | n < 9240  = fibSeq' n9218 9218 n9219 9219 n
  | n < 9260  = fibSeq' n9238 9238 n9239 9239 n
  | n < 9280  = fibSeq' n9258 9258 n9259 9259 n
  | n < 9300  = fibSeq' n9278 9278 n9279 9279 n
  | n < 9320  = fibSeq' n9298 9298 n9299 9299 n
  | n < 9340  = fibSeq' n9318 9318 n9319 9319 n
  | n < 9360  = fibSeq' n9338 9338 n9339 9339 n
  | n < 9380  = fibSeq' n9358 9358 n9359 9359 n
  | n < 9400  = fibSeq' n9378 9378 n9379 9379 n
  | n < 9420  = fibSeq' n9398 9398 n9399 9399 n
  | n < 9440  = fibSeq' n9418 9418 n9419 9419 n
  | n < 9460  = fibSeq' n9438 9438 n9439 9439 n
  | n < 9480  = fibSeq' n9458 9458 n9459 9459 n
  | n < 9500  = fibSeq' n9478 9478 n9479 9479 n
  | n < 9520  = fibSeq' n9498 9498 n9499 9499 n
  | n < 9540  = fibSeq' n9518 9518 n9519 9519 n
  | n < 9560  = fibSeq' n9538 9538 n9539 9539 n
  | n < 9580  = fibSeq' n9558 9558 n9559 9559 n
  | n < 9600  = fibSeq' n9578 9578 n9579 9579 n
  | n < 9620  = fibSeq' n9598 9598 n9599 9599 n
  | n < 9640  = fibSeq' n9618 9618 n9619 9619 n
  | n < 9660  = fibSeq' n9638 9638 n9639 9639 n
  | n < 9680  = fibSeq' n9658 9658 n9659 9659 n
  | n < 9700  = fibSeq' n9678 9678 n9679 9679 n
  | n < 9720  = fibSeq' n9698 9698 n9699 9699 n
  | n < 9740  = fibSeq' n9718 9718 n9719 9719 n
  | n < 9760  = fibSeq' n9738 9738 n9739 9739 n
  | n < 9780  = fibSeq' n9758 9758 n9759 9759 n
  | n < 9800  = fibSeq' n9778 9778 n9779 9779 n
  | n < 9820  = fibSeq' n9798 9798 n9799 9799 n
  | n < 9840  = fibSeq' n9818 9818 n9819 9819 n
  | n < 9860  = fibSeq' n9838 9838 n9839 9839 n
  | n < 9880  = fibSeq' n9858 9858 n9859 9859 n
  | n < 9900  = fibSeq' n9878 9878 n9879 9879 n
  | n < 9920  = fibSeq' n9898 9898 n9899 9899 n
  | n < 9940  = fibSeq' n9918 9918 n9919 9919 n
  | n < 9960  = fibSeq' n9938 9938 n9939 9939 n
  | n < 9980  = fibSeq' n9958 9958 n9959 9959 n
  | n < 10000  = fibSeq' n9978 9978 n9979 9979 n
  | n < 10020  = fibSeq' n9998 9998 n9999 9999 n
  | n < 10040  = fibSeq' n10018 10018 n10019 10019 n
  | n < 10060  = fibSeq' n10038 10038 n10039 10039 n
  | n < 10080  = fibSeq' n10058 10058 n10059 10059 n
  | n < 10100  = fibSeq' n10078 10078 n10079 10079 n
  | n < 10120  = fibSeq' n10098 10098 n10099 10099 n
  | n < 10140  = fibSeq' n10118 10118 n10119 10119 n
  | n < 10160  = fibSeq' n10138 10138 n10139 10139 n
  | n < 10180  = fibSeq' n10158 10158 n10159 10159 n
  | n < 10200  = fibSeq' n10178 10178 n10179 10179 n
  | n < 10220  = fibSeq' n10198 10198 n10199 10199 n
  | n < 10240  = fibSeq' n10218 10218 n10219 10219 n
  | n < 10260  = fibSeq' n10238 10238 n10239 10239 n
  | n < 10280  = fibSeq' n10258 10258 n10259 10259 n
  | n < 10300  = fibSeq' n10278 10278 n10279 10279 n
  | n < 10320  = fibSeq' n10298 10298 n10299 10299 n
  | n < 10340  = fibSeq' n10318 10318 n10319 10319 n
  | n < 10360  = fibSeq' n10338 10338 n10339 10339 n
  | n < 10380  = fibSeq' n10358 10358 n10359 10359 n
  | n < 10400  = fibSeq' n10378 10378 n10379 10379 n
  | n < 10420  = fibSeq' n10398 10398 n10399 10399 n
  | n < 10440  = fibSeq' n10418 10418 n10419 10419 n
  | n < 10460  = fibSeq' n10438 10438 n10439 10439 n
  | n < 10480  = fibSeq' n10458 10458 n10459 10459 n
  | n < 10500  = fibSeq' n10478 10478 n10479 10479 n
  | n < 10520  = fibSeq' n10498 10498 n10499 10499 n
  | n < 10540  = fibSeq' n10518 10518 n10519 10519 n
  | n < 10560  = fibSeq' n10538 10538 n10539 10539 n
  | n < 10580  = fibSeq' n10558 10558 n10559 10559 n
  | n < 10600  = fibSeq' n10578 10578 n10579 10579 n
  | n < 10620  = fibSeq' n10598 10598 n10599 10599 n
  | n < 10640  = fibSeq' n10618 10618 n10619 10619 n
  | n < 10660  = fibSeq' n10638 10638 n10639 10639 n
  | n < 10680  = fibSeq' n10658 10658 n10659 10659 n
  | n < 10700  = fibSeq' n10678 10678 n10679 10679 n
  | n < 10720  = fibSeq' n10698 10698 n10699 10699 n
  | n < 10740  = fibSeq' n10718 10718 n10719 10719 n
  | n < 10760  = fibSeq' n10738 10738 n10739 10739 n
  | n < 10780  = fibSeq' n10758 10758 n10759 10759 n
  | n < 10800  = fibSeq' n10778 10778 n10779 10779 n
  | n < 10820  = fibSeq' n10798 10798 n10799 10799 n
  | n < 10840  = fibSeq' n10818 10818 n10819 10819 n
  | n < 10860  = fibSeq' n10838 10838 n10839 10839 n
  | n < 10880  = fibSeq' n10858 10858 n10859 10859 n
  | n < 10900  = fibSeq' n10878 10878 n10879 10879 n
  | n < 10920  = fibSeq' n10898 10898 n10899 10899 n
  | n < 10940  = fibSeq' n10918 10918 n10919 10919 n
  | n < 10960  = fibSeq' n10938 10938 n10939 10939 n
  | n < 10980  = fibSeq' n10958 10958 n10959 10959 n
  | n < 11000  = fibSeq' n10978 10978 n10979 10979 n
  | n < 11020  = fibSeq' n10998 10998 n10999 10999 n
  | n < 11040  = fibSeq' n11018 11018 n11019 11019 n
  | n < 11060  = fibSeq' n11038 11038 n11039 11039 n
  | n < 11080  = fibSeq' n11058 11058 n11059 11059 n
  | n < 11100  = fibSeq' n11078 11078 n11079 11079 n
  | n < 11120  = fibSeq' n11098 11098 n11099 11099 n
  | n < 11140  = fibSeq' n11118 11118 n11119 11119 n
  | n < 11160  = fibSeq' n11138 11138 n11139 11139 n
  | n < 11180  = fibSeq' n11158 11158 n11159 11159 n
  | n < 11200  = fibSeq' n11178 11178 n11179 11179 n
  | n < 11220  = fibSeq' n11198 11198 n11199 11199 n
  | n < 11240  = fibSeq' n11218 11218 n11219 11219 n
  | n < 11260  = fibSeq' n11238 11238 n11239 11239 n
  | n < 11280  = fibSeq' n11258 11258 n11259 11259 n
  | n < 11300  = fibSeq' n11278 11278 n11279 11279 n
  | n < 11320  = fibSeq' n11298 11298 n11299 11299 n
  | n < 11340  = fibSeq' n11318 11318 n11319 11319 n
  | n < 11360  = fibSeq' n11338 11338 n11339 11339 n
  | n < 11380  = fibSeq' n11358 11358 n11359 11359 n
  | n < 11400  = fibSeq' n11378 11378 n11379 11379 n
  | n < 11420  = fibSeq' n11398 11398 n11399 11399 n
  | n < 11440  = fibSeq' n11418 11418 n11419 11419 n
  | n < 11460  = fibSeq' n11438 11438 n11439 11439 n
  | n < 11480  = fibSeq' n11458 11458 n11459 11459 n
  | n < 11500  = fibSeq' n11478 11478 n11479 11479 n
  | n < 11520  = fibSeq' n11498 11498 n11499 11499 n
  | n < 11540  = fibSeq' n11518 11518 n11519 11519 n
  | n < 11560  = fibSeq' n11538 11538 n11539 11539 n
  | n < 11580  = fibSeq' n11558 11558 n11559 11559 n
  | n < 11600  = fibSeq' n11578 11578 n11579 11579 n
  | n < 11620  = fibSeq' n11598 11598 n11599 11599 n
  | n < 11640  = fibSeq' n11618 11618 n11619 11619 n
  | n < 11660  = fibSeq' n11638 11638 n11639 11639 n
  | n < 11680  = fibSeq' n11658 11658 n11659 11659 n
  | n < 11700  = fibSeq' n11678 11678 n11679 11679 n
  | n < 11720  = fibSeq' n11698 11698 n11699 11699 n
  | n < 11740  = fibSeq' n11718 11718 n11719 11719 n
  | n < 11760  = fibSeq' n11738 11738 n11739 11739 n
  | n < 11780  = fibSeq' n11758 11758 n11759 11759 n
  | n < 11800  = fibSeq' n11778 11778 n11779 11779 n
  | n < 11820  = fibSeq' n11798 11798 n11799 11799 n
  | n < 11840  = fibSeq' n11818 11818 n11819 11819 n
  | n < 11860  = fibSeq' n11838 11838 n11839 11839 n
  | n < 11880  = fibSeq' n11858 11858 n11859 11859 n
  | n < 11900  = fibSeq' n11878 11878 n11879 11879 n
  | n < 11920  = fibSeq' n11898 11898 n11899 11899 n
  | n < 11940  = fibSeq' n11918 11918 n11919 11919 n
  | n < 11960  = fibSeq' n11938 11938 n11939 11939 n
  | n < 11980  = fibSeq' n11958 11958 n11959 11959 n
  | n < 12000  = fibSeq' n11978 11978 n11979 11979 n
  | n < 12020  = fibSeq' n11998 11998 n11999 11999 n
  | n < 12040  = fibSeq' n12018 12018 n12019 12019 n
  | n < 12060  = fibSeq' n12038 12038 n12039 12039 n
  | n < 12080  = fibSeq' n12058 12058 n12059 12059 n
  | n < 12100  = fibSeq' n12078 12078 n12079 12079 n
  | n < 12120  = fibSeq' n12098 12098 n12099 12099 n
  | n < 12140  = fibSeq' n12118 12118 n12119 12119 n
  | n < 12160  = fibSeq' n12138 12138 n12139 12139 n
  | n < 12180  = fibSeq' n12158 12158 n12159 12159 n
  | n < 12200  = fibSeq' n12178 12178 n12179 12179 n
  | n < 12220  = fibSeq' n12198 12198 n12199 12199 n
  | n < 12240  = fibSeq' n12218 12218 n12219 12219 n
  | n < 12260  = fibSeq' n12238 12238 n12239 12239 n
  | n < 12280  = fibSeq' n12258 12258 n12259 12259 n
  | n < 12300  = fibSeq' n12278 12278 n12279 12279 n
  | n < 12320  = fibSeq' n12298 12298 n12299 12299 n
  | n < 12340  = fibSeq' n12318 12318 n12319 12319 n
  | n < 12360  = fibSeq' n12338 12338 n12339 12339 n
  | n < 12380  = fibSeq' n12358 12358 n12359 12359 n
  | n < 12400  = fibSeq' n12378 12378 n12379 12379 n
  | n < 12420  = fibSeq' n12398 12398 n12399 12399 n
  | n < 12440  = fibSeq' n12418 12418 n12419 12419 n
  | n < 12460  = fibSeq' n12438 12438 n12439 12439 n
  | n < 12480  = fibSeq' n12458 12458 n12459 12459 n
  | n < 12500  = fibSeq' n12478 12478 n12479 12479 n
  | n < 12520  = fibSeq' n12498 12498 n12499 12499 n
  | n < 12540  = fibSeq' n12518 12518 n12519 12519 n
  | n < 12560  = fibSeq' n12538 12538 n12539 12539 n
  | n < 12580  = fibSeq' n12558 12558 n12559 12559 n
  | n < 12600  = fibSeq' n12578 12578 n12579 12579 n
  | n < 12620  = fibSeq' n12598 12598 n12599 12599 n
  | n < 12640  = fibSeq' n12618 12618 n12619 12619 n
  | n < 12660  = fibSeq' n12638 12638 n12639 12639 n
  | n < 12680  = fibSeq' n12658 12658 n12659 12659 n
  | n < 12700  = fibSeq' n12678 12678 n12679 12679 n
  | n < 12720  = fibSeq' n12698 12698 n12699 12699 n
  | n < 12740  = fibSeq' n12718 12718 n12719 12719 n
  | n < 12760  = fibSeq' n12738 12738 n12739 12739 n
  | n < 12780  = fibSeq' n12758 12758 n12759 12759 n
  | n < 12800  = fibSeq' n12778 12778 n12779 12779 n
  | n < 12820  = fibSeq' n12798 12798 n12799 12799 n
  | n < 12840  = fibSeq' n12818 12818 n12819 12819 n
  | n < 12860  = fibSeq' n12838 12838 n12839 12839 n
  | n < 12880  = fibSeq' n12858 12858 n12859 12859 n
  | n < 12900  = fibSeq' n12878 12878 n12879 12879 n
  | n < 12920  = fibSeq' n12898 12898 n12899 12899 n
  | n < 12940  = fibSeq' n12918 12918 n12919 12919 n
  | n < 12960  = fibSeq' n12938 12938 n12939 12939 n
  | n < 12980  = fibSeq' n12958 12958 n12959 12959 n
  | n < 13000  = fibSeq' n12978 12978 n12979 12979 n
  | n < 13020  = fibSeq' n12998 12998 n12999 12999 n
  | n < 13040  = fibSeq' n13018 13018 n13019 13019 n
  | n < 13060  = fibSeq' n13038 13038 n13039 13039 n
  | n < 13080  = fibSeq' n13058 13058 n13059 13059 n
  | n < 13100  = fibSeq' n13078 13078 n13079 13079 n
  | n < 13120  = fibSeq' n13098 13098 n13099 13099 n
  | n < 13140  = fibSeq' n13118 13118 n13119 13119 n
  | n < 13160  = fibSeq' n13138 13138 n13139 13139 n
  | n < 13180  = fibSeq' n13158 13158 n13159 13159 n
  | n < 13200  = fibSeq' n13178 13178 n13179 13179 n
  | n < 13220  = fibSeq' n13198 13198 n13199 13199 n
  | n < 13240  = fibSeq' n13218 13218 n13219 13219 n
  | n < 13260  = fibSeq' n13238 13238 n13239 13239 n
  | n < 13280  = fibSeq' n13258 13258 n13259 13259 n
  | n < 13300  = fibSeq' n13278 13278 n13279 13279 n
  | n < 13320  = fibSeq' n13298 13298 n13299 13299 n
  | n < 13340  = fibSeq' n13318 13318 n13319 13319 n
  | n < 13360  = fibSeq' n13338 13338 n13339 13339 n
  | n < 13380  = fibSeq' n13358 13358 n13359 13359 n
  | n < 13400  = fibSeq' n13378 13378 n13379 13379 n
  | n < 13420  = fibSeq' n13398 13398 n13399 13399 n
  | n < 13440  = fibSeq' n13418 13418 n13419 13419 n
  | n < 13460  = fibSeq' n13438 13438 n13439 13439 n
  | n < 13480  = fibSeq' n13458 13458 n13459 13459 n
  | n < 13500  = fibSeq' n13478 13478 n13479 13479 n
  | n < 13520  = fibSeq' n13498 13498 n13499 13499 n
  | n < 13540  = fibSeq' n13518 13518 n13519 13519 n
  | n < 13560  = fibSeq' n13538 13538 n13539 13539 n
  | n < 13580  = fibSeq' n13558 13558 n13559 13559 n
  | n < 13600  = fibSeq' n13578 13578 n13579 13579 n
  | n < 13620  = fibSeq' n13598 13598 n13599 13599 n
  | n < 13640  = fibSeq' n13618 13618 n13619 13619 n
  | n < 13660  = fibSeq' n13638 13638 n13639 13639 n
  | n < 13680  = fibSeq' n13658 13658 n13659 13659 n
  | n < 13700  = fibSeq' n13678 13678 n13679 13679 n
  | n < 13720  = fibSeq' n13698 13698 n13699 13699 n
  | n < 13740  = fibSeq' n13718 13718 n13719 13719 n
  | n < 13760  = fibSeq' n13738 13738 n13739 13739 n
  | n < 13780  = fibSeq' n13758 13758 n13759 13759 n
  | n < 13800  = fibSeq' n13778 13778 n13779 13779 n
  | n < 13820  = fibSeq' n13798 13798 n13799 13799 n
  | n < 13840  = fibSeq' n13818 13818 n13819 13819 n
  | n < 13860  = fibSeq' n13838 13838 n13839 13839 n
  | n < 13880  = fibSeq' n13858 13858 n13859 13859 n
  | n < 13900  = fibSeq' n13878 13878 n13879 13879 n
  | n < 13920  = fibSeq' n13898 13898 n13899 13899 n
  | n < 13940  = fibSeq' n13918 13918 n13919 13919 n
  | n < 13960  = fibSeq' n13938 13938 n13939 13939 n
  | n < 13980  = fibSeq' n13958 13958 n13959 13959 n
  | n < 14000  = fibSeq' n13978 13978 n13979 13979 n
  | n < 14020  = fibSeq' n13998 13998 n13999 13999 n
  | n < 14040  = fibSeq' n14018 14018 n14019 14019 n
  | n < 14060  = fibSeq' n14038 14038 n14039 14039 n
  | n < 14080  = fibSeq' n14058 14058 n14059 14059 n
  | n < 14100  = fibSeq' n14078 14078 n14079 14079 n
  | n < 14120  = fibSeq' n14098 14098 n14099 14099 n
  | n < 14140  = fibSeq' n14118 14118 n14119 14119 n
  | n < 14160  = fibSeq' n14138 14138 n14139 14139 n
  | n < 14180  = fibSeq' n14158 14158 n14159 14159 n
  | n < 14200  = fibSeq' n14178 14178 n14179 14179 n
  | n < 14220  = fibSeq' n14198 14198 n14199 14199 n
  | n < 14240  = fibSeq' n14218 14218 n14219 14219 n
  | n < 14260  = fibSeq' n14238 14238 n14239 14239 n
  | n < 14280  = fibSeq' n14258 14258 n14259 14259 n
  | n < 14300  = fibSeq' n14278 14278 n14279 14279 n
  | n < 14320  = fibSeq' n14298 14298 n14299 14299 n
  | n < 14340  = fibSeq' n14318 14318 n14319 14319 n
  | n < 14360  = fibSeq' n14338 14338 n14339 14339 n
  | n < 14380  = fibSeq' n14358 14358 n14359 14359 n
  | n < 14400  = fibSeq' n14378 14378 n14379 14379 n
  | n < 14420  = fibSeq' n14398 14398 n14399 14399 n
  | n < 14440  = fibSeq' n14418 14418 n14419 14419 n
  | n < 14460  = fibSeq' n14438 14438 n14439 14439 n
  | n < 14480  = fibSeq' n14458 14458 n14459 14459 n
  | n < 14500  = fibSeq' n14478 14478 n14479 14479 n
  | n < 14520  = fibSeq' n14498 14498 n14499 14499 n
  | n < 14540  = fibSeq' n14518 14518 n14519 14519 n
  | n < 14560  = fibSeq' n14538 14538 n14539 14539 n
  | n < 14580  = fibSeq' n14558 14558 n14559 14559 n
  | n < 14600  = fibSeq' n14578 14578 n14579 14579 n
  | n < 14620  = fibSeq' n14598 14598 n14599 14599 n
  | n < 14640  = fibSeq' n14618 14618 n14619 14619 n
  | n < 14660  = fibSeq' n14638 14638 n14639 14639 n
  | n < 14680  = fibSeq' n14658 14658 n14659 14659 n
  | n < 14700  = fibSeq' n14678 14678 n14679 14679 n
  | n < 14720  = fibSeq' n14698 14698 n14699 14699 n
  | n < 14740  = fibSeq' n14718 14718 n14719 14719 n
  | n < 14760  = fibSeq' n14738 14738 n14739 14739 n
  | n < 14780  = fibSeq' n14758 14758 n14759 14759 n
  | n < 14800  = fibSeq' n14778 14778 n14779 14779 n
  | n < 14820  = fibSeq' n14798 14798 n14799 14799 n
  | n < 14840  = fibSeq' n14818 14818 n14819 14819 n
  | n < 14860  = fibSeq' n14838 14838 n14839 14839 n
  | n < 14880  = fibSeq' n14858 14858 n14859 14859 n
  | n < 14900  = fibSeq' n14878 14878 n14879 14879 n
  | n < 14920  = fibSeq' n14898 14898 n14899 14899 n
  | n < 14940  = fibSeq' n14918 14918 n14919 14919 n
  | n < 14960  = fibSeq' n14938 14938 n14939 14939 n
  | n < 14980  = fibSeq' n14958 14958 n14959 14959 n
  | n < 15000  = fibSeq' n14978 14978 n14979 14979 n
  | n < 15020  = fibSeq' n14998 14998 n14999 14999 n
  | n < 15040  = fibSeq' n15018 15018 n15019 15019 n
  | n < 15060  = fibSeq' n15038 15038 n15039 15039 n
  | n < 15080  = fibSeq' n15058 15058 n15059 15059 n
  | n < 15100  = fibSeq' n15078 15078 n15079 15079 n
  | n < 15120  = fibSeq' n15098 15098 n15099 15099 n
  | n < 15140  = fibSeq' n15118 15118 n15119 15119 n
  | n < 15160  = fibSeq' n15138 15138 n15139 15139 n
  | n < 15180  = fibSeq' n15158 15158 n15159 15159 n
  | n < 15200  = fibSeq' n15178 15178 n15179 15179 n
  | n < 15220  = fibSeq' n15198 15198 n15199 15199 n
  | n < 15240  = fibSeq' n15218 15218 n15219 15219 n
  | n < 15260  = fibSeq' n15238 15238 n15239 15239 n
  | n < 15280  = fibSeq' n15258 15258 n15259 15259 n
  | n < 15300  = fibSeq' n15278 15278 n15279 15279 n
  | n < 15320  = fibSeq' n15298 15298 n15299 15299 n
  | n < 15340  = fibSeq' n15318 15318 n15319 15319 n
  | n < 15360  = fibSeq' n15338 15338 n15339 15339 n
  | n < 15380  = fibSeq' n15358 15358 n15359 15359 n
  | n < 15400  = fibSeq' n15378 15378 n15379 15379 n
  | n < 15420  = fibSeq' n15398 15398 n15399 15399 n
  | n < 15440  = fibSeq' n15418 15418 n15419 15419 n
  | n < 15460  = fibSeq' n15438 15438 n15439 15439 n
  | n < 15480  = fibSeq' n15458 15458 n15459 15459 n
  | n < 15500  = fibSeq' n15478 15478 n15479 15479 n
  | n < 15520  = fibSeq' n15498 15498 n15499 15499 n
  | n < 15540  = fibSeq' n15518 15518 n15519 15519 n
  | n < 15560  = fibSeq' n15538 15538 n15539 15539 n
  | n < 15580  = fibSeq' n15558 15558 n15559 15559 n
  | n < 15600  = fibSeq' n15578 15578 n15579 15579 n
  | n < 15620  = fibSeq' n15598 15598 n15599 15599 n
  | n < 15640  = fibSeq' n15618 15618 n15619 15619 n
  | n < 15660  = fibSeq' n15638 15638 n15639 15639 n
  | n < 15680  = fibSeq' n15658 15658 n15659 15659 n
  | n < 15700  = fibSeq' n15678 15678 n15679 15679 n
  | n < 15720  = fibSeq' n15698 15698 n15699 15699 n
  | n < 15740  = fibSeq' n15718 15718 n15719 15719 n
  | n < 15760  = fibSeq' n15738 15738 n15739 15739 n
  | n < 15780  = fibSeq' n15758 15758 n15759 15759 n
  | n < 15800  = fibSeq' n15778 15778 n15779 15779 n
  | n < 15820  = fibSeq' n15798 15798 n15799 15799 n
  | n < 15840  = fibSeq' n15818 15818 n15819 15819 n
  | n < 15860  = fibSeq' n15838 15838 n15839 15839 n
  | n < 15880  = fibSeq' n15858 15858 n15859 15859 n
  | n < 15900  = fibSeq' n15878 15878 n15879 15879 n
  | n < 15920  = fibSeq' n15898 15898 n15899 15899 n
  | n < 15940  = fibSeq' n15918 15918 n15919 15919 n
  | n < 15960  = fibSeq' n15938 15938 n15939 15939 n
  | n < 15980  = fibSeq' n15958 15958 n15959 15959 n
  | n < 16000  = fibSeq' n15978 15978 n15979 15979 n
  | n < 16020  = fibSeq' n15998 15998 n15999 15999 n
  | n < 16040  = fibSeq' n16018 16018 n16019 16019 n
  | n < 16060  = fibSeq' n16038 16038 n16039 16039 n
  | n < 16080  = fibSeq' n16058 16058 n16059 16059 n
  | n < 16100  = fibSeq' n16078 16078 n16079 16079 n
  | n < 16120  = fibSeq' n16098 16098 n16099 16099 n
  | n < 16140  = fibSeq' n16118 16118 n16119 16119 n
  | n < 16160  = fibSeq' n16138 16138 n16139 16139 n
  | n < 16180  = fibSeq' n16158 16158 n16159 16159 n
  | n < 16200  = fibSeq' n16178 16178 n16179 16179 n
  | n < 16220  = fibSeq' n16198 16198 n16199 16199 n
  | n < 16240  = fibSeq' n16218 16218 n16219 16219 n
  | n < 16260  = fibSeq' n16238 16238 n16239 16239 n
  | n < 16280  = fibSeq' n16258 16258 n16259 16259 n
  | n < 16300  = fibSeq' n16278 16278 n16279 16279 n
  | n < 16320  = fibSeq' n16298 16298 n16299 16299 n
  | n < 16340  = fibSeq' n16318 16318 n16319 16319 n
  | n < 16360  = fibSeq' n16338 16338 n16339 16339 n
  | n < 16380  = fibSeq' n16358 16358 n16359 16359 n
  | n < 16400  = fibSeq' n16378 16378 n16379 16379 n
  | n < 16420  = fibSeq' n16398 16398 n16399 16399 n
  | n < 16440  = fibSeq' n16418 16418 n16419 16419 n
  | n < 16460  = fibSeq' n16438 16438 n16439 16439 n
  | n < 16480  = fibSeq' n16458 16458 n16459 16459 n
  | n < 16500  = fibSeq' n16478 16478 n16479 16479 n
  | n < 16520  = fibSeq' n16498 16498 n16499 16499 n
  | n < 16540  = fibSeq' n16518 16518 n16519 16519 n
  | n < 16560  = fibSeq' n16538 16538 n16539 16539 n
  | n < 16580  = fibSeq' n16558 16558 n16559 16559 n
  | n < 16600  = fibSeq' n16578 16578 n16579 16579 n
  | n < 16620  = fibSeq' n16598 16598 n16599 16599 n
  | n < 16640  = fibSeq' n16618 16618 n16619 16619 n
  | n < 16660  = fibSeq' n16638 16638 n16639 16639 n
  | n < 16680  = fibSeq' n16658 16658 n16659 16659 n
  | n < 16700  = fibSeq' n16678 16678 n16679 16679 n
  | n < 16720  = fibSeq' n16698 16698 n16699 16699 n
  | n < 16740  = fibSeq' n16718 16718 n16719 16719 n
  | n < 16760  = fibSeq' n16738 16738 n16739 16739 n
  | n < 16780  = fibSeq' n16758 16758 n16759 16759 n
  | n < 16800  = fibSeq' n16778 16778 n16779 16779 n
  | n < 16820  = fibSeq' n16798 16798 n16799 16799 n
  | n < 16840  = fibSeq' n16818 16818 n16819 16819 n
  | n < 16860  = fibSeq' n16838 16838 n16839 16839 n
  | n < 16880  = fibSeq' n16858 16858 n16859 16859 n
  | n < 16900  = fibSeq' n16878 16878 n16879 16879 n
  | n < 16920  = fibSeq' n16898 16898 n16899 16899 n
  | n < 16940  = fibSeq' n16918 16918 n16919 16919 n
  | n < 16960  = fibSeq' n16938 16938 n16939 16939 n
  | n < 16980  = fibSeq' n16958 16958 n16959 16959 n
  | n < 17000  = fibSeq' n16978 16978 n16979 16979 n
  | n < 17020  = fibSeq' n16998 16998 n16999 16999 n
  | n < 17040  = fibSeq' n17018 17018 n17019 17019 n
  | n < 17060  = fibSeq' n17038 17038 n17039 17039 n
  | n < 17080  = fibSeq' n17058 17058 n17059 17059 n
  | n < 17100  = fibSeq' n17078 17078 n17079 17079 n
  | n < 17120  = fibSeq' n17098 17098 n17099 17099 n
  | n < 17140  = fibSeq' n17118 17118 n17119 17119 n
  | n < 17160  = fibSeq' n17138 17138 n17139 17139 n
  | n < 17180  = fibSeq' n17158 17158 n17159 17159 n
  | n < 17200  = fibSeq' n17178 17178 n17179 17179 n
  | n < 17220  = fibSeq' n17198 17198 n17199 17199 n
  | n < 17240  = fibSeq' n17218 17218 n17219 17219 n
  | n < 17260  = fibSeq' n17238 17238 n17239 17239 n
  | n < 17280  = fibSeq' n17258 17258 n17259 17259 n
  | n < 17300  = fibSeq' n17278 17278 n17279 17279 n
  | n < 17320  = fibSeq' n17298 17298 n17299 17299 n
  | n < 17340  = fibSeq' n17318 17318 n17319 17319 n
  | n < 17360  = fibSeq' n17338 17338 n17339 17339 n
  | n < 17380  = fibSeq' n17358 17358 n17359 17359 n
  | n < 17400  = fibSeq' n17378 17378 n17379 17379 n
  | n < 17420  = fibSeq' n17398 17398 n17399 17399 n
  | n < 17440  = fibSeq' n17418 17418 n17419 17419 n
  | n < 17460  = fibSeq' n17438 17438 n17439 17439 n
  | n < 17480  = fibSeq' n17458 17458 n17459 17459 n
  | n < 17500  = fibSeq' n17478 17478 n17479 17479 n
  | n < 17520  = fibSeq' n17498 17498 n17499 17499 n
  | n < 17540  = fibSeq' n17518 17518 n17519 17519 n
  | n < 17560  = fibSeq' n17538 17538 n17539 17539 n
  | n < 17580  = fibSeq' n17558 17558 n17559 17559 n
  | n < 17600  = fibSeq' n17578 17578 n17579 17579 n
  | n < 17620  = fibSeq' n17598 17598 n17599 17599 n
  | n < 17640  = fibSeq' n17618 17618 n17619 17619 n
  | n < 17660  = fibSeq' n17638 17638 n17639 17639 n
  | n < 17680  = fibSeq' n17658 17658 n17659 17659 n
  | n < 17700  = fibSeq' n17678 17678 n17679 17679 n
  | n < 17720  = fibSeq' n17698 17698 n17699 17699 n
  | n < 17740  = fibSeq' n17718 17718 n17719 17719 n
  | n < 17760  = fibSeq' n17738 17738 n17739 17739 n
  | n < 17780  = fibSeq' n17758 17758 n17759 17759 n
  | n < 17800  = fibSeq' n17778 17778 n17779 17779 n
  | n < 17820  = fibSeq' n17798 17798 n17799 17799 n
  | n < 17840  = fibSeq' n17818 17818 n17819 17819 n
  | n < 17860  = fibSeq' n17838 17838 n17839 17839 n
  | n < 17880  = fibSeq' n17858 17858 n17859 17859 n
  | n < 17900  = fibSeq' n17878 17878 n17879 17879 n
  | n < 17920  = fibSeq' n17898 17898 n17899 17899 n
  | n < 17940  = fibSeq' n17918 17918 n17919 17919 n
  | n < 17960  = fibSeq' n17938 17938 n17939 17939 n
  | n < 17980  = fibSeq' n17958 17958 n17959 17959 n
  | n < 18000  = fibSeq' n17978 17978 n17979 17979 n
  | n < 18020  = fibSeq' n17998 17998 n17999 17999 n
  | n < 18040  = fibSeq' n18018 18018 n18019 18019 n
  | n < 18060  = fibSeq' n18038 18038 n18039 18039 n
  | n < 18080  = fibSeq' n18058 18058 n18059 18059 n
  | n < 18100  = fibSeq' n18078 18078 n18079 18079 n
  | n < 18120  = fibSeq' n18098 18098 n18099 18099 n
  | n < 18140  = fibSeq' n18118 18118 n18119 18119 n
  | n < 18160  = fibSeq' n18138 18138 n18139 18139 n
  | n < 18180  = fibSeq' n18158 18158 n18159 18159 n
  | n < 18200  = fibSeq' n18178 18178 n18179 18179 n
  | n < 18220  = fibSeq' n18198 18198 n18199 18199 n
  | n < 18240  = fibSeq' n18218 18218 n18219 18219 n
  | n < 18260  = fibSeq' n18238 18238 n18239 18239 n
  | n < 18280  = fibSeq' n18258 18258 n18259 18259 n
  | n < 18300  = fibSeq' n18278 18278 n18279 18279 n
  | n < 18320  = fibSeq' n18298 18298 n18299 18299 n
  | n < 18340  = fibSeq' n18318 18318 n18319 18319 n
  | n < 18360  = fibSeq' n18338 18338 n18339 18339 n
  | n < 18380  = fibSeq' n18358 18358 n18359 18359 n
  | n < 18400  = fibSeq' n18378 18378 n18379 18379 n
  | n < 18420  = fibSeq' n18398 18398 n18399 18399 n
  | n < 18440  = fibSeq' n18418 18418 n18419 18419 n
  | n < 18460  = fibSeq' n18438 18438 n18439 18439 n
  | n < 18480  = fibSeq' n18458 18458 n18459 18459 n
  | n < 18500  = fibSeq' n18478 18478 n18479 18479 n
  | n < 18520  = fibSeq' n18498 18498 n18499 18499 n
  | n < 18540  = fibSeq' n18518 18518 n18519 18519 n
  | n < 18560  = fibSeq' n18538 18538 n18539 18539 n
  | n < 18580  = fibSeq' n18558 18558 n18559 18559 n
  | n < 18600  = fibSeq' n18578 18578 n18579 18579 n
  | n < 18620  = fibSeq' n18598 18598 n18599 18599 n
  | n < 18640  = fibSeq' n18618 18618 n18619 18619 n
  | n < 18660  = fibSeq' n18638 18638 n18639 18639 n
  | n < 18680  = fibSeq' n18658 18658 n18659 18659 n
  | n < 18700  = fibSeq' n18678 18678 n18679 18679 n
  | n < 18720  = fibSeq' n18698 18698 n18699 18699 n
  | n < 18740  = fibSeq' n18718 18718 n18719 18719 n
  | n < 18760  = fibSeq' n18738 18738 n18739 18739 n
  | n < 18780  = fibSeq' n18758 18758 n18759 18759 n
  | n < 18800  = fibSeq' n18778 18778 n18779 18779 n
  | n < 18820  = fibSeq' n18798 18798 n18799 18799 n
  | n < 18840  = fibSeq' n18818 18818 n18819 18819 n
  | n < 18860  = fibSeq' n18838 18838 n18839 18839 n
  | n < 18880  = fibSeq' n18858 18858 n18859 18859 n
  | n < 18900  = fibSeq' n18878 18878 n18879 18879 n
  | n < 18920  = fibSeq' n18898 18898 n18899 18899 n
  | n < 18940  = fibSeq' n18918 18918 n18919 18919 n
  | n < 18960  = fibSeq' n18938 18938 n18939 18939 n
  | n < 18980  = fibSeq' n18958 18958 n18959 18959 n
  | n < 19000  = fibSeq' n18978 18978 n18979 18979 n
  | n < 19020  = fibSeq' n18998 18998 n18999 18999 n
  | n < 19040  = fibSeq' n19018 19018 n19019 19019 n
  | n < 19060  = fibSeq' n19038 19038 n19039 19039 n
  | n < 19080  = fibSeq' n19058 19058 n19059 19059 n
  | n < 19100  = fibSeq' n19078 19078 n19079 19079 n
  | n < 19120  = fibSeq' n19098 19098 n19099 19099 n
  | n < 19140  = fibSeq' n19118 19118 n19119 19119 n
  | n < 19160  = fibSeq' n19138 19138 n19139 19139 n
  | n < 19180  = fibSeq' n19158 19158 n19159 19159 n
  | n < 19200  = fibSeq' n19178 19178 n19179 19179 n
  | n < 19220  = fibSeq' n19198 19198 n19199 19199 n
  | n < 19240  = fibSeq' n19218 19218 n19219 19219 n
  | n < 19260  = fibSeq' n19238 19238 n19239 19239 n
  | n < 19280  = fibSeq' n19258 19258 n19259 19259 n
  | n < 19300  = fibSeq' n19278 19278 n19279 19279 n
  | n < 19320  = fibSeq' n19298 19298 n19299 19299 n
  | n < 19340  = fibSeq' n19318 19318 n19319 19319 n
  | n < 19360  = fibSeq' n19338 19338 n19339 19339 n
  | n < 19380  = fibSeq' n19358 19358 n19359 19359 n
  | n < 19400  = fibSeq' n19378 19378 n19379 19379 n
  | n < 19420  = fibSeq' n19398 19398 n19399 19399 n
  | n < 19440  = fibSeq' n19418 19418 n19419 19419 n
  | n < 19460  = fibSeq' n19438 19438 n19439 19439 n
  | n < 19480  = fibSeq' n19458 19458 n19459 19459 n
  | n < 19500  = fibSeq' n19478 19478 n19479 19479 n
  | n < 19520  = fibSeq' n19498 19498 n19499 19499 n
  | n < 19540  = fibSeq' n19518 19518 n19519 19519 n
  | n < 19560  = fibSeq' n19538 19538 n19539 19539 n
  | n < 19580  = fibSeq' n19558 19558 n19559 19559 n
  | n < 19600  = fibSeq' n19578 19578 n19579 19579 n
  | n < 19620  = fibSeq' n19598 19598 n19599 19599 n
  | n < 19640  = fibSeq' n19618 19618 n19619 19619 n
  | n < 19660  = fibSeq' n19638 19638 n19639 19639 n
  | n < 19680  = fibSeq' n19658 19658 n19659 19659 n
  | n < 19700  = fibSeq' n19678 19678 n19679 19679 n
  | n < 19720  = fibSeq' n19698 19698 n19699 19699 n
  | n < 19740  = fibSeq' n19718 19718 n19719 19719 n
  | n < 19760  = fibSeq' n19738 19738 n19739 19739 n
  | n < 19780  = fibSeq' n19758 19758 n19759 19759 n
  | n < 19800  = fibSeq' n19778 19778 n19779 19779 n
  | n < 19820  = fibSeq' n19798 19798 n19799 19799 n
  | n < 19840  = fibSeq' n19818 19818 n19819 19819 n
  | n < 19860  = fibSeq' n19838 19838 n19839 19839 n
  | n < 19880  = fibSeq' n19858 19858 n19859 19859 n
  | n < 19900  = fibSeq' n19878 19878 n19879 19879 n
  | n < 19920  = fibSeq' n19898 19898 n19899 19899 n
  | n < 19940  = fibSeq' n19918 19918 n19919 19919 n
  | n < 19960  = fibSeq' n19938 19938 n19939 19939 n
  | n < 19980  = fibSeq' n19958 19958 n19959 19959 n
  | n < 20000  = fibSeq' n19978 19978 n19979 19979 n
  | otherwise = fibSeq' n19998 19998 n19999 19999 n
  where
    n18 = fibboosted 18; n19 = fibboosted 19
    n38 = fibboosted 38; n39 = fibboosted 39
    n58 = fibboosted 58; n59 = fibboosted 59
    n78 = fibboosted 78; n79 = fibboosted 79
    n98 = fibboosted 98; n99 = fibboosted 99
    n118 = fibboosted 118; n119 = fibboosted 119
    n138 = fibboosted 138; n139 = fibboosted 139
    n158 = fibboosted 158; n159 = fibboosted 159
    n178 = fibboosted 178; n179 = fibboosted 179
    n198 = fibboosted 198; n199 = fibboosted 199
    n218 = fibboosted 218; n219 = fibboosted 219
    n238 = fibboosted 238; n239 = fibboosted 239
    n258 = fibboosted 258; n259 = fibboosted 259
    n278 = fibboosted 278; n279 = fibboosted 279
    n298 = fibboosted 298; n299 = fibboosted 299
    n318 = fibboosted 318; n319 = fibboosted 319
    n338 = fibboosted 338; n339 = fibboosted 339
    n358 = fibboosted 358; n359 = fibboosted 359
    n378 = fibboosted 378; n379 = fibboosted 379
    n398 = fibboosted 398; n399 = fibboosted 399
    n418 = fibboosted 418; n419 = fibboosted 419
    n438 = fibboosted 438; n439 = fibboosted 439
    n458 = fibboosted 458; n459 = fibboosted 459
    n478 = fibboosted 478; n479 = fibboosted 479
    n498 = fibboosted 498; n499 = fibboosted 499
    n518 = fibboosted 518; n519 = fibboosted 519
    n538 = fibboosted 538; n539 = fibboosted 539
    n558 = fibboosted 558; n559 = fibboosted 559
    n578 = fibboosted 578; n579 = fibboosted 579
    n598 = fibboosted 598; n599 = fibboosted 599
    n618 = fibboosted 618; n619 = fibboosted 619
    n638 = fibboosted 638; n639 = fibboosted 639
    n658 = fibboosted 658; n659 = fibboosted 659
    n678 = fibboosted 678; n679 = fibboosted 679
    n698 = fibboosted 698; n699 = fibboosted 699
    n718 = fibboosted 718; n719 = fibboosted 719
    n738 = fibboosted 738; n739 = fibboosted 739
    n758 = fibboosted 758; n759 = fibboosted 759
    n778 = fibboosted 778; n779 = fibboosted 779
    n798 = fibboosted 798; n799 = fibboosted 799
    n818 = fibboosted 818; n819 = fibboosted 819
    n838 = fibboosted 838; n839 = fibboosted 839
    n858 = fibboosted 858; n859 = fibboosted 859
    n878 = fibboosted 878; n879 = fibboosted 879
    n898 = fibboosted 898; n899 = fibboosted 899
    n918 = fibboosted 918; n919 = fibboosted 919
    n938 = fibboosted 938; n939 = fibboosted 939
    n958 = fibboosted 958; n959 = fibboosted 959
    n978 = fibboosted 978; n979 = fibboosted 979
    n998 = fibboosted 998; n999 = fibboosted 999
    n1018 = fibboosted 1018; n1019 = fibboosted 1019
    n1038 = fibboosted 1038; n1039 = fibboosted 1039
    n1058 = fibboosted 1058; n1059 = fibboosted 1059
    n1078 = fibboosted 1078; n1079 = fibboosted 1079
    n1098 = fibboosted 1098; n1099 = fibboosted 1099
    n1118 = fibboosted 1118; n1119 = fibboosted 1119
    n1138 = fibboosted 1138; n1139 = fibboosted 1139
    n1158 = fibboosted 1158; n1159 = fibboosted 1159
    n1178 = fibboosted 1178; n1179 = fibboosted 1179
    n1198 = fibboosted 1198; n1199 = fibboosted 1199
    n1218 = fibboosted 1218; n1219 = fibboosted 1219
    n1238 = fibboosted 1238; n1239 = fibboosted 1239
    n1258 = fibboosted 1258; n1259 = fibboosted 1259
    n1278 = fibboosted 1278; n1279 = fibboosted 1279
    n1298 = fibboosted 1298; n1299 = fibboosted 1299
    n1318 = fibboosted 1318; n1319 = fibboosted 1319
    n1338 = fibboosted 1338; n1339 = fibboosted 1339
    n1358 = fibboosted 1358; n1359 = fibboosted 1359
    n1378 = fibboosted 1378; n1379 = fibboosted 1379
    n1398 = fibboosted 1398; n1399 = fibboosted 1399
    n1418 = fibboosted 1418; n1419 = fibboosted 1419
    n1438 = fibboosted 1438; n1439 = fibboosted 1439
    n1458 = fibboosted 1458; n1459 = fibboosted 1459
    n1478 = fibboosted 1478; n1479 = fibboosted 1479
    n1498 = fibboosted 1498; n1499 = fibboosted 1499
    n1518 = fibboosted 1518; n1519 = fibboosted 1519
    n1538 = fibboosted 1538; n1539 = fibboosted 1539
    n1558 = fibboosted 1558; n1559 = fibboosted 1559
    n1578 = fibboosted 1578; n1579 = fibboosted 1579
    n1598 = fibboosted 1598; n1599 = fibboosted 1599
    n1618 = fibboosted 1618; n1619 = fibboosted 1619
    n1638 = fibboosted 1638; n1639 = fibboosted 1639
    n1658 = fibboosted 1658; n1659 = fibboosted 1659
    n1678 = fibboosted 1678; n1679 = fibboosted 1679
    n1698 = fibboosted 1698; n1699 = fibboosted 1699
    n1718 = fibboosted 1718; n1719 = fibboosted 1719
    n1738 = fibboosted 1738; n1739 = fibboosted 1739
    n1758 = fibboosted 1758; n1759 = fibboosted 1759
    n1778 = fibboosted 1778; n1779 = fibboosted 1779
    n1798 = fibboosted 1798; n1799 = fibboosted 1799
    n1818 = fibboosted 1818; n1819 = fibboosted 1819
    n1838 = fibboosted 1838; n1839 = fibboosted 1839
    n1858 = fibboosted 1858; n1859 = fibboosted 1859
    n1878 = fibboosted 1878; n1879 = fibboosted 1879
    n1898 = fibboosted 1898; n1899 = fibboosted 1899
    n1918 = fibboosted 1918; n1919 = fibboosted 1919
    n1938 = fibboosted 1938; n1939 = fibboosted 1939
    n1958 = fibboosted 1958; n1959 = fibboosted 1959
    n1978 = fibboosted 1978; n1979 = fibboosted 1979
    n1998 = fibboosted 1998; n1999 = fibboosted 1999
    n2018 = fibboosted 2018; n2019 = fibboosted 2019
    n2038 = fibboosted 2038; n2039 = fibboosted 2039
    n2058 = fibboosted 2058; n2059 = fibboosted 2059
    n2078 = fibboosted 2078; n2079 = fibboosted 2079
    n2098 = fibboosted 2098; n2099 = fibboosted 2099
    n2118 = fibboosted 2118; n2119 = fibboosted 2119
    n2138 = fibboosted 2138; n2139 = fibboosted 2139
    n2158 = fibboosted 2158; n2159 = fibboosted 2159
    n2178 = fibboosted 2178; n2179 = fibboosted 2179
    n2198 = fibboosted 2198; n2199 = fibboosted 2199
    n2218 = fibboosted 2218; n2219 = fibboosted 2219
    n2238 = fibboosted 2238; n2239 = fibboosted 2239
    n2258 = fibboosted 2258; n2259 = fibboosted 2259
    n2278 = fibboosted 2278; n2279 = fibboosted 2279
    n2298 = fibboosted 2298; n2299 = fibboosted 2299
    n2318 = fibboosted 2318; n2319 = fibboosted 2319
    n2338 = fibboosted 2338; n2339 = fibboosted 2339
    n2358 = fibboosted 2358; n2359 = fibboosted 2359
    n2378 = fibboosted 2378; n2379 = fibboosted 2379
    n2398 = fibboosted 2398; n2399 = fibboosted 2399
    n2418 = fibboosted 2418; n2419 = fibboosted 2419
    n2438 = fibboosted 2438; n2439 = fibboosted 2439
    n2458 = fibboosted 2458; n2459 = fibboosted 2459
    n2478 = fibboosted 2478; n2479 = fibboosted 2479
    n2498 = fibboosted 2498; n2499 = fibboosted 2499
    n2518 = fibboosted 2518; n2519 = fibboosted 2519
    n2538 = fibboosted 2538; n2539 = fibboosted 2539
    n2558 = fibboosted 2558; n2559 = fibboosted 2559
    n2578 = fibboosted 2578; n2579 = fibboosted 2579
    n2598 = fibboosted 2598; n2599 = fibboosted 2599
    n2618 = fibboosted 2618; n2619 = fibboosted 2619
    n2638 = fibboosted 2638; n2639 = fibboosted 2639
    n2658 = fibboosted 2658; n2659 = fibboosted 2659
    n2678 = fibboosted 2678; n2679 = fibboosted 2679
    n2698 = fibboosted 2698; n2699 = fibboosted 2699
    n2718 = fibboosted 2718; n2719 = fibboosted 2719
    n2738 = fibboosted 2738; n2739 = fibboosted 2739
    n2758 = fibboosted 2758; n2759 = fibboosted 2759
    n2778 = fibboosted 2778; n2779 = fibboosted 2779
    n2798 = fibboosted 2798; n2799 = fibboosted 2799
    n2818 = fibboosted 2818; n2819 = fibboosted 2819
    n2838 = fibboosted 2838; n2839 = fibboosted 2839
    n2858 = fibboosted 2858; n2859 = fibboosted 2859
    n2878 = fibboosted 2878; n2879 = fibboosted 2879
    n2898 = fibboosted 2898; n2899 = fibboosted 2899
    n2918 = fibboosted 2918; n2919 = fibboosted 2919
    n2938 = fibboosted 2938; n2939 = fibboosted 2939
    n2958 = fibboosted 2958; n2959 = fibboosted 2959
    n2978 = fibboosted 2978; n2979 = fibboosted 2979
    n2998 = fibboosted 2998; n2999 = fibboosted 2999
    n3018 = fibboosted 3018; n3019 = fibboosted 3019
    n3038 = fibboosted 3038; n3039 = fibboosted 3039
    n3058 = fibboosted 3058; n3059 = fibboosted 3059
    n3078 = fibboosted 3078; n3079 = fibboosted 3079
    n3098 = fibboosted 3098; n3099 = fibboosted 3099
    n3118 = fibboosted 3118; n3119 = fibboosted 3119
    n3138 = fibboosted 3138; n3139 = fibboosted 3139
    n3158 = fibboosted 3158; n3159 = fibboosted 3159
    n3178 = fibboosted 3178; n3179 = fibboosted 3179
    n3198 = fibboosted 3198; n3199 = fibboosted 3199
    n3218 = fibboosted 3218; n3219 = fibboosted 3219
    n3238 = fibboosted 3238; n3239 = fibboosted 3239
    n3258 = fibboosted 3258; n3259 = fibboosted 3259
    n3278 = fibboosted 3278; n3279 = fibboosted 3279
    n3298 = fibboosted 3298; n3299 = fibboosted 3299
    n3318 = fibboosted 3318; n3319 = fibboosted 3319
    n3338 = fibboosted 3338; n3339 = fibboosted 3339
    n3358 = fibboosted 3358; n3359 = fibboosted 3359
    n3378 = fibboosted 3378; n3379 = fibboosted 3379
    n3398 = fibboosted 3398; n3399 = fibboosted 3399
    n3418 = fibboosted 3418; n3419 = fibboosted 3419
    n3438 = fibboosted 3438; n3439 = fibboosted 3439
    n3458 = fibboosted 3458; n3459 = fibboosted 3459
    n3478 = fibboosted 3478; n3479 = fibboosted 3479
    n3498 = fibboosted 3498; n3499 = fibboosted 3499
    n3518 = fibboosted 3518; n3519 = fibboosted 3519
    n3538 = fibboosted 3538; n3539 = fibboosted 3539
    n3558 = fibboosted 3558; n3559 = fibboosted 3559
    n3578 = fibboosted 3578; n3579 = fibboosted 3579
    n3598 = fibboosted 3598; n3599 = fibboosted 3599
    n3618 = fibboosted 3618; n3619 = fibboosted 3619
    n3638 = fibboosted 3638; n3639 = fibboosted 3639
    n3658 = fibboosted 3658; n3659 = fibboosted 3659
    n3678 = fibboosted 3678; n3679 = fibboosted 3679
    n3698 = fibboosted 3698; n3699 = fibboosted 3699
    n3718 = fibboosted 3718; n3719 = fibboosted 3719
    n3738 = fibboosted 3738; n3739 = fibboosted 3739
    n3758 = fibboosted 3758; n3759 = fibboosted 3759
    n3778 = fibboosted 3778; n3779 = fibboosted 3779
    n3798 = fibboosted 3798; n3799 = fibboosted 3799
    n3818 = fibboosted 3818; n3819 = fibboosted 3819
    n3838 = fibboosted 3838; n3839 = fibboosted 3839
    n3858 = fibboosted 3858; n3859 = fibboosted 3859
    n3878 = fibboosted 3878; n3879 = fibboosted 3879
    n3898 = fibboosted 3898; n3899 = fibboosted 3899
    n3918 = fibboosted 3918; n3919 = fibboosted 3919
    n3938 = fibboosted 3938; n3939 = fibboosted 3939
    n3958 = fibboosted 3958; n3959 = fibboosted 3959
    n3978 = fibboosted 3978; n3979 = fibboosted 3979
    n3998 = fibboosted 3998; n3999 = fibboosted 3999
    n4018 = fibboosted 4018; n4019 = fibboosted 4019
    n4038 = fibboosted 4038; n4039 = fibboosted 4039
    n4058 = fibboosted 4058; n4059 = fibboosted 4059
    n4078 = fibboosted 4078; n4079 = fibboosted 4079
    n4098 = fibboosted 4098; n4099 = fibboosted 4099
    n4118 = fibboosted 4118; n4119 = fibboosted 4119
    n4138 = fibboosted 4138; n4139 = fibboosted 4139
    n4158 = fibboosted 4158; n4159 = fibboosted 4159
    n4178 = fibboosted 4178; n4179 = fibboosted 4179
    n4198 = fibboosted 4198; n4199 = fibboosted 4199
    n4218 = fibboosted 4218; n4219 = fibboosted 4219
    n4238 = fibboosted 4238; n4239 = fibboosted 4239
    n4258 = fibboosted 4258; n4259 = fibboosted 4259
    n4278 = fibboosted 4278; n4279 = fibboosted 4279
    n4298 = fibboosted 4298; n4299 = fibboosted 4299
    n4318 = fibboosted 4318; n4319 = fibboosted 4319
    n4338 = fibboosted 4338; n4339 = fibboosted 4339
    n4358 = fibboosted 4358; n4359 = fibboosted 4359
    n4378 = fibboosted 4378; n4379 = fibboosted 4379
    n4398 = fibboosted 4398; n4399 = fibboosted 4399
    n4418 = fibboosted 4418; n4419 = fibboosted 4419
    n4438 = fibboosted 4438; n4439 = fibboosted 4439
    n4458 = fibboosted 4458; n4459 = fibboosted 4459
    n4478 = fibboosted 4478; n4479 = fibboosted 4479
    n4498 = fibboosted 4498; n4499 = fibboosted 4499
    n4518 = fibboosted 4518; n4519 = fibboosted 4519
    n4538 = fibboosted 4538; n4539 = fibboosted 4539
    n4558 = fibboosted 4558; n4559 = fibboosted 4559
    n4578 = fibboosted 4578; n4579 = fibboosted 4579
    n4598 = fibboosted 4598; n4599 = fibboosted 4599
    n4618 = fibboosted 4618; n4619 = fibboosted 4619
    n4638 = fibboosted 4638; n4639 = fibboosted 4639
    n4658 = fibboosted 4658; n4659 = fibboosted 4659
    n4678 = fibboosted 4678; n4679 = fibboosted 4679
    n4698 = fibboosted 4698; n4699 = fibboosted 4699
    n4718 = fibboosted 4718; n4719 = fibboosted 4719
    n4738 = fibboosted 4738; n4739 = fibboosted 4739
    n4758 = fibboosted 4758; n4759 = fibboosted 4759
    n4778 = fibboosted 4778; n4779 = fibboosted 4779
    n4798 = fibboosted 4798; n4799 = fibboosted 4799
    n4818 = fibboosted 4818; n4819 = fibboosted 4819
    n4838 = fibboosted 4838; n4839 = fibboosted 4839
    n4858 = fibboosted 4858; n4859 = fibboosted 4859
    n4878 = fibboosted 4878; n4879 = fibboosted 4879
    n4898 = fibboosted 4898; n4899 = fibboosted 4899
    n4918 = fibboosted 4918; n4919 = fibboosted 4919
    n4938 = fibboosted 4938; n4939 = fibboosted 4939
    n4958 = fibboosted 4958; n4959 = fibboosted 4959
    n4978 = fibboosted 4978; n4979 = fibboosted 4979
    n4998 = fibboosted 4998; n4999 = fibboosted 4999
    n5018 = fibboosted 5018; n5019 = fibboosted 5019
    n5038 = fibboosted 5038; n5039 = fibboosted 5039
    n5058 = fibboosted 5058; n5059 = fibboosted 5059
    n5078 = fibboosted 5078; n5079 = fibboosted 5079
    n5098 = fibboosted 5098; n5099 = fibboosted 5099
    n5118 = fibboosted 5118; n5119 = fibboosted 5119
    n5138 = fibboosted 5138; n5139 = fibboosted 5139
    n5158 = fibboosted 5158; n5159 = fibboosted 5159
    n5178 = fibboosted 5178; n5179 = fibboosted 5179
    n5198 = fibboosted 5198; n5199 = fibboosted 5199
    n5218 = fibboosted 5218; n5219 = fibboosted 5219
    n5238 = fibboosted 5238; n5239 = fibboosted 5239
    n5258 = fibboosted 5258; n5259 = fibboosted 5259
    n5278 = fibboosted 5278; n5279 = fibboosted 5279
    n5298 = fibboosted 5298; n5299 = fibboosted 5299
    n5318 = fibboosted 5318; n5319 = fibboosted 5319
    n5338 = fibboosted 5338; n5339 = fibboosted 5339
    n5358 = fibboosted 5358; n5359 = fibboosted 5359
    n5378 = fibboosted 5378; n5379 = fibboosted 5379
    n5398 = fibboosted 5398; n5399 = fibboosted 5399
    n5418 = fibboosted 5418; n5419 = fibboosted 5419
    n5438 = fibboosted 5438; n5439 = fibboosted 5439
    n5458 = fibboosted 5458; n5459 = fibboosted 5459
    n5478 = fibboosted 5478; n5479 = fibboosted 5479
    n5498 = fibboosted 5498; n5499 = fibboosted 5499
    n5518 = fibboosted 5518; n5519 = fibboosted 5519
    n5538 = fibboosted 5538; n5539 = fibboosted 5539
    n5558 = fibboosted 5558; n5559 = fibboosted 5559
    n5578 = fibboosted 5578; n5579 = fibboosted 5579
    n5598 = fibboosted 5598; n5599 = fibboosted 5599
    n5618 = fibboosted 5618; n5619 = fibboosted 5619
    n5638 = fibboosted 5638; n5639 = fibboosted 5639
    n5658 = fibboosted 5658; n5659 = fibboosted 5659
    n5678 = fibboosted 5678; n5679 = fibboosted 5679
    n5698 = fibboosted 5698; n5699 = fibboosted 5699
    n5718 = fibboosted 5718; n5719 = fibboosted 5719
    n5738 = fibboosted 5738; n5739 = fibboosted 5739
    n5758 = fibboosted 5758; n5759 = fibboosted 5759
    n5778 = fibboosted 5778; n5779 = fibboosted 5779
    n5798 = fibboosted 5798; n5799 = fibboosted 5799
    n5818 = fibboosted 5818; n5819 = fibboosted 5819
    n5838 = fibboosted 5838; n5839 = fibboosted 5839
    n5858 = fibboosted 5858; n5859 = fibboosted 5859
    n5878 = fibboosted 5878; n5879 = fibboosted 5879
    n5898 = fibboosted 5898; n5899 = fibboosted 5899
    n5918 = fibboosted 5918; n5919 = fibboosted 5919
    n5938 = fibboosted 5938; n5939 = fibboosted 5939
    n5958 = fibboosted 5958; n5959 = fibboosted 5959
    n5978 = fibboosted 5978; n5979 = fibboosted 5979
    n5998 = fibboosted 5998; n5999 = fibboosted 5999
    n6018 = fibboosted 6018; n6019 = fibboosted 6019
    n6038 = fibboosted 6038; n6039 = fibboosted 6039
    n6058 = fibboosted 6058; n6059 = fibboosted 6059
    n6078 = fibboosted 6078; n6079 = fibboosted 6079
    n6098 = fibboosted 6098; n6099 = fibboosted 6099
    n6118 = fibboosted 6118; n6119 = fibboosted 6119
    n6138 = fibboosted 6138; n6139 = fibboosted 6139
    n6158 = fibboosted 6158; n6159 = fibboosted 6159
    n6178 = fibboosted 6178; n6179 = fibboosted 6179
    n6198 = fibboosted 6198; n6199 = fibboosted 6199
    n6218 = fibboosted 6218; n6219 = fibboosted 6219
    n6238 = fibboosted 6238; n6239 = fibboosted 6239
    n6258 = fibboosted 6258; n6259 = fibboosted 6259
    n6278 = fibboosted 6278; n6279 = fibboosted 6279
    n6298 = fibboosted 6298; n6299 = fibboosted 6299
    n6318 = fibboosted 6318; n6319 = fibboosted 6319
    n6338 = fibboosted 6338; n6339 = fibboosted 6339
    n6358 = fibboosted 6358; n6359 = fibboosted 6359
    n6378 = fibboosted 6378; n6379 = fibboosted 6379
    n6398 = fibboosted 6398; n6399 = fibboosted 6399
    n6418 = fibboosted 6418; n6419 = fibboosted 6419
    n6438 = fibboosted 6438; n6439 = fibboosted 6439
    n6458 = fibboosted 6458; n6459 = fibboosted 6459
    n6478 = fibboosted 6478; n6479 = fibboosted 6479
    n6498 = fibboosted 6498; n6499 = fibboosted 6499
    n6518 = fibboosted 6518; n6519 = fibboosted 6519
    n6538 = fibboosted 6538; n6539 = fibboosted 6539
    n6558 = fibboosted 6558; n6559 = fibboosted 6559
    n6578 = fibboosted 6578; n6579 = fibboosted 6579
    n6598 = fibboosted 6598; n6599 = fibboosted 6599
    n6618 = fibboosted 6618; n6619 = fibboosted 6619
    n6638 = fibboosted 6638; n6639 = fibboosted 6639
    n6658 = fibboosted 6658; n6659 = fibboosted 6659
    n6678 = fibboosted 6678; n6679 = fibboosted 6679
    n6698 = fibboosted 6698; n6699 = fibboosted 6699
    n6718 = fibboosted 6718; n6719 = fibboosted 6719
    n6738 = fibboosted 6738; n6739 = fibboosted 6739
    n6758 = fibboosted 6758; n6759 = fibboosted 6759
    n6778 = fibboosted 6778; n6779 = fibboosted 6779
    n6798 = fibboosted 6798; n6799 = fibboosted 6799
    n6818 = fibboosted 6818; n6819 = fibboosted 6819
    n6838 = fibboosted 6838; n6839 = fibboosted 6839
    n6858 = fibboosted 6858; n6859 = fibboosted 6859
    n6878 = fibboosted 6878; n6879 = fibboosted 6879
    n6898 = fibboosted 6898; n6899 = fibboosted 6899
    n6918 = fibboosted 6918; n6919 = fibboosted 6919
    n6938 = fibboosted 6938; n6939 = fibboosted 6939
    n6958 = fibboosted 6958; n6959 = fibboosted 6959
    n6978 = fibboosted 6978; n6979 = fibboosted 6979
    n6998 = fibboosted 6998; n6999 = fibboosted 6999
    n7018 = fibboosted 7018; n7019 = fibboosted 7019
    n7038 = fibboosted 7038; n7039 = fibboosted 7039
    n7058 = fibboosted 7058; n7059 = fibboosted 7059
    n7078 = fibboosted 7078; n7079 = fibboosted 7079
    n7098 = fibboosted 7098; n7099 = fibboosted 7099
    n7118 = fibboosted 7118; n7119 = fibboosted 7119
    n7138 = fibboosted 7138; n7139 = fibboosted 7139
    n7158 = fibboosted 7158; n7159 = fibboosted 7159
    n7178 = fibboosted 7178; n7179 = fibboosted 7179
    n7198 = fibboosted 7198; n7199 = fibboosted 7199
    n7218 = fibboosted 7218; n7219 = fibboosted 7219
    n7238 = fibboosted 7238; n7239 = fibboosted 7239
    n7258 = fibboosted 7258; n7259 = fibboosted 7259
    n7278 = fibboosted 7278; n7279 = fibboosted 7279
    n7298 = fibboosted 7298; n7299 = fibboosted 7299
    n7318 = fibboosted 7318; n7319 = fibboosted 7319
    n7338 = fibboosted 7338; n7339 = fibboosted 7339
    n7358 = fibboosted 7358; n7359 = fibboosted 7359
    n7378 = fibboosted 7378; n7379 = fibboosted 7379
    n7398 = fibboosted 7398; n7399 = fibboosted 7399
    n7418 = fibboosted 7418; n7419 = fibboosted 7419
    n7438 = fibboosted 7438; n7439 = fibboosted 7439
    n7458 = fibboosted 7458; n7459 = fibboosted 7459
    n7478 = fibboosted 7478; n7479 = fibboosted 7479
    n7498 = fibboosted 7498; n7499 = fibboosted 7499
    n7518 = fibboosted 7518; n7519 = fibboosted 7519
    n7538 = fibboosted 7538; n7539 = fibboosted 7539
    n7558 = fibboosted 7558; n7559 = fibboosted 7559
    n7578 = fibboosted 7578; n7579 = fibboosted 7579
    n7598 = fibboosted 7598; n7599 = fibboosted 7599
    n7618 = fibboosted 7618; n7619 = fibboosted 7619
    n7638 = fibboosted 7638; n7639 = fibboosted 7639
    n7658 = fibboosted 7658; n7659 = fibboosted 7659
    n7678 = fibboosted 7678; n7679 = fibboosted 7679
    n7698 = fibboosted 7698; n7699 = fibboosted 7699
    n7718 = fibboosted 7718; n7719 = fibboosted 7719
    n7738 = fibboosted 7738; n7739 = fibboosted 7739
    n7758 = fibboosted 7758; n7759 = fibboosted 7759
    n7778 = fibboosted 7778; n7779 = fibboosted 7779
    n7798 = fibboosted 7798; n7799 = fibboosted 7799
    n7818 = fibboosted 7818; n7819 = fibboosted 7819
    n7838 = fibboosted 7838; n7839 = fibboosted 7839
    n7858 = fibboosted 7858; n7859 = fibboosted 7859
    n7878 = fibboosted 7878; n7879 = fibboosted 7879
    n7898 = fibboosted 7898; n7899 = fibboosted 7899
    n7918 = fibboosted 7918; n7919 = fibboosted 7919
    n7938 = fibboosted 7938; n7939 = fibboosted 7939
    n7958 = fibboosted 7958; n7959 = fibboosted 7959
    n7978 = fibboosted 7978; n7979 = fibboosted 7979
    n7998 = fibboosted 7998; n7999 = fibboosted 7999
    n8018 = fibboosted 8018; n8019 = fibboosted 8019
    n8038 = fibboosted 8038; n8039 = fibboosted 8039
    n8058 = fibboosted 8058; n8059 = fibboosted 8059
    n8078 = fibboosted 8078; n8079 = fibboosted 8079
    n8098 = fibboosted 8098; n8099 = fibboosted 8099
    n8118 = fibboosted 8118; n8119 = fibboosted 8119
    n8138 = fibboosted 8138; n8139 = fibboosted 8139
    n8158 = fibboosted 8158; n8159 = fibboosted 8159
    n8178 = fibboosted 8178; n8179 = fibboosted 8179
    n8198 = fibboosted 8198; n8199 = fibboosted 8199
    n8218 = fibboosted 8218; n8219 = fibboosted 8219
    n8238 = fibboosted 8238; n8239 = fibboosted 8239
    n8258 = fibboosted 8258; n8259 = fibboosted 8259
    n8278 = fibboosted 8278; n8279 = fibboosted 8279
    n8298 = fibboosted 8298; n8299 = fibboosted 8299
    n8318 = fibboosted 8318; n8319 = fibboosted 8319
    n8338 = fibboosted 8338; n8339 = fibboosted 8339
    n8358 = fibboosted 8358; n8359 = fibboosted 8359
    n8378 = fibboosted 8378; n8379 = fibboosted 8379
    n8398 = fibboosted 8398; n8399 = fibboosted 8399
    n8418 = fibboosted 8418; n8419 = fibboosted 8419
    n8438 = fibboosted 8438; n8439 = fibboosted 8439
    n8458 = fibboosted 8458; n8459 = fibboosted 8459
    n8478 = fibboosted 8478; n8479 = fibboosted 8479
    n8498 = fibboosted 8498; n8499 = fibboosted 8499
    n8518 = fibboosted 8518; n8519 = fibboosted 8519
    n8538 = fibboosted 8538; n8539 = fibboosted 8539
    n8558 = fibboosted 8558; n8559 = fibboosted 8559
    n8578 = fibboosted 8578; n8579 = fibboosted 8579
    n8598 = fibboosted 8598; n8599 = fibboosted 8599
    n8618 = fibboosted 8618; n8619 = fibboosted 8619
    n8638 = fibboosted 8638; n8639 = fibboosted 8639
    n8658 = fibboosted 8658; n8659 = fibboosted 8659
    n8678 = fibboosted 8678; n8679 = fibboosted 8679
    n8698 = fibboosted 8698; n8699 = fibboosted 8699
    n8718 = fibboosted 8718; n8719 = fibboosted 8719
    n8738 = fibboosted 8738; n8739 = fibboosted 8739
    n8758 = fibboosted 8758; n8759 = fibboosted 8759
    n8778 = fibboosted 8778; n8779 = fibboosted 8779
    n8798 = fibboosted 8798; n8799 = fibboosted 8799
    n8818 = fibboosted 8818; n8819 = fibboosted 8819
    n8838 = fibboosted 8838; n8839 = fibboosted 8839
    n8858 = fibboosted 8858; n8859 = fibboosted 8859
    n8878 = fibboosted 8878; n8879 = fibboosted 8879
    n8898 = fibboosted 8898; n8899 = fibboosted 8899
    n8918 = fibboosted 8918; n8919 = fibboosted 8919
    n8938 = fibboosted 8938; n8939 = fibboosted 8939
    n8958 = fibboosted 8958; n8959 = fibboosted 8959
    n8978 = fibboosted 8978; n8979 = fibboosted 8979
    n8998 = fibboosted 8998; n8999 = fibboosted 8999
    n9018 = fibboosted 9018; n9019 = fibboosted 9019
    n9038 = fibboosted 9038; n9039 = fibboosted 9039
    n9058 = fibboosted 9058; n9059 = fibboosted 9059
    n9078 = fibboosted 9078; n9079 = fibboosted 9079
    n9098 = fibboosted 9098; n9099 = fibboosted 9099
    n9118 = fibboosted 9118; n9119 = fibboosted 9119
    n9138 = fibboosted 9138; n9139 = fibboosted 9139
    n9158 = fibboosted 9158; n9159 = fibboosted 9159
    n9178 = fibboosted 9178; n9179 = fibboosted 9179
    n9198 = fibboosted 9198; n9199 = fibboosted 9199
    n9218 = fibboosted 9218; n9219 = fibboosted 9219
    n9238 = fibboosted 9238; n9239 = fibboosted 9239
    n9258 = fibboosted 9258; n9259 = fibboosted 9259
    n9278 = fibboosted 9278; n9279 = fibboosted 9279
    n9298 = fibboosted 9298; n9299 = fibboosted 9299
    n9318 = fibboosted 9318; n9319 = fibboosted 9319
    n9338 = fibboosted 9338; n9339 = fibboosted 9339
    n9358 = fibboosted 9358; n9359 = fibboosted 9359
    n9378 = fibboosted 9378; n9379 = fibboosted 9379
    n9398 = fibboosted 9398; n9399 = fibboosted 9399
    n9418 = fibboosted 9418; n9419 = fibboosted 9419
    n9438 = fibboosted 9438; n9439 = fibboosted 9439
    n9458 = fibboosted 9458; n9459 = fibboosted 9459
    n9478 = fibboosted 9478; n9479 = fibboosted 9479
    n9498 = fibboosted 9498; n9499 = fibboosted 9499
    n9518 = fibboosted 9518; n9519 = fibboosted 9519
    n9538 = fibboosted 9538; n9539 = fibboosted 9539
    n9558 = fibboosted 9558; n9559 = fibboosted 9559
    n9578 = fibboosted 9578; n9579 = fibboosted 9579
    n9598 = fibboosted 9598; n9599 = fibboosted 9599
    n9618 = fibboosted 9618; n9619 = fibboosted 9619
    n9638 = fibboosted 9638; n9639 = fibboosted 9639
    n9658 = fibboosted 9658; n9659 = fibboosted 9659
    n9678 = fibboosted 9678; n9679 = fibboosted 9679
    n9698 = fibboosted 9698; n9699 = fibboosted 9699
    n9718 = fibboosted 9718; n9719 = fibboosted 9719
    n9738 = fibboosted 9738; n9739 = fibboosted 9739
    n9758 = fibboosted 9758; n9759 = fibboosted 9759
    n9778 = fibboosted 9778; n9779 = fibboosted 9779
    n9798 = fibboosted 9798; n9799 = fibboosted 9799
    n9818 = fibboosted 9818; n9819 = fibboosted 9819
    n9838 = fibboosted 9838; n9839 = fibboosted 9839
    n9858 = fibboosted 9858; n9859 = fibboosted 9859
    n9878 = fibboosted 9878; n9879 = fibboosted 9879
    n9898 = fibboosted 9898; n9899 = fibboosted 9899
    n9918 = fibboosted 9918; n9919 = fibboosted 9919
    n9938 = fibboosted 9938; n9939 = fibboosted 9939
    n9958 = fibboosted 9958; n9959 = fibboosted 9959
    n9978 = fibboosted 9978; n9979 = fibboosted 9979
    n9998 = fibboosted 9998; n9999 = fibboosted 9999
    n10018 = fibboosted 10018; n10019 = fibboosted 10019
    n10038 = fibboosted 10038; n10039 = fibboosted 10039
    n10058 = fibboosted 10058; n10059 = fibboosted 10059
    n10078 = fibboosted 10078; n10079 = fibboosted 10079
    n10098 = fibboosted 10098; n10099 = fibboosted 10099
    n10118 = fibboosted 10118; n10119 = fibboosted 10119
    n10138 = fibboosted 10138; n10139 = fibboosted 10139
    n10158 = fibboosted 10158; n10159 = fibboosted 10159
    n10178 = fibboosted 10178; n10179 = fibboosted 10179
    n10198 = fibboosted 10198; n10199 = fibboosted 10199
    n10218 = fibboosted 10218; n10219 = fibboosted 10219
    n10238 = fibboosted 10238; n10239 = fibboosted 10239
    n10258 = fibboosted 10258; n10259 = fibboosted 10259
    n10278 = fibboosted 10278; n10279 = fibboosted 10279
    n10298 = fibboosted 10298; n10299 = fibboosted 10299
    n10318 = fibboosted 10318; n10319 = fibboosted 10319
    n10338 = fibboosted 10338; n10339 = fibboosted 10339
    n10358 = fibboosted 10358; n10359 = fibboosted 10359
    n10378 = fibboosted 10378; n10379 = fibboosted 10379
    n10398 = fibboosted 10398; n10399 = fibboosted 10399
    n10418 = fibboosted 10418; n10419 = fibboosted 10419
    n10438 = fibboosted 10438; n10439 = fibboosted 10439
    n10458 = fibboosted 10458; n10459 = fibboosted 10459
    n10478 = fibboosted 10478; n10479 = fibboosted 10479
    n10498 = fibboosted 10498; n10499 = fibboosted 10499
    n10518 = fibboosted 10518; n10519 = fibboosted 10519
    n10538 = fibboosted 10538; n10539 = fibboosted 10539
    n10558 = fibboosted 10558; n10559 = fibboosted 10559
    n10578 = fibboosted 10578; n10579 = fibboosted 10579
    n10598 = fibboosted 10598; n10599 = fibboosted 10599
    n10618 = fibboosted 10618; n10619 = fibboosted 10619
    n10638 = fibboosted 10638; n10639 = fibboosted 10639
    n10658 = fibboosted 10658; n10659 = fibboosted 10659
    n10678 = fibboosted 10678; n10679 = fibboosted 10679
    n10698 = fibboosted 10698; n10699 = fibboosted 10699
    n10718 = fibboosted 10718; n10719 = fibboosted 10719
    n10738 = fibboosted 10738; n10739 = fibboosted 10739
    n10758 = fibboosted 10758; n10759 = fibboosted 10759
    n10778 = fibboosted 10778; n10779 = fibboosted 10779
    n10798 = fibboosted 10798; n10799 = fibboosted 10799
    n10818 = fibboosted 10818; n10819 = fibboosted 10819
    n10838 = fibboosted 10838; n10839 = fibboosted 10839
    n10858 = fibboosted 10858; n10859 = fibboosted 10859
    n10878 = fibboosted 10878; n10879 = fibboosted 10879
    n10898 = fibboosted 10898; n10899 = fibboosted 10899
    n10918 = fibboosted 10918; n10919 = fibboosted 10919
    n10938 = fibboosted 10938; n10939 = fibboosted 10939
    n10958 = fibboosted 10958; n10959 = fibboosted 10959
    n10978 = fibboosted 10978; n10979 = fibboosted 10979
    n10998 = fibboosted 10998; n10999 = fibboosted 10999
    n11018 = fibboosted 11018; n11019 = fibboosted 11019
    n11038 = fibboosted 11038; n11039 = fibboosted 11039
    n11058 = fibboosted 11058; n11059 = fibboosted 11059
    n11078 = fibboosted 11078; n11079 = fibboosted 11079
    n11098 = fibboosted 11098; n11099 = fibboosted 11099
    n11118 = fibboosted 11118; n11119 = fibboosted 11119
    n11138 = fibboosted 11138; n11139 = fibboosted 11139
    n11158 = fibboosted 11158; n11159 = fibboosted 11159
    n11178 = fibboosted 11178; n11179 = fibboosted 11179
    n11198 = fibboosted 11198; n11199 = fibboosted 11199
    n11218 = fibboosted 11218; n11219 = fibboosted 11219
    n11238 = fibboosted 11238; n11239 = fibboosted 11239
    n11258 = fibboosted 11258; n11259 = fibboosted 11259
    n11278 = fibboosted 11278; n11279 = fibboosted 11279
    n11298 = fibboosted 11298; n11299 = fibboosted 11299
    n11318 = fibboosted 11318; n11319 = fibboosted 11319
    n11338 = fibboosted 11338; n11339 = fibboosted 11339
    n11358 = fibboosted 11358; n11359 = fibboosted 11359
    n11378 = fibboosted 11378; n11379 = fibboosted 11379
    n11398 = fibboosted 11398; n11399 = fibboosted 11399
    n11418 = fibboosted 11418; n11419 = fibboosted 11419
    n11438 = fibboosted 11438; n11439 = fibboosted 11439
    n11458 = fibboosted 11458; n11459 = fibboosted 11459
    n11478 = fibboosted 11478; n11479 = fibboosted 11479
    n11498 = fibboosted 11498; n11499 = fibboosted 11499
    n11518 = fibboosted 11518; n11519 = fibboosted 11519
    n11538 = fibboosted 11538; n11539 = fibboosted 11539
    n11558 = fibboosted 11558; n11559 = fibboosted 11559
    n11578 = fibboosted 11578; n11579 = fibboosted 11579
    n11598 = fibboosted 11598; n11599 = fibboosted 11599
    n11618 = fibboosted 11618; n11619 = fibboosted 11619
    n11638 = fibboosted 11638; n11639 = fibboosted 11639
    n11658 = fibboosted 11658; n11659 = fibboosted 11659
    n11678 = fibboosted 11678; n11679 = fibboosted 11679
    n11698 = fibboosted 11698; n11699 = fibboosted 11699
    n11718 = fibboosted 11718; n11719 = fibboosted 11719
    n11738 = fibboosted 11738; n11739 = fibboosted 11739
    n11758 = fibboosted 11758; n11759 = fibboosted 11759
    n11778 = fibboosted 11778; n11779 = fibboosted 11779
    n11798 = fibboosted 11798; n11799 = fibboosted 11799
    n11818 = fibboosted 11818; n11819 = fibboosted 11819
    n11838 = fibboosted 11838; n11839 = fibboosted 11839
    n11858 = fibboosted 11858; n11859 = fibboosted 11859
    n11878 = fibboosted 11878; n11879 = fibboosted 11879
    n11898 = fibboosted 11898; n11899 = fibboosted 11899
    n11918 = fibboosted 11918; n11919 = fibboosted 11919
    n11938 = fibboosted 11938; n11939 = fibboosted 11939
    n11958 = fibboosted 11958; n11959 = fibboosted 11959
    n11978 = fibboosted 11978; n11979 = fibboosted 11979
    n11998 = fibboosted 11998; n11999 = fibboosted 11999
    n12018 = fibboosted 12018; n12019 = fibboosted 12019
    n12038 = fibboosted 12038; n12039 = fibboosted 12039
    n12058 = fibboosted 12058; n12059 = fibboosted 12059
    n12078 = fibboosted 12078; n12079 = fibboosted 12079
    n12098 = fibboosted 12098; n12099 = fibboosted 12099
    n12118 = fibboosted 12118; n12119 = fibboosted 12119
    n12138 = fibboosted 12138; n12139 = fibboosted 12139
    n12158 = fibboosted 12158; n12159 = fibboosted 12159
    n12178 = fibboosted 12178; n12179 = fibboosted 12179
    n12198 = fibboosted 12198; n12199 = fibboosted 12199
    n12218 = fibboosted 12218; n12219 = fibboosted 12219
    n12238 = fibboosted 12238; n12239 = fibboosted 12239
    n12258 = fibboosted 12258; n12259 = fibboosted 12259
    n12278 = fibboosted 12278; n12279 = fibboosted 12279
    n12298 = fibboosted 12298; n12299 = fibboosted 12299
    n12318 = fibboosted 12318; n12319 = fibboosted 12319
    n12338 = fibboosted 12338; n12339 = fibboosted 12339
    n12358 = fibboosted 12358; n12359 = fibboosted 12359
    n12378 = fibboosted 12378; n12379 = fibboosted 12379
    n12398 = fibboosted 12398; n12399 = fibboosted 12399
    n12418 = fibboosted 12418; n12419 = fibboosted 12419
    n12438 = fibboosted 12438; n12439 = fibboosted 12439
    n12458 = fibboosted 12458; n12459 = fibboosted 12459
    n12478 = fibboosted 12478; n12479 = fibboosted 12479
    n12498 = fibboosted 12498; n12499 = fibboosted 12499
    n12518 = fibboosted 12518; n12519 = fibboosted 12519
    n12538 = fibboosted 12538; n12539 = fibboosted 12539
    n12558 = fibboosted 12558; n12559 = fibboosted 12559
    n12578 = fibboosted 12578; n12579 = fibboosted 12579
    n12598 = fibboosted 12598; n12599 = fibboosted 12599
    n12618 = fibboosted 12618; n12619 = fibboosted 12619
    n12638 = fibboosted 12638; n12639 = fibboosted 12639
    n12658 = fibboosted 12658; n12659 = fibboosted 12659
    n12678 = fibboosted 12678; n12679 = fibboosted 12679
    n12698 = fibboosted 12698; n12699 = fibboosted 12699
    n12718 = fibboosted 12718; n12719 = fibboosted 12719
    n12738 = fibboosted 12738; n12739 = fibboosted 12739
    n12758 = fibboosted 12758; n12759 = fibboosted 12759
    n12778 = fibboosted 12778; n12779 = fibboosted 12779
    n12798 = fibboosted 12798; n12799 = fibboosted 12799
    n12818 = fibboosted 12818; n12819 = fibboosted 12819
    n12838 = fibboosted 12838; n12839 = fibboosted 12839
    n12858 = fibboosted 12858; n12859 = fibboosted 12859
    n12878 = fibboosted 12878; n12879 = fibboosted 12879
    n12898 = fibboosted 12898; n12899 = fibboosted 12899
    n12918 = fibboosted 12918; n12919 = fibboosted 12919
    n12938 = fibboosted 12938; n12939 = fibboosted 12939
    n12958 = fibboosted 12958; n12959 = fibboosted 12959
    n12978 = fibboosted 12978; n12979 = fibboosted 12979
    n12998 = fibboosted 12998; n12999 = fibboosted 12999
    n13018 = fibboosted 13018; n13019 = fibboosted 13019
    n13038 = fibboosted 13038; n13039 = fibboosted 13039
    n13058 = fibboosted 13058; n13059 = fibboosted 13059
    n13078 = fibboosted 13078; n13079 = fibboosted 13079
    n13098 = fibboosted 13098; n13099 = fibboosted 13099
    n13118 = fibboosted 13118; n13119 = fibboosted 13119
    n13138 = fibboosted 13138; n13139 = fibboosted 13139
    n13158 = fibboosted 13158; n13159 = fibboosted 13159
    n13178 = fibboosted 13178; n13179 = fibboosted 13179
    n13198 = fibboosted 13198; n13199 = fibboosted 13199
    n13218 = fibboosted 13218; n13219 = fibboosted 13219
    n13238 = fibboosted 13238; n13239 = fibboosted 13239
    n13258 = fibboosted 13258; n13259 = fibboosted 13259
    n13278 = fibboosted 13278; n13279 = fibboosted 13279
    n13298 = fibboosted 13298; n13299 = fibboosted 13299
    n13318 = fibboosted 13318; n13319 = fibboosted 13319
    n13338 = fibboosted 13338; n13339 = fibboosted 13339
    n13358 = fibboosted 13358; n13359 = fibboosted 13359
    n13378 = fibboosted 13378; n13379 = fibboosted 13379
    n13398 = fibboosted 13398; n13399 = fibboosted 13399
    n13418 = fibboosted 13418; n13419 = fibboosted 13419
    n13438 = fibboosted 13438; n13439 = fibboosted 13439
    n13458 = fibboosted 13458; n13459 = fibboosted 13459
    n13478 = fibboosted 13478; n13479 = fibboosted 13479
    n13498 = fibboosted 13498; n13499 = fibboosted 13499
    n13518 = fibboosted 13518; n13519 = fibboosted 13519
    n13538 = fibboosted 13538; n13539 = fibboosted 13539
    n13558 = fibboosted 13558; n13559 = fibboosted 13559
    n13578 = fibboosted 13578; n13579 = fibboosted 13579
    n13598 = fibboosted 13598; n13599 = fibboosted 13599
    n13618 = fibboosted 13618; n13619 = fibboosted 13619
    n13638 = fibboosted 13638; n13639 = fibboosted 13639
    n13658 = fibboosted 13658; n13659 = fibboosted 13659
    n13678 = fibboosted 13678; n13679 = fibboosted 13679
    n13698 = fibboosted 13698; n13699 = fibboosted 13699
    n13718 = fibboosted 13718; n13719 = fibboosted 13719
    n13738 = fibboosted 13738; n13739 = fibboosted 13739
    n13758 = fibboosted 13758; n13759 = fibboosted 13759
    n13778 = fibboosted 13778; n13779 = fibboosted 13779
    n13798 = fibboosted 13798; n13799 = fibboosted 13799
    n13818 = fibboosted 13818; n13819 = fibboosted 13819
    n13838 = fibboosted 13838; n13839 = fibboosted 13839
    n13858 = fibboosted 13858; n13859 = fibboosted 13859
    n13878 = fibboosted 13878; n13879 = fibboosted 13879
    n13898 = fibboosted 13898; n13899 = fibboosted 13899
    n13918 = fibboosted 13918; n13919 = fibboosted 13919
    n13938 = fibboosted 13938; n13939 = fibboosted 13939
    n13958 = fibboosted 13958; n13959 = fibboosted 13959
    n13978 = fibboosted 13978; n13979 = fibboosted 13979
    n13998 = fibboosted 13998; n13999 = fibboosted 13999
    n14018 = fibboosted 14018; n14019 = fibboosted 14019
    n14038 = fibboosted 14038; n14039 = fibboosted 14039
    n14058 = fibboosted 14058; n14059 = fibboosted 14059
    n14078 = fibboosted 14078; n14079 = fibboosted 14079
    n14098 = fibboosted 14098; n14099 = fibboosted 14099
    n14118 = fibboosted 14118; n14119 = fibboosted 14119
    n14138 = fibboosted 14138; n14139 = fibboosted 14139
    n14158 = fibboosted 14158; n14159 = fibboosted 14159
    n14178 = fibboosted 14178; n14179 = fibboosted 14179
    n14198 = fibboosted 14198; n14199 = fibboosted 14199
    n14218 = fibboosted 14218; n14219 = fibboosted 14219
    n14238 = fibboosted 14238; n14239 = fibboosted 14239
    n14258 = fibboosted 14258; n14259 = fibboosted 14259
    n14278 = fibboosted 14278; n14279 = fibboosted 14279
    n14298 = fibboosted 14298; n14299 = fibboosted 14299
    n14318 = fibboosted 14318; n14319 = fibboosted 14319
    n14338 = fibboosted 14338; n14339 = fibboosted 14339
    n14358 = fibboosted 14358; n14359 = fibboosted 14359
    n14378 = fibboosted 14378; n14379 = fibboosted 14379
    n14398 = fibboosted 14398; n14399 = fibboosted 14399
    n14418 = fibboosted 14418; n14419 = fibboosted 14419
    n14438 = fibboosted 14438; n14439 = fibboosted 14439
    n14458 = fibboosted 14458; n14459 = fibboosted 14459
    n14478 = fibboosted 14478; n14479 = fibboosted 14479
    n14498 = fibboosted 14498; n14499 = fibboosted 14499
    n14518 = fibboosted 14518; n14519 = fibboosted 14519
    n14538 = fibboosted 14538; n14539 = fibboosted 14539
    n14558 = fibboosted 14558; n14559 = fibboosted 14559
    n14578 = fibboosted 14578; n14579 = fibboosted 14579
    n14598 = fibboosted 14598; n14599 = fibboosted 14599
    n14618 = fibboosted 14618; n14619 = fibboosted 14619
    n14638 = fibboosted 14638; n14639 = fibboosted 14639
    n14658 = fibboosted 14658; n14659 = fibboosted 14659
    n14678 = fibboosted 14678; n14679 = fibboosted 14679
    n14698 = fibboosted 14698; n14699 = fibboosted 14699
    n14718 = fibboosted 14718; n14719 = fibboosted 14719
    n14738 = fibboosted 14738; n14739 = fibboosted 14739
    n14758 = fibboosted 14758; n14759 = fibboosted 14759
    n14778 = fibboosted 14778; n14779 = fibboosted 14779
    n14798 = fibboosted 14798; n14799 = fibboosted 14799
    n14818 = fibboosted 14818; n14819 = fibboosted 14819
    n14838 = fibboosted 14838; n14839 = fibboosted 14839
    n14858 = fibboosted 14858; n14859 = fibboosted 14859
    n14878 = fibboosted 14878; n14879 = fibboosted 14879
    n14898 = fibboosted 14898; n14899 = fibboosted 14899
    n14918 = fibboosted 14918; n14919 = fibboosted 14919
    n14938 = fibboosted 14938; n14939 = fibboosted 14939
    n14958 = fibboosted 14958; n14959 = fibboosted 14959
    n14978 = fibboosted 14978; n14979 = fibboosted 14979
    n14998 = fibboosted 14998; n14999 = fibboosted 14999
    n15018 = fibboosted 15018; n15019 = fibboosted 15019
    n15038 = fibboosted 15038; n15039 = fibboosted 15039
    n15058 = fibboosted 15058; n15059 = fibboosted 15059
    n15078 = fibboosted 15078; n15079 = fibboosted 15079
    n15098 = fibboosted 15098; n15099 = fibboosted 15099
    n15118 = fibboosted 15118; n15119 = fibboosted 15119
    n15138 = fibboosted 15138; n15139 = fibboosted 15139
    n15158 = fibboosted 15158; n15159 = fibboosted 15159
    n15178 = fibboosted 15178; n15179 = fibboosted 15179
    n15198 = fibboosted 15198; n15199 = fibboosted 15199
    n15218 = fibboosted 15218; n15219 = fibboosted 15219
    n15238 = fibboosted 15238; n15239 = fibboosted 15239
    n15258 = fibboosted 15258; n15259 = fibboosted 15259
    n15278 = fibboosted 15278; n15279 = fibboosted 15279
    n15298 = fibboosted 15298; n15299 = fibboosted 15299
    n15318 = fibboosted 15318; n15319 = fibboosted 15319
    n15338 = fibboosted 15338; n15339 = fibboosted 15339
    n15358 = fibboosted 15358; n15359 = fibboosted 15359
    n15378 = fibboosted 15378; n15379 = fibboosted 15379
    n15398 = fibboosted 15398; n15399 = fibboosted 15399
    n15418 = fibboosted 15418; n15419 = fibboosted 15419
    n15438 = fibboosted 15438; n15439 = fibboosted 15439
    n15458 = fibboosted 15458; n15459 = fibboosted 15459
    n15478 = fibboosted 15478; n15479 = fibboosted 15479
    n15498 = fibboosted 15498; n15499 = fibboosted 15499
    n15518 = fibboosted 15518; n15519 = fibboosted 15519
    n15538 = fibboosted 15538; n15539 = fibboosted 15539
    n15558 = fibboosted 15558; n15559 = fibboosted 15559
    n15578 = fibboosted 15578; n15579 = fibboosted 15579
    n15598 = fibboosted 15598; n15599 = fibboosted 15599
    n15618 = fibboosted 15618; n15619 = fibboosted 15619
    n15638 = fibboosted 15638; n15639 = fibboosted 15639
    n15658 = fibboosted 15658; n15659 = fibboosted 15659
    n15678 = fibboosted 15678; n15679 = fibboosted 15679
    n15698 = fibboosted 15698; n15699 = fibboosted 15699
    n15718 = fibboosted 15718; n15719 = fibboosted 15719
    n15738 = fibboosted 15738; n15739 = fibboosted 15739
    n15758 = fibboosted 15758; n15759 = fibboosted 15759
    n15778 = fibboosted 15778; n15779 = fibboosted 15779
    n15798 = fibboosted 15798; n15799 = fibboosted 15799
    n15818 = fibboosted 15818; n15819 = fibboosted 15819
    n15838 = fibboosted 15838; n15839 = fibboosted 15839
    n15858 = fibboosted 15858; n15859 = fibboosted 15859
    n15878 = fibboosted 15878; n15879 = fibboosted 15879
    n15898 = fibboosted 15898; n15899 = fibboosted 15899
    n15918 = fibboosted 15918; n15919 = fibboosted 15919
    n15938 = fibboosted 15938; n15939 = fibboosted 15939
    n15958 = fibboosted 15958; n15959 = fibboosted 15959
    n15978 = fibboosted 15978; n15979 = fibboosted 15979
    n15998 = fibboosted 15998; n15999 = fibboosted 15999
    n16018 = fibboosted 16018; n16019 = fibboosted 16019
    n16038 = fibboosted 16038; n16039 = fibboosted 16039
    n16058 = fibboosted 16058; n16059 = fibboosted 16059
    n16078 = fibboosted 16078; n16079 = fibboosted 16079
    n16098 = fibboosted 16098; n16099 = fibboosted 16099
    n16118 = fibboosted 16118; n16119 = fibboosted 16119
    n16138 = fibboosted 16138; n16139 = fibboosted 16139
    n16158 = fibboosted 16158; n16159 = fibboosted 16159
    n16178 = fibboosted 16178; n16179 = fibboosted 16179
    n16198 = fibboosted 16198; n16199 = fibboosted 16199
    n16218 = fibboosted 16218; n16219 = fibboosted 16219
    n16238 = fibboosted 16238; n16239 = fibboosted 16239
    n16258 = fibboosted 16258; n16259 = fibboosted 16259
    n16278 = fibboosted 16278; n16279 = fibboosted 16279
    n16298 = fibboosted 16298; n16299 = fibboosted 16299
    n16318 = fibboosted 16318; n16319 = fibboosted 16319
    n16338 = fibboosted 16338; n16339 = fibboosted 16339
    n16358 = fibboosted 16358; n16359 = fibboosted 16359
    n16378 = fibboosted 16378; n16379 = fibboosted 16379
    n16398 = fibboosted 16398; n16399 = fibboosted 16399
    n16418 = fibboosted 16418; n16419 = fibboosted 16419
    n16438 = fibboosted 16438; n16439 = fibboosted 16439
    n16458 = fibboosted 16458; n16459 = fibboosted 16459
    n16478 = fibboosted 16478; n16479 = fibboosted 16479
    n16498 = fibboosted 16498; n16499 = fibboosted 16499
    n16518 = fibboosted 16518; n16519 = fibboosted 16519
    n16538 = fibboosted 16538; n16539 = fibboosted 16539
    n16558 = fibboosted 16558; n16559 = fibboosted 16559
    n16578 = fibboosted 16578; n16579 = fibboosted 16579
    n16598 = fibboosted 16598; n16599 = fibboosted 16599
    n16618 = fibboosted 16618; n16619 = fibboosted 16619
    n16638 = fibboosted 16638; n16639 = fibboosted 16639
    n16658 = fibboosted 16658; n16659 = fibboosted 16659
    n16678 = fibboosted 16678; n16679 = fibboosted 16679
    n16698 = fibboosted 16698; n16699 = fibboosted 16699
    n16718 = fibboosted 16718; n16719 = fibboosted 16719
    n16738 = fibboosted 16738; n16739 = fibboosted 16739
    n16758 = fibboosted 16758; n16759 = fibboosted 16759
    n16778 = fibboosted 16778; n16779 = fibboosted 16779
    n16798 = fibboosted 16798; n16799 = fibboosted 16799
    n16818 = fibboosted 16818; n16819 = fibboosted 16819
    n16838 = fibboosted 16838; n16839 = fibboosted 16839
    n16858 = fibboosted 16858; n16859 = fibboosted 16859
    n16878 = fibboosted 16878; n16879 = fibboosted 16879
    n16898 = fibboosted 16898; n16899 = fibboosted 16899
    n16918 = fibboosted 16918; n16919 = fibboosted 16919
    n16938 = fibboosted 16938; n16939 = fibboosted 16939
    n16958 = fibboosted 16958; n16959 = fibboosted 16959
    n16978 = fibboosted 16978; n16979 = fibboosted 16979
    n16998 = fibboosted 16998; n16999 = fibboosted 16999
    n17018 = fibboosted 17018; n17019 = fibboosted 17019
    n17038 = fibboosted 17038; n17039 = fibboosted 17039
    n17058 = fibboosted 17058; n17059 = fibboosted 17059
    n17078 = fibboosted 17078; n17079 = fibboosted 17079
    n17098 = fibboosted 17098; n17099 = fibboosted 17099
    n17118 = fibboosted 17118; n17119 = fibboosted 17119
    n17138 = fibboosted 17138; n17139 = fibboosted 17139
    n17158 = fibboosted 17158; n17159 = fibboosted 17159
    n17178 = fibboosted 17178; n17179 = fibboosted 17179
    n17198 = fibboosted 17198; n17199 = fibboosted 17199
    n17218 = fibboosted 17218; n17219 = fibboosted 17219
    n17238 = fibboosted 17238; n17239 = fibboosted 17239
    n17258 = fibboosted 17258; n17259 = fibboosted 17259
    n17278 = fibboosted 17278; n17279 = fibboosted 17279
    n17298 = fibboosted 17298; n17299 = fibboosted 17299
    n17318 = fibboosted 17318; n17319 = fibboosted 17319
    n17338 = fibboosted 17338; n17339 = fibboosted 17339
    n17358 = fibboosted 17358; n17359 = fibboosted 17359
    n17378 = fibboosted 17378; n17379 = fibboosted 17379
    n17398 = fibboosted 17398; n17399 = fibboosted 17399
    n17418 = fibboosted 17418; n17419 = fibboosted 17419
    n17438 = fibboosted 17438; n17439 = fibboosted 17439
    n17458 = fibboosted 17458; n17459 = fibboosted 17459
    n17478 = fibboosted 17478; n17479 = fibboosted 17479
    n17498 = fibboosted 17498; n17499 = fibboosted 17499
    n17518 = fibboosted 17518; n17519 = fibboosted 17519
    n17538 = fibboosted 17538; n17539 = fibboosted 17539
    n17558 = fibboosted 17558; n17559 = fibboosted 17559
    n17578 = fibboosted 17578; n17579 = fibboosted 17579
    n17598 = fibboosted 17598; n17599 = fibboosted 17599
    n17618 = fibboosted 17618; n17619 = fibboosted 17619
    n17638 = fibboosted 17638; n17639 = fibboosted 17639
    n17658 = fibboosted 17658; n17659 = fibboosted 17659
    n17678 = fibboosted 17678; n17679 = fibboosted 17679
    n17698 = fibboosted 17698; n17699 = fibboosted 17699
    n17718 = fibboosted 17718; n17719 = fibboosted 17719
    n17738 = fibboosted 17738; n17739 = fibboosted 17739
    n17758 = fibboosted 17758; n17759 = fibboosted 17759
    n17778 = fibboosted 17778; n17779 = fibboosted 17779
    n17798 = fibboosted 17798; n17799 = fibboosted 17799
    n17818 = fibboosted 17818; n17819 = fibboosted 17819
    n17838 = fibboosted 17838; n17839 = fibboosted 17839
    n17858 = fibboosted 17858; n17859 = fibboosted 17859
    n17878 = fibboosted 17878; n17879 = fibboosted 17879
    n17898 = fibboosted 17898; n17899 = fibboosted 17899
    n17918 = fibboosted 17918; n17919 = fibboosted 17919
    n17938 = fibboosted 17938; n17939 = fibboosted 17939
    n17958 = fibboosted 17958; n17959 = fibboosted 17959
    n17978 = fibboosted 17978; n17979 = fibboosted 17979
    n17998 = fibboosted 17998; n17999 = fibboosted 17999
    n18018 = fibboosted 18018; n18019 = fibboosted 18019
    n18038 = fibboosted 18038; n18039 = fibboosted 18039
    n18058 = fibboosted 18058; n18059 = fibboosted 18059
    n18078 = fibboosted 18078; n18079 = fibboosted 18079
    n18098 = fibboosted 18098; n18099 = fibboosted 18099
    n18118 = fibboosted 18118; n18119 = fibboosted 18119
    n18138 = fibboosted 18138; n18139 = fibboosted 18139
    n18158 = fibboosted 18158; n18159 = fibboosted 18159
    n18178 = fibboosted 18178; n18179 = fibboosted 18179
    n18198 = fibboosted 18198; n18199 = fibboosted 18199
    n18218 = fibboosted 18218; n18219 = fibboosted 18219
    n18238 = fibboosted 18238; n18239 = fibboosted 18239
    n18258 = fibboosted 18258; n18259 = fibboosted 18259
    n18278 = fibboosted 18278; n18279 = fibboosted 18279
    n18298 = fibboosted 18298; n18299 = fibboosted 18299
    n18318 = fibboosted 18318; n18319 = fibboosted 18319
    n18338 = fibboosted 18338; n18339 = fibboosted 18339
    n18358 = fibboosted 18358; n18359 = fibboosted 18359
    n18378 = fibboosted 18378; n18379 = fibboosted 18379
    n18398 = fibboosted 18398; n18399 = fibboosted 18399
    n18418 = fibboosted 18418; n18419 = fibboosted 18419
    n18438 = fibboosted 18438; n18439 = fibboosted 18439
    n18458 = fibboosted 18458; n18459 = fibboosted 18459
    n18478 = fibboosted 18478; n18479 = fibboosted 18479
    n18498 = fibboosted 18498; n18499 = fibboosted 18499
    n18518 = fibboosted 18518; n18519 = fibboosted 18519
    n18538 = fibboosted 18538; n18539 = fibboosted 18539
    n18558 = fibboosted 18558; n18559 = fibboosted 18559
    n18578 = fibboosted 18578; n18579 = fibboosted 18579
    n18598 = fibboosted 18598; n18599 = fibboosted 18599
    n18618 = fibboosted 18618; n18619 = fibboosted 18619
    n18638 = fibboosted 18638; n18639 = fibboosted 18639
    n18658 = fibboosted 18658; n18659 = fibboosted 18659
    n18678 = fibboosted 18678; n18679 = fibboosted 18679
    n18698 = fibboosted 18698; n18699 = fibboosted 18699
    n18718 = fibboosted 18718; n18719 = fibboosted 18719
    n18738 = fibboosted 18738; n18739 = fibboosted 18739
    n18758 = fibboosted 18758; n18759 = fibboosted 18759
    n18778 = fibboosted 18778; n18779 = fibboosted 18779
    n18798 = fibboosted 18798; n18799 = fibboosted 18799
    n18818 = fibboosted 18818; n18819 = fibboosted 18819
    n18838 = fibboosted 18838; n18839 = fibboosted 18839
    n18858 = fibboosted 18858; n18859 = fibboosted 18859
    n18878 = fibboosted 18878; n18879 = fibboosted 18879
    n18898 = fibboosted 18898; n18899 = fibboosted 18899
    n18918 = fibboosted 18918; n18919 = fibboosted 18919
    n18938 = fibboosted 18938; n18939 = fibboosted 18939
    n18958 = fibboosted 18958; n18959 = fibboosted 18959
    n18978 = fibboosted 18978; n18979 = fibboosted 18979
    n18998 = fibboosted 18998; n18999 = fibboosted 18999
    n19018 = fibboosted 19018; n19019 = fibboosted 19019
    n19038 = fibboosted 19038; n19039 = fibboosted 19039
    n19058 = fibboosted 19058; n19059 = fibboosted 19059
    n19078 = fibboosted 19078; n19079 = fibboosted 19079
    n19098 = fibboosted 19098; n19099 = fibboosted 19099
    n19118 = fibboosted 19118; n19119 = fibboosted 19119
    n19138 = fibboosted 19138; n19139 = fibboosted 19139
    n19158 = fibboosted 19158; n19159 = fibboosted 19159
    n19178 = fibboosted 19178; n19179 = fibboosted 19179
    n19198 = fibboosted 19198; n19199 = fibboosted 19199
    n19218 = fibboosted 19218; n19219 = fibboosted 19219
    n19238 = fibboosted 19238; n19239 = fibboosted 19239
    n19258 = fibboosted 19258; n19259 = fibboosted 19259
    n19278 = fibboosted 19278; n19279 = fibboosted 19279
    n19298 = fibboosted 19298; n19299 = fibboosted 19299
    n19318 = fibboosted 19318; n19319 = fibboosted 19319
    n19338 = fibboosted 19338; n19339 = fibboosted 19339
    n19358 = fibboosted 19358; n19359 = fibboosted 19359
    n19378 = fibboosted 19378; n19379 = fibboosted 19379
    n19398 = fibboosted 19398; n19399 = fibboosted 19399
    n19418 = fibboosted 19418; n19419 = fibboosted 19419
    n19438 = fibboosted 19438; n19439 = fibboosted 19439
    n19458 = fibboosted 19458; n19459 = fibboosted 19459
    n19478 = fibboosted 19478; n19479 = fibboosted 19479
    n19498 = fibboosted 19498; n19499 = fibboosted 19499
    n19518 = fibboosted 19518; n19519 = fibboosted 19519
    n19538 = fibboosted 19538; n19539 = fibboosted 19539
    n19558 = fibboosted 19558; n19559 = fibboosted 19559
    n19578 = fibboosted 19578; n19579 = fibboosted 19579
    n19598 = fibboosted 19598; n19599 = fibboosted 19599
    n19618 = fibboosted 19618; n19619 = fibboosted 19619
    n19638 = fibboosted 19638; n19639 = fibboosted 19639
    n19658 = fibboosted 19658; n19659 = fibboosted 19659
    n19678 = fibboosted 19678; n19679 = fibboosted 19679
    n19698 = fibboosted 19698; n19699 = fibboosted 19699
    n19718 = fibboosted 19718; n19719 = fibboosted 19719
    n19738 = fibboosted 19738; n19739 = fibboosted 19739
    n19758 = fibboosted 19758; n19759 = fibboosted 19759
    n19778 = fibboosted 19778; n19779 = fibboosted 19779
    n19798 = fibboosted 19798; n19799 = fibboosted 19799
    n19818 = fibboosted 19818; n19819 = fibboosted 19819
    n19838 = fibboosted 19838; n19839 = fibboosted 19839
    n19858 = fibboosted 19858; n19859 = fibboosted 19859
    n19878 = fibboosted 19878; n19879 = fibboosted 19879
    n19898 = fibboosted 19898; n19899 = fibboosted 19899
    n19918 = fibboosted 19918; n19919 = fibboosted 19919
    n19938 = fibboosted 19938; n19939 = fibboosted 19939
    n19958 = fibboosted 19958; n19959 = fibboosted 19959
    n19978 = fibboosted 19978; n19979 = fibboosted 19979
    n19998 = fibboosted 19998; n19999 = fibboosted 19999
