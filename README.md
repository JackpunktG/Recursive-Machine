# Ich bin eine rekursiv Machine

### Exploring recursion 

A little project to for me to explore recursive sequences. Whilst I was doing some exercises, I was think about "boosting" the recursive function. (Giving it a new starting point with pre-calculatzed values)
Or having a program that automatically after a certin amount of time, recomplied with new end cases to start from, greatly improving effiency as the number gets big.
Not quite there yet, in terms of skill set - So then I started to experiment with making functions that have a similar prinziple. 

Has this been done before?? *Probably, yes* 
Is there a better way?? **Almost definately!!**

However, as I'm still new to programming it sounded like a fun little project for me to work on whilst doing my studies.


## Goal

To start to experiment with different ways to improve recursive functions
Slowly document all the different recusrive sequences and see what techniques work for optimising them. Are that going to be the same?? *I know know* :) 


### Fibonacci Sequence

Ahh... that classic. Easy to understand, easy to medium to master.
````
	fibSeq :: Integer -> Integer
	fibSeq 0 = 0
	fibSeq 1 = 1
	fibSeq n = fibSeq (n-1) + fibSeq (n-2)
````
The basic fibonacci recursive sequences gets really really slow when wanting large values.

Therefor my idea of was:
````
	fibonacci :: Integer -> Integer
	fibonacci n
            | n < 20 = fibSeq n
	    | n < 40 = fibSeq' n18 18 n19 19 n
	    | n < 60 = fibSeq' n38 38 n39 39 n
	    ...
		...
		...
	    where
	        n18  = fibonacci 18;  n19  = fibonacci 19
	        n38  = fibonacci 38;  n39  = fibonacci 39
	        ...
		...
 		      

	fibSeq' :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
	fibSeq' nn8 xx8 nn9 xx9 n
	    | n == xx8 = nn8
	    | n == xx9 = nn9
	fibSeq' nn8 xx8 nn9 xx9 n = (fibSeq' nn8 xx8 nn9 xx9 (n-1)) + (fibSeq' nn8 xx8 nn9 xx9 (n-2))
````
#### Looking at

This first draft works like a chram to boost the basic function and giving great speed up to around n = 200

- Want to work out a way to automatically increase the "| n <" as n increases, with adding addition where cases. Using where so they don't need to be again calculated each time.
- Optimal gap between n values to just to the next one. Would like to add a automised way to increase test values of the gab and see which is optimal *Probably bigger at the start and then getting smaller. But what is the formula??*
