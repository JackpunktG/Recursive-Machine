# Ich bin eine rekursiv Machine

### Exploring recursion 

A little project to for me to explore recursive sequences. Whilst I was doing some exercises, I was think about "boosting" the recursive function. (Giving it a new starting point with pre-calculatzed values)
Or having a program that automatically after a certin amount of time, recomplied with new end cases to start from, greatly improving effiency as the number gets big.
Not quite there yet, in terms of skill set - So then I started to experiment with making functions that have a similar prinziple. 

Has this been done before?? *Probably, yes* 
Is there a better way?? **Almost definately!!**

Now after doing some research, i've dived into the world of memoization... head first.

However, as I'm still new to programming it sounded like a fun little project for me to work on whilst doing my studies.


## Sub-routine

Due to Haskell's laziness, the basic loops where not giving accurate times, as after calculating the first number it was save and share this value with future calls. 
So I got around this by creating each call a subroutine. Completely issolating the calculation and giving a accurate time per value. Obvs... super slower calling each check a subroutine, but will just take number then in 100 difference or more as they get bigger 

## Goal

To start to experiment with different ways to improve recursive functions
Slowly, but god dam surely document all the different recusrive sequences and see what techniques work for optimising them. Are that going to be the same?? *I know don't* :) 

I'm still intrested in the optimal "boosting" (#fibboost-id) funtion, however only having to sum up any part one, well lets just say that save some BIG TIME!!
Now that the flood gates are opened, I asked the question. Which is the most optimial way. So far I've intergrated Hashmaps, Array and ZipWith im a memoization way. More research needed, and want to test them out all out!

So with each sequence come the set of questions
- how many ways are there to do memoization
- which is quicker
	- for smaller values
	- as the number gets bigger
- and tricks or tips i learn! always always


### Fibonacci Sequence


#### Memoization

Array = Red
Hashmap = Purple
List = Green
* Started the testing at n = 50000 (as all basically instance before then) to 300000 in jumps of +200. Hence the somewhat sporadic data points at the end.

!(/Images/Fib_ArrayHashmapList1.png)
 
!(/Images/Fib_ArrayHashmapList2.png) 

!(/Images/Fib_ArrayHashmapList3.png) 

As we can see all are really quick, but Hashmap is by far the slowest, with List and Array being pretty close. However towards the best of the test, can definately feel my computer starting to tank, however it does seem that all 3 have a slightly different curve, with List winning the race!

Want to do more testing for bigger numbers up to n = 1000000, however not to sure my poor not so great laptop is up to the job haha :)
Also want to give them a try with some multithreading to see how each benifits from it. Is one going to be better with multithreading?? Lets find out, till next time <3

#### Non-Memoization

!(/Images/Fib_BasicBoosted1.png)

!(/Images/Fib_BasicBoosted2.png)

As we can see if the Boosted function (orange) starts off well, but then gets extremly slow exponentally.

<a name="fibboost-id" />
Therefor my idea of was:

````
	fibboosted :: Int -> Integer
	fibboosted n
	| n < 20    = fibSeq n
	| n < 40    = fibSeq' n18 18 n19 19 n
	| n < 60    = fibSeq' n38 38 n39 39 n
	| n < 80    = fibSeq' n58 58 n59 59 n
	| n < 100   = fibSeq' n78 78 n79 79 n
	...
	  where
		n18 = fibboosted 18; n19 = fibboosted 19
		n38 = fibboosted 38; n39 = fibboosted 39
		n58 = fibboosted 58; n59 = fibboosted 59
		n78 = fibboosted 78; n79 = fibboosted 79
		...
		
	fibSeq' :: Integer -> Int -> Integer -> Int -> Int -> Integer
	fibSeq' nn8 xx8 nn9 xx9 n
		| n == xx8 = nn8
		| n == xx9 = nn9
	fibSeq' nn8 xx8 nn9 xx9 n = (fibSeq' nn8 xx8 nn9 xx9 (n-1)) + (fibSeq' nn8 xx8 nn9 xx9 (n-2))
````

#### Looking at

This first draft works like a chram to boost the basic function and giving sub second speeds up to n = ~120, then starts to get slow fast - Whereas the basic fibonacci function can only do sub second up to n = ~30

- Want to work out a way to automatically increase the "| n <" as n increases, with adding addition where cases. Using where so they don't need to be again calculated each time.
- Optimal gap between n values to just to the next one. Would like to add a automised way to increase test values of the gab and see which is optimal *Probably bigger at the start and then getting smaller. But what is the formula??*
