# Ich bin eine rekursiv Machine

### Exploring recursion 

A little project to for me to explore recursive sequences. Whilst I was doing some exercises, I was think about "boosting" the recursive function. (Giving it a new starting point with pre-calculatzed values)
Or having a program that automatically after a certin amount of time, recomplied with new end cases to start from, greatly improving effiency as the number gets big.
Not quite there yet, in terms of skill set - So then I started to experiment with making functions that have a similar prinziple. 

Has this been done before?? *Probably, yes* 
Is there a better way?? **Almost definately!!**

Now after doing some research, i've dived into the world of memoization... head first.



However, as I'm still new to programming it sounded like a fun little project for me to work on whilst doing my studies.


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

<a name="fibboost-id" />
Therefor my idea of was:

````
	-- "boosted" self-recursive function
	fibboosted :: Int -> Integer
	fibboosted n
	| n < 2 = fromIntegral n
	| otherwise = fibSeq' (n - mod n 20) (fibboosted (n - mod n 20)) (fibboosted (n - mod n 20 + 1)) n

       
	-- helper function to build the pairs dynamically with the boosted recursive function
	fibSeq' :: Int -> Integer -> Integer -> Int -> Integer
	fibSeq' k fk1 fk2 n
	| k == n    = fk1
	| k + 1 == n = fk2
	| otherwise = fibSeq' (k + 1) fk2 (fk1 + fk2) n 
````

#### Looking at

This first draft works like a chram to boost the basic function and giving great speed up to around n = 200

- Want to work out a way to automatically increase the "| n <" as n increases, with adding addition where cases. Using where so they don't need to be again calculated each time.
- Optimal gap between n values to just to the next one. Would like to add a automised way to increase test values of the gab and see which is optimal *Probably bigger at the start and then getting smaller. But what is the formula??*
