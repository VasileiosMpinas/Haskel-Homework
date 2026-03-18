---AM :4434
---Vasileios Binas


-----------------------------------------------------------------------------------------

-- ASKHSH 1

ab :: Int->(Int,Int) 
ab 1 = (1, 1)
ab n 
	|primeCheck 2 n == 0 = (1, n)
	|2<=n = (n`div`comp (n-1) n 2 n n, comp (n-1) n 2 n n)
	|otherwise = (-1, -1) 


primeCheck :: Int->Int->Int
primeCheck a b 
	|b `mod` a /= 0 = primeCheck (a+1) b
	|b == a = 0
	|otherwise = -1 




comp :: Int->Int->Int->Int->Int->Int

comp a b c d k 
		|k `mod` a /= 0 = comp (a-1) (k`div`(a-1)) c d k
		|k `mod` a == 0 && abs (d-c) > abs(b-a) = comp a b a b k
		|k `mod` a == 0 && abs(d-c) == abs(b-a) = comp (a-1) (k`div`(a-1)) c d k
		|k `mod` a == 0 && abs(d-c) < abs(b-a) = c		
		|a<2 = c
		




-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

sum2021 :: Integer->Integer->Integer             

sum2021 m n
	    |m<n = (power (n+i) m) + sum2020 m n (i+1)
	    |m == n = power (n+m) m	
	    |otherwise = -1
	    where i = m	

sum2020 :: Integer->Integer->Integer->Integer
sum2020 m n i
	    |i<n = (power (n+i) m) + sum2020 m n (i+1)	
	    |i == n = power (n+i) m	 

power :: Integer->Integer->Integer

power a 1 = a
power a b = a*power a (b-1)

