

-----------------------------------------------------------------------------------------

-- ASKHSH 1


grade :: Int->Int->Int                           

grade a b 
      |c>47 && a<=47 && b>=0 && b<21 = 47
      |c>47 && c<50 && a>47  && b>=0 && b<21 = 50
      |a>= 0 && a<101 && b>=0 && b<21 = c
      |otherwise = -1                                         
	where c = ((8*a) `div` 10) +b
		




-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

digits :: Int->Int->Int
digits x y 
	|x<1 = money k
	|x`mod`10 /= y`mod`10 = countDigit (x`div`10) (y`div`10) (k-1)
	|otherwise = digits (x`div`10) (y`div`10)
	where k=8


countDigit :: Int->Int->Int->Int
countDigit x y k
	|x<1 = money k
	|x`mod`10 /= y`mod`10 = countDigit (x`div`10) (y`div`10) (k-1)
	|otherwise = countDigit (x`div`10) (y`div`10) k




money :: Int -> Int
money k
      |k == 8 = 1000000
      |k == 7 = 100000
      |k == 6 = 8000
      |k == 5 = 300       
      |k == 4 = 20
      |k == 3 = 5
      |k == 2 = 1
      |otherwise = 0
      

