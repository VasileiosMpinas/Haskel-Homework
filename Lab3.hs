---AM :4434
---Vasileios Binas


-----------------------------------------------------------------------------------------

-- ASKHSH 1

statistics :: [(Int,Int)]->(Int,Int,Int,Int,Int)
statistics [] = (0,0,0,0,0)
statistics s = (length s,(points s),(sumList s 1),(sumList s 2),(fifth s (distance(head s))))
 


points :: [(Int,Int)] -> Int
points [] = 0
points s 
	|comp2 (head s) ==1 = 0 + points (tail s)
	|comp2 (head s) ==2 = 1 + points (tail s)
	|comp2 (head s) ==3 = 3 + points (tail s)
	

comp2 :: (Int,Int)->Int
comp2 (n,m)
	|n < m = 1
	|n == m = 2
	|n > m = 3



sumList :: [(Int,Int)] -> Int -> Int
sumList [] k = 0 
sumList s k 
	|k==1 = goal (head s) 1 + sumList (tail s) 1
	|k==2 = goal (head s) 2 + sumList (tail s) 2

goal :: (Int,Int) -> Int-> Int
goal (n,m) k
	|k==1 = n
	|k==2 = m



fifth :: [(Int,Int)]-> Int -> Int
fifth [(n,m)] d = n-m
fifth s d
	|d>= distance (head (tail s)) && head(tail s)==last s = d
	|d< distance (head (tail s)) && head(tail s)== last s = distance (head (tail s))
	|d>= distance (head (tail s))&& head(tail s)/=last s = fifth (tail s) d
	|d< distance (head (tail s))&& head(tail s)/=last s = fifth (tail s) (distance (head (tail s)))

distance :: (Int,Int)->Int
distance (n,m) = n-m



-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

partition :: String->[[String]]

partition w 
	|length w ==1 =[]
	|otherwise= totalSplit w w [] [] 1 1  

split :: String->[String]->[String]
split w k
	|w=="" = reverse k
	|otherwise = split (tail w) (((head w):[]):k) 
	


totalSplit :: String->String->[String]->[[String]]->Int->Int->[[String]]
totalSplit a b x y i j
	|i==j = totalSplit b b [] ((tail x):y) 1 (j+1)
	|otherwise =  totalSplit (init a) b (((last a):[]):x) y (i+1) j

