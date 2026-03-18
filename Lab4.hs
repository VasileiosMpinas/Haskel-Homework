---AM :4434
---Vasileios Binas


-----------------------------------------------------------------------------------------


-- ASKHSH 1

move :: Eq u => [u]->u->Int->[u]
move [] x n =[]
move s x n 
	|elemList s x==False =s
	|(elemList s x) == True && n>0 = moveRight s x n [] 
	|(elemList s x) == True && n<0 = reverse (moveLeft (reverse s) x (abs n) [] (numberOfX s x 0)) 
	|(elemList s x) == True && n==0 = removeValue s x [] 


elemList ::Eq u => [u]->u->Bool
elemList (h:t) x
		|x==h = True 
		|x/=h =elemList t x
elemList [] x = False

checkInf :: Eq u=>[u]->Int->Bool
checkInf [] i=False
checkInf (h:t) i
		|i>100 = True
		|otherwise = checkInf t (i+1)


moveRight :: Eq u =>[u]->u->Int->[u]->[u]
moveRight (h:t) x n s
			|((length t)+1) == 1 = reverse (h:s) 
			|n==0 = moveRight t x n (h:s)
			|h==x = moveRight (h:(tail t))  x (n-1) ((head t):s)
			|h/=x = moveRight t x n (h:s)


numberOfX ::  Eq u => [u]->u->Int->Int
numberOfX [] x i = i
numberOfX (h:t) x i
		|h==x = numberOfX t x (i+1)
		|otherwise = numberOfX t x i

			
moveLeft :: Eq u =>[u]->u->Int->[u]->Int->[u]
moveLeft (h:t) x n s i 
			|((length t)+1) == 1 = reverse (h:s) 
			|n==0 = moveLeft t x n (h:s) i
			|h==x && i/=1 = moveLeft t x n (h:s) (i-1)
			|h==x && i==1 = moveLeft (h:(tail t))  x (n-1) ((head t):s) i
			|h/=x = moveLeft t x n (h:s) i

infLeft :: Eq u =>[u]->u->[u]->[u]
infLeft (h:t) x s
		|h==x = reverse (h:s)
		|otherwise = infLeft t x (h:s)


removeValue :: Eq u =>[u]->u->[u]->[u]
removeValue (h:t) x s 
			|((length t)+1)==1 =reverse s
			|h==x = removeValue (h:(tail t)) x ((head t):s)
			|h/=x = removeValue t x (h:s)


		

-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

combine :: [u]->[v]->(u->v->w)->(u->v->w)->(Int->Bool)->[w]

combine s t f g h = result s t f g h 1 []

  
result :: [u]->[v]->(u->v->w)->(u->v->w)->(Int->Bool)->Int->[w]->[w]
result [] [] f g h i x = reverse x
result [] t f g h i x = reverse x
result s [] f g h i x = reverse x
result s t f g h i x
		|h i==True = result (tail s) (tail t) f g h (i+1) ((f (head s) (head t)):x)
		|h i==False = result (tail s) (tail t) f g h (i+1) ((g (head s) (head t)):x)

