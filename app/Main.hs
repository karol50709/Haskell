import Control.Monad

--Zadanie 1

foo :: Double -> Double -> Double
foo n y = 
  if y/=0
    then ((n/(y*y))+2*y)/3
    else 0

pierwiastek :: Double -> Double -> Int -> Int -> IO()

pierwiastek n y x a = do
  --    pierwiastek 27 1 0 10
  --    pierwiastek liczba do pierwiastkowania | y poczatkowe | poczatek licznika | liczba powtorzen
  let count = x
  if x<a
    then do
      let zm = foo n y
      let temp = count
      let count = temp + 1
      pierwiastek n zm count a
      print $ zm
    else
      print "OK!"

--Zadanie 2

fibcount :: Int -> Int
fibcount 0 = 1
fibcount 1 = 1
fibcount n = fibcount (n-1) + fibcount (n-2) + 1

fastfib :: Int -> Integer -> Integer -> Integer
fastfib 0 a _ = a
fastfib n a b = fastfib (n-1) b (a + b)
fastfib n a b =
  if n == 0
  then 1
  else fastfib (n-1) b (a + b) +1


--fibonacci n = F(n)
fibonacci :: Integer -> Integer
fibonacci n | n >= 0 = fst (fib n)

--fib n = (F(n), F(n+1))
fib :: Integer -> (Integer, Integer)
fib 0 = (0, 1)
fib n =
	let (a, b) = fib (div n 2)
	    c = a * (b * 2 - a)
	    d = a * a + b * b
	in if mod n 2 == 0
		then (c, d)
		else (d, c + d)

fastfibcount :: Int -> Integer
fastfibcount 0 = 1 
fastfibcount 1 = 1
fastfibcount n = ((fastfib n 1 1)*2)-1  

-- Zadanie 4
filterM' :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM' p [] = return []
filterM' p (x:xs) =
    let rest = filterM' p xs in
      do b  <- p x
         if b then liftM (x:) rest
              else            rest
 
main :: IO ()
main = do
    
    --print "Zadanie 1"
    --pierwiastek 102004 1 0 100
    --print "Zadanie 2"
    --print $ fibcount 25
    --print $ fastfibcount 25
    --print $ fastfib 25 1 1
    --test1
    --print "Zadanie 4"
    --print $ fibonacci 25
    print $ filterM' (const [True,False]) [1,2,3,4,5]



  