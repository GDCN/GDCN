module Prime where

import Data.Binary
import Data.ByteString.Lazy (ByteString)

{- A somewhat inefficent prime algorithm
   (Possible improvments: Use something else then lists
                          Increment with more then 1
                          And so on...
   )
-}

run :: [ByteString] -> ByteString
run (h:rs) = let (start, end) = (decode h) :: (Integer, Integer) -- Reads input of task
                 primes = (concatMap decode rs) :: [Integer] -- Reads previous results
             in encode $ prime start end primes []

prime :: Integer -> Integer -> [Integer] -> [Integer] -> [Integer]
prime n end primes buffer
  | n >= end  = buffer
  | otherwise = if isPrime n (primes ++ buffer)
                   then prime (n+1) end primes (reverse $ n:(reverse buffer))
                   else prime (n+1) end primes buffer

isPrime :: Integer -> [Integer] -> Bool
isPrime n (p:ps) = if p <= squareRoot n
                      then if n `mod` p /= 0
                              then isPrime n ps
                              else False
                      else True
isPrime n [] = True

-- From http://www.haskell.org/haskellwiki/Generic_number_type#squareRoot
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n
 
squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters
