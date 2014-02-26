import Data.List

divisibleRec :: Int -> Int -> Bool
divisibleRec i j 
  | j == 1         = False 
  | i `mod` j == 0 = True 
  | otherwise      = divisibleRec i (j-1)

divisible :: Int -> Bool
divisible i = divisibleRec i (i-1)

--r = [ x | x <- [2..20000], not (divisible x)]

main :: IO ()
main = print $ foldl' (\s x -> if divisible x then s else s+1) 0 [2..20000]
--main = print (length r)
