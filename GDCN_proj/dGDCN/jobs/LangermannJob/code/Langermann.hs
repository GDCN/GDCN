
module Langermann where

import Data.List
import Control.Monad.State.Lazy
import System.Random

import Data.ByteString.Lazy (ByteString)
import GDCN.Trusted.Data.Binary (encode, decode)

type Vector = [Double]
type Matrix = [Vector]

-- Constant variables

dimensions :: Int
dimensions = 2

mConst :: Int
mConst = 5

cConst :: Vector
cConst = [1 , 2, 5, 2, 3]

aConst :: Matrix
aConst = [[3, 5], [5, 2], [2, 1], [1, 4], [7, 9]]

bot :: Double
bot = 0

top :: Double
top = 10

tests :: Int
tests = 10000000

-- Vector and matrix functions

lookupVector :: Vector -> Int -> Double
lookupVector vector x = vector !! (x - 1)

lookupMatrix :: Matrix -> (Int, Int) -> Double
lookupMatrix matrix (x, y) = (matrix !! (x - 1)) !! (y - 1)

-- Random functions

randomVector :: State StdGen Vector
randomVector = randomVector' dimensions []

randomVector' :: Int -> Vector -> State StdGen Vector
randomVector' 0 nums = return nums
randomVector' n nums = do
    gen <- get
    let (num, gen') = randomR (bot, top) gen
    put gen'
    randomVector' (n-1) (num:nums)

-- Calculation function

langermann :: Vector -> Double
langermann x = sum [ let sumValue = theSum x i
                     in lookupVector cConst i * exp (invPi * sumValue) * cos (pi * sumValue)
                   | i <- [1..mConst] ]

invPi :: Double
invPi = - (1 / pi)

theSum :: Vector -> Int -> Double
theSum x i = sum [ (lookupVector x j - lookupMatrix aConst (i, j)) ^ 2 | j <- [1..dimensions] ]

--

randomLangermann :: State StdGen (Vector, Double)
randomLangermann = do
    vector <- randomVector
    return (vector, langermann vector)

search :: Int -> State StdGen (Vector, Double)
search depth = do
    results <- repeatM randomLangermann depth
    return $ maximumBy (\a b -> compare (snd a) (snd b)) results

multiSearch :: State StdGen (Vector, Double)
multiSearch = do
    results <- repeatM (search 10000) (tests `div` 10000)
    return $ maximumBy (\a b -> compare (snd a) (snd b)) results

fullSearch :: Int -> (Vector, Double)
fullSearch seed = evalState multiSearch (mkStdGen seed)

run :: [ByteString] -> (ByteString, String)
run (seedData:_) = let seed = decode seedData :: Int
                       (resultVec, resultN) = fullSearch seed
                       resultData = encode resultVec
                   in (resultData, show resultVec ++ " -> " ++ show resultN)

--

repeatM :: Monad m => m a -> Int -> m [a]
repeatM m 0 = return []
repeatM m n = do
    a <- m
    rest <- repeatM m (n-1)
    return (a : rest)
