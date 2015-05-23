module ReferenceColors where

import Data.List (sort, delete)
import Data.Functor
import Control.Applicative
import Test.QuickCheck


--
-- Coloring
data Coloring = Coloring
    { getR :: [Int]
    , getG :: [Int]
    , getB :: [Int]
    , getW :: [Int]
    } deriving (Show,Eq)

isValidC :: Coloring -> Bool
isValidC (Coloring r g b w) = pieces == sort (concat [r,g,b,w])
--

--
-- HM5 - Clock time to 5 min precision. Hour and min are
-- both in range [0.11] inclusive.
data HM5 = HM5
    { getHour :: Int
    , getMin5 :: Int
    } deriving (Show,Eq)

isValidHM5 :: HM5 -> Bool
isValidHM5 (HM5 h m5) = p h && p m5
  where
    p x = (0 <= x) && (x <= 11)
--

--
-- Main stuff
deColor :: Coloring -> HM5
deColor (Coloring r g b _) = HM5 h m5
  where
    h  = sum $ r ++ b
    m5 = sum $ g ++ b

color :: HM5 -> Either String Coloring
color hm5@(HM5 h m5) = loop (min h m5) hm5
  where
    loop least hm5 = if least < 0
                        then Left "Exhausted all arrangements"
                        else case backTracker least hm5 of
                               Just c  -> Right c
                               Nothing -> loop (least-1) hm5

backTracker :: Int -> HM5 -> Maybe Coloring
backTracker least (HM5 h m5) = do
  let p0 = reverse pieces
  b <- greedySum p0 least
  --
  let p1 = delete' b p0
  r <- greedySum p1 (h - least)
  --
  let p2 = delete' r p1
  g <- greedySum p2 (m5 - least)
  --
  let w  = delete' g p2
  return $ Coloring r g b w

-- delete' deletes each element of the first list
-- out of the second list.
delete' :: Eq a => [a] -> [a] -> [a]
delete' ds ls = foldr delete ls ds

--
-- Helpers

-- greedySum expects a list sorted in descending order
greedySum :: [Int] -> Int -> Maybe [Int]
greedySum ns v = if v == s'
                    then return seen'
                    else fail ""
  where
    f acc@(s,seen) n = if (s + n) <= v
                          then (s+n, n:seen)
                          else acc
    (s',seen') = foldl f (0,[]) ns

pieces :: [Int]
pieces = take 5 fibs -- [1,1,2,3,5]

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
--

-- Instances
instance Arbitrary HM5 where
  arbitrary = HM5 <$> choose (0,11)
                  <*> choose (0,11)

