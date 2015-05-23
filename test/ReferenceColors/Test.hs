module Main where

import Test.QuickCheck
import qualified Test.QuickCheck.Property as P

import ReferenceColors


main :: IO ()
main = quickCheckWith args prop_correct_coloring

prop_correct_coloring :: HM5 -> P.Result
prop_correct_coloring hm5 = case color hm5 of
                              Left err -> P.failed { P.reason = err }
                              Right c  -> if hm5 == deColor c
                                             then P.succeeded
                                             else P.failed

args :: Args
args = Args { replay = Nothing
            , maxSuccess = 1000
            , maxSize = 1000
            , maxDiscardRatio = 10
            , chatty = True
            }
