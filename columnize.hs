-- An implementation of Columnize in Haskell
module Columnize where

import qualified Data.List as L
import qualified Data.Tuple as T
import Test.QuickCheck

testWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty", "twentyone", "twentytwo", "twentythree", "twentyfour", "twentyfive", "twentysix", "twentyseven"]

groupBy _ [] = []
groupBy 0 _ = []
groupBy n list
  | n < 0 = error "Cannot groupBy with negative numbers"
  | length list <= n = [list]
  | otherwise = x : groupBy n xs
  where (x, xs) = splitAt n list

maxLength = maximum . map length

rows_and_colwidths_for_h n = rows_and_colwidths . rows_and_cols n

rows_and_colwidths_for_v n = rows_and_colwidths . T.swap . rows_and_cols n

rows_and_colwidths (rows, cols) = (rows, colwidths)
  where colwidths = map maxLength cols

rows_and_cols n list = (rows, cols)
  where rows = groupBy n list
        cols = L.transpose rows

-- TODO: start here with figuring out how to select the first size where sum colwidths <= display_width
compute_rows_and_colwidths display_width list
  | maxLength list >= display_width = rows_and_colwidths_for_h 1 list
  | otherwise = rows_and_colwidths_for_h (length list) list
  where sizes = [n, n-1..1]
        n = length list

-- NOTE: [(rs, cws) | s <- sizes, let (rs, cs) = rows_and_cols_by_size s testWords, let cws = map (maximum . map length) cs, (sum cws) <= 80]

