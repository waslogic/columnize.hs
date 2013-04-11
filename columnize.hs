import qualified Data.List as L
import Test.QuickCheck

testWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty", "twentyone", "twentytwo", "twentythree", "twentyfour", "twentyfive", "twentysix", "twentyseven"]

groupBy _ [] = []
groupBy 0 _ = []
groupBy n list
  | n < 0 = error "Cannot groupBy with negative numbers"
  | length list <= n = [list]
  | otherwise = x : groupBy n xs
  where (x, xs) = splitAt n list


compute xs = map (\x -> groupBy x xs) [1.. length xs]

-- map (maximum . map length) $ L.transpose rows
-- where rows = groupBy 5 testWords

-- map (maximum . map length) $ L.transpose . groupBy 5 $ testWords

prop_numOfGroups n list = (n >= 0) ==> (L.genericLength $ groupBy n list) == nrows (fromIntegral n) list
  where nrows _ [] = 0
        nrows 0 _ = 0
        nrows n list = floor $ ((L.genericLength list) + n - 1) / n

prop_length_of_inner_lists n list =
  (n > 0) ==>
  not (null list) ==>
    (all (==n) $ map length full_pages)
  &&
    (length last_page == remainder)
  where pages = groupBy n list
        full_pages = init pages
        last_page = last pages
        remainder = if r == 0 then n else r
        r = (length list) `mod` n

prop_flattened_groups_is_list n list = (n > 0) ==> (concat $ groupBy n list) == list

rows_and_cols_by_size n list = (rows,cols)
  where rows = groupBy n list
        cols = L.transpose rows

-- NOTE: Data.Tuple.swap :: (a,b) -> (b,a)

compute_rows_and_colwidths display_width list
  | biggest_atom >= display_width = (groupBy 1 list, [biggest_atom])
  | otherwise = ([list], [sum cell_widths])
  where cell_widths = map length list
        biggest_atom = maximum cell_widths
        sizes = [n, n-1..1]
        n = length list

-- NOTE: [(rs, cws) | s <- sizes, let (rs, cs) = rows_and_cols_by_size s testWords, let cws = map (maximum . map length) cs, (sum cws) <= 80]
