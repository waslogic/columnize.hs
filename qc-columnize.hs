import Columnize
import Test.QuickCheck.Batch

options = TestOptions
      { no_of_tests         = 200
      , length_of_tests     = 1
      , debug_tests         = False }


main = do
  runTests "groupBy" options
    [ run prop_numOfGroups
    , run prop_length_of_inner_lists
    , run prop_flattened_groups_is_list ]


-- test groupBy
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


