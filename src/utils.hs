module Utils where

import System.Random (StdGen, randomR)
import Data.Set (fromList, toList, (\\))
import Data.List (delete, intercalate, elemIndices)
import Data.List.Split (splitOn)
import Types

-- returns a random item from a list.
rand_choice :: StdGen -> [a] -> (a, StdGen)
rand_choice gen list = (list !! i, new_gen)
    where
        (i, new_gen) = randomR (0, length list - 1) gen

-- remove randomly an item from a list.
-- (generator, list) -> (removed item, new list, new generator)
rand_rem :: Ord a => StdGen -> [a] -> (a, [a], StdGen)
rand_rem gen list = (elem, nlist, ngen)
    where
        (elem, ngen) = rand_choice gen list
        nlist = delete elem list

-- removes k random items from a list.
-- (generator, list, k) -> (removed items, new list, new generator)
rand_rem_k :: Ord a => StdGen -> [a] -> Int -> ([a], [a], StdGen)
rand_rem_k gen list 0 = ([], list, gen)
rand_rem_k gen list k = ([elem] ++ rest, new_list, ngen)
    where
        nk = k - 1
        (elem, tlist, tgen) = rand_rem gen list
        (rest, new_list, ngen) = rand_rem_k gen tlist nk

-- removes duplicates from a list.
rem_dups :: Ord a => [a] -> [a]
rem_dups = toList . fromList

-- removes a list of elements from other list. 
rem_sublist :: Ord a => [a] -> [a] -> [a]
rem_sublist sublist list = toList $ x \\ y
    where
        x = fromList list
        y = fromList sublist

-- returns the adjacents boxes of a box in a grid.
get_adj :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
get_adj (n, m) (x, y) = 
    filter (\ (a, b) -> 0 <= a && a < n && 0 <= b && b < m) adj
        where
            adj = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- returns the adjacents boxes of a list of boxes in a grid.
-- (size, corral) -> new corral
get_adj_list :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
get_adj_list _ [] = []
get_adj_list size (coord:coords) =
    rem_dups $ get_adj size coord ++ get_adj_list size coords

-- replaces an element in a grid.
replace_at :: [[a]] -> a -> (Int, Int) -> [[a]]
replace_at grid elem (x, y) = out_l_list ++ [list] ++ out_r_list
    where
        (out_l_list, inn_list:out_r_list) = splitAt x grid
        (inn_l_list, _:inn_r_list) = splitAt y inn_list
        list = inn_l_list ++ [elem] ++ inn_r_list

-- Replaces a sublist from a list.
replace :: Eq a => ([a], [a]) -> [a] -> [a]
replace (from, to) list = intercalate to (splitOn from list)

replace_many :: Eq a => [([a], [a])] -> [a] -> [a]
replace_many [] list = list
replace_many (from_to : rest) list = new_list
    where
        temp_list = replace_many rest list
        new_list = replace from_to temp_list 

-- Returns the all the coords of an item in the grid.
get_coords :: Eq a => [[a]] -> a -> [(Int, Int)]
get_coords grid elem = _get_coords grid elem 0
_get_coords :: Eq a => [[a]] -> a -> Int -> [(Int, Int)]
_get_coords [] elem _ = []
_get_coords (row : grid) elem x =
  [(x, y) | y <- elemIndices elem row] ++ _get_coords grid elem nx
  where
    len = length row - 1
    nx = x + 1

-- Gets the element at a position of the Env if exists.
-- An element may no exists if it is outside the boundaries of the grid.
get_elem :: Env -> (Int, Int) -> Maybe Elem
get_elem (Env grid _) (x, y) =
  if x < 0 || y < 0 || x >= length grid || y >= length (grid !! 0)
    then Nothing
    else Just (grid !! x !! y)