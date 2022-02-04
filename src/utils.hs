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
rem_sublist [] list = list
rem_sublist (x:xs) list = filter (/= x) (rem_sublist xs list)

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

-- Returns the indexes of elements in the list that satisfy the predicate.
satisfy_indexes :: [a] -> (a -> Bool) -> [Int]
satisfy_indexes list p = _satisfy_indexes list p 0
_satisfy_indexes :: [a] -> (a -> Bool) -> Int -> [Int]
_satisfy_indexes [] _ _ = []
_satisfy_indexes (x : xs) p i = (if p x then [i] else []) ++ _satisfy_indexes xs p ni
    where ni = i + 1

-- Returns all the coords that satisfy a predicate.
get_coords :: [[a]] -> (a -> Bool) -> [(Int, Int)]
get_coords grid p = _get_coords grid p 0
_get_coords :: [[a]] -> (a -> Bool) -> Int -> [(Int, Int)]
_get_coords [] _ _ = []
_get_coords (row : grid) p x =
  [(x, y) | y <- satisfy_indexes row p] ++ _get_coords grid p nx
    where nx = x + 1

-- Gets the element at a position of the Env if exists.
-- An element may no exists if it is outside the boundaries of the grid.
get_elem :: Env -> (Int, Int) -> Elem
get_elem (Env grid _) (x, y) =
  if x < 0 || y < 0 || x >= length grid || y >= length (grid !! 0)
    then Void
    else (grid !! x !! y)

-- Checks if a box of the Env contains a Robot
is_robot :: Elem -> Bool
is_robot (Robot _ _) = True
is_robot (MultiElem (elem1, _)) = is_robot elem1
is_robot _ = False

-- Checks if a box of the Env contains a Kid
is_kid :: Elem -> Bool
is_kid Kid = True
is_kid (MultiElem (_, Kid)) = True
is_kid _ = False

is_kid_alone :: Elem -> Bool
is_kid_alone elem = (is_kid elem) && (not (is_robot elem))

is_dirt :: Elem -> Bool
is_dirt Dirt = True
is_dirt (MultiElem (_, Dirt)) = True
is_dirt _ = False

is_dirt_alone :: Elem -> Bool
is_dirt_alone elem = (is_dirt elem) && (not (is_robot elem))

get_robot :: Elem -> Elem
get_robot robot@(Robot has_kid pos) = robot
get_robot (MultiElem (robot@(Robot _ _), _)) = robot

force_value :: Maybe a -> a
force_value (Just a) = a
