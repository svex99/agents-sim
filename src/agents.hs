module Agents where

import Data.Sequence (Seq((:<|)), (|>), (><), deleteAt, fromList, singleton)
import qualified Data.Sequence as Seq
import Types
import Environment
import Utils

-- Returns the score for an Env in range [0, 100]
score_env :: Env -> Float
score_env env = let
    empties = length $ fetch_all env Empty
    dirty = length $ fetch_all env Dirt
    in (fromIntegral empties) / (fromIntegral (empties + dirty)) * 100

-- Returns True if box is blocked for the Robot, False in other case.
is_blocked :: Env -> (Int, Int) -> Bool
is_blocked env@(Env _ corral) pos =
    e == Just Obstacle || e == Nothing || (e == Just Kid && elem pos corral)
    where
        e = get_elem env pos

-- Returns the score for a box in the corral.
-- Every side of the box that contains (void, dirt, obstacle, kid) sums +1
-- Every side consecutive of the box bloqued sums +1.
corral_box_score :: Env -> (Int, Int) -> Int
corral_box_score env (x, y) =
    (if up then 1 else 0) +
    (if down then 1 else 0) +
    (if left then 1 else 0) +
    (if right then 1 else 0) +
    (if up && right then 1 else 0) +
    (if up && left then 1 else 0) +
    (if down && right then 1 else 0) +
    (if up && left then 1 else 0)
        where
            up = is_blocked env (x - 1, y)
            down = is_blocked env (x + 1, y)
            left = is_blocked env (x, y - 1)
            right = is_blocked env (x, y + 1)

-- Returns the adjacent boxes to a box
-- adj_elems :: Env -> (Int, Int) -> Bool -> [Maybe Elem]
-- adj_elems env (x, y) clear = [
--     e | e <- [
--         get_elem env (x - 1, y),
--         get_elem env (x + 1, y),
--         get_elem env (x, y - 1),
--         get_elem env (x, y + 1)
--     ], not clear || e /= Nothing]

-- Finds the shortest path for the Robot in the Env (BFS).
shortest_path :: Env -> (Int, Int) -> [((Int, Int), [(Int, Int)])] -> Seq ((Int, Int), [(Int, Int)]) -> [(Int, Int)]
shortest_path _ _ _ Seq.Empty = []
shortest_path env@(Env grid corral) end visited pending =
    let
        box@(actual, path) :<| temp_pending = pending
    in (if end == actual
        then
            path ++ [end]
        else
            let
                adj = get_adj (length grid, length (grid !! 0)) (actual)
                can_walk_over = (\ pos ->
                    let
                        e = get_elem env pos
                    in (
                        e == Just Empty ||
                        e == Just Dirt ||
                        (e == Just Kid && not (elem pos corral))))
                new_pending =
                    temp_pending >< (fromList [ (pos, path ++ [actual]) |
                                    pos <- adj,
                                    can_walk_over pos,
                                    not (elem pos [x | (x, _) <- visited])])
                new_visited =  visited ++ [box]
            in shortest_path env end new_visited new_pending
    )

-- Returns the shortest path for a Robot
get_path :: Env -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
get_path env start end = shortest_path env end [] (singleton (start, []))
