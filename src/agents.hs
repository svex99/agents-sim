module Agents where

import Data.Sequence (Seq((:<|)), (|>), (><), deleteAt, fromList, singleton)
import qualified Data.Sequence as Seq
import Types
import Environment
import Utils

-- Returns the score for an Env in range [0, 100]
score_env :: Env -> Float
score_env env = let
    empties = length $ fetch_all_out env (\ x -> x == Empty )
    dirty = length $ fetch_all env (\ x -> is_dirt x)
    in (fromIntegral empties) / (fromIntegral (empties + dirty)) * 100

-- Returns True if box is blocked for the Robot, False in other case.
is_blocked :: Env -> (Int, Int) -> Bool
is_blocked env@(Env grid corral) pos =
    e == Obstacle || e == Void || (e == Kid && elem pos corral)
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
    (if down && left then 1 else 0)
        where
            up = is_blocked env (x - 1, y)
            down = is_blocked env (x + 1, y)
            left = is_blocked env (x, y - 1)
            right = is_blocked env (x, y + 1)

-- Returns the adjacent boxes to a box
-- adj_elems :: Env -> (Int, Int) -> Bool -> [Maybe Elem]
-- adj_elems env (x, y) clear = [
--     e | e <- [
--         get_m_elem env (x - 1, y),
--         get_m_elem env (x + 1, y),
--         get_m_elem env (x, y - 1),
--         get_m_elem env (x, y + 1)
--     ], not clear || e /= Nothing]

-- Returns True if Robot can walk over this box, False in other case
-- (environment, robot, box position) -> True if can walk
can_walk_over :: Env -> Elem -> (Int, Int) -> Bool
can_walk_over env@(Env _ corral) robot pos =
    let
        (Robot has_kid _) = get_robot(robot)
        elem_ = get_elem env pos
    in (
        -- can walk over an Empty box in or outside the corral
        (elem_ == Empty) ||
        -- can walk over a Dirt that no share box with a robot
        ((is_dirt elem_) && (not (is_robot elem_))) ||
        -- can walk over a Kid if:
            -- not carrying a Kid
            -- and Kid is not sharing box with a Robot
            -- and Kid is not in the corral
        ((not has_kid) && (is_kid elem_) && (not (is_robot elem_)) && (not (elem pos corral))))

-- Finds the shortest path for the Robot to a pos that satisfies a property (BFS).
_shortest_path :: Env -> ((Int, Int) -> Bool) -> [((Int, Int), [(Int, Int)])] -> Seq ((Int, Int), [(Int, Int)]) -> Elem -> [(Int, Int)]
_shortest_path _ _ _ Seq.Empty _ = []
_shortest_path env@(Env grid corral) end_prop visited pending robot =
    let
        box@(actual, path) :<| temp_pending = pending
    in (if end_prop actual
        then
            path ++ [actual]
        else
            let
                adj = get_adj (length grid, length (grid !! 0)) (actual)
                new_pending =
                    temp_pending >< (fromList [ (pos, path ++ [actual]) |
                                    pos <- adj,
                                    can_walk_over env robot pos,
                                    not (elem pos [x | (x, _) <- visited])])
                new_visited = visited ++ [box]
            in _shortest_path env end_prop new_visited new_pending robot
    )

-- Returns the shortest path for a Robot to a position
-- (env, robot, end position) -> path
get_path :: Env -> Elem -> (Int, Int) -> [(Int, Int)]
get_path env robot@(Robot _ pos) end =
    _shortest_path env (\ p -> p == end) [] (singleton (pos, [])) robot

-- Retruns the path from an Elem to the nearest Elem that satisfy a property
_get_nearest :: Env -> (Int, Int) -> (Elem -> Bool) -> Elem -> [(Int, Int)]
_get_nearest env start elem_prop robot =
    _shortest_path env (\p -> elem_prop (get_elem env p)) [] (singleton (start, [])) robot

-- Returns the path to the nearest Kid
nearest_kid :: Env -> (Int, Int) -> Elem -> [(Int, Int)]
nearest_kid env start robot = _get_nearest env start is_kid robot

-- Returns the path to the nearest Dirt
nearest_dirt :: Env -> (Int, Int) -> Elem -> [(Int, Int)]
nearest_dirt env start robot = _get_nearest env start is_dirt robot

-- Moves the Robot to a new position of the Env.
move_robot :: Env -> Elem -> (Int, Int) -> Env
move_robot env@(Env grid corral) robot@(Robot has_kid pos) new_pos =
    new_env
        where
            -- move robot to new_pos
            tenv = add_elem_to_env_at env new_new_pos_elem new_pos
                where
                    at_new_pos = get_elem env new_pos
                    new_new_pos_elem 
                        | at_new_pos == Kid = Robot True new_pos
                        | at_new_pos == Dirt = MultiElem (Robot has_kid new_pos, Dirt)
                        | otherwise = Robot has_kid new_pos
            -- remove robot from pos
            new_env = add_elem_to_env_at tenv new_pos_elem pos
                where
                    at_pos = get_elem tenv pos
                    new_pos_elem
                        | at_pos == MultiElem (robot, Kid) = Kid
                        | at_pos == MultiElem (robot, Dirt) = Dirt
                        | otherwise = Empty

-- Lefts the kid of a Robot.
left_kid :: Env -> Elem -> Env
left_kid env (Robot True pos) = 
    add_elem_to_env_at env (MultiElem (Robot False pos, Kid)) pos
