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
    e == Just Obstacle || e == Nothing || (e == Just Kid && elem pos corral)
    where
        e = get_m_elem env pos

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
--         get_m_elem env (x - 1, y),
--         get_m_elem env (x + 1, y),
--         get_m_elem env (x, y - 1),
--         get_m_elem env (x, y + 1)
--     ], not clear || e /= Nothing]

-- Finds the shortest path for the Robot in the Env (BFS).
shortest_path :: Env -> (Int, Int) -> [((Int, Int), [(Int, Int)])] -> Seq ((Int, Int), [(Int, Int)]) -> Bool -> [(Int, Int)]
shortest_path _ _ _ Seq.Empty _ = []
shortest_path env@(Env grid corral) end visited pending ignore_kids =
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
                        e == Empty ||
                        e == Dirt ||
                        ((e == Kid && not ignore_kids) && not (elem pos corral))))
                new_pending =
                    temp_pending >< (fromList [ (pos, path ++ [actual]) |
                                    pos <- adj,
                                    can_walk_over pos,
                                    not (elem pos [x | (x, _) <- visited])])
                new_visited =  visited ++ [box]
            in shortest_path env end new_visited new_pending ignore_kids
    )

-- Returns the shortest path for a Robot
-- (env, robot, end position) -> path
get_path :: Env -> Elem -> (Int, Int) -> [(Int, Int)]
get_path env (Robot has_kid pos) end =
    shortest_path env end [] (singleton (pos, [])) has_kid

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
        
    


-- -- Returns the action choosed by the Robot in his turn
-- robot_action :: Env -> 