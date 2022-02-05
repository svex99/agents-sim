module Agents where

import Data.Sequence (Seq((:<|)), (|>), (><), deleteAt, fromList, singleton)
import qualified Data.Sequence as Seq
import Types
import Environment
import Utils

-- Returns the score for an Env in range [0, 100]
_score_env :: Env -> Int -> Float
_score_env env extra_dirt = let
    empties = (length $ fetch_all_out env (\ x -> x == Empty )) - extra_dirt
    dirty = (length $ fetch_all env (\ x -> is_dirt x)) + extra_dirt
    in (fromIntegral empties) / (fromIntegral (empties + dirty)) * 100
score_env :: Env -> Float
score_env env = _score_env env 0

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
corral_box_score env pos@(x, y) =
    if (get_elem env pos) /= Empty
        then -1
        else let
            s = (if up then 1 else 0) +
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
            in if s == 8 then (-1) else s

-- Returns the boxes in the corral with max score, and the score
max_score_corral :: Env -> ([(Int, Int)], Int)
max_score_corral env@(Env _ corral) = let
    box_scores = [(b, corral_box_score env b) | b <- corral]
    max_score = maximum [s | (_, s) <- box_scores]
    in ([b | (b, _) <- (filter (\ (_, s) -> s == max_score) box_scores)], max_score)

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
can_walk_over :: Env -> Elem -> (Int, Int) -> Bool -> Bool
can_walk_over env@(Env _ corral) robot pos exclude_lockeds =
    let
        (Robot has_kid _) = get_robot(robot)
        elem_ = get_elem env pos
    in (
        -- can walk over an Empty box in or outside the corral
        (elem_ == Empty) ||
        -- can walk over a Dirt that no share box with a robot
        (is_dirt_alone elem_) ||
        -- can walk over a Kid if:
            -- not carrying a Kid
            -- and Kid is not sharing box with a Robot
            -- and Kid is not in the corral
        ((not has_kid) && (is_kid_alone elem_) && (not (elem pos corral)) && (xor exclude_lockeds (is_kid_locked env pos))))

-- Finds the shortest path for the Robot to a pos that satisfies a property (BFS).
_shortest_path :: Env -> ((Int, Int) -> Bool) -> [((Int, Int), [(Int, Int)])] -> Seq ((Int, Int), [(Int, Int)]) -> Elem  -> Bool -> [(Int, Int)]
_shortest_path _ _ _ Seq.Empty _ _= []
_shortest_path env@(Env grid corral) end_prop visited pending robot exclude_lockeds =
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
                                    can_walk_over env robot pos exclude_lockeds,
                                    not (elem pos [x | (x, _) <- visited])])
                new_visited = visited ++ [box]
            in _shortest_path env end_prop new_visited new_pending robot exclude_lockeds
    )

-- Returns the shortest path for a Robot to a position
-- (env, robot, end position) -> path
get_path :: Env -> Elem -> (Int, Int) -> [(Int, Int)]
get_path env robot@(Robot _ pos) end =
    _shortest_path env (\ p -> p == end) [] (singleton (pos, [])) robot False

-- Retruns the path from an Elem to the nearest position that satisfy a property
_get_nearest :: Env -> (Int, Int) -> ((Int, Int) -> Bool) -> Elem -> Bool -> [(Int, Int)]
_get_nearest env start pos_prop robot exclude_lockeds =
    _shortest_path env pos_prop [] (singleton (start, [])) robot exclude_lockeds

--pth to the nearest Kid
-- nearest_kid :: Env -> (Int, Int) -> Elem -> [(Int, Int)]
-- nearest_kid env start robot = _get_nearest env start is_kid robot

-- Returns the nearest kid that can move
-- nearest_kid_unlocked :: Env -> (Int, Int) -> Elem -> [(Int, Int)]
-- nearest_kid_unlocked env start robot = let
--     condition = (\ pos -> let
--         e = get_elem env pos
--         in (is_kid_alone e) && (not (is_kid_locked env pos)))
--     in _get_nearest env start condition robot

-- Returns the nearest kid that can dirt
nearest_kid_can_dirt :: Env -> (Int, Int) -> Elem -> [(Int, Int)]
nearest_kid_can_dirt env@(Env _ corral) start robot = let
    condition = (\ pos -> elem pos (free_kids env robot))
    in _get_nearest env start condition robot True

-- Returns the nearest kid
nearest_kid :: Env -> (Int, Int) -> Elem -> [(Int, Int)]
nearest_kid env@(Env _ corral) start robot = let
    condition = (\ pos -> is_kid_alone (get_elem env pos))
    in _get_nearest env start condition robot False

-- Returns the path to the nearest Dirt
nearest_dirt :: Env -> (Int, Int) -> Elem -> [(Int, Int)]
nearest_dirt env start robot =
    _get_nearest env start (\ p -> is_dirt_alone (get_elem env p)) robot True

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

-- Cleans the Dirt that is sharing box with a Robot
clean_dirt :: Env -> Elem -> Env
clean_dirt env robot@(Robot _ pos) =
    add_elem_to_env_at env robot pos

-- Returns True if Kid cannot move, False in other case.
is_kid_locked :: Env -> (Int, Int) -> Bool
is_kid_locked env pos =
    let options = rem_sublist [pos] (kid_move_opt env pos)
    in length options == 0

-- Returns the estimate score for the env in the next round
estimate_score :: Env -> Float
estimate_score env = let
    kids = fetch_all_out env is_kid
    max_dirt = (\ pos ->
        if is_kid_locked env pos
            then 0
            else min
                (amount_to_be_dirty env pos)
                (length $ empties_near env pos))
    sum_max_dirt = div (sum (map max_dirt kids)) 2
    in _score_env env sum_max_dirt

-- Returns all the Kids that
--  are outside the corral
--  and not sharing a box with a robot
--  and not locked
free_kids :: Env -> Elem -> [(Int, Int)]
free_kids env (Robot hk pos)= let
    condition = (\ e -> is_kid_alone e)
    in filter (\ p -> not (is_kid_locked env p) || ((not hk) && (is_adj p pos)))
        (fetch_all_out env condition)

-- Checks if a goal still is valid
is_valid_goal :: Env -> (String, (Int, Int)) -> Elem -> Bool
is_valid_goal env ("move to", pos) _ = let
    e = get_elem env pos
    in e == Empty || is_dirt_alone e
is_valid_goal env ("get kid", pos) _ =
    is_kid_alone (get_elem env pos)
is_valid_goal env ("left kid", pos) (Robot _ rpos) = let
    e = get_elem env pos
    in e == Empty || rpos == pos
is_valid_goal env ("clean dirt", pos) _ =
    is_dirt (get_elem env pos)
is_valid_goal env ("do nothing", _) _= True

check_goals :: Env -> [(String, (Int, Int))] -> Elem -> Bool
check_goals env [goal0, goal1, last_goal] robot =
    is_valid_goal env last_goal robot &&
    is_valid_goal env goal0 robot &&
    is_valid_goal env goal1 robot

do_nothing :: Env -> Elem -> Env
do_nothing env _ = env