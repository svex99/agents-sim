module No_coop_agent where

import Data.List (elemIndex)
import Agents
import Types
import Environment
import Utils
import Agents

data Plan = Plan [(String, (Int, Int))]

data Plans = Plans [(Elem, Plan)]

-- data Assign = Assign [(Elem, (Int, Int))]

instance Show Plan where
    show (Plan plans) = "Plan " ++ show plans

plan_no_coop :: Env -> Elem -> Plan
plan_no_coop env robot@(Robot has_kid pos)
    -- not free kids just clean dirt
    | (length f_kids == 0) = let
        path = nearest_dirt env pos robot
        in if length path == 0
            then Plan [("do nothing", pos)]
            else let
                (_ : fpath) = path
                in Plan ([("move to", pos) | pos <- fpath]
                    ++ [("clean dirt", last fpath)])
    -- go to find a kid and become an OP robot XD
    | not has_kid = let
        (_ : path) = nearest_kid_unlocked env pos robot
        in Plan ([("move to", pos) | pos <- take (length path - 1) path]
            ++ [("get kid", last path)])
    -- go to left the kid in the corral
    | has_kid && score /= 0 = let
        (_ : path) = get_path env robot (max_cb !! 0)
        in Plan ([("move to", pos) | pos <- path]
            ++ [("left kid", last path)])
    -- corral is blocked, go to clean the nearest dirt to the corral
    | has_kid && score == 0 = let
        (_ : path) = nearest_dirt env (max_cb !! 0) robot
        in Plan ([("move to", pos) | pos <- take (length path - 1) path]
            ++ [("clean dirt", last path)])
    | otherwise = error ("Undeterminated goal for Robot " ++ show has_kid ++ " " ++ show pos)
    where
        f_kids = free_kids env
        -- next_env_score = estimate_score env
        (max_cb, score) = max_score_corral env

get_plan :: Plans -> Elem -> Plan
get_plan (Plans []) (Robot _ pos) = error ("No plan for the robot at " ++ show pos)
get_plan (Plans ((robot_, plan) : plans)) robot
    | robot_ == robot = plan
    | otherwise = get_plan (Plans plans) robot

update_plan :: Env -> Plans -> Elem -> Plan -> Bool -> (Int, Int) -> Plans
update_plan env (Plans plans) robot@(Robot _ pos) plan has_kid new_pos = let
    robots = [r | (r, _) <- plans]
    index = elemIndex robot robots
    in (if index == Nothing
        then Plans (plans ++ [(robot, plan)])
        else let
            (front, p: back) = splitAt (force_value index) plans
            in Plans (front ++ [(Robot has_kid new_pos, plan)] ++ back))

_update_all_plans :: Env -> Plans -> [Elem] -> Plans
_update_all_plans env plans [] = plans
_update_all_plans env plans (robot@(Robot has_kid pos): robots) = nplans
    where
        tplans = _update_all_plans env plans robots
        nplans = update_plan env tplans robot (plan_no_coop env robot) has_kid pos
update_all_plans :: Env -> Plans
update_all_plans env = _update_all_plans env (Plans []) (all_robots env)

goal_to_action :: String -> (Int, Int) -> (Env -> Elem -> Env)
goal_to_action "move to" pos = (\ env robot -> move_robot env robot pos)
goal_to_action "get kid" pos = (\ env robot -> move_robot env robot pos)
goal_to_action "left kid" _ = (\ env robot -> left_kid env robot)
goal_to_action "clean dirt" _ = (\ env robot -> clean_dirt env robot)
goal_to_action "do nothing" _ = (\ env robot -> do_nothing env robot)

-- Returns the tuple env, the plans, and a boolean
--  indicating if was executed an action over the env and plans.
make_goal :: Env -> Plans -> Elem -> (Env, Plans, String)
make_goal env plans robot@(Robot has_kid _)
    | check_goals env [goal0, goal1, last_goal] robot =
        (action env robot, update_plan env plans robot nplan nhas_k fpos, fgoal ++ " " ++ show fpos)
    | otherwise = (env, plans, "no action")
    where
        plan@(Plan goals) = get_plan plans robot
        tplan@(Plan tgoals) = if length goals == 0
            then plan_no_coop env robot
            else plan
        last_goal = last tgoals
        goal0@(g0, pos0) = tgoals !! 0
        goal1@(g1, pos1) = if length tgoals > 1 then tgoals !! 1 else goal0
        ((fgoal, fpos), nplan) =
            if has_kid && (g1 == "move to" || g1 == "get kid")
                then (goal1, Plan (drop (if length tgoals > 1 then 2 else 1) tgoals))
                else (goal0, Plan (drop 1 tgoals))
        nhas_k
            | fgoal == "get kid" = True 
            | fgoal == "left kid" = False
            | otherwise = has_kid
        action = goal_to_action fgoal fpos
