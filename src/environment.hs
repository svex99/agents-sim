module Environment where

import System.Random (StdGen, randomR)
import Types
import Utils

---------------------------- Environment functions ---------------------------

-- builds the environment's grid of n x m (n, m > 0)
build_grid :: (Int, Int) -> [[Elem]]
build_grid (1, m) = [[Empty | _ <- [1 .. m]]]
build_grid (n, m) = [[Empty | _ <- [1 .. m]]] ++ build_grid (nn, m)
  where
    nn = n - 1

-- gets a random box adjacent to the corral
choose_adj_to_corral :: StdGen -> (Int, Int) -> [(Int, Int)] -> ((Int, Int), StdGen)
choose_adj_to_corral gen (n, m) [] = ((x, y), ngen)
  where
    (x, tgen) = randomR (0, n - 1) gen
    (y, ngen) = randomR (0, m - 1) tgen
choose_adj_to_corral gen size corral = (box, ngen)
  where
    adj_to_corral = get_adj_list size corral
    clean_adj = rem_sublist corral adj_to_corral
    (box, ngen) = rand_choice gen clean_adj

-- builds the corral as a list of adjacent boxes
-- (generator, (n, m), amount of kids, corral) -> (corral, new generator)
build_corral :: StdGen -> (Int, Int) -> Int -> [(Int, Int)] -> ([(Int, Int)], StdGen)
build_corral gen size 1 corral = ([box] ++ corral, ngen)
  where
    (box, ngen) = choose_adj_to_corral gen size corral
build_corral gen size c corral = (rem_dups $ new_corral ++ corral_rest, ngen)
  where
    nc = c - 1
    (box, tgen) = choose_adj_to_corral gen size corral
    new_corral = corral ++ [box]
    (corral_rest, ngen) = build_corral tgen size nc (new_corral)

-- gets empty spaces in the grid outside the corral
-- the Int parameter should be setted to 0
empties :: [[Elem]] -> [(Int, Int)] -> Int -> [(Int, Int)]
empties [] corral _ = []
empties (row : grid) corral x =
  rem_sublist corral [(x, y) | y <- [0 .. len], (row !! y) == Empty]
    ++ empties grid corral nx
  where
    len = length row - 1
    nx = x + 1

-- add randomly an Elem in an empty space of the Env
add_elem :: StdGen -> Env -> Elem -> (Env, StdGen)
add_elem gen (Env grid corral) elem = (Env new_grid corral, ngen)
  where
    empty_boxes = empties grid corral 0
    (box, ngen) = rand_choice gen empty_boxes
    new_grid = replace_at grid elem box

-- add randomly n Elems in empty spaces of the Env and outside the corral
add_elems :: StdGen -> Env -> Elem -> Int -> (Env, StdGen)
add_elems gen env@(Env grid _) _ 0 = (env, gen)
add_elems gen env@(Env grid corral) elem n = (nenv, ngen)
  where
    nn = n - 1
    (tenv, tgen) = add_elems gen env elem nn
    (nenv, ngen) = add_elem tgen tenv elem

add_elem_to_env_at :: Env -> Elem -> (Int, Int) -> Env
add_elem_to_env_at (Env grid corral) elem pos = Env (replace_at grid elem pos) corral

-- Returns all the coords outside the corral that contains determinated Elem.
fetch_all :: Env -> Elem -> [(Int, Int)]
fetch_all (Env grid corral) elem = rem_sublist corral coords
    where
        coords = get_coords grid elem

-- returns a new Env randomly generated
random_env :: StdGen -> (Int, Int) -> (Int, Int, Int) -> (Env, StdGen)
random_env gen size (kids, obstacles, dirt) = (nenv, ngen)
    where
        grid = build_grid size
        (corral, gen1) = build_corral gen size kids []
        env0 = Env grid corral
        -- add kids
        (env1, gen2) = add_elems gen1 env0 Kid kids
        -- add obstacles
        (env2, gen3) = add_elems gen2 env1 Obstacle obstacles
        -- add dirt
        (env3, gen4) = add_elems gen3 env2 Dirt dirt
        -- add robot
        (nenv, ngen) = add_elems gen4 env3 (Robot False (0, 0)) 1

----------------------------- Obstacle functions -----------------------------

obs_can_be_pushed_up :: Env -> (Int, Int) -> Bool
obs_can_be_pushed_up env@(Env grid corral) (x, y)
  | x <= 0 = False
  | up_elem == Empty && not (elem (x - 1, y) corral) = True
  | up_elem == Obstacle = obs_can_be_pushed_up env (x - 1, y)
  | otherwise = False
  where
    up_elem = grid !! (x - 1) !! y

obs_can_be_pushed_down :: Env -> (Int, Int) -> Bool
obs_can_be_pushed_down env@(Env grid corral) (x, y)
  | x >= length grid - 1 = False
  | down_elem == Empty && not (elem (x + 1, y) corral) = True
  | down_elem == Obstacle = obs_can_be_pushed_down env (x + 1, y)
  | otherwise = False
  where
    down_elem = grid !! (x + 1) !! y

obs_can_be_pushed_left :: Env -> (Int, Int) -> Bool
obs_can_be_pushed_left env@(Env grid corral) (x, y)
  | y <= 0 = False
  | left_elem == Empty && not (elem (x, y - 1) corral) = True
  | left_elem == Obstacle = obs_can_be_pushed_left env (x, y - 1)
  | otherwise = False
  where
    left_elem = grid !! x !! (y - 1)

obs_can_be_pushed_right :: Env -> (Int, Int) -> Bool
obs_can_be_pushed_right env@(Env grid corral) (x, y)
  | y >= length (grid !! 0) - 1 = False
  | right_elem == Empty && not (elem (x, y + 1) corral) = True
  | right_elem == Obstacle = obs_can_be_pushed_right env (x, y + 1)
  | otherwise = False
  where
    right_elem = grid !! x !! (y + 1)

----------------------------- Kids functions -----------------------------

kid_can_move_up :: Env -> (Int, Int) -> Bool
kid_can_move_up env@(Env grid corral) pos@(x, y)
  | x <= 0 || elem pos corral || elem up_pos corral = False
  | up_elem == Empty || up_elem == Obstacle = True
  | otherwise = False
  where
    up_pos = (x - 1, y)
    up_elem = grid !! (x - 1) !! y

kid_can_move_down :: Env -> (Int, Int) -> Bool
kid_can_move_down env@(Env grid corral) pos@(x, y)
  | x >= length grid - 1 || elem pos corral || elem down_pos corral = False
  | down_elem == Empty || down_elem == Obstacle = True
  | otherwise = False
  where
    down_pos = (x + 1, y)
    down_elem = grid !! (x + 1) !! y

kid_can_move_left :: Env -> (Int, Int) -> Bool
kid_can_move_left env@(Env grid corral) pos@(x, y)
  | y <= 0 || elem pos corral || elem left_pos corral = False
  | left_elem == Empty || left_elem == Obstacle = True
  | otherwise = False
  where
    left_pos = (x, y - 1)
    left_elem = grid !! x !! (y - 1)

kid_can_move_right :: Env -> (Int, Int) -> Bool
kid_can_move_right env@(Env grid corral) pos@(x, y)
  | y >= length (grid !! 0) - 1 || elem pos corral || elem right_pos corral = False
  | right_elem == Empty || right_elem == Obstacle = True
  | otherwise = False
  where
    right_pos = (x, y + 1)
    right_elem = grid !! x !! (y + 1)

-- returns the list of options of movement for a kid.
-- if a position is blocked by obstacles, returns the actual position to keep
--    that choice as not movement.
kid_move_opt :: Env -> (Int, Int) -> [(Int, Int)]
kid_move_opt env@(Env grid corral) pos@(x, y) =
  up ++ down ++ left ++ right ++ [pos]
  where
    up_pos = (x - 1, y)
    up =
      if kid_can_move_up env pos
        then
          if (grid !! (x - 1) !! y) == Obstacle
            then
              if obs_can_be_pushed_up env up_pos
                then [up_pos]
                else [pos]
            else [up_pos]
        else []
    down_pos = (x + 1, y)
    down =
      if kid_can_move_down env pos
        then
          if (grid !! (x + 1) !! y) == Obstacle
            then
              if obs_can_be_pushed_down env down_pos
                then [down_pos]
                else [pos]
            else [down_pos]
        else []
    left_pos = (x, y - 1)
    left =
      if kid_can_move_left env pos
        then
          if (grid !! x !! (y - 1)) == Obstacle
            then
              if obs_can_be_pushed_left env left_pos
                then [left_pos]
                else [pos]
            else [left_pos]
        else []
    right_pos = (x, y + 1)
    right =
      if kid_can_move_right env pos
        then
          if (grid !! x !! (y + 1)) == Obstacle
            then
              if (obs_can_be_pushed_right env right_pos)
                then [right_pos]
                else [pos]
            else [right_pos]
        else []

-- choose randomly a position to move the kid.
kid_new_pos :: StdGen -> Env -> (Int, Int) -> ((Int, Int), StdGen)
kid_new_pos gen env pos = rand_choice gen options
  where
    options = (kid_move_opt env pos)

-- returns the amount of boxes that must be dirty after a kid moves.
amount_to_be_dirty :: Env -> (Int, Int) -> Int
amount_to_be_dirty env@(Env grid _) (x, y)
  | kids == 1 = 1
  | kids == 2 = 3
  | otherwise = 6
  where
    ul = if (get_elem env (x - 1, y - 1)) == Just Kid then 1 else 0
    u = if (get_elem env (x - 1, y)) == Just Kid then 1 else 0
    ur = if (get_elem env (x - 1, y + 1)) == Just Kid then 1 else 0
    dl = if (get_elem env (x + 1, y - 1)) == Just Kid then 1 else 0
    d = if (get_elem env (x + 1, y)) == Just Kid then 1 else 0
    dr = if (get_elem env (x + 1, y + 1)) == Just Kid then 1 else 0
    l = if (get_elem env (x, y - 1)) == Just Kid then 1 else 0
    r = if (get_elem env (x, y + 1)) == Just Kid then 1 else 0
    kids = ul + u + ur + dl + d + dr + l + r + 1

-- returns the boxes empties and outside the corral near a box.
empties_near :: Env -> (Int, Int) -> [(Int, Int)]
empties_near env@(Env grid corral) (x, y) =
  ul ++ u ++ ur ++ dl ++ d ++ dr ++ l ++ r
  where
    (ul_pos, u_pos, ur_pos) = ((x - 1, y - 1), (x - 1, y), (x - 1, y + 1))
    (dl_pos, d_pos, dr_pos) = ((x + 1, y - 1), (x + 1, y), (x + 1, y + 1))
    (l_pos, r_pos) = ((x, y - 1), (x, y + 1))
    ul = if not (elem ul_pos corral) && (get_elem env ul_pos) == Just Empty then [ul_pos] else []
    u = if not (elem u_pos corral) && (get_elem env u_pos) == Just Empty then [u_pos] else []
    ur = if not (elem ur_pos corral) && (get_elem env ur_pos) == Just Empty then [ur_pos] else []
    dl = if not (elem dl_pos corral) && (get_elem env dl_pos) == Just Empty then [dl_pos] else []
    d = if not (elem d_pos corral) && (get_elem env d_pos) == Just Empty then [d_pos] else []
    dr = if not (elem dr_pos corral) && (get_elem env dr_pos) == Just Empty then [dr_pos] else []
    l = if not (elem l_pos corral) && (get_elem env l_pos) == Just Empty then [l_pos] else []
    r = if not (elem r_pos corral) && (get_elem env r_pos) == Just Empty then [r_pos] else []

-- gets the boxes to be dirty after a kid moves.
boxes_to_be_dirty :: StdGen -> Env -> (Int, Int) -> (Int, Int) -> ([(Int, Int)], StdGen)
boxes_to_be_dirty gen env@(Env grid corral) pos new_pos = (boxes, ngen)
  where 
    to_be_dirty = amount_to_be_dirty env pos
    -- get empties near to pos, then remove new_pos and add pos to options.
    options = (rem_sublist [new_pos] (empties_near env pos)) ++ [pos]
    (boxes, _, ngen)
      | to_be_dirty >= length options = (options, [], gen)
      | otherwise = rand_rem_k gen options to_be_dirty

-- Dirts the Env and returns the new one
do_dirt :: Env -> ([(Int, Int)]) -> Env
do_dirt env [] = env
do_dirt env@(Env grid corral) (pos@(x, y) : rest) = nenv
    where
        tenv = do_dirt env rest
        nenv
            | (grid !! x !! y) == Empty = add_elem_to_env_at tenv Dirt pos
            | otherwise = tenv

-- Returns the Env as result of push an Obstacle in a direction.
-- (env, pos of pusher, pos of obstacle pushed) -> new env
push_obs :: Env -> (Int, Int) -> (Int, Int) -> Env
push_obs env@(Env grid corral) (px, py) obs_pos@(ox, oy) = new_env
    where
        new_pos@(nx, ny) = (2 * ox - px, 2 * oy - py)
        temp_env@(Env tgrid _) = if (grid !! nx !! ny) == Obstacle
            then push_obs env obs_pos new_pos
            else env
        new_env = if (tgrid !! nx !! ny) == Empty
            then add_elem_to_env_at 
                (add_elem_to_env_at temp_env Empty obs_pos) Obstacle new_pos
            else temp_env

-- Makes the movement of a kid over the environment.
-- Returns the new env, the new generator and the new pos of the kid.
make_kid_move :: StdGen -> Env -> (Int, Int) -> (Env, StdGen, (Int, Int))
make_kid_move gen env pos = (nenv, ngen, new_pos)
    where
        (new_pos, tgen1) = kid_new_pos gen env pos
        (make_dirt, tgen2) = randomR (0, 1) tgen1 :: (Int, StdGen)
        (to_be_dirty, ngen) = boxes_to_be_dirty tgen2 env pos new_pos
        -- checks if kid moved or not
        tenv =
            let
                tenv0 = if get_elem env new_pos == Just Obstacle
                    then push_obs env pos new_pos
                    else env
                tenv1 = add_elem_to_env_at tenv0 Empty pos
                tenv2 = add_elem_to_env_at tenv1 Kid new_pos
            in (if pos /= new_pos then tenv2 else env)
        nenv = if (pos /= new_pos) && (make_dirt == 1)
            then do_dirt tenv to_be_dirty
            else tenv
