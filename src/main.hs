import System.Random (StdGen, mkStdGen, newStdGen)
import Types
import Agents
import Environment
import UI
import Utils
import No_coop_agent

-- test :: (Int, Int) -> (Int, Int, Int) -> Env
-- test size (amount_c, amount_o, amount_d) = Env fgrid corral
test :: Env
test = env
  where
    size = (6, 6)
    amount_elems = (5, 5, 4, 2)
    gen = mkStdGen 9
    (env, _) = random_env gen size amount_elems 

--
move_kids :: StdGen -> Env -> [(Int, Int)] -> IO (Env, StdGen)
move_kids gen env [] = return (env, gen)
move_kids gen env (kid : kids) = do
    let (nenv, ngen, npos) = make_kid_move gen env kid
    if kid /= npos
        then
            do  putStrLn (" \ESC[36m>>> Kid " ++ show kid ++ " moved to " ++ show npos ++ "\ESC[0m")
                print_env nenv
        else
            putStrLn (" \ESC[36m>>> Kid " ++ show kid ++ " didn't moved\ESC[0m")
    move_kids ngen nenv kids

move_robots_no_coop :: Env -> Plans -> [Elem] -> IO (Env, Plans)
move_robots_no_coop env plans [] = return (env, plans)
move_robots_no_coop env plans frobots@(robot@(Robot has_kid pos) : robots) = do
    let (nenv, nplans, action) = make_goal env plans robot
    -- putStrLn ("aaaaaaaaaa" ++ action)
    if action /= "no action"
        then
            do  putStrLn ("Robot " ++ show pos ++ " " ++ action)
                print_env nenv
                move_robots_no_coop nenv nplans robots
        else
            do  let upd_plans = update_plan env plans robot (plan_no_coop env robot) has_kid pos
                putStrLn ("New plan for " ++ show pos ++ " -> " ++ show (get_plan upd_plans robot))
                move_robots_no_coop env upd_plans frobots

-- Runs a simulation with the generator, the env and the number of
--      rounds passed as arguments.
sim :: StdGen -> Env -> Plans -> Int -> IO ()
sim _ _ _ 0 = do
    putStrLn (" \ESC[32m" ++ replicate 31 '-')
    putStrLn " | > Simulation Finished!      |"
    putStrLn (" " ++ replicate 31 '-' ++ "\ESC[0m")
sim gen env plans round = do
    putStrLn (" \ESC[32m" ++ replicate 31 '-')
    putStrLn (
        " | > Round " ++ show round ++
        " (clean "++ show (truncate $ score_env env) ++ " %)")
    putStrLn (" " ++ replicate 31 '-' ++ "\ESC[0m")
    print_plans plans
    -- make kids movements
    let kids = fetch_all_out env (\ x -> is_kid x)
    (tenv1, tgen1) <- move_kids gen env kids
    -- make robot action
    let robots = all_robots env
    putStrLn (show (max_score_corral env))
    (tenv2, nplans) <- move_robots_no_coop tenv1 plans robots
    -- go next round
    sim tgen1 tenv2 nplans (round - 1)

-- Runs a simulation with a seed
seed_sim :: Int -> Int -> IO ()
seed_sim seed rounds = do
    let (env, gen) = random_env (mkStdGen seed) (6, 6) (5, 5, 5, 3)
    let plans = update_all_plans env
    putStrLn start_label
    print_env env
    sim gen env plans rounds

-- Runs a simulation without a seed
rand_sim :: Int -> IO ()
rand_sim rounds = do
    gen <- newStdGen
    let (env, tgen) = random_env gen (6, 6) (4, 4, 4, 2)
    let plans = update_all_plans env
    putStrLn start_label
    print_env env
    sim tgen env plans rounds