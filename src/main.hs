import System.Random (StdGen, mkStdGen, newStdGen)
import Types
import Agents
import Environment
import UI
import Utils

-- test :: (Int, Int) -> (Int, Int, Int) -> Env
-- test size (amount_c, amount_o, amount_d) = Env fgrid corral
test :: Env
test = env
  where
    size = (6, 6)
    amount_elems = (5, 5, 4)
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

-- Runs a simulation with the generator, the env and the number of
--      rounds passed as arguments.
sim :: StdGen -> Env -> Int -> IO ()
sim _ _ 0 = do
    putStrLn (" \ESC[32m" ++ replicate 31 '-')
    putStrLn " | > Simulation Finished!      |"
    putStrLn (" " ++ replicate 31 '-' ++ "\ESC[0m")
sim gen env round = do
    putStrLn (" \ESC[32m" ++ replicate 31 '-')
    putStrLn (
        " | > Round " ++ show round ++
        " (clean "++ show (truncate $ score_env env) ++ " %)")
    putStrLn (" " ++ replicate 31 '-' ++ "\ESC[0m")
    -- make kids movements
    let kids = fetch_all_out env (\ x -> is_kid x)
    (tenv, tgen) <- move_kids gen env kids
    -- make robot action

    -- go next round
    sim tgen tenv (round - 1)

-- Runs a simulation with a seed
seed_sim :: Int -> Int -> IO ()
seed_sim seed rounds = do
    let (env, gen) = random_env (mkStdGen seed) (6, 6) (4, 4, 4)
    putStrLn start_label
    print_env env
    sim gen env rounds

-- Runs a simulation without a seed
rand_sim :: Int -> IO ()
rand_sim rounds = do
    gen <- newStdGen
    let (env, tgen) = random_env gen (6, 6) (4, 4, 4)
    putStrLn start_label
    print_env env
    sim tgen env rounds