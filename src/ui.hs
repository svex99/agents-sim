module UI where

import Types
import Environment
import Utils
import No_coop_agent

---------------------------- visual functions --------------------------------

mark_corral :: [String] -> [(Int, Int)] -> [String]
mark_corral rows [] = rows
mark_corral rows ((x, y) : rest) = new_rows
    where
        temp_rows = mark_corral rows rest
        char = temp_rows !! x !! (2 * y + 1)
        mark_char
            | char == 'N' = 'V'
            | char == 'E' = 'W'
            | char == 'K' = 'X'
            | char == 'R' = 'Y'
            | char == 'B' = 'Z'
        new_rows = replace_at temp_rows mark_char (x, 2 * y + 1)

color_corral :: String -> String
color_corral corral_str =
    (replace_many
        [
            ("V", "\ESC[32mN\ESC[0m"),  -- green
            ("W", "\ESC[32m■\ESC[0m"),  -- green
            ("X", "\ESC[32mK\ESC[0m"),  -- green
            ("Y", "\ESC[32mR\ESC[0m"),  -- green
            ("Z", "\ESC[32mB\ESC[0m"),  -- green
            ("K", "\ESC[36mK\ESC[0m"),  -- cyan
            ("D", "\ESC[33m■\ESC[0m"),  -- yellow
            ("O", "\ESC[35m■\ESC[0m"),  -- magenta
            ("N", "\ESC[31mN\ESC[0m"),  -- red
            ("L", "\ESC[33mB\ESC[0m"),  -- yellow
            ("B", "\ESC[31mB\ESC[0m"),  -- red
            ("M", "\ESC[33mR\ESC[0m"),  -- yellow
            ("R", "\ESC[31mR\ESC[0m"),  -- red
            ("E", "■"), (",", "  "), ("[", "    | "), ("]", " | ")
        ] corral_str)

start_label :: String
start_label =
    " \ESC[32m" ++ (replicate 31 '-') ++ "\n" ++
    " | > Simulation Started!       |\n" ++
    " " ++ (replicate 31 '-') ++ "\ESC[0m"

legend :: String
legend = 
    "■ - Empty box\n" ++
    "■ - Corral\n" ++
    "■ - Dirt\n" ++
    "■ - Obstacle\n" ++
    "K - Kid\n" ++
    "R - Robot\n" ++
    "B - Robot carring a kid"

print_env :: Env -> IO ()
print_env (Env grid corral) = putStr nice_env
    where
        marked_corral = mark_corral (map show grid) corral
        colored_corral = color_corral (unlines marked_corral)
        line = "    " ++ replicate (length (grid !! 0) * 3 + 2) '-' ++ " \n"
        nice_env = line ++ colored_corral ++ line

print_plans :: Plans -> IO ()
print_plans (Plans []) = putStrLn ""
print_plans (Plans ((Robot has_kid pos, Plan goals) : plans)) = do
    putStrLn ("Robot " ++ show has_kid ++ " " ++ show pos ++ " -> " ++ show goals)
    print_plans (Plans plans)
