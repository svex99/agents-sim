module Types where

data Elem = Empty | Obstacle | Dirt | Kid | Robot Bool (Int, Int) deriving (Eq, Ord)

data ShowElem = E | O | D | K | R | B deriving (Show) -- B means Robot is charging a Kid

instance Show Elem where
    show Empty = show E
    show Obstacle = show O
    show Dirt = show D
    show Kid = show K
    show (Robot kid _) = if kid then show B else show R

data Env = Env [[Elem]] [(Int, Int)] deriving (Show)