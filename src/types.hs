module Types where

data Elem =
    Empty
    | Obstacle
    | Dirt
    | Kid
    | Robot Bool (Int, Int)
    | MultiElem (Elem, Elem)
    deriving (Eq, Ord)

data ShowElem =
    E       -- for empty box (corral)
    | O     -- for osbtacle
    | D     -- for dirt
    | K     -- for kid (corral)
    | R     -- for Robot with hands empty (corral)
    | B     -- for Robot carring a kid (corral)
    | L     -- for Robot carring a kid near a Dirt
    | M     -- for Robot with hands empty near a Dirt
    | N     -- for Robot near a Kid (corral)
    deriving (Show) -- B means Robot is charging a Kid

instance Show Elem where
    show Empty = show E
    show Obstacle = show O
    show Dirt = show D
    show Kid = show K
    show (Robot kid _) = if kid then show B else show R
    show (MultiElem ((Robot has_kid _), Dirt)) = if has_kid then show L else show M 
    show (MultiElem (_, Kid)) = show N

data Env = Env [[Elem]] [(Int, Int)] deriving (Show)
