{-
    Module: Minimax.

    *** PART I (60pt) and PART II (10pt) *** 
-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Players.Minimax where 

import Data.Maybe
import Data.Graph
import Data.Ord
import Data.Tree
import Data.List ( find, maximumBy, minimumBy, sortBy, delete, foldl' )
import Data.Array
import Data.Bifunctor

import Types
import Constants
import Cell
import Action
import Board 
import Player
import Game
import Players.Dumb (dumbAction)
import Debug.Trace


{-
    StateTree util.
-}

-- Map a function through the nodes of the tree.
mapStateTree :: (v -> w) -> StateTree v a -> StateTree w a
mapStateTree f (StateTree x ts) = StateTree (f x) [(a, mapStateTree f t) | (a, t)<-ts]

-- Calculate the depth of the tree (used to test pruneDepth).
stateTreeDepth :: StateTree v a -> Int 
stateTreeDepth (StateTree _ []) = 0
stateTreeDepth (StateTree _ ts) = 1 + (maximum (map (stateTreeDepth . snd) ts))

-- Calculate the breadth of the tree (used to test pruneBreadth).
stateTreeBreadth :: StateTree v a -> Int
stateTreeBreadth (StateTree _ []) = 0
stateTreeBreadth (StateTree _ ts) = max (length ts) (maximum (map (stateTreeBreadth . snd) ts))

{-
    Result util.
-}

-- Negating the result is simply negating the score. You may ignore this although it may be useful
-- to implement the minimax algorithm.
negResult :: Result -> Result
negResult (Result x as) = Result (-x) as

{- :
    *** Part I.a (10pt) ***

    First, we will generate a tree containing all the possible game states.
-}

-- Given a game, return a tree that encodes all the possible future game states.
-- [Hint: Use 'validActions' and 'performAction'.]
-- [Note: To speed things up, you may want to, at this stage, heuristically select which actions are 
--  more relevant. In particular, you probably don't want to consider every single possible wall.]
generateGameTree :: Game -> GameTree
generateGameTree v = StateTree v $ map (`format` v) $ validActions v

format :: Action -> Game -> (Action, GameTree)
format a v = (a, generateGameTree (fromJust (performAction v a)))

lengthS :: GameTree -> Int
lengthS (StateTree g s) = length s

wallToCell :: Wall -> [Cell]
wallToCell ((a,b),(c,d)) = [a, b, c, d]
{-
    *** PART I.b (5pt) ***

    Re-order the tree so that when traversed by the minimax algorithm, when it traverses the 
    branches at each node, finds either the higher scores or the lower scores first, depending on
    the depth of the tree.
-}

-- Higher scoring nodes go first.
-- [Hint: You should use 'lowFirst'.]
highFirst :: (Ord v) => StateTree v a -> StateTree v a
highFirst (StateTree v a) = StateTree v $ sortBy compareValue $ map (second lowFirst) a

-- Lower scoring nodes go first.
-- [Hint: You should use 'highFirst'.]
lowFirst :: (Ord v) => StateTree v a -> StateTree v a
lowFirst (StateTree v a) = StateTree v $ sortBy (flip compareValue) $ map (second lowFirst) a

compareValue :: Ord v => (a, StateTree v a) -> (a, StateTree v a) -> Ordering
compareValue (_, StateTree a _) (_, StateTree b _)
    | a == b = EQ
    | a <= b = GT
    | otherwise = LT

{-
    *** Part I.c (5pt) ***

    We don't want to look at all possible future game states as that would consume too much time and
    memory. Instead, we will only look a given number of steps into the future. Formally, the future
    game states are encoded in a tree, so we need a function that reduces the depth of a tree.
-}

-- Given a depth and a tree, return the same tree but cutting off the branches when the depth is 
-- exceeded. 
-- [Hint: You may want to use guards and recursion.]
pruneDepth :: Int -> StateTree v a -> StateTree v a
pruneDepth i (StateTree v a)
    | i > 0 = StateTree v $ map (second (pruneDepth (i - 1))) a
    | otherwise = StateTree v []

{-
    *** Part I.d (5pt) ***

    Similarly, we can also make our tree smaller by not considering all the possible game states at
    a given point. We need a function that reduces the breadth (or width) of a tree.
-}

-- Given a breadth (Int n) and a tree, return the same tree but only keeping the first n branches at
-- every node. 
-- [Hint: Use 'take'.]
pruneBreadth :: Int -> StateTree v a -> StateTree v a
pruneBreadth i (StateTree v a) =  StateTree v $ take i $ map (second (pruneBreadth i)) a

{-
    *** Part I.e (15pt) ***

    A crucial part of the minimax algorithm is defining a good utility function. It should measure
    how good a game position is for the current player. In our case, a game state should be better
    than another one if the player is closer to its winning positions.
-}

-- Assign a value to each game (from the point of view of the current player).
-- [Hint 1: You may want to calculate the distance between the player's current cell and its winning
--  positions.]
-- [Hint 2: One way would be to use 'reachableCells' repeatedly.]
utility :: Game -> Int
utility (Game b (p:ps)) = boardSize ^ 2 - search 0 b [] (winningPositions p) [(0, 0, currentCell p)] (currentCell p)

-- Utility for IV which takes into account the walls remaining, opponent distance and our distance from the winning positions.
betterUtility :: Game -> Int
betterUtility (Game b (p:ps)) =  
                wallsDifference ps
                + minimum (map (\x -> search 0 b (map currentCell ps ++ [currentCell p]) (winningPositions x) [(0, 0, currentCell x)] (currentCell x)) ps)
                - search 0 b (map currentCell ps) (winningPositions p) [(0, 0, currentCell p)] (currentCell p)

search :: Int -> Board -> [Cell] -> [Cell] -> [(Int, Int, Cell)] -> Cell -> Int
search i b v w h c
    | c `elem` w = i
    | null h = boardSize ^ 2 --no paths to winning pos in this case
    | otherwise = search (scnd smallestVal) b visited w (delete smallestVal newHold) (thrd smallestVal)
    where
        newHold :: [(Int, Int, Cell)]
        newHold = h ++ map (\ x -> (manhattan w x + i + 1, i + 1, x)) (filter (`notElem` visited) (reachableCells b c))

        visited :: [Cell]
        visited = v ++ [c]

        manhattan :: [Cell] -> Cell -> Int
        manhattan ((_, w):_) (_, c) = abs(w - c)

        smallestVal :: (Int, Int, Cell)
        smallestVal = minimumBy (comparing frst) newHold

        frst (x, _, _) = x
        scnd (_, y, _) = y
        thrd (_, _, z) = z

wallsDifference :: [Player] -> Int
wallsDifference [p] = totalWalls - remainingWalls p
wallsDifference (p:ps) = wallsDifference ps - remainingWalls p 

evalTree :: GameTree -> EvalTree
evalTree = mapStateTree utility


{-
    *** Part I.f (20pt) ***

    Finally, we ask you to implement the minimax algorithm. Given an evaluation tree, it should 
    return the a high scoring action (according to the minimax algorithm).
-}

-- Given an evaluation tree (it stores a score in the node and each branch is labelled with the 
-- action that leads to the next child) return a list of actions
-- [Hint 1: Use a helper function to keep track of the highest and lowest scores.]
-- [Hint 2: Use the 'Result' datatype.]

{-
The idea of the Result datatype is that you can use it to keep track of sequences of actions as you traverse your evaluation tree. 
Then you'll end up choosing the result with the "best" score according to minimax. And that will come with a list of actions. 
Then you have to decide which of those actions should be returned. Does that make sense?
-}

getResultActions :: Result -> [Action]
getResultActions (Result _ a) = a

minimaxFromTree :: EvalTree -> Action
minimaxFromTree s = head $ getResultActions $ minimaxFromTree' [] s

minimaxFromTree' :: [Action] -> EvalTree -> Result
minimaxFromTree' a (StateTree i []) = Result i a
minimaxFromTree' a (StateTree i as) = negResult $ minimum $ map (\(a', s) -> minimaxFromTree' (a ++ [a']) s) as

{-
    *** Part II (10pt) ***

    Extension of Part I.e, using alpha-beta pruning. You will need to change the 'minimax' function
    below to make it use this function.
-}

-- Same as above but now use alpha-beta pruning.
-- [Hint 1: Extend the helper function in I.e to keep track of alpha and beta.]
-- [Hint 2: Use the 'Result' datatype.]
minimaxABFromTree :: EvalTree -> Action
minimaxABFromTree s = last $ getResultActions $ minimaxABFromTree' [] (Result alpha []) (Result beta []) s
    where
        alpha = minBound :: Int
        beta = maxBound :: Int

minimaxABFromTree' :: [Action] -> Result -> Result  -> EvalTree -> Result
minimaxABFromTree' a al be (StateTree i []) = max al $ min be $ Result i a
minimaxABFromTree' a al be (StateTree i ((a', x):xs))
    | al' >= be = al'
    | null xs = al
    | otherwise = minimaxABFromTree' a al' be (StateTree i xs)
    where
        al' = negResult $ minimaxABFromTree' (a':a) (negResult be) (negResult al) x

{-
    Putting everything together.
-}

-- Given depth for pruning (should be even).
depth :: Int 
depth = 4

-- Given breadth for pruning.
breadth :: Int 
breadth = 10

-- Function that combines all the different parts implemented in Part I.
minimax :: Game -> Action
minimax =
      minimaxFromTree -- or 'minimaxABFromTree'
    . pruneBreadth breadth
    . lowFirst
    . evalTree
    . pruneDepth depth
    . generateGameTree
    
-- Given a game state, calls minimax and returns an action.
minimaxAction :: Board -> [Player] -> String -> Int -> Maybe Action
minimaxAction b ps _ r = let g = Game b ps in minimaxAction' g (minimax g)
    where 
        -- Goes through the list of actions until it finds a valid one. 
        minimaxAction' :: Game -> Action -> Maybe Action
        minimaxAction' g' (Move s)
            | validStepAction g' s = Just (Move s)
            | otherwise = error "Minimax chose an invalid action."
        minimaxAction' g' (Place w)
            | validWallAction g' w = Just (Place w)
            | otherwise = error "Minimax chose an invalid action."

-- Make minimaxPlayer in the usual way using 'minimaxAction'.
makeMinimaxPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeMinimaxPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = minimaxAction }