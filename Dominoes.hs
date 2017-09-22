{-Author: Anthony Cohn-Richardby
--Last Modified: 22/10/14.-}

module Dominoes where

----DATA STRUCTURES----
type Domino = (Integer, Integer)
type Board =[Domino]
type Hand = [Domino]
--deriving (Show) so it's printed out in scoreN.
data End = LeftEnd | RightEnd deriving (Show)

----DOMINO FUNCTIONS----
newDomino :: Integer -> Integer -> Domino
newDomino a b = (a, b)

rotDomino :: Domino -> Domino
rotDomino (a, b) = (b, a)

--All the permutations of a <- 0..6, b <- 0..6.
fullSet :: [Domino]
fullSet = [(a, b) | a <- [0..6], b <- [0..a]]

----BOARD FUNCTIONS----
newBoard :: Board
newBoard = []

----HAND FUNCTIONS----
newHand :: Hand
newHand = []

----GAME FUNCTIONS----
{-goesP
--Returns:
---------: True if the domino may be placed at this position, or false.
--Takes: The domino to, potentially, be played.
-------: The end of the board to check for success.
-------: The board on which to check for success.-}
goesP :: Domino -> End -> Board -> Bool
--Any domino can be played on the empty board.
goesP _ _ [] = True                  
--Compare both sides of played domino against num at end.
goesP (l, r) LeftEnd (dom:doms)
    | l == fst dom   = True
    | r == fst dom   = True
    | otherwise      = False
goesP (l, r) RightEnd doms
    | l == snd (last doms) = True
    | r == snd (last doms) = True
    | otherwise            = False

{-knockingP
--Returns: 
---------: True if there are no dominoes in the hand that can be played 
---------: upon the board, or false.
--Takes:
-------: A hand of dominoes to be checked.
-------: The board on which to check for plays.-}
knockingP :: Hand -> Board -> Bool
--Can't play with no dominoes.
knockingP [] _ = True
--Recursively check doms in hand via goesP.
--If goesP returns true for LEnd or REnd, can't knock.
knockingP (dom:doms) board 
    | goesP dom LeftEnd board      = False
    | goesP dom RightEnd board     = False
    | otherwise                    = knockingP doms board 

{-playedP
--Returns: 
---------: True if a given domino has been played on a given board, or
---------: False.
--Takes: The domino to check.
-------: The board of dominoes to be checked.-}
playedP :: Domino -> Board -> Bool
--Basic case, board is empty thus nothing has been played.
playedP dom []                  = False
--Run through the dominoes played.
--If the domino equals any played(Either way around) returns true.
playedP dom (bDom:bDoms)
    | dom == bDom               = True
    | (rotDomino dom) == bDom   = True
    | otherwise                 = playedP dom bDoms

{-possPlaysA
--Returns: 
---------: The set of dominoes in a hand that may be played on a single
---------: end of a board, to be used by possPlays.
--Takes: 
-------: A hand of dominoes to check for possible plays on a certain end.
-------: The end of the board that you wish to check.
-------: The board you wish to check.-}
possPlaysA :: Hand -> End -> Board -> [Domino]
--No possible plays if hand is empty.
possPlaysA [] _ board = []
--Recursively run though Hand, add element of hand to list if playable.
possPlaysA (dom:doms) end board =
    if goesP dom end board
        then dom:(possPlaysA doms end board)
        else possPlaysA doms end board
{-possPlays
--Returns: 
---------: All the possible dominoes in a hand, that may be played on a
---------: given board, in the form of a tuple, the first element contains
---------: dominoes that may be played on the left, the second contains 
---------: dominoes that may be played on the right.
--Takes: 
-------: The hand to check for possible plays.
-------: The board being potentially played on. -}
possPlays :: Hand -> Board -> ([Domino], [Domino])
possPlays hand board = (possPlaysA hand LeftEnd board, 
                        possPlaysA hand RightEnd board)

{-playDom
--Returns: 
---------: The updated board with the played domino if the play is valid, 
---------: or Nothing.
--Takes:
-------: The domino to be played.
-------: The end of the board the domino is to be played upon.
-------: The board on which the domino is being played. -}
playDom :: Domino -> End -> Board -> Maybe Board
--No rules as to the first played domino...
playDom domino _ [] = Just [domino]
--Split into case of LeftEnd and RightEnd due to differences in operations.
playDom domino@(a, b) LeftEnd board@(dom:doms)
    | b == fst dom                 = Just (domino:board)
    | a == fst dom                 = Just ((rotDomino domino):board)
    | otherwise                    = Nothing
playDom domino@(a, b) RightEnd board
    | a == snd lastDom              = Just (board++[domino])
    | b == snd lastDom              = Just (board++[(rotDomino domino)])
    | otherwise                     = Nothing
        where lastDom = last board

{-scoreBoardA
--Returns: 
---------: The value of a specified end of a board according to the rules
---------: to the rules of 3s and 5s, to be used in scoreBoard.
--Takes: 
-------: The board to check an end of.
-------: The end to get the value of.-}
scoreBoardA :: Board -> End -> Integer
scoreBoardA [(a, b)] LeftEnd = a
scoreBoardA [(a, b)] RightEnd = b
scoreBoardA ((a, b):doms) LeftEnd = if a == b then 2*a else a
scoreBoardA board RightEnd = let (a, b) = (last board) 
                             in if a == b then 2*b else b
{-scoreBoard
--Returns: 
---------: The 3s and 5s score for a certain board of dominoes.
--Takes: 
-------: The board of dominoes to check.-}
scoreBoard :: Board -> Integer
scoreBoard board
--Infix (quot) to do integer division to convert a value to a score.
    | totalScore `mod` 15 == 0 = (totalScore `quot` 5)+(totalScore `quot` 3)
    | totalScore `mod` 5 == 0  = totalScore `quot` 5
    | totalScore `mod` 3 == 0  = totalScore `quot` 3
    | otherwise                = 0
        where leftScore  = (scoreBoardA board LeftEnd)
              rightScore = (scoreBoardA board RightEnd)
              totalScore = leftScore+rightScore

{-scoreNA
--Returns: 
---------: A subset of the given domino list that when played at a given 
---------: end, results in the given score target. Intended to be used by
---------: the full scoreN function.
--Takes:
-------: A list of dominoes to check for validity.
-------: The end which the dominoes are to be checked against.
-------: The board on which to check.
-------: The target score to make.-}
scoreNA :: [Domino] -> End -> Board -> Integer -> [Domino]
--Basic case
scoreNA [] end board tar = []
scoreNA (dom:doms) end board tar
--Check the score of the resulting board recursively over the list of 
----dominoes, if it's the target, add to return list.
    | scoreBoard newBoard == tar = dom : (scoreNA doms end board tar)
    | otherwise                  = scoreNA doms end board tar
        where Just newBoard = playDom dom end board 

{-scoreN
--Returns: 
---------: A subset of the standard set of dominoes such that when played 
---------: on a given board, results in a new board that would score the 
---------: given target. Returns each domino in a tuple with the end that 
---------: it may be played on.
--Takes:
-------: The board on which the tile is to be placed.
-------: The target score that is to be made.-}
scoreN :: Board -> Integer -> [(Domino, End)]
--Zip the results from the aux function with the End to be played on and
----append for a single output
scoreN board tar = (zip tarLeft leftCycle) ++
                    (zip tarRight rightCycle)
--Generate the set of valid unplayed tiles (fullSet defined under 
----DOMINO FUNCTIONS) and run it though possPlays ready to be passed
----to the aux function
        where notPlayed  = [notPlayed | notPlayed <- fullSet, 
                                        not (playedP notPlayed board)]
              playable   = possPlays notPlayed board -- ([left], [right])
              left       = fst playable              -- [left]
              right      = snd playable              -- [right]
              tarLeft    = scoreNA left LeftEnd board tar
              tarRight   = scoreNA right RightEnd board tar
              leftCycle  = cycle [LeftEnd]
              rightCycle = cycle [RightEnd]

