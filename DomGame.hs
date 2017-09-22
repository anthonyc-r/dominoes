{-Author: Anthony Cohn-Richardby
--Last Modified: 23/11/14-}
module DomGame where

import Dominoes
import System.Random
import Data.List

type DomsPlayer = Hand -> Board -> (Domino, End)

--Integer types used throughout to maintain compatibility with Dominoes.hs
----which also uses ints for returns and such, with the exception of seeds.
type Score = (Integer, String)

{-shuffDoms
--Returns:
---------: The list of all valid dominoes in a random order.
--Takes:
-------: A random number generator.-}
shuffleDoms :: StdGen -> [Domino]
shuffleDoms gen = let ints    = take 28 (randoms gen)::[Int]
                      domZip  = zip ints fullSet
                      domSort = sortBy (\(i1, d1) (i2, d2) -> compare i1 i2)
                                    domZip in
                  snd (unzip domSort)

{-playDomsRoundA
--Returns:
---------: The final score of a game between two given players, only to
---------: be used by playDomsRound
--Takes: Two DomsPlayers to play each other
-------: Two hands, one for each player
-------: The game board to play upon
-------: A tupple of each player's current scores.
--Notes:
-------: Updates game data if the player can play and calls itself with the
-------: next player's hand and player in the first positions for each. If
-------: the player cannot go, the above is done without updated data. If
-------: both players are knocking, the game ends and the function returns.-}
playDomsRoundA :: DomsPlayer -> DomsPlayer -> Hand -> Hand-> Board 
                    -> (Score, Score) -> (Score, Score) 
--Cases redundant due to knocking check, including to be safe...
playDomsRoundA pA pB [] [] b (sA, sB) = (sA, sB)
playDomsRoundA pA pB [] hB b (sA, sB) = playDomsRoundA pB pA hB [] b (sB, sA)
playDomsRoundA pA pB hA hB b (sA@(s, a), sB) 
    | not (knockingP hA b) = playDomsRoundA pB pA hB hA' b' (sB, sA')
    | not (knockingP hB b) = playDomsRoundA pB pA hB hA b (sB, sA)
    | otherwise            = (sA, sB)
--Updated values...
        where (drop, end) = pA hA b
              hA'         = delete drop hA
              Just b'     = playDom drop end b
              sA'         = (s + (scoreBoard b'), a)
                                        
{-playDomsRound
--Returns:
---------: The final score after conducting a round of dominoes between
---------: the two players.
--Takes:
-------: Two DomsPlayers to face each other, Player A and Player B.
-------: An Int seed to determine the shuffle order.
--Notes: The scores are tagged with A or B as their return order is
-------: not constant.-}
playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> (Score, Score)
playDomsRound pA pB seed = let hX = shuffleDoms (mkStdGen seed)
                               (hA, hB) = splitAt 14 hX
                               b = newBoard in
                           playDomsRoundA pA pB hA hB b ((0, "A"), (0, "B"))

{-simplePlayer
--Notes:
-------: Returns the first playable domino in hand.-}
simplePlayer :: DomsPlayer
simplePlayer h b
--Only called if both lists aren't empty so...
    | lps == [] = (head rps, RightEnd)
    | rps == [] = (head lps, LeftEnd)
    | otherwise = (head rps, RightEnd)
        where (lps, rps) = possPlays h b
 
{-domScores
--Returns: 
---------: A tupple of the given dominoes and their scores when played upon
---------: a given side of a board.
--Takes: 
-------: A list of dominoes that are known to be playable on the specified
-------: side.
-------: The end of the board to play the dominoes.
-------: The board to play upon.-}
domScores :: [Domino] -> End -> Board -> [(Domino, Integer)]
domScores [] end b = []
domScores (d:ds) end b  = let Just b' = playDom d end b in
                          (d, scoreBoard b'):(domScores ds end b)  

{-hsdPlayer
--Notes: Returns the highest scoring playable domino.-}
hsdPlayer :: DomsPlayer
hsdPlayer h b
    | sLS == []                       = (fst (head sRS), RightEnd)
    | sRS == []                       = (fst (head sLS), LeftEnd)
    | snd (head sLS) > snd (head sRS) = (fst (head sLS), LeftEnd)
    | otherwise                       = (fst (head sRS), RightEnd)
        where (lps, rps) = possPlays h b
			   --Get the scores for every play
              (scL, scR) = (domScores lps LeftEnd b,
                                domScores rps RightEnd b)
			   --Ensure highest scoring is first in the list
              (sLS, sRS) = (sortBy 
                                (\(d1, s1) (d2, s2) -> compare s2 s1) scL,
                            sortBy 
                                (\(d1, s1) (d2, s2) -> compare s2 s1) scR)
