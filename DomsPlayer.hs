module DomsPlayer where

import DomsMatch
import Data.List
import Data.Tuple

type Tactic = Player -> Hand->DomBoard->Scores->(Dom, End)
type Strategy = Hand -> DomBoard -> Player -> Scores -> Tactic

--Returns opponent's hand
getOpponentHand :: Hand -> History -> Hand
getOpponentHand ha hi = [x | x <- domSet, (not (x `elem` (played++ha)))]
    where 
        (played, players, moveNums) = (unzip3 hi)
        
        
--PLAYERS
winPlayer :: DomsPlayer
winPlayer h b p s
    | canWin h b pScore = winGame p h b s
    | otherwise = playHsd p h b s
        where 
            (pScore, oScore) = if p == P1 then s else swap s 


winBlockPlayer :: DomsPlayer
winBlockPlayer h b p s
    | canWin h b pScore = winGame p h b s
    | opCanWin h b oScore = blockWin p h b s --No point checking at less
    | otherwise = playHsd p h b s
        where
            (pScore, oScore) = if p == P1 then s else swap s

winBlockHrdPlayer :: DomsPlayer
winBlockHrdPlayer h InitBoard p s = playHrd p h InitBoard s
winBlockHrdPlayer h b p s
    | canWin h b pScore = winGame p h b s
    | opCanWin h b oScore = blockWin p h b s--As above
    --If the opponent is more than 10 points ahead of me
    --try and play one that forces opponent to play a lower scoring value
    | (oScore - pScore) > 10 = playHrd p h b s
    | otherwise = playHsd p h b s
        where
            (pScore, oScore) = if p == P1 then s else swap s
 
blockOpPlayer :: DomsPlayer
blockOpPlayer h b p s
    | canWin h b pScore = winGame p h b s 
    | opCanWin h b oScore = blockWin p h b s
    | canBlock h b = blockOp p h b s
    | otherwise = playHsd p h b s
        where
            (pScore, oScore) = if p == P1 then s else swap s

-----SITUATIONAL TEST PREDICATES-----
{-canWin
--Returns:
---------: True if in the next round it is possible to win, or false.
--Takes: The player's hand of dominos.
-------: The current domino game board.
-------: The player's current score.-}
canWin :: Hand -> DomBoard -> Int -> Bool
canWin _ InitBoard _ = False --Can't win on your first go
canWin [] b s = False
canWin (d:doms) b s
    | (goesLP d b) && ((scoreDom d L b + s)== 61) = True
    | (goesRP d b) && ((scoreDom d R b + s)== 61) = True
    | otherwise = canWin doms b s
opCanWin :: Hand -> DomBoard -> Int -> Bool
opCanWin _ InitBoard _ = False--Above
opCanWin [] b os = False
opCanWin h b@(Board _  _ hi) os =
    let
        oh = getOpponentHand h hi
    in
        canWin oh b os
{-canBlock
--Returns:
---------: True if it is possible for the player to play a domino such that the opponent must knock.
--Takes: The player's hand of dominos.
-------: The current domino game board.-}    
canBlock :: Hand -> DomBoard -> Bool
canBlock h InitBoard = False
canBlock h b@(Board _ _ his)
    --make sure they can't play both sides, we can only block one!
    | ((rightdrops oh b) /= []) && ((leftdrops oh b) /= []) = False  
    | (rightdrops oh b) == [] = canBlockA h oh b L -- he can only `play on the left, check it!
    | (leftdrops oh b) == [] = canBlockA h oh b R
    | otherwise = False
        where 
            oh = getOpponentHand h his 
            lb = leftdrops h b
            rb = rightdrops h b
--Check if the player can block the opponent
canBlockA :: Hand -> Hand -> DomBoard -> End -> Bool
canBlockA [] _ _ _ = False
canBlockA (d:doms) oh b e
    | knocking oh pb = True
    | otherwise = canBlockA doms oh b e
        where 
            Just pb = playDom P1 d e b   -- Player doesn't matter
            

-----TACTICS-----
{-playHsd
--Notes: Chooses the highest scoring playable domino.-}
playHsd :: Tactic
playHsd _ h b _ 
    | sLS == []                       = (fst (head sRS), R) --If no left plays pick the first in the ordered right list
    | sRS == []                       = (fst (head sLS), L) --As above
    | snd (head sLS) > snd (head sRS) = (fst (head sLS), L) --Else compare the highest scoring left and right
    | otherwise                       = (fst (head sRS), R)
        where
            (lps, rps) = (leftdrops h b, rightdrops h b)
            --Get the scores for every play
            (scL, scR) = (map (\d->(d, scoreDom d L b)) lps,
                            map (\d->(d, scoreDom d R b)) rps)
            --Ensure highest scoring is first in the list
            (sLS, sRS) = (sortBy 
                            (\(d1, s1) (d2, s2) -> compare s2 s1) scL,
                          sortBy 
                            (\(d1, s1) (d2, s2) -> compare s2 s1) scR)

{-winGame
--Notes: Chooses the game-winning domino. Only to be used if canWin returns True.-}
winGame :: Tactic
winGame p (d:doms) b s
    --If the domino goes and playing it results in a winning score choose it
    | (goesLP d b) && (((scoreDom d L b) + pScore)== 61) = (d, L)
    | (goesRP d b) && (((scoreDom d R b) + pScore)== 61) = (d, R)
    | otherwise = winGame p doms b s
        where 
            (pScore, oScore) = if p == P1 then s else swap s
        
{-blockOp
--Notes: Chooses the domino that forces the opponent to knock on their turn. Only to be used if canBlock returns True.-}
blockOp :: Tactic
--Don't try and block if first drop
blockOp p h InitBoard s = playHsd p h InitBoard s
blockOp p h b@(Board _ _ his) s
    | (rightdrops oh b) == [] = blockOpA h oh b L -- he can play on the left, check it!
    | (leftdrops oh b) == [] = blockOpA h oh b R
        where 
            oh = getOpponentHand h his 
            lb = leftdrops h b
            rb = rightdrops h b
blockOpA :: Hand -> Hand -> DomBoard -> End -> (Dom, End)
blockOpA (d:doms) oh b e
    | knocking oh plb = (d, e)
    | otherwise = blockOpA doms oh b e
        where 
            Just plb = playDom P1 d e b   -- Player doesn't matter

           
                            
                            
{-playHrd
--Notes: Chooses the domino such that the ratio of (my score)/(opponents highest score on their turn) is highest.-}
playHrd :: Tactic
playHrd p h InitBoard s = playHsd p h InitBoard s 
playHrd _ h b@(Board _ _ hi) _
    | lsort == [] = (ttfst (head rsort), ttsnd (head rsort))
    | rsort == [] = (ttfst (head lsort), ttsnd (head lsort))
    | (ttthrd (head rsort)) > (ttthrd (head lsort)) = (ttfst (head rsort), ttsnd (head rsort))
    | otherwise = (ttfst (head lsort), ttsnd (head lsort))
    where
        oh = getOpponentHand h hi
        (ld, rd) = (leftdrops h b, rightdrops h b)
        (ls, rs) = (playHrdA ld oh b L, playHrdA rd oh b R)
        (lsort, rsort) = (sortBy (\(d1, e1, s1) (d2, e2, s2) -> compare s2 s1) ls,
                            sortBy (\(d1, e1, s1) (d2, e2, s2) -> compare s2 s1) rs)
playHrdA :: Hand -> Hand -> DomBoard -> End -> [(Dom, End, Float)]
playHrdA [] _ _ _ = []
--Not all of the opponent's hand may be playable OR Non may be playable!
playHrdA (d:doms) oh b@(Board _ _ hi) e 
    | olds == [] && ords == [] = (d, e, (myScore/0.5)):(playHrdA doms oh b e) --Opponent cannot play from this domino, the domino is worth more
    | olds == [] = (d, e, (myScore/orScore)):(playHrdA doms oh b e)--Opponent can only play on the right
    | otherwise = (d, e, (myScore/olScore)):(playHrdA doms oh b e)--Opponent can only play on the left
        where
            Just pb = playDom P1 d e b --It will go, player doesn't matter
            (olds, ords) = (leftdrops oh pb, rightdrops oh pb)
            (lhsds, rhsds) = (getHsdAndScore olds pb, getHsdAndScore ords pb)
            myScore = fromIntegral(scoreDom d e b)
            (olScore, orScore) = (fromIntegral(snd(lhsds)), fromIntegral(snd(rhsds)))        

--Returns the highest scoring domino, but as a tuple together with it's score                            
getHsdAndScore :: Hand -> DomBoard -> (Dom, Int)
getHsdAndScore h b 
    | sLS == []                       = (fst (head sRS), snd (head sRS))
    | sRS == []                       = (fst (head sLS), snd (head sLS))
    | snd (head sLS) > snd (head sRS) = (fst (head sLS), snd (head sLS))
    | otherwise                       = (fst (head sRS), snd (head sRS))
        where 
            --Get the scores for every play
            (lps, rps) = (leftdrops h b, rightdrops h b)
            (scL, scR) = (map (\d->(d, (scoreDom d L b))) lps,
                            map (\d->(d, (scoreDom d R b))) rps)
            (sLS, sRS) = (sortBy (\(d1, s1) (d2, s2) -> compare s2 s1) scL,
                            sortBy (\(d1, s1) (d2, s2) -> compare s2 s1) scR)
                            

{-blockWin
--Notes: Chooses the highest scoring domino such that on the next round the opponent cannot win. Or if this is not possible, the highest scoring domino-}                            
blockWin :: Tactic
blockWin p h InitBoard s = playHsd p h InitBoard s --Shouldn't be called at the start of the game
blockWin p h b@(Board _ _ hi) s
    | nonLosingL++nonLosingR == [] = playHsd p h b s
    | otherwise = playHsd p (nonLosingL++nonLosingR) b s
        where 
            (pScore, oScore) = if p == P1 then s else swap s
            oh = getOpponentHand h hi
            (lds, rds) = (leftdrops h b, rightdrops h b)
            nonLosingL = [x | x <- lds, (not (canWin oh (playleft p x b) oScore))] --playleft/right doesn't return 'maybe'
            nonLosingR = [x | x <- rds, (not (canWin oh (playright p x b) oScore))]
                                

--Three-tuple functions
ttthrd :: (a, b, c) -> c
ttthrd (_, _, c) = c
ttfst ::(a, b, c) -> a
ttfst (a, _, _) = a
ttsnd ::(a, b, c) -> b
ttsnd (_, b, _) = b           