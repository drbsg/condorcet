{-# LANGUAGE FlexibleContexts #-}
-- Condorcet voting.
-- Copyright (C) 2005 Evan Martin <martine@danga.com>
module Condorcet(Candidate, Ballot, run) where

import GHC.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unsafe

-- | Candidates are represented as integers.
type Candidate = Int
{-| Ballots are a ranking of candidates.

Ballots are lists, where each element is a list of candidates the rank
the same.  Earlier entries in the ballot list are ranked higher.

E.g., this ballot:

> [ [1,3], [4], [2] ]

Means that 1 and 3 are tied for first, outranking 4, and everyone beats 2.
-}
type Ballot = [[Candidate]]

-- Entry (i,j) having value k means that i beats j in k votes.
type VoteArray = UArray (Int,Int) Int
type STVoteArray s = STUArray s (Int,Int) Int

loadVotes :: STVoteArray s -> [[Candidate]] -> ST s ()
loadVotes _ []        = return ()
loadVotes vs (cs:rest) = do
  loadVoteSet cs rest  -- each candidate at this ranks beat the ones below
  loadVotes vs rest    -- and recurse on lower ranks
    where
    -- loadVoteSet cs rest: store that each candidate in cs beat each in rest.
    loadVoteSet ds rst =
      mapM_ loadVote [(a,b) | a <- ds, b <- concat rst]

    -- loadVote (a,b): store that a beat b in the vote matrix.
    loadVote pair = do
      v <- readArray vs pair
      writeArray vs pair (v+1)

dim :: VoteArray -> [Int]
dim vs = [minim..maxim] where ((minim, _), (maxim, _)) = bounds vs

initPaths :: VoteArray -> STVoteArray s -> ST s ()
initPaths vs paths = mapM_ writeDelta (indices vs) where
    writeDelta (i,j) = writeArray paths (i,j) (max 0 (vs!(i,j) - vs!(j,i)))

floyd :: VoteArray -> STVoteArray s -> ST s ()
floyd vs paths =
  mapM_ update [(i,j,k) | i<-ranges, j<-ranges, i/=j, k<-ranges, i/=k, j/=k] where
    ranges = dim vs
    update (i,j,k) = do
      a <- readArray paths (j,i)
      b <- readArray paths (i,k)
      let s = min a b
      cur <- readArray paths (j,k)
      if cur < s
        then writeArray paths (j,k) s
        else return ()

strongPath :: VoteArray -> ST s VoteArray
strongPath vs = do
  paths <- thaw vs    -- make a copy of vs
  initPaths vs paths  -- load delta votes into paths
  floyd vs paths      -- run floyd over paths
  unsafeFreeze paths  -- and return paths

winners :: VoteArray -> [Candidate]
winners paths = filter isWinner candidates where
  isWinner c  = (c `beats`) `all` candidates
  i `beats` j = paths!(i,j) >= paths!(j,i)
  candidates  = dim paths

-- join :: String -> [String] -> String
-- join sep []       = ""
-- join sep [a]      = a
-- join sep (a:b:as) = a ++ sep ++ (join sep (b:as))

-- showVA :: VoteArray -> String
-- showVA vs = join "\n" (map showRow positions) where
--     showRow y = join ", " (map (\x -> show $ vs ! (x,y)) positions)
--     positions = dim vs

-- loadBallots :: [Ballot] -> ST s (STVoteArray s)
-- loadBallots ballots = do
--   let size = maximum $ concat $ concat ballots
--   votes <- newArray ((1,1),(size,size)) 0
--   mapM_ (loadVotes votes) ballots
--   return votes

-- | 'run' runs the process, taking a list of 'Ballot's and returning a
-- list of winning candidates.
run :: [Ballot]     -- ^ A list of ballots
    -> [Candidate]  -- ^ The winning candidates
run ballots = runST realrun where
  realrun = do
    let size = maximum $ concat $ concat ballots
    votes <- newArray ((1,1),(size,size)) 0
    mapM_ (loadVotes votes) ballots
    fvotes <- unsafeFreeze votes
    paths <- strongPath fvotes
    return $ winners paths

-- vim: set ts=2 sw=2 et :
