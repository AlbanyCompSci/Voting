\begin{code}
module RankCount where

import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.List     (intercalate, sortBy)
import qualified Data.Map      as M
import           Data.Monoid   (Monoid, Sum(..), (<>), getSum, mappend, mempty)

import Types (RankVote(..), SBCandidate)
\end{code}

Definition of the `RankCount` newtype (`* -> *`) representing a vote or tally of votes for a slate of items (usually candidates) of type `a`. The underlying `Map` represents the relationship of an item/candidate key with the votes that are currently designated for them and a tally of votes representing how votes should have been designated in the absence of the item in the key (second, third choice etc.). A single vote will take the form of a singleton map for the first choice with a vote count of 1.0 and an alternative of the second choice (itself nesting the third choice and so on) (see `mayRankVoteToRankCount` for implementation of conversion to a single vote tally).
\begin{code}
newtype RankCount a = RC { unRC :: M.Map a (Sum Double,RankCount a) }
\end{code}

`RankCount` is a monoid, which allows for easy `fold`ing of structures containing votes into tallies. The instance differs from the standard `Map` monoid instance in that it uses monoid appending `unionWith` instead of a plain `union` (which discards data from duplicate keys).
\begin{code}
instance Ord a => Monoid (RankCount a) where
    mempty                = RC $ M.empty
    mappend (RC a) (RC b) = RC $ M.unionWith (<>) a b
\end{code}

A (hopefully) reasonable epsilon for float comparison. `Double`s have been chosen over `Rational`s for performance, but introduce the complication of float comparison.
\begin{code}
epsilon :: Double
epsilon = 0.001
\end{code}

Since there is not (to my knowledge) a reasonable way to maintain a running tally equivalent to a result for rank-choice voting, there `finalize` transforms a `RankCount` tally into a list of successful candidates for a given number of seats.
\begin{code}
finalize :: Ord a => Int -> RankCount a -> [a]
finalize seats tally
\end{code}
If there are enough seats for all candidates, then finalization is done, and the list of all candidates (the keys to the map) is the result.
\begin{code}
    | done      = M.keys $ unRC tally
\end{code}
If there are still too many candidates, but some have already won, their votes abover threshold (see below) must be reallocated first before eliminating any candidates.
\begin{code}
    | unredist  = finalize seats $ redistribute seats tally
\end{code}
Otherwise, the lowest performing candidate must be eliminated so the alternative votes (secondary choices) associated with them can be reallocated.
\begin{code}
    | otherwise = finalize seats $ eliminate tally
\end{code}
The definitions for the above conditions:
\begin{code}
      where
          done :: Bool
          done = M.size (unRC tally) <= seats
          unredist :: Bool
          unredist = M.size (M.filter (aboveThresh . getSum . fst) $ unRC tally) > 0
          aboveThresh :: Double -> Bool
          aboveThresh c = abs (c - (thresh seats tally)) > epsilon
\end{code}

The Droop quota ($\lparen\frac{\text{Total Valid Poll}}{\text{Seats} + 1}\rparen + 1$)
\begin{code}
thresh :: Int -> RankCount a -> Double
thresh seats tally = numVotes / (fromIntegral seats + 1) + 1
  where numVotes = getSum $ F.foldMap fst $ unRC tally
\end{code}

For some reason this isn't standard, however, it just provides a `Fractional` instance for all `Sum`s for `Fractional` types.
\begin{code}
instance Fractional a => Fractional (Sum a) where
    fromRational = Sum . fromRational
    recip        = Sum . recip . getSum
\end{code}

Partially redistribute votes for candidate who are over quota.
\begin{code}
-- TODO: ordering of candidates (redistribute highest first?)
redistribute :: Ord a => Int -> RankCount a -> RankCount a
\end{code}
Iterate over all candidates, proportionally reallocating excess votes if they are over quota.
\begin{code}
redistribute seats (RC votes) = M.foldrWithKey accum mempty votes
\end{code}
If (within float tolerance) a candidate is above quota:
\begin{code}
  where
      -- accum :: a -> (Sum Double,RankCount a) -> RankCount a -> RankCount a
      accum k (vs,alt) z = if abs (getSum $ vs - thresh') > epsilon
\end{code}
Scale the votes they are currently recieving down to the threshold, combine it with the excess proportion of the alternatives along with the accumulator tally.
\begin{code}
        then (scale (getSum $ (thresh' / vs)) $ RC $ M.singleton k (vs,alt))
          <> scale (getSum $ (vs - thresh') / vs) alt
          <> z
\end{code}
Otherwise, add all of the candidates votes back to the accumulator tally.
\begin{code}
        else (RC $ M.singleton k (vs,alt))
          <> z
      thresh' = Sum $ thresh seats $ RC votes
\end{code}

Multiply all votes (and their alternatives) by a scalar.
\begin{code}
scale :: Double -> RankCount a -> RankCount a
scale k (RC votes) = RC $ fmap (\(vs,alt) -> (Sum k * vs,scale k alt)) votes
\end{code}

Eliminate the lowest performing candidate and reallocate their votes.
\begin{code}
-- TODO: case of equal losers
eliminate :: Ord a => RankCount a -> RankCount a
\end{code}
Delete the loser and add the associated alternative votes to the top level of the map.
\begin{code}
eliminate (RC votes) = mappend alt $ RC $ M.delete loser votes
  where loser = fst losingEntry
        alt   = snd . snd $ losingEntry
        losingEntry = head $ sortBy (compare `on` (fst . snd)) $ M.toList votes
\end{code}

Convert a (possibly non-existent) ranked vote (`Maybe (RankVote a)`) into a tally. In hindsight, these two type could probably have been combine (which is one of the things I am trying in the `New` directory). This is done by ordering candidates, then nesting them(in reverse order) into a `RankCount` tally.
\begin{code}
mayRankVoteToRankCount :: Maybe (RankVote SBCandidate) -> RankCount SBCandidate
mayRankVoteToRankCount Nothing      = mempty
mayRankVoteToRankCount (Just tally) = foldr (\k z -> RC $ M.singleton k (Sum 1.0,z)) mempty
                                    $ reverse
                                    $ map fst
                                    $ sortBy (compare `on` snd)
                                    $ M.toList
                                    $ unRV tally
\end{code}

Ugly `Show` instance for RankCount, shows the list of successful candidates on seperate lines. The show should really not include the finalization step, but this eases integration into the simplistic frontend which assumes a simple `Show` instance (this is another one of the issues I am working on in the `New` directory).
\begin{code}
instance (Ord a, Show a) => Show (RankCount a) where
    show tally = intercalate "\n" $ map show $ finalize 3 tally
\end{code}
