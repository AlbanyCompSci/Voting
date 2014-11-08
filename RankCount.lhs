\begin{code}
-- RankCount monoid for running tally on rank order voting ballots
module RankCount where

import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.List     (intercalate, sortBy)
import qualified Data.Map      as M
import           Data.Monoid   (Monoid, Sum(..), (<>), getSum, mappend, mempty)

import Types (RankVote(..), SBCandidate)

newtype RankCount a = RC { unRC :: M.Map a (Sum Double,RankCount a) }

instance Ord a => Monoid (RankCount a) where
    mempty                = RC $ M.empty
    mappend (RC a) (RC b) = RC $ M.unionWith (<>) a b

epsilon :: Double
epsilon = 0.001

finalize :: Ord a => Int -> RankCount a -> [a]
finalize seats tally
    | done      = M.keys $ unRC tally
    | unredist  = finalize seats $ redistribute seats tally
    | otherwise = finalize seats $ eliminate tally
      where
          done :: Bool
          done = M.size (unRC tally) <= seats
          unredist :: Bool
          unredist = M.size (M.filter (aboveThresh . getSum . fst) $ unRC tally) > 0
          aboveThresh :: Double -> Bool
          aboveThresh c = abs (c - (thresh seats tally)) > epsilon

thresh :: Int -> RankCount a -> Double
thresh seats tally = numVotes / (fromIntegral seats + 1) + 1
  where numVotes = getSum $ F.foldMap fst $ unRC tally

instance Fractional a => Fractional (Sum a) where
    fromRational = Sum . fromRational
    recip        = Sum . recip . getSum

-- TODO: ordering of candidates (redistribute highest first?)
redistribute :: Ord a => Int -> RankCount a -> RankCount a
redistribute seats (RC votes) = M.foldrWithKey accum mempty votes
  where
      -- accum :: a -> (Sum Double,RankCount a) -> RankCount a -> RankCount a
      accum k (vs,alt) z = if abs (getSum $ vs - thresh') > epsilon
        then (scale (getSum $ (thresh' / vs)) $ RC $ M.singleton k (vs,alt))
          <> scale (getSum $ (vs - thresh') / vs) alt
          <> z
        else (RC $ M.singleton k (vs,alt))
          <> z
      thresh' = Sum $ thresh seats $ RC votes

scale :: Double -> RankCount a -> RankCount a
scale k (RC votes) = RC $ fmap (\(vs,alt) -> (Sum k * vs,scale k alt)) votes

eliminate :: Ord a => RankCount a -> RankCount a
eliminate (RC votes) = RC $ M.delete loser votes
  where loser = fst $ head $ sortBy (compare `on` (fst . snd)) $ M.toList votes
        -- TODO: case of equal losers

mayRankVoteToRankCount :: Maybe (RankVote SBCandidate) -> RankCount SBCandidate
mayRankVoteToRankCount Nothing      = mempty
mayRankVoteToRankCount (Just tally) = foldr (\k z -> RC $ M.singleton k (Sum 1.0,z)) mempty
                                    $ reverse
                                    $ map fst
                                    $ sortBy (compare `on` snd)
                                    $ M.toList
                                    $ unRV tally

instance (Ord a, Show a) => Show (RankCount a) where
    show tally = intercalate "\n" $ map show $ finalize 3 tally
\end{code}
