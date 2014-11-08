-- RankCount monoid for running tally on rank order voting ballots
module RankCount ( RankCount
    , mayRankVoteToRankCount
    , resolve
    ) where

import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.List     (sortBy, intercalate)
import qualified Data.Map      as M
import           Data.Monoid   (Monoid, Sum(..), mappend, mconcat, mempty, (<>))
import Debug.Trace (trace)

import Types

newtype RankCount a = RankCount { unRC :: M.Map a (Sum Double,RankCount a) }

-- TODO, figure out how to pass number of candidates to select
instance (Show a, Ord a) => Show (RankCount a) where
    show = intercalate "\n" . map show . resolve 3

instance Ord a => Monoid (RankCount a) where
    mempty      = RankCount M.empty
    mappend a b = RankCount $ M.unionWith (<>) (unRC a) (unRC b)

mayRankVoteToRankCount :: Maybe (RankVote SBCandidate) -> RankCount SBCandidate
mayRankVoteToRankCount Nothing  = RankCount M.empty
mayRankVoteToRankCount (Just v) =
    foldr (\a z -> RankCount $ M.singleton a (Sum 1,z)) mempty
    $ fst
    $ unzip
    $ reverse
    $ sortBy (compare `on` snd)
    $ M.toList
    $ unRV v

epsilon = 0.001 -- TODO: make non hard coded

resolve :: Ord a => Int -> RankCount a -> [a]
resolve seats c
    | M.size (unRC c) <= seats = trace "done" $ M.keys $ unRC c
    | unredist  = trace ("thresh:" ++ show thresh) $ resolve seats $ redistribute thresh c
    | otherwise = trace "otherwise" $ resolve seats $ eliminate c
      where unredist      = (> 0) . M.size . M.filter (aboveThresh . fst) $ unRC c
            aboveThresh s = abs (thresh - getSum s) > epsilon
            thresh        = trace ("numVotes: " ++ show numVotes) $ (numVotes / (fromIntegral seats + 1)) + 1
            numVotes      = getSum $ F.foldMap fst $ M.elems $ unRC c

redistribute :: Ord a => Double -> RankCount a -> RankCount a
redistribute thresh = M.foldrWithKey accum mempty . unRC
    where accum k (c,a) z = if abs (getSum c - thresh) > epsilon
                               then RankCount (M.singleton k (Sum thresh, a))
                                 <> scale (getSum c - thresh) a
                                 <> z
                               else RankCount (M.singleton k (c,a))
                                 <> z

eliminate :: Ord a => RankCount a -> RankCount a
eliminate c = if M.null $ unRC c
                 then c
                 else RankCount (M.delete (loser c) (unRC c))
                      <> snd (unRC c M.! loser c)
          -- TODO: case of two losers
    where loser = M.foldrWithKey accum arb . unRC
          accum k v z = if (fst v) < count z then k else z
          count k = fst $ (unRC c) M.! k
          arb = head $ M.keys $ unRC c

scale :: Double -> RankCount a -> RankCount a
scale x = RankCount . fmap (\(c,a) -> (Sum x * c,scale x a)) . unRC
