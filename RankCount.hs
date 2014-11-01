-- RankCount monoid for running tally on rank order voting ballots
module RankCount ( RankCount
    , mayRankVoteToRankCount
    , resolve
    ) where

import           Data.Function (on)
import           Data.List     (sortBy, intercalate)
import qualified Data.Map      as M
import           Data.Monoid   (Monoid, Sum(..), mappend, mempty, (<>))

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

resolve :: Ord a => Int -> RankCount a -> [a]
resolve n c
    | n >= length (M.keys $ unRC c) = M.keys $ unRC c
    | otherwise = resolve n $ redistribute (1 / fromIntegral n) c

redistribute :: Ord a => Double -> RankCount a -> RankCount a
redistribute thresh = M.foldrWithKey accum mempty . unRC . eliminate
    where accum k (c,a) z = if getSum c > thresh
                               then RankCount (M.singleton k (Sum thresh, mempty))
                                    <> scale (getSum c - thresh) a
                               else RankCount (M.singleton k (c,a))

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
