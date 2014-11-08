-- RR monoid for running tally on rank order voting ballots
module RankVote ( RankResponse ) where

import           Data.Function (on)
import           Data.List     (sortBy, intercalate)
import qualified Data.Map      as M
import           Data.Monoid   (Monoid, Sum(..), mappend, mempty, (<>))

import Types

newtype RankResponse a = RR { unRR :: M.Map a (Sum Double,RankResponse a) }

instance Ord a => Monoid (RankResponse a) where
    mempty      = RR M.empty
    mappend a b = RR $ M.unionWith (<>) (unRR a) (unRC b)

instance ToRankMap a => FromNamedRecord (RankResponse a) where
    parseNamedRecord r = foldr (\a z -> RankCount $ M.singleton a (Sum 1,z)) mempty
                       $ fst $ unzip
                       $ reverse
                       $ sortBy (compare `on` snd)
                       $ M.fromList $ parseRanks r

instance Reconcilable (RankResponse a) where
    reconcile a b = if M.null (unRR a) then b else a

resolve :: Ord a => Int -> RankResponse a -> [a]
resolve n c
    | n >= length (M.keys $ unRR c) = M.keys $ unRC c
    | otherwise = resolve n $ redistribute (1 / fromIntegral n) c

redistribute :: Ord a => Double -> RankResponse a -> RankResponse a
redistribute thresh = M.foldrWithKey accum mempty . unRR . eliminate
    where accum k (c,a) z = if getSum c > thresh
                               then RR (M.singleton k (Sum thresh, mempty))
                                    <> scale (getSum c - thresh) a
                               else RR (M.singleton k (c,a))

eliminate :: Ord a => RankResponse a -> RankResponse a
eliminate c = if M.null $ unRR c
                 then c
                 else RR (M.delete (loser c) (unRR c))
                      <> snd (unRR c M.! loser c)
          -- TODO: case of two losers
    where loser = M.foldrWithKey accum arb . unRR
          accum k v z = if (fst v) < count z then k else z
          count k = fst $ (unRR c) M.! k
          arb = head $ M.keys $ unRR c

scale :: Double -> RankResponse a -> RankResponse a
scale x = RR . fmap (\(c,a) -> (Sum x * c,scale x a)) . unRR
