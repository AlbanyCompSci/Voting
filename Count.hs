module Count
    ( Count
    , mayToCount
    , mayListVoteToCount
    --, winner
    ) where

import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.List     (intercalate, nub, union)
import qualified Data.Map      as M
import           Data.Monoid   (Monoid, mappend, mempty)

import Types

newtype Count a = Count { unCount :: M.Map a Int }

instance Show a => Show (Count a) where
    show (Count m) = intercalate "\n"
                   $ map (\(k,v) -> show k ++ ": " ++ show v)
                   $ M.toList m

instance Ord a => Monoid (Count a) where
    mempty = Count M.empty
    mappend a b = Count $ M.unionWith (+) (unCount a) (unCount b)

mayToCount :: Maybe a -> Count a
mayToCount Nothing  = Count M.empty
mayToCount (Just a) = Count $ M.singleton a 1

mayListVoteToCount :: Maybe (ListVote SBCandidate) -> Count SBCandidate
mayListVoteToCount Nothing              = Count M.empty
mayListVoteToCount (Just (ListVote [])) = Count M.empty
mayListVoteToCount (Just (ListVote vs)) = Count $ M.fromList
                                        $ zip vs (repeat 1)

{-
newtype MaxKV k v = MaxKV { unMaxKV :: [(k,v)] }

instance (Eq k, Ord v) => Monoid (MaxKV k v) where
    mempty = MaxKV []
    mappend a b = MaxKV . nub . maxsBy (compare `on` snd)
                $ union (unMaxKV a) (unMaxKV b)

maxsBy :: (a -> a -> Ordering) -> [a] -> [a]
maxsBy f = foldr accum []
  where accum a [] = [a]
        accum a bs = case f a (head bs) of
                          LT -> bs
                          EQ -> a : bs
                          GT -> [a]

winner :: Eq a => Count a -> [a]
winner = map fst . unMaxKV . F.foldMap (MaxKV . return) . M.toList . unCount
-}
