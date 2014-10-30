module Count where

import Control.Applicative (empty, pure, (<|>), (<$>), (<*>))
import qualified Data.Map as M
import Data.Monoid (Monoid, mappend, mempty, (<>))
import Data.Foldable (foldMap)
import Data.List (nub, union)
import Data.Function (on)
import Data.Maybe (catMaybes)

import Types

reconcile :: Vote -> Vote -> Maybe Vote
reconcile a b
    | username a /= username b = empty
    | time a <= time b         = pure $ merge a b
    | otherwise                = pure $ merge b a
    where
        -- make a new vote with a time for the last submitted vote and merge
        -- all answers, with preference being given to the first vote
        merge :: Vote -> Vote -> Vote
        merge a b = Vote
                    (time b) -- the latter time
                    (username a) -- both usernames are the same (see above)
                    (governor a <|> governor b)
                    (prop1    a <|> prop1    b)
                    (prop2    a <|> prop2    b)
                    (prop45   a <|> prop45   b)
                    (prop46   a <|> prop46   b)
                    (prop47   a <|> prop47   b)
                    (prop48   a <|> prop48   b)
                    (listvote a <|> listvote b)
                    (rankvote a <|> rankvote b)

data Tally = Tally
    { tgovernor :: Count     GCandidate
    , tprop1    :: Count     Bool
    , tprop2    :: Count     Bool
    , tprop45   :: Count     Bool
    , tprop46   :: Count     Bool
    , tprop47   :: Count     Bool
    , tprop48   :: Count     Bool
    , tlistvote :: Count     SBCandidate
    , trankvote :: RankCount SBCandidate
    }

newtype Count a = Count { unCount :: M.Map a Int }

instance Ord a => Monoid (Count a) where
    mempty = Count M.empty
    mappend a b = Count $ M.unionWith (+) (unCount a) (unCount b)

mayToCount :: Maybe a -> Count a
mayToCount Nothing  = Count M.empty
mayToCount (Just a) = Count $ M.singleton a 1

mayListVoteToCount :: Maybe ListVote -> Count SBCandidate
mayListVoteToCount Nothing              = Count M.empty
mayListVoteToCount (Just (ListVote [])) = Count M.empty
mayListVoteToCount (Just (ListVote vs)) = Count $ M.fromList
                                        $ zip vs (repeat 1)

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
winner = map fst . unMaxKV . foldMap (MaxKV . return) . M.toList . unCount

data RankCount a = RankCount a

instance Monoid (RankCount a) where
    mempty = undefined
    mappend = undefined

mayRankVoteToRankCount :: Maybe RankVote -> RankCount SBCandidate
mayRankVoteToRankCount = undefined

instance Monoid Tally where
    mempty  = Tally mempty mempty mempty mempty mempty
                    mempty mempty mempty mempty
    mappend a b = Tally 
                  (tgovernor a <> tgovernor b)
                  (tprop1    a <> tprop1    b)
                  (tprop2    a <> tprop2    b)
                  (tprop45   a <> tprop45   b)
                  (tprop46   a <> tprop46   b)
                  (tprop47   a <> tprop47   b)
                  (tprop48   a <> tprop48   b)
                  (tlistvote a <> tlistvote b)
                  (trankvote a <> trankvote b)

voteToTally :: Vote -> Tally
voteToTally v =  Tally
                 (mayToCount             $ governor v)
                 (mayToCount             $ prop1    v)
                 (mayToCount             $ prop2    v)
                 (mayToCount             $ prop45   v)
                 (mayToCount             $ prop46   v)
                 (mayToCount             $ prop47   v)
                 (mayToCount             $ prop48   v)
                 (mayListVoteToCount     $ listvote v)
                 (mayRankVoteToRankCount $ rankvote v)
