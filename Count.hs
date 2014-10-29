module Count where

import Control.Applicative (empty, pure, (<|>))
import qualified Data.Map as M
import Data.Monoid (Monoid, mappend, mempty)
import Data.Foldable (foldMap)
import Data.List (nub, union)
import Data.Function (on)

import Types
import Parse () -- FromNamedRecord Vote

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
    { tgovernor :: Count GCandidate
    , tprop1    :: Count GCandidate
    , tprop2    :: Count GCandidate
    , tprop45   :: Count GCandidate
    , tprop46   :: Count GCandidate
    , tprop47   :: Count GCandidate
    , tprop48   :: Count GCandidate
    , tlistvote :: ()
    , trankvote :: ()
    }

newtype Count a = Count { unCount :: M.Map a Int }

instance Ord a => Monoid (Count a) where
    mempty = Count M.empty
    mappend a b = Count $ M.unionWith (+) (unCount a) (unCount b)

mkCount :: Maybe a -> Count a
mkCount Nothing  = Count M.empty
mkCount (Just a) = Count $ M.singleton a 1

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
