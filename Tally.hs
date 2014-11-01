module Tally where

import Control.Applicative (empty, pure, (<|>), (<$>), (<*>))
import qualified Data.Map as M
import Data.Monoid (Monoid, mappend, mempty, (<>))
import Data.Foldable (foldMap)
import Data.List (nub, union)
import Data.Function (on)
import Data.Maybe (catMaybes)

import Types
import Count
import RankCount

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

instance Show Tally where
    show t = "Tally:\n" ++
             "======\n" ++
             "Governor:\n" ++
             "---------\n"
             ++ show (tgovernor t) ++ "\n\n" ++
             "Proposition 1:\n" ++
             "--------------\n"
             ++ show (tprop1    t) ++ "\n\n" ++
             "Proposition 2:\n" ++
             "--------------\n"
             ++ show (tprop2    t) ++ "\n\n" ++
             "Proposition 45:\n" ++
             "---------------\n"
             ++ show (tprop45   t) ++ "\n\n" ++
             "Proposition 46:\n" ++
             "---------------\n"
             ++ show (tprop46   t) ++ "\n\n" ++
             "Proposition 47:\n" ++
             "---------------\n"
             ++ show (tprop47   t) ++ "\n\n" ++
             "Proposition 48:\n" ++
             "---------------\n"
             ++ show (tprop48   t) ++ "\n\n" ++
             "School Board (List Voting):\n" ++
             "---------------------------\n"
             ++ show (tlistvote t) ++ "\n\n" ++
             "School Board (Rank Voting):\n" ++
             "---------------------------\n"
             ++ show (trankvote t)

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
