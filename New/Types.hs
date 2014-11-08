{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Types where

import           Control.Applicative (pure, (<$>), (<*>))
import           Data.Csv    (FromNamedRecord, Record, parseNamedRecord)
import qualified Data.Map    as M
import           Data.Monoid (Monoid, mappend, mempty, (<>))
import           Data.Time.Clock (UTCTime)

data VoteRecord a = VoteRecord
            { time     :: UTCTime
            , username :: String
            , vote     :: Vote a
            }

class Item i where
    type Response i :: *
    title       :: Response i -> String
    finalize    :: Response i -> String

data Nul
data a :&: b

data Vote :: * -> * where
    NewVote     :: Vote Nul
    AddResponse :: Response i -> Vote l -> Vote (i :&: l)

instance Monoid (Vote Nul) where
    mempty      = NewVote
    mappend _ _ = NewVote
instance (Monoid (Vote l), Monoid (Response i)) => Monoid (Vote (i :&: l)) where
    mempty = AddResponse mempty $ mempty
    mappend (AddResponse r1 l1) (AddResponse r2 l2) =
        AddResponse (r1 <> r2) (l1 <> l2)

instance FromNamedRecord (Vote Nul) where
    parseNamedRecord _ = pure NewVote
instance (FromNamedRecord (Vote l), FromNamedRecord (Response i)) => FromNamedRecord (Vote (i :&: l)) where
    parseNamedRecord r = AddResponse
                      <$> parseNamedRecord r
                      <*> parseNamedRecord r

class Reconcilable a where
    reconcile :: a -> a -> a
instance Reconcilable a => Reconcilable (VoteRecord a) where
    reconcile a b = if time a <= time b
                       then reconcile a b
                       else reconcile b a
instance Reconcilable (Vote Nul) where
    reconcile _ _ = NewVote
instance (Reconcilable (Vote l), Reconcilable (Response i)) => Reconcilable (Vote (i :&: l)) where
    reconcile (AddResponse r1 l1) (AddResponse r2 l2) =
        AddResponse (reconcile r1 r2) (reconcile l1 l2)

{-
instance Show (Vote Nul) where
    show _ = ""
instance (Show (Response i), Show (Vote l)) => Show (Vote (i :&: l)) where
    show (AddResponse r l) = title r ++ ":\n" ++ show r ++ "\n\n" ++ show l
-}

class Final a where
    final :: a -> String
instance Final (Vote Nul) where
    final _ = ""
instance (Final (Vote l), Final (Response i)) => Final (Vote (i :&: l)) where
    final (AddResponse r l) = title r ++ ":\n"
                           ++ finalize r
                           ++ "\n\n" ++ finalize l
{-
instance Final (Response i) where
    final = finalize
-}

class ToRankMap a where
    parseRank :: Record -> a -> Int
parseRanks :: (ToRankMap a, Enum a, Ord a) => Record -> M.Map a Int
parseRanks r = M.fromList $ zip allAs (map (parseRank r) $ allAs)
    where allAs = enumFrom minBound
