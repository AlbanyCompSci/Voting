{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE FlexibleInstances #-}

module Types
    ( Vote(..)
    , ListVote(..)
    , RankVote(..)
    , GCandidate(..)
    , SBCandidate(..)
    ) where

import qualified Data.Map               as Map
import           Data.Time.Clock        (UTCTime)
import           Data.Time.Format       () -- Show UTCTime

data Vote = Vote
    { time     :: UTCTime
    , username :: String
    , governor :: Maybe GCandidate
    , prop1    :: Maybe Bool
    , prop2    :: Maybe Bool
    , prop45   :: Maybe Bool
    , prop46   :: Maybe Bool
    , prop47   :: Maybe Bool
    , prop48   :: Maybe Bool
    , listvote :: Maybe ListVote
    , rankvote :: Maybe RankVote
    } deriving (Eq, Show)

newtype ListVote = ListVote [SBCandidate] deriving (Eq, Show)
newtype RankVote = RankVote (Map.Map SBCandidate (Maybe Int)) deriving (Eq, Show)

data GCandidate = Brown | Kashkari
                        deriving (Eq, Ord, Enum, Bounded, Show)
data SBCandidate = Black | Chin | Gray | Blanchard
                        deriving (Eq, Ord, Enum, Bounded)

instance Show SBCandidate where
    show Black     = "Paul Black"
    show Chin      = "Elliot Chin"
    show Gray      = "Ross Stapleton-Gray"
    show Blanchard = "Charles Blanchard"
