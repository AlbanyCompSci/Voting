{-# LANGUAGE OverloadedStrings #-}

module Election (Ballot) where

import Data.Csv (FromNamedRecord, FromField, parseNamedRecord, parseField)

import Types (Nul, (:&:), ToRankMap)
import ListVote (CandidateListResponse, PropResponse)
import RankVote (RankResponse)

type Ballot =  Governor
           :&: Prop1
           :&: Prop2
           :&: Prop45
           :&: Prop46
           :&: Prop47
           :&: Prop48
           :&: ListSchoolBoard
           :&: RankSchoolBoard
           :&: Nul

data GubernatorialCandidate = Brown | Kashkari
instance FromNamedRecord (Response Governor) where
    fromNamedRecord r = parseField <$> r .: "Governor"
instance FromField Gubernatorial Candidate where
    parseField "Edmund G. \"Jerry\" Brown (Democrat)" = pure Brown
    parseField "Neel Kashkari (Republican)"           = pure Kashkari
    parseField _                                      = empty
instance Show Gubernatorial Candidate where
    show Brown    = "Edmund G. \"Jerry\" Brown (Democrat)"
    show Kashkari = "Neel Kashkari (Republican)"
instance Item Governor where
    Response Governor = CandidateListResponse GubernatorialCandidate
    title           _ = "Governor"
    finalize          = show

instance FromNamedRecord (Response Prop1) where
    fromNamedRecord r = parseField <$> r .: "Proposition 1"
instance Item Prop1 where
    Response Prop1 = PropResponse
    title        _ = "Proposition 1"
    finalize       = show

instance FromNamedRecord (Response Prop2) where
    fromNamedRecord r = parseField <$> r .: "Proposition 2"
instance Item Prop2 where
    Response Prop2 = PropResponse
    title        _ = "Proposition 2"
    finalize       = show

instance FromNamedRecord (Response Prop45) where
    fromNamedRecord r = parseField <$> r .: "Proposition 45"
instance Item Prop45 where
    Response Prop45 = PropResponse
    title         _ = "Proposition 45"
    finalize        = show

instance FromNamedRecord (Response Prop46) where
    fromNamedRecord r = parseField <$> r .: "Proposition 46"
instance Item Prop46 where
    Response Prop46 = PropResponse
    title         _ = "Proposition 46"
    finalize        = show

instance FromNamedRecord (Response Prop47) where
    fromNamedRecord r = parseField <$> r .: "Proposition 47"
instance Item Prop47 where
    Response Prop47 = PropResponse
    title         _ = "Proposition 47"
    finalize        = show

instance FromNamedRecord (Response Prop48) where
    fromNamedRecord r = parseField <$> r .: "Proposition 48"
instance Item Prop48 where
    Response Prop48 = PropResponse
    title         _ = "Proposition 48"
    finalize        = show

data SchoolBoardCandidate = Black | Chin | Gray | Blanchard
                          deriving (Eq, Ord, Enum, Bounded)
instance FromNamedRecord (Response ListSchoolBoard) where
    parseNamedRecord = parseField <$> r .: "School Board"
instance FromField SchoolBoardCandidate where
    parseField "Paul Black"          = pure Black
    parseField "Elliot Chin"         = pure Chin
    parseField "Ross Stapleton-Gray" = pure Gray
    parseField "Charles Blanchard"   = pure Blanchard
    parseField _                     = empty
instance Show SBCandidate where
    show Black     = "Paul Black"
    show Chin      = "Elliot Chin"
    show Gray      = "Ross Stapleton-Gray"
    show Blanchard = "Charles Blanchard"
instance Item ListSchoolBoard where
    Response ListSchoolBoard = CandidateListResponse SchoolBoardCandidate
    title                    = "School Board (Standard Voting)"
    finalize                 = show

instance ToRankMap SBCandidate where
    parseRank c r = show <$> r .: "School Board [" ++ show c ++ "]"
instance Item RankSchoolBoard where
    Response RankSchoolBoard = RankResponse SchoolBoardCandidate
    title                    = "School Board (Rank Voting)"
    finalize                 = intercalate "\n" . map show . resolve 3
