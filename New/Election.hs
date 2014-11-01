module Election (Ballot) where

import Types (Nul, (':))
import ListVote (CandidateListResponse, PropResponse)
import RankVote (RankResponse)

type Ballot = Governor
           ': Prop1
           ': Prop2
           ': Prop45
           ': Prop46
           ': Prop47
           ': Prop48
           ': ListSchoolBoard
           ': RankSchoolBoard
           ': Nul

data GubernatorialCandidate = Brown | Kashkari
instance Read Gubernatorial Candidate where
    readSPrec = undefined
instance Show Gubernatorial Candidate where
    show = undefined
instance Item Governor where
    Response Governor = CandidateListResponse GubernatorialCandidate
    title           _ = "Governor"
    description     _ = undefined

instance Item Prop1 where
    Response Prop1 = PropResponse
    title        _ = "Proposition 1"
    description  _ = undefined

instance Item Prop2 where
    Response Prop2 = PropResponse
    title        _ = "Proposition 2"
    description  _ = undefined

instance Item Prop45 where
    Response Prop45 = PropResponse
    title         _ = "Proposition 45"
    description   _ = undefined

instance Item Prop46 where
    Response Prop46 = PropResponse
    title         _ = "Proposition 46"
    description   _ = undefined

instance Item Prop47 where
    Response Prop47 = PropResponse
    title         _ = "Proposition 47"
    description   _ = undefined

instance Item Prop48 where
    Response Prop48 = PropResponse
    title         _ = "Proposition 48"
    description   _ = undefined

data SchoolBoardCandidate = Black | Chin | Grey | Blanchard
instance Read SchoolBoardCandidate where
    readSPrec = undefined
instance Show SchoolBoardCandidate where
    show = undefined
instance Item ListSchoolBoard where
    Response ListSchoolBoard = CandidateListResponse SchoolBoardCandidate
    title                    = "School Board (Standard Voting)"
    description              = undefined

instance Fielded SchoolBoardCandidate where
    toField = undefined
instance Item RankSchoolBoard where
    Response RankSchoolBoard = RankResponse SchoolBoardCandidate
    title                    = "School Board (Rank Voting)"
    description              = undefined
