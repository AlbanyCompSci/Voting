{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE FlexibleInstances #-}

module Parse where

import           Control.Applicative    (pure, empty, (<$>), (<*>))
import           Control.Error.Safe     (assertMay)
import           Control.Monad          (join)
import           Data.ByteString.Char8  (unpack)
import qualified Data.ByteString.Lazy   as BS
import qualified Data.ByteString.Search as BSSearch
import           Data.Foldable          (foldMap)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Map               as Map
import           Data.Monoid            (All(All), getAll)
import           Data.String            (fromString)
import           Data.Time.Clock        (UTCTime)
import           Data.Time.Format       (parseTime)
import qualified Data.Traversable       as T
import qualified Data.Vector            as V
import           System.Locale          (defaultTimeLocale)
import           Safe                   (readMay)

import Data.Csv ( FromNamedRecord
                , FromField
                , Parser
                , parseNamedRecord
                , parseField
                , (.:)
                )

import Types ( Vote(..)
             , ListVote(..)
             , RankVote(..)
             , GCandidate(..)
             , SBCandidate(..)
             )

instance FromField GCandidate where
    parseField "Edmund G. \"Jerry\" Brown (Democrat)" = pure Brown
    parseField "Neel Kashkari (Republican)"           = pure Kashkari
    parseField _                                      = empty

instance FromField SBCandidate where
    parseField "Paul Black"          = pure Black
    parseField "Elliot Chin"         = pure Chin
    parseField "Ross Stapleton-Gray" = pure Gray
    parseField "Charles Blanchard"   = pure Blanchard
    parseField _                     = empty

instance FromField Bool where
    parseField "Yes" = pure True
    parseField "No"  = pure False
    parseField _     = empty

instance FromField UTCTime where
    parseField s = case readTime $ unpack s of
                        Just t  -> pure t
                        Nothing -> empty
        where readTime = parseTime defaultTimeLocale "%m/%d/%Y %k:%M:%S"

instance FromField a => FromField (ListVote a) where
    parseField = fmap ListVote
               . T.sequenceA
               . map parseField
               . BSSearch.split ", "

instance FromNamedRecord (Maybe (RankVote SBCandidate)) where
    parseNamedRecord r = if hasFields then pure <$> rankVote else pure empty
        where
            hasFields :: Bool
            hasFields = getAll $ foldMap All
                      $ map ((`elem` (HM.keys r)) . fromString) fields
            rankVote :: Parser (RankVote SBCandidate)
            rankVote = fmap (RankVote . Map.fromList . zip enumAll)
                     $ T.sequenceA
                     $ map ((r .:) . fromString) fields
            fields :: [String]
            fields = map (\c -> fromString $ "School Board [" ++ show c ++ "]")
                         (enumAll :: [SBCandidate])

instance FromField a => FromNamedRecord (Maybe (ListVote a)) where
    parseNamedRecord r = if ("School Board" `elem` HM.keys r)
                            then pure <$> (r .: "School Board")
                            else pure empty

instance FromNamedRecord Vote where
    parseNamedRecord r =  Vote
                      <$> r .: "Timestamp"
                      <*> r .: "Username"
                      <*> r .: "Governor"
                      <*> r .: "Proposition 1"
                      <*> r .: "Proposition 2"
                      <*> r .: "Proposition 45"
                      <*> r .: "Proposition 46"
                      <*> r .: "Proposition 47"
                      <*> r .: "Proposition 48"
                      <*> parseNamedRecord r
                      <*> parseNamedRecord r

enumAll :: (Enum a, Bounded a) => [a]
enumAll = enumFrom minBound
