module Main where

import           Control.Applicative           ((<$>), (<*>))
import qualified Data.ByteString.Lazy          as BSL
import           Data.Csv                      (decodeByName)
import qualified Data.Foldable                 as F
import qualified Data.Map                      as M
import           Data.Maybe                    (fromJust)
import qualified Data.Vector                   as V

import Types    (VoteRecord(..))
import Election (Ballot)

main :: IO ()
main = do
    listFile <- BSL.readFile "List.csv"
    rankFile <- BSL.readFile "Rank.csv"
    case tally listFile rankFile of
         Left  s -> putStrLn $ "Error: " ++ s
         Right t -> putStrLn $ show t

tally :: BSL.ByteString -> BSL.ByteString -> Either String Tally
tally listFile rankFile = do
    listVotes <- getVotes listFile
    rankVotes <- getVotes rankFile
    let label = M.fromList . V.toList . fmap ((,) <$> username <*> id)
    return $ F.foldMap vote
           $ M.unionWith (fmap fromJust . reconcile)
             (label listVotes) (label rankVotes)

-- exists mostly to provide type signature
getVotes :: BSL.ByteString -> Either String (V.Vector (VoteRecord Ballot))
getVotes = fmap snd . decodeByName
