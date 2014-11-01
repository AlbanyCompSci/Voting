module Main where

import           Control.Applicative           ((<$>), (<*>))
import qualified Data.ByteString.Lazy          as BSL
import           Data.Csv                      (decodeByName)
import qualified Data.Foldable                 as F
import qualified Data.Map                      as M
import           Data.Maybe                    (fromJust)
import qualified Data.Vector                   as V

import Tally
import Types
import Parse

main :: IO ()
main = do
    listFile <- BSL.readFile "testList.csv"
    rankFile <- BSL.readFile "testRank.csv"
    case tally listFile rankFile of
         Left  s -> putStrLn $ "Error: " ++ s
         Right t -> putStrLn $ show t

tally :: BSL.ByteString -> BSL.ByteString -> Either String Tally
tally listFile rankFile = do
    (listHeader,listVotes) <- decodeByName listFile
    (rankHeader,rankVotes) <- decodeByName rankFile
    let label = M.fromList . V.toList . fmap ((,) <$> username <*> id)
    return $ F.foldMap voteToTally
           $ M.unionWith (fmap fromJust . reconcile)
             (label listVotes) (label rankVotes)
