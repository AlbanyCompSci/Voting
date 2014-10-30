module Main where

main :: IO ()
main = do
    listFile <- BS.readFile "List.csv"
    rankFile <- BS.readFile "Rank.csv"
    let votes = map reconcile <$> decode listFile <*> decode rankFile
    let tally = foldMap voteToTally <$> votes
    case tally of
         Left s -> putStrLn $ "Error: " ++ s
         Right t -> do
             return ()
