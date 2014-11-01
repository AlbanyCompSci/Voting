data VoteRecord a = VoteRecord
            { time     :: UTCTime
            , username :: String
            , vote     :: Vote a
            }

class Item i where
    Response i  :: *
    title       :: Response i -> String
    description :: Response i -> String

data Nul
data a ': b

data Vote a :: * -> * where
    NewVote     :: Vote Nul
    AddResponse :: Response i -> Vote l -> Vote (i ': l)

instance Monoid Vote Nul where
    mempty      = NewVote
    mappend _ _ = NewVote
instance (Monoid (Vote l), Monoid (Response i)) => Monoid (Vote (i ': l)) where
    mempty = AddResponse mempty $ mempty
    mappend (AddResponse r1 l1) (AddResponse r2 l2) =
        AddResponse (r1 <> r2) (l1 <> l2)

instance FromNamedRecord (Vote Nul) where
    parseNamedRecord _ = NewVote
instance (FromNamedRecord (Vote l), FromNamedRecord (Response i)) => FromNamedRecord (Vote (i ': l)) where
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
instance (Reconcilable (Vote l), Alternative (Response i)) => Reconcilable (Vote (i ': l)) where
    reconcile (AddResponse r1 l1) (AddResponse r2 l2) =
        AddResponse (r1 <|> r2) $ reconcile l1 l2

instance Show (Vote Nul) where
    show _ = ""
instance (Show (Issue i), Show (Response i), Show (Vote l)) => Show (Vote (i ': l)) where
    show (AddResponse r l) = title r ++ ":\n" ++ show r ++ "\n\n" ++ show l
