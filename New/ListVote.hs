module ListVote where

import qualified Data.Map                    as M
import           Data.Monoid                 (Monoid, mempty, mappend, (<>))
import qualified Data.ByteString.Lazy.Search as BSLS

import Data.Csv (FromField, parseField)

import Types (Reconcilable)

type CandidateListResponse a = ListResponse (Maybe a)
type PropResponse            = ListResponse (Maybe Bool)

instance FromField Bool where
    parseField "Yes" = pure True
    parseField "No"  = pure False

newtype ListResponse a = LR { unLR :: M.Map a (Sum Int) }

instance Monoid (ListResponse a) where
    mempty                = LR M.empty
    mappend (LR a) (LR b) = LR $ unionWith (<>) a b

instance FromField a => FromField (ListResponse a) where
    parseField = fmap (ListResponse . M.fromList . (flip zip) (repeat (Sum 1)))
               . T.sequenceA
               . map parseField
               . BSLS.split ", "

instance Reconcilable (ListResponse a) where
    reconcile a b = if M.null (unLR a) then b else a

instance Show a => Show (ListResponse a) where
    show = intercalate "\n"
         . map (\(k,v) -> show k ++ ": " ++ show v)
         . M.toList . unLR
