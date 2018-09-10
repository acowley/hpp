{-# LANGUAGE TupleSections #-}
-- | A name binding context, or environment.
module Hpp.Env where
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

emptyEnv :: HashMap ByteString a
emptyEnv = HashMap.empty

insertPair :: (ByteString, a) -> HashMap ByteString a -> HashMap ByteString a
insertPair = uncurry HashMap.insert

deleteKey :: ByteString -> HashMap ByteString a -> HashMap ByteString a
deleteKey = HashMap.delete

lookupKey :: ByteString -> HashMap ByteString a -> Maybe a
lookupKey = HashMap.lookup
