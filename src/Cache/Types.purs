module Cache.Types where

foreign import data CacheConn :: Type

foreign import data Multi :: Type

data CacheConnOpts

type MultiToMulti = Multi -> Multi

