module Cache.Stream
  ( Entry(..)
  , EntryID(AutoID, AfterLastID, MaxID, MinID, NewID)
  , Item
  , TrimStrategy(..)
  , firstEntryId
  , newEntryId
  , xack
  , xadd
  , xclaim
  , xdel
  , xgroupCreate
  , xgroupDelConsumer
  , xgroupDestroy
  , xgroupSetId
  , xlen
  , xrange
  , xread
  , xreadGroup
  , xrevrange
  , xtrim
  ) where

import Cache.Types (CacheConn)
import Control.Monad.Except (Except, runExcept)
import Control.MonadPlus ((>>=))
import Control.Promise (Promise, toAff)
import Data.Array (filter, length, range, singleton, zip, (!!), (:))
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt, toString)
import Data.BigInt (fromInt, fromString, shl) as BigInt
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5, Fn7, runFn2, runFn3, runFn4, runFn5, runFn7)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Int (even, odd)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (Pattern(..), split)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff, attempt)
import Effect.Exception (Error, error)
import Foreign (F, Foreign, isNull, readArray, readString)
import Foreign.Object (Object, empty, fromFoldable)
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Show, Unit, bind, map, pure, show, ($), (&&), (<), (<$>), (<*>), (<<<), (<>), (==), (>=), (>>>))

-- Streams API

type Item = Tuple String String

bigZero :: BigInt
bigZero = BigInt.fromInt 0

-- Validate a BigInt as being an unsigned 64-bit integer
filter64bit :: BigInt -> Maybe BigInt
filter64bit n = if n >= bigZero && n < uint64Max then Just n else Nothing
  where
        uint64Max     = BigInt.shl (BigInt.fromInt 1) 64.0

data EntryID = EntryID BigInt BigInt
             | AutoID
             | AfterLastID
             | MinID
             | MaxID
             | NewID
derive instance genericEntryID :: Generic EntryID _
instance eqEntryId :: Eq EntryID where
  eq = genericEq

data Entry = Entry EntryID (Array Item)

newEntryId :: BigInt -> BigInt -> Maybe EntryID
newEntryId ms seq = EntryID <$> filter64bit ms <*> filter64bit seq

firstEntryId :: EntryID
firstEntryId = EntryID bigZero bigZero

instance showEntryID :: Show EntryID where
  show :: EntryID -> String
  show (EntryID ms seq) = toString ms <> "-" <> toString seq
  show (AutoID        ) = "*"
  show (AfterLastID   ) = "$"
  show (MinID         ) = "-"
  show (MaxID         ) = "+"
  show (NewID         ) = ">"

fromString :: String -> Maybe EntryID
fromString s =
  let parts         = split (Pattern "-") s
      ms            = parts !! 0 >>= BigInt.fromString >>= filter64bit
      seq           = parts !! 1 >>= BigInt.fromString >>= filter64bit
  in
    if length parts == 2
      then EntryID <$> ms <*> seq
      else Nothing

foreign import xaddJ :: Fn4 CacheConn String String (Array String) (Promise String)

xadd :: CacheConn -> String -> EntryID -> Array Item -> Aff (Either Error EntryID)
xadd _ _ AfterLastID _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xadd _ _ MinID       _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xadd _ _ MaxID       _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xadd _ _ NewID       _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xadd cacheConn key entryId args = do
  res <- attempt <<< toAff $ runFn4 xaddJ cacheConn key (show entryId) $ foldMap tupleToArray args
  pure $ forceFromString  <$> res
  where
        tupleToArray (Tuple a b) = a : singleton b

foreign import xdelJ :: Fn3 CacheConn String String (Promise Int)

xdel :: CacheConn -> String -> EntryID -> Aff (Either Error Int)
xdel cacheConn key entryId = attempt <<< toAff $ runFn3 xdelJ cacheConn key (show entryId)

-- FIXME: this and all Ints should actually be 64-bit (instead of the current 32-bit)
foreign import xlenJ :: Fn2 CacheConn String (Promise Int)

-- Note that the API does not distinguish between a non-existing key and an
-- empty stream
xlen :: CacheConn -> String -> Aff (Either Error Int)
xlen cacheConn key = attempt <<< toAff $ runFn2 xlenJ cacheConn key

-- The return value is of the form:
-- [ [[entry_id, [key1, val1, key2, val2]], [entry_id, [...]]] ]
foreign import xrangeJ :: Fn5 CacheConn String String String Int (Promise (Array Foreign))

xrange :: CacheConn -> String -> EntryID -> EntryID -> Maybe Int -> Aff (Either Error (Array Entry))
xrange cacheConn stream start end mCount = do
  let count = fromMaybe 0 mCount
  res <- attempt <<< toAff $ runFn5 xrangeJ cacheConn stream (show start) (show end) count
  pure $ res >>= readEntries

-- The return value is of the form:
-- [ [[entry_id, [key1, val1, key2, val2]], [entry_id, [...]]] ]
foreign import xrevrangeJ :: Fn5 CacheConn String String String Int (Promise (Array Foreign))

xrevrange :: CacheConn -> String -> EntryID -> EntryID -> Maybe Int -> Aff (Either Error (Array Entry))
xrevrange cacheConn stream start end mCount = do
  let count = fromMaybe 0 mCount
  res <- attempt <<< toAff $ runFn5 xrangeJ cacheConn stream (show start) (show end) count
  pure $ res >>= readEntries

-- The return value is of the form:
-- [ [stream_name, [[entry_id, [key1, val1, key2, val2]], [entry_id, [...]]]], [stream_name, [...]] ]
foreign import xreadJ :: Fn4 CacheConn Int (Array String) (Array String) (Promise Foreign)

xread :: CacheConn -> Maybe Int -> Array (Tuple String EntryID) -> Aff (Either Error (Object (Array Entry)))
xread cacheConn mCount streamIds = do
  let count   = fromMaybe 0 mCount
      streams = fst <$> streamIds
      ids     = show <<< snd <$> streamIds
  res <- attempt <<< toAff $ runFn4 xreadJ cacheConn count streams ids
  pure $ do
     -- Either monad
     response <- res
     if isNull response
       then Right empty
       else do
          arrStreams    <- parseWithError $ readArray response
          streamEntries <- sequence $ readStreamEntries <$> arrStreams
          Right $ fromFoldable streamEntries

data TrimStrategy = Maxlen

instance showTrimStrategy :: Show TrimStrategy where
  show Maxlen = "MAXLEN"

foreign import xtrimJ :: Fn5 CacheConn String String Boolean Int (Promise Int)

xtrim :: CacheConn -> String -> TrimStrategy -> Boolean -> Int -> Aff (Either Error Int)
xtrim cacheConn key strategy approx len = attempt <<< toAff $ runFn5 xtrimJ cacheConn key (show strategy) approx len

-- Consumer group API

foreign import xgroupCreateJ :: Fn4 CacheConn String String String (Promise Unit)

xgroupCreate :: CacheConn -> String -> String -> EntryID -> Aff (Either Error Unit)
xgroupCreate _ _ _ AutoID = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreate _ _ _ MinID  = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreate _ _ _ MaxID  = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreate _ _ _ NewID  = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreate cacheConn key groupName entryId = attempt <<< toAff $ runFn4 xgroupCreateJ cacheConn key groupName (show entryId)

foreign import xgroupDestroyJ :: Fn3 CacheConn String String (Promise Unit)

xgroupDestroy :: CacheConn -> String -> String -> Aff (Either Error Unit)
xgroupDestroy cacheConn key groupName = attempt <<< toAff $ runFn3 xgroupDestroyJ cacheConn key groupName

foreign import xgroupDelConsumerJ :: Fn4 CacheConn String String String (Promise Unit)

xgroupDelConsumer :: CacheConn -> String -> String -> String -> Aff (Either Error Unit)
xgroupDelConsumer cacheConn key groupName consumerName = attempt <<< toAff $ runFn4 xgroupDelConsumerJ cacheConn key groupName consumerName

foreign import xgroupSetIdJ :: Fn4 CacheConn String String String (Promise Unit)

xgroupSetId :: CacheConn -> String -> String -> EntryID -> Aff (Either Error Unit)
xgroupSetId _ _ _ AutoID = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetId _ _ _ MinID  = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetId _ _ _ MaxID  = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetId _ _ _ NewID  = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetId cacheConn key groupName entryId = attempt <<< toAff $ runFn4 xgroupSetIdJ cacheConn key groupName (show entryId)

foreign import xreadGroupJ :: Fn7 CacheConn String String Int Boolean (Array String) (Array String) (Promise Foreign)

xreadGroup :: CacheConn -> String -> String -> Maybe Int -> Boolean -> Array (Tuple String EntryID) -> Aff (Either Error (Object (Array Entry)))
xreadGroup cacheConn groupName consumerName mCount noAck streamIds = do
  let count   = fromMaybe 0 mCount
      streams = fst <$> streamIds
      ids     = show <<< snd <$> streamIds
  res <- attempt <<< toAff $ runFn7 xreadGroupJ cacheConn groupName consumerName count noAck streams ids
  pure $ do
     -- Either monad
     response <- res
     if isNull response
       then Right empty
       else do
          arrStreams    <- parseWithError $ readArray response
          streamEntries <- sequence $ readStreamEntries <$> arrStreams
          Right $ fromFoldable streamEntries

foreign import xackJ :: Fn4 CacheConn String String (Array String) (Promise Int)

xack :: CacheConn -> String -> String -> Array EntryID -> Aff (Either Error Int)
xack cacheConn key group entryIds = attempt <<< toAff $ runFn4 xackJ cacheConn key group (show <$> entryIds)

foreign import xclaimJ :: Fn7 CacheConn String String String Int (Array String) Boolean (Promise (Array Foreign))

xclaim :: CacheConn -> String -> String -> String -> Int -> Array EntryID -> Boolean -> Aff (Either Error (Array Entry))
xclaim cacheConn key groupName consumerName idleTimeMs entryIds force = do
  res <- attempt <<< toAff $ runFn7 xclaimJ cacheConn key groupName consumerName idleTimeMs (show <$> entryIds) force
  pure $ res >>= readEntries

-- Utility functions for parsing

-- unsafePartial here as we expect redis to never give us an invalid entry ID
forceFromString :: String -> EntryID
forceFromString s = unsafePartial $ fromJust $ fromString s

parseWithError :: forall e' a. Show e' => Except e' a -> Either Error a
parseWithError = lmap (error <<< show) <<< runExcept

parseArrayItem :: forall a. String -> Array Foreign -> Int -> (Foreign -> F a) -> Either Error a
parseArrayItem name arr ix parse =
  fromMaybe
    (Left $ error $ "Could not read " <> name)
    (parseWithError <<< parse <$> arr !! ix)

readStreamEntries :: Foreign -> Either Error (Tuple String (Array Entry))
readStreamEntries v = do
   vals     <- parseWithError $ readArray v
   stream   <- parseArrayItem "stream key" vals 0 readString
   fEntries <- parseArrayItem "stream entries" vals 1 readArray
   entries  <- readEntries fEntries
   Right $ Tuple stream entries

readEntries :: Array Foreign -> Either Error (Array Entry)
readEntries e = sequence $ readEntry <$> e

readEntry :: Foreign -> Either Error Entry
readEntry v = do
   vals   <- parseWithError $ readArray v
   entry  <- parseArrayItem "entry ID" vals 0 (map forceFromString <<< readString)
   fItems <- parseArrayItem "entry items" vals 1 readArray
   items  <- readItems fItems
   Right $ Entry entry items

readItems :: Array Foreign -> Either Error (Array Item)
readItems v =
  if odd $ length v
    then
      Left $ error "unexpected number of keys + values"
    else do
      array <- sequence $ parseWithError <<< readString <$> v
      let indexed = zip (range 1 $ length v) array
          keys    = snd <$> filter (fst >>> odd) indexed
          values  = snd <$> filter (fst >>> even) indexed
      Right $ zip keys values