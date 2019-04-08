{-
 Copyright (c) 2012-2017 "JUSPAY Technologies"
 JUSPAY Technologies Pvt. Ltd. [https://www.juspay.in]
 This file is part of JUSPAY Platform.
 JUSPAY Platform is free software: you can redistribute it and/or modify
 it for only educational purposes under the terms of the GNU Affero General
 Public License (GNU AGPL) as published by the Free Software Foundation,
 either version 3 of the License, or (at your option) any later version.
 For Enterprise/Commerical licenses, contact <info@juspay.in>.
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  The end user will
 be liable for all damages without limitation, which is caused by the
 ABUSE of the LICENSED SOFTWARE and shall INDEMNIFY JUSPAY for such
 damages, claims, cost, including reasonable attorney fee claimed on Juspay.
 The end user has NO right to claim any indemnification based on its use
 of Licensed Software. See the GNU Affero General Public License for more details.
 You should have received a copy of the GNU Affero General Public License
 along with this program. If not, see <https://www.gnu.org/licenses/agpl.html>.
-}

module Cache
 ( module Cache.Types
 , db
 , delKey
 , delKeyList
 , delKeyListMulti
 , delKeyMulti
 , exec
 , exists
 , expire
 , expireMulti
 , getConn
 , getHashKey
 , getHashKeyMulti
 , getKey
 , getKeyMulti
 , getMulti
 , host
 , incr
 , incrMulti
 , lindex
 , lindexMulti
 , lpop
 , lpopMulti
 , lpush
 , lpushMulti
 , port
 , publishToChannel
 , publishToChannelMulti
 , retryStrategy
 , rpop
 , rpopMulti
 , rpush
 , rpushMulti
 , set
 , setex
 , setexKeyMulti
 , setHash
 , setHashMulti
 , setKey
 , setKeyMulti
 , setMessageHandler
 , setMulti
 , socketKeepAlive
 , subscribe
 , subscribeMulti
 , tryAfter
 , zipkinEnable
 , zipkinRedis
 , zipkinServiceName
 , zipkinURL
 ) where

import Cache.Types (CacheConn, CacheConnOpts, Multi, MultiToMulti)

import Control.Monad.Except (runExcept)
import Control.Promise (Promise, toAff, toAffE)
import Data.Either (Either, hush)
import Data.Maybe (Maybe)
import Data.Options (Option, Options, opt, options)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Foreign (Foreign, readString)
import Prelude (Unit, map, pure, ($), (<<<))

host :: Option CacheConnOpts String
host = opt "host"

port :: Option CacheConnOpts Int
port = opt "port"

db :: Option CacheConnOpts Int
db = opt "db"

socketKeepAlive :: Option CacheConnOpts Boolean
socketKeepAlive = opt "socket_keepalive"

-- sentinels :: Option CacheConnOpts (Array { host :: String, port :: Int })
-- sentinels = opt "sentinels"

-- name :: Option CacheConnOpts String
-- name = opt "name"

tryAfter :: Option CacheConnOpts Int
tryAfter = opt "try_after"

retryStrategy :: Option CacheConnOpts (CacheConnOpts -> Int)
retryStrategy = opt "retry_strategy"
-- retryStrategy = opt "retryStrategy"

zipkinEnable :: Option CacheConnOpts String
zipkinEnable = opt "zipkinEnable"

zipkinRedis :: Option CacheConnOpts String
zipkinRedis = opt "zipkinRedis"

zipkinURL :: Option CacheConnOpts String
zipkinURL = opt "zipkinURL"

zipkinServiceName :: Option CacheConnOpts String
zipkinServiceName = opt "zipkinServiceName"

foreign import setJ :: CacheConn -> Array String -> Promise String
foreign import setKeyJ :: CacheConn -> String -> String -> Promise String
foreign import getKeyJ :: CacheConn -> String -> Promise String
foreign import existsJ :: CacheConn -> String -> Promise Boolean
foreign import setexJ :: CacheConn -> String -> String -> String -> Promise String
foreign import delKeyJ :: CacheConn -> Array String -> Promise String
foreign import expireJ :: CacheConn -> String -> String -> Promise String
foreign import incrJ :: CacheConn -> String -> Promise String
foreign import setHashJ :: CacheConn -> String -> String -> String -> Promise String
foreign import getHashKeyJ :: CacheConn -> String -> String -> Promise String
foreign import publishToChannelJ :: CacheConn -> String -> String -> Promise String
foreign import subscribeJ :: CacheConn -> String -> Effect (Promise String)
foreign import setMessageHandlerJ :: CacheConn -> (String -> String -> Effect Unit) -> Effect (Promise String)
foreign import _newCache :: Foreign -> Effect CacheConn
foreign import _newMulti :: CacheConn -> Effect Multi
foreign import execMulti :: Multi -> Promise (Array String)
foreign import rpopJ :: CacheConn -> String -> Promise Foreign
foreign import rpushJ :: CacheConn -> String -> String -> Promise Int
foreign import lpopJ :: CacheConn -> String -> Promise Foreign
foreign import lpushJ :: CacheConn -> String -> String -> Promise Int
foreign import lindexJ :: CacheConn -> String -> Int -> Promise Foreign

foreign import setMultiJ ::  Array String -> MultiToMulti
foreign import getKeyMultiJ ::  String -> MultiToMulti
foreign import setKeyMultiJ ::  String -> String -> MultiToMulti
foreign import setexKeyMultiJ :: String -> String -> String -> MultiToMulti
foreign import delKeyMultiJ :: Array String -> MultiToMulti
foreign import expireMultiJ :: String -> String -> MultiToMulti
foreign import incrMultiJ ::  String -> MultiToMulti
foreign import setHashMultiJ :: String -> String -> String -> MultiToMulti
foreign import getHashMultiJ :: String -> String -> MultiToMulti
foreign import publishCMultiJ :: String -> String -> MultiToMulti
foreign import subscribeMultiJ :: String -> MultiToMulti
foreign import rpopMultiJ :: String -> MultiToMulti
foreign import rpushMultiJ :: String -> String -> MultiToMulti
foreign import lpopMultiJ :: String -> MultiToMulti
foreign import lpushMultiJ :: String -> String -> MultiToMulti
foreign import lindexMultiJ :: String -> Int -> MultiToMulti

getConn :: Options CacheConnOpts -> Aff CacheConn
getConn = liftEffect <<< _newCache <<< options

getMulti :: CacheConn -> Aff Multi
getMulti = liftEffect <<< _newMulti

exec :: Multi -> Aff (Either Error (Array String))
exec = attempt <<< toAff <<< execMulti

setMulti :: Array String -> Multi -> Aff Multi
setMulti vals =  pure <<< setMultiJ vals

set :: CacheConn -> Array String -> Aff (Either Error String)
set cacheConn arr = attempt $ toAff $ setJ cacheConn arr

setKeyMulti :: String -> String -> Multi -> Aff Multi
setKeyMulti key val =  pure <<< setKeyMultiJ key val

setKey :: CacheConn -> String -> String -> Aff (Either Error String)
setKey cacheConn key value = attempt $ toAff $ setKeyJ cacheConn key value

setexKeyMulti :: String -> String -> String -> Multi -> Aff Multi
setexKeyMulti key value ttl =  pure <<< setexKeyMultiJ key value ttl

setex :: CacheConn -> String -> String -> String -> Aff (Either Error String)
setex cacheConn key value ttl = attempt $ toAff $ setexJ cacheConn key value ttl

getKeyMulti :: String -> Multi -> Aff Multi
getKeyMulti val =  pure <<< getKeyMultiJ val

getKey :: CacheConn -> String -> Aff (Either Error String)
getKey cacheConn key = attempt $ toAff $ getKeyJ cacheConn key

exists :: CacheConn -> String -> Aff (Either Error Boolean)
exists cacheConn = attempt <<< toAff <<< existsJ cacheConn

delKeyMulti :: String -> Multi -> Aff Multi
delKeyMulti key = pure <<< delKeyMultiJ [key]

delKey :: CacheConn -> String -> Aff (Either Error String)
delKey cacheConn key = attempt $ toAff $ delKeyJ cacheConn [key]

delKeyListMulti :: Array String -> Multi -> Aff Multi
delKeyListMulti keys =  pure <<< delKeyMultiJ keys

delKeyList :: CacheConn -> Array String -> Aff (Either Error String)
delKeyList cacheConn key = attempt $ toAff $ delKeyJ cacheConn key

expireMulti :: String -> String -> Multi -> Aff Multi
expireMulti key ttl = pure <<< expireMultiJ key ttl

expire :: CacheConn -> String -> String -> Aff (Either Error String)
expire cacheConn key ttl = attempt $ toAff $ expireJ cacheConn key ttl

incrMulti :: String -> Multi -> Aff Multi
incrMulti key = pure <<< incrMultiJ key

incr :: CacheConn -> String -> Aff (Either Error String)
incr cacheConn key = attempt $ toAff $ incrJ cacheConn key

setHashMulti :: String -> String -> String -> Multi -> Aff Multi
setHashMulti key field val = pure <<< setHashMultiJ key field val

setHash :: CacheConn -> String -> String -> String -> Aff (Either Error String)
setHash cacheConn key field value = attempt $ toAff $ setHashJ cacheConn key field value

getHashKeyMulti :: String -> String -> Multi -> Aff Multi
getHashKeyMulti key field = pure <<< getHashMultiJ key field

getHashKey :: CacheConn -> String -> String -> Aff (Either Error String)
getHashKey cacheConn key field = attempt $ toAff $ getHashKeyJ cacheConn key field

publishToChannelMulti :: String -> String -> Multi ->  Aff Multi
publishToChannelMulti channel message = pure <<< publishCMultiJ channel message

publishToChannel :: CacheConn -> String -> String -> Aff (Either Error String)
publishToChannel cacheConn channel message = attempt $ toAff $ publishToChannelJ cacheConn channel message

subscribeMulti :: String -> Multi -> Aff Multi
subscribeMulti channel =  pure <<< subscribeMultiJ channel

subscribe :: CacheConn -> String -> Aff (Either Error String)
subscribe cacheConn channel = attempt $ toAffE $ subscribeJ cacheConn channel

readStringMaybe :: Foreign -> Maybe String
readStringMaybe = hush <<< runExcept <<< readString

rpopMulti :: String -> Multi -> Aff Multi
rpopMulti listName = pure <<< rpopMultiJ listName

rpop :: CacheConn -> String -> Aff (Either Error (Maybe String))
rpop cacheConn listName = attempt $ map readStringMaybe $ toAff $ rpopJ cacheConn listName

rpushMulti :: String -> String -> Multi -> Aff Multi
rpushMulti listName value = pure <<< rpushMultiJ listName value

rpush :: CacheConn -> String -> String -> Aff (Either Error Int)
rpush cacheConn listName value = attempt $ toAff $ rpushJ cacheConn listName value

lpopMulti :: String -> Multi -> Aff Multi
lpopMulti listName = pure <<< lpopMultiJ listName

lpop :: CacheConn -> String -> Aff (Either Error (Maybe String))
lpop cacheConn listName = attempt $ map readStringMaybe $ toAff $ lpopJ cacheConn listName

lpushMulti :: String -> String -> Multi -> Aff Multi
lpushMulti listName value = pure <<< lpushMultiJ listName value

lpush :: CacheConn -> String -> String -> Aff (Either Error Int)
lpush cacheConn listName value = attempt $ toAff $ lpushJ cacheConn listName value

lindexMulti :: String -> Int -> Multi -> Aff Multi
lindexMulti listName index = pure <<< lindexMultiJ  listName index

lindex :: CacheConn -> String -> Int -> Aff (Either Error (Maybe String))
lindex cacheConn listName index = attempt $ map readStringMaybe $ toAff $ lindexJ cacheConn listName index

setMessageHandler :: CacheConn -> (String -> String -> Effect Unit) -> Aff (Either Error String)
setMessageHandler cacheConn f = attempt $ toAffE $ setMessageHandlerJ cacheConn f
