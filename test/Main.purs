module Test.Main where

import Prelude

import Cache (db, getConn, host, port, socketKeepAlive) as C
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Data.Options ((:=))
import Test.Multi (multiTest)
import Test.Queue (queueTest)
import Test.Stream (streamTest)

startTest :: Aff Unit
startTest = do
    let cacheOpts = C.host := "127.0.0.1" <> C.port := 6379 <> C.db := 0 <> C.socketKeepAlive := true
    cacheConn <- C.getConn cacheOpts
    multiTest cacheConn
    queueTest cacheConn
    streamTest cacheConn
    pure unit

main :: Effect Unit
main = launchAff startTest *> pure unit
