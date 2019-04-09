module Test.Main where

import Prelude

import Cache (db, getConn, host, port, socketKeepAlive) as C
import Data.Options ((:=))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Test.Multi (multiTest)
import Test.Queue (queueTest)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Stream (streamTest)

startTest :: Aff Unit
startTest = do
    let cacheOpts = C.host := "127.0.0.1" <> C.port := 6379 <> C.db := 0 <> C.socketKeepAlive := true
    cacheConn <- C.getConn cacheOpts
    queueTest cacheConn
    runSpec [consoleReporter] do
       multiTest cacheConn
       streamTest cacheConn

main :: Effect Unit
main = launchAff startTest *> pure unit
