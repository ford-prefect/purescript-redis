module Test.Queue where

import Prelude

import Cache (CacheConn, lindex, lpop, rpush, setex) as C
import Effect.Aff (Aff)
import Debug.Trace (spy, traceM)

queueTest :: C.CacheConn -> Aff Unit
queueTest cacheConn = do
    v0 <- C.lpop cacheConn "test-queue"
    traceM v0
    v1 <- C.rpush cacheConn "test-queue" "hi"
    traceM v1
    v2 <- C.lpop cacheConn "test-queue"
    traceM v2
    v3 <- C.lpop cacheConn "test-queue"
    traceM v3
    v <- C.setex cacheConn "talk" "i am awesome" "10000"
    l <- C.rpush cacheConn "DBACTIONS" "SELCT * FROM CUSTOMERS;"
    pop <- C.lpop cacheConn "DBACTIONS"
    peek <- C.lindex cacheConn "DBACTIONS" 0
    _ <- pure $ spy "peek" peek
    _ <- pure $ spy "It worked" unit
    pure unit
