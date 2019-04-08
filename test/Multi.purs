module Test.Multi where


import Prelude

import Cache (CacheConn, exec, getHashKeyMulti, getMulti, setHashMulti) as C
import Debug.Trace (spy)
import Effect.Aff (Aff)


multiTest :: C.CacheConn -> Aff Unit
multiTest cacheConn = do
    multi <- C.getMulti cacheConn
    multi' <- C.setHashMulti "myhash" "firstKey" "100" multi
    _ <- C.getHashKeyMulti "myhash" "firstKey" multi'
    {-- val <- C.setKeyMulti "tt" "100" multi >>= C.setexKeyMulti "testing" "200" "1000" >>= C.incrMulti "testing" >>= C.getKeyMulti "tt" --} 
    val <- C.exec multi
    _ <- pure $ spy "value" val
    pure unit
