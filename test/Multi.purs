module Test.Multi where


import Prelude

import Cache (CacheConn)
import Cache.Multi (execMulti, getHashKeyMulti, newMulti, setHashMulti) as C
import Debug.Trace (spy)
import Effect.Aff (Aff)

multiTest :: CacheConn -> Aff Unit
multiTest cacheConn = do
    let multi   = C.newMulti cacheConn
        multi'  = C.setHashMulti "myhash" "firstKey" "100" multi
        multi'' = C.getHashKeyMulti "myhash" "firstKey" multi'
    {-- val <- C.setKeyMulti "tt" "100" multi >>= C.setexKeyMulti "testing" "200" "1000" >>= C.incrMulti "testing" >>= C.getKeyMulti "tt" --}
    val <- C.execMulti multi''
    _ <- pure $ spy "value" val
    pure unit
