module HttpStuff where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

-- replace with other websites
-- if desired or needed
urls :: [String]
urls = [ --"http://httpbin.com/ip"
        "http://httpbin.org/bytes/5"
       ]

mappingGet :: IO [(Response ByteString)]
mappingGet = traverse get urls
