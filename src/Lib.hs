module Lib
    ( someFunc
    ) where

import qualified API.Types as API

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data DeepTweet =
    DeepTweet
        API.Status
        [API.Comment]
        [API.Picture]
