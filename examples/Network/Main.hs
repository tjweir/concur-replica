{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Concur.Core
import           Concur.Replica

import qualified Data.Text           as T
import           Network.HTTP.Client
import           Prelude             hiding (div)

counter :: Int -> Widget HTML a
counter x = do


  click <- div []
    [ Left  <$> button [ onClick ] [ text "Subtract 1" ]
    , text $ T.pack $ show x
    , Right <$> button [ onClick ] [ text "Add 1" ]
    ]

  case click of
    Left _  -> counter (x - 1)
    Right _ -> counter (x + 1)

main :: IO ()
main = do
  let settings = managerSetProxy
           (proxyEnvironment Nothing)
           defaultManagerSettings
  man <- newManager settings
  let req = "http://httpbin.org"
        -- Note that the following settings will be completely ignored.
        { proxy = Just $ Proxy "localhost" 1234
        }
  httpLbs req man >>= print
  runDefault 8080 "Network" $ (\_ -> counter 0)


