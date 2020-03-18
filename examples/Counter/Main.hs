{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Concur.Core
import           Concur.Replica

import qualified Data.Text      as T
import           Prelude        hiding (div)

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
main = runDefault 8080 "Counter" $ (\_ -> counter 0)


