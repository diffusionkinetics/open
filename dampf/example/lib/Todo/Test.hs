module Todo.Test where

import Test.HTTP
import Data.List (isInfixOf)

todoTest :: IO ()
todoTest = defaultMain $ httpTestCase "pingpong" "http://todo.diffusionkinetics.com" $ do
    pong <- get "/ping"
    assert "pongs" $ pong == "pong"
