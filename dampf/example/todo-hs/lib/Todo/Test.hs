module Todo.Test where

import Test.HTTP

todoTest :: IO ()
todoTest = defaultMain $ httpTestCase "pingpong" "http://todo.diffusionkinetics.com" $ do
    pong <- get "/ping"
    assert "pongs" $ pong == "pong"
