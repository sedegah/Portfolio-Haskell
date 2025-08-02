{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Templates (portfolioPage)
import Lucid (renderText)
import Network.Wai.Middleware.Static

main :: IO ()
main = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "static")

  get "/" $ do
    html $ renderText portfolioPage
