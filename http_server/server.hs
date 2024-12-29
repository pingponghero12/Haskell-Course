{-# LANGUAGE OverloadedStrings #-}

import Network.Wai.Middleware.Static
import Web.Scotty

main :: IO ()
main =
  scotty 3000 $ do
    middleware $ staticPolicy (addBase "static")

    get "/" $
      file "static/index.html"
