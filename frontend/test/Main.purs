module Test.Main where

import AppPrelude

import Api.Client (AuthenticateForm(..), _token, makeClient)
import Api.Token as Token
import Effect.Aff (Aff, attempt, runAff_)
import Effect.Class.Console (logShow)
import Test.Util as Util


apiEndpoint :: String
apiEndpoint = Util.getAppEnv "API_ENDPOINT"


main :: Effect Unit
main = runAff_ (const $ pure unit) do
  testAuthenticateSuccess
  testAuthenticateFail


testAuthenticateSuccess :: Aff Unit
testAuthenticateSuccess = do
  cli_ <- Token.authenticate cli form
  logShow $ cli_ ^. _token
  where
    code = "ﾝ001"
    tel = "0123456789"
    form = AuthenticateForm { code, tel }
    cli = makeClient apiEndpoint

testAuthenticateFail :: Aff Unit
testAuthenticateFail = do
  res <- attempt $ Token.authenticate cli form
  logShow res
  where
    code = "20"
    tel = "0123456789"
    form = AuthenticateForm { code, tel }
    cli = makeClient apiEndpoint
