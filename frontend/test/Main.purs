module Test.Main where

import Prelude

import Api.Token as Token
import Control.Monad.Aff (Aff, attempt, runAff_)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, errorShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Network.HTTP.Affjax (AJAX)
import Test.Util as Util


apiEndpoint :: String
apiEndpoint = Util.getAppEnv "API_ENDPOINT"


type AppEffs = (ajax :: AJAX, console :: CONSOLE, exception :: EXCEPTION)

main :: Eff AppEffs Unit
main = runAff_ errorShow do
  testAuthenticateSuccess
  testAuthenticateFail


testAuthenticateSuccess :: Aff AppEffs Unit
testAuthenticateSuccess = do
  token <- Token.authenticate (apiEndpoint <> "/token") 100 "03-1234-5678"
  logShow token

testAuthenticateFail :: Aff AppEffs Unit
testAuthenticateFail = do
  res <- attempt $ Token.authenticate (apiEndpoint <> "/token") 200 "03-1234-5678"
  logShow res
