module Test.Main where

import Prelude

import Api as Api
import Api.Token as Token
import Control.Monad.Aff (Aff, attempt, runAff_)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, errorShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Lens ((^.), (.~))
import Data.Maybe (Maybe(..))
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
  cli_ <- Token.authenticate cli form
  logShow $ cli_ ^. Api._token
  where
    code = "100"
    tel = "03-1234-5678"
    form = Api.AuthenticateForm { code, tel }
    cli = Api.makeClient apiEndpoint

testAuthenticateFail :: Aff AppEffs Unit
testAuthenticateFail = do
  res <- attempt $ Token.authenticate cli form
  logShow res
  where
    code = "20"
    tel = "03-1234-5678"
    form = Api.AuthenticateForm { code, tel }
    cli = Api.makeClient apiEndpoint
