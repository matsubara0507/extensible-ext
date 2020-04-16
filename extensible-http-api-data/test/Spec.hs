import qualified Spec.Data.Extensible.FormUrlEncoded
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = defaultMain =<< testGroup "Data.Extensible" <$> sequence
  [ testSpec "Data.Extensible.FormUrlEncoded" $ Spec.Data.Extensible.FormUrlEncoded.spec
  ]
