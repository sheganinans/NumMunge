import Test.Tasty
import qualified Tests.NumMunge.Project
import Prelude

main :: IO ()
main = defaultMain Tests.NumMunge.Project.tests
