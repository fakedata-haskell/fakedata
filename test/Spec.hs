import Faker
import Faker.Address
import Test.Hspec
import qualified Data.Vector as V

main :: IO ()
main = hspec $ do
         describe "Address" $ do
                 it "parses countries" $ do
                                ctries <- countries defaultFakerSettings
                                ctries `shouldSatisfy` (\x -> V.length x >= 40)
