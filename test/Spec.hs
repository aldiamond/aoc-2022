import Data.Text (Text, pack)
import qualified Day01 (maxCalories, parseCalories)
import Test.Hspec (describe, hspec, it, shouldBe)

testInput :: Text
testInput = pack "1000\n2000\n3000\n\n4000\n\n5000"

main :: IO ()
main = hspec $ do
  describe "AoC Problem 1" $ do
    it "correctly parses the file" $ do
      Day01.parseCalories testInput `shouldBe` [[1000, 2000, 3000], [4000], [5000]]
    it "determines the total calories" $ do
      let cals = [[1000, 2000, 3000], [4000], [5000]] :: [[Integer]]
      Day01.maxCalories cals `shouldBe` (6000 :: Integer)