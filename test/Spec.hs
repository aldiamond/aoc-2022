import Data.Text (Text, pack)
import qualified Day01 (maxCalories, parseCalories, top)
import Test.Hspec (describe, hspec, it, shouldBe)

testInput :: Text
testInput = pack "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"

testCal :: [[Integer]]
testCal = [[1000, 2000, 3000], [4000], [5000, 6000], [7000, 8000, 9000], [10000]]

main :: IO ()
main = hspec $ do
  describe "AoC Problem 1" $ do
    it "correctly parses the file" $ do
      Day01.parseCalories testInput `shouldBe` Right testCal
    it "determines the max total calories" $ do
      Day01.maxCalories testCal `shouldBe` (24000 :: Integer)
    it "determines the top two elf calories" $ do
      let top2 = Day01.top 2
      top2 testCal `shouldBe` ([24000, 11000] :: [Integer])