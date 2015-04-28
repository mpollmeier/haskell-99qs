import Test.Hspec
import Test.QuickCheck

type Cell = (Int, Int)
type Grid = [Cell]

next :: Grid -> Grid
next [] = []
next grid = filter ((\count -> count == 2 || count == 3) . neighbourCount grid) grid
-- next grid = filter (((flip elem) [2,3]) . neighbourCount grid) grid

neighbourCount :: Grid -> Cell-> Int
neighbourCount grid (x,y) = length (filter (isNeighbour (x,y)) grid)

isNeighbour :: Cell -> Cell -> Bool
isNeighbour a@(x,y) a'@(x',y') = abs (x-x') <= 1 && abs (y-y') <= 1 && (a/=a')

allNeighbours :: Cell -> [Cell]
allNeighbours cell@(x,y) = filter (\c -> c /= cell) all
-- allNeighbours cell@(x,y) = filter (flip elem all) all
  where all = [(x + x',y + y')|x' <- [-1..1], y' <- [-1,0,1]]

main :: IO ()
main = hspec $ do
  describe "allNeighbours" $
    it "returns all neighbours" $
      allNeighbours (1,3) `shouldBe` [(0,2),(0,3),(0,4),(1,2),(1,4),(2,2),(2,3),(2,4)]

  describe "isNeighbour" $ do
    it "returns True if the cells are neighbours" $ property $
      \n m -> isNeighbour (n,m)(n-1,m) `shouldBe` True
    it "returns False if the cells are no neighbours" $ property $
      \n m -> isNeighbour (n,m)(n-2,m-2) `shouldBe` False
    it "doesn't count itself as a neighbour" $ property $
      \n m -> isNeighbour (n,m)(n,m) `shouldBe` False

  describe "world.next" $ do
    it "is empty if it was empty before" $
      next [] `shouldBe` []
    it "is empty if it had only one inhabitant" $ property $
      \n m -> next [(n,m)] `shouldBe` []
    it "only contains the middle cell if we had three cells in a row" $ property $
      \n m -> next [(n-1,m),(n,m),(n+1,m)] `shouldBe` [(n,m)]
    it "contains the cells that used to have three live neighbours" $ property $
      -- \grid -> next grid `shouldBe` []
      \n m -> next [(n-1,m),(n,m),(n+1,m),(n,m+1)] `shouldBe` [(n-1,m),(n,m),(n+1,m),(n,m+1)]

-- https://gist.github.com/vertexcite/e28fa8cc8389646b175a
