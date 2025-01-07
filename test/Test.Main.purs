module Test.Main where

import Prelude

import Control.Monad.Error.Class (liftMaybe)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Traversable (traverse)
import Dog.Breed (Breed(..), Breeds)
import Dog.Breed as Breed
import Effect (Effect)
import Effect.Exception (error)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- depth beyond 1 not used by the dog API,
-- but the Breed tree structure is completely
-- blind to this.
--
-- The "mini australian shepherd" with depth 2
-- added to test this case.
testBreeds :: Array String
testBreeds =
  [ "affenpinscher"
  , "australian-kelpie"
  , "australian-shepherd"
  , "australian-shepherd-mini"
  , "bakharwal-indian"
  , "briard"
  , "buhund-norwegian"
  ]

testBreedTree :: Breeds Unit
testBreedTree =
  pure (BreedLeaf (Breed.Id (pure "affenpinscher")) unit)
  <> pure
       ( BreedNode
          (Breed.Id (pure "australian"))
          unit
          ( pure (BreedLeaf (Breed.Id (pure "australian" <> pure "kelpie")) unit)
            <>  pure
              ( BreedNode
                  (Breed.Id (pure "australian" <> pure "shepherd"))
                  unit
                  (pure (BreedLeaf (Breed.Id (pure "australian" <> pure "shepherd" <> pure "mini")) unit))
              )
          )
       )
            <>  pure
              ( BreedNode
                  (Breed.Id (pure "bakharwal"))
                  unit
                  (pure (BreedLeaf (Breed.Id (pure "bakharwal" <> pure "indian")) unit))
              )
  <> pure (BreedLeaf (Breed.Id (pure "briard")) unit)
            <>  pure
              ( BreedNode
                  (Breed.Id (pure "buhund"))
                  unit
                  (pure (BreedLeaf (Breed.Id (pure "buhund" <> pure "norwegian")) unit))
              )

main :: Effect Unit
main = runSpecAndExitProcess [specReporter] do
  describe "Dog.Breed" do
    it "parses" do
      ids <-
        traverse Breed.idFromString testBreeds
        >>= Array.NonEmpty.fromArray
        # liftMaybe (error "failed to parse ids")
      let tree = Breed.fromIds ids
      tree `shouldEqual` testBreedTree
    it "map" do
      ids <-
        ["fruit", "fruit-orange", "vegeble", "vegeble-zukini", "vegeble-brogli", "vegeble-brogli-coliflour"]
        # traverse Breed.idFromString
        >>= Array.NonEmpty.fromArray
        # liftMaybe (error "failed to parse ids")
      let tree = (\id _ -> Breed.idDisplay id) `Breed.mapWithIndex` (Breed.fromIds ids)
      tree `shouldEqual` (
        pure
          ( BreedNode
              (Breed.Id (pure "fruit"))
              "Fruit"
              ( pure (BreedLeaf (Breed.Id (pure "fruit" <> pure "orange")) "Orange Fruit")
              )
          )
        <> pure
          ( BreedNode
              (Breed.Id (pure "vegeble"))
              "Vegeble"
              ( pure (BreedLeaf (Breed.Id (pure "vegeble" <> pure "zukini")) "Zukini Vegeble")
                <> pure
                     ( BreedNode
                         (Breed.Id (pure "vegeble" <> pure "brogli"))
                         "Brogli Vegeble"
                         ( pure
                             (BreedLeaf
                               (Breed.Id (pure "vegeble" <> pure "brogli" <> pure "coliflour"))
                               "Coliflour Brogli Vegeble"
                             )
                         )
                     )
              )
          )
      )
