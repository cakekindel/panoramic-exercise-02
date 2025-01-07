module Dog.API where

import Prelude hiding ((/))

import Data.Set as Set
import Control.Monad.Error.Class (liftEither, liftMaybe, throwError)
import Control.Monad.Except as Except
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.URL ((/), URL)
import Data.URL as URL
import Dog.Breed as Breed
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.HTTP as HTTP
import Effect.Aff.HTTP.Response as HTTP.Rep
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

baseURI :: URL
baseURI = unsafePartial $ fromJust $ URL.fromString $ "https://dog.ceo/api"

okJSON ::
  forall @a.
  DecodeJson { message :: a } =>
  HTTP.Rep.Response ->
  Aff (Either String a)
okJSON rep = Except.runExceptT do
  statusCode <- liftEffect $ HTTP.Rep.status rep
  when (statusCode < 200 || statusCode >= 300)
    (throwError $ "non-ok status code " <> show statusCode)

  text <- HTTP.Rep.text rep # Except.lift
  json <- jsonParser text # liftEither
  { status } <- decodeJson @{ status :: String } json
    # lmap printJsonDecodeError
    # liftEither
  when (status /= "success")
    (throwError $ "expected status \"success\", got: " <> show status)
  { message } <- decodeJson @{ message :: a } json # lmap printJsonDecodeError #
    liftEither
  pure message

breeds :: forall m. MonadAff m => m (Either String (Breed.Breeds Unit))
breeds = Except.runExceptT do
  rawBreeds <-
    HTTP.fetch HTTP.GET (baseURI / "breeds" / "list" / "all") {}
      >>= okJSON @(Object (Array String))
      # liftAff
      # Except.ExceptT
  let
    idArray =
      foldlWithIndex
        ( \a ids' bs ->
            let
              aId = Breed.idFromSegments $ pure a
              underA b = Breed.idFromSegments $ pure a <> pure b
            in
              ids' <> [ aId ] <> (underA <$> bs)
        )
        []
        rawBreeds
  idArray
    # Array.NonEmpty.fromArray
    # liftMaybe "No breeds returned!"
    <#> Breed.fromIds

randomImage :: forall m. MonadAff m => Breed.Id -> m URL
randomImage = unsafeCrashWith "unimplemented"

randomImages :: forall m. MonadAff m => Int -> Breed.Id -> m (Set URL)
randomImages = unsafeCrashWith "unimplemented"

allImages :: forall m. MonadAff m => Breed.Id -> m (Either String (Set URL))
allImages id =
  HTTP.fetch HTTP.GET (baseURI / "breed" / Breed.idToPathSegments id / "images")
    {}
    >>= okJSON @(Array String)
    # liftAff
    <#> (map $ Set.fromFoldable <<< Array.catMaybes <<< map URL.fromString)
