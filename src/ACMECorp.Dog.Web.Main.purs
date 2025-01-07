module ACMECorp.Dog.Web.Main where

import Prelude hiding (div)

import Control.Monad.Error.Class (class MonadThrow, liftEither, liftMaybe)
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Bifunctor (lmap)
import Data.DateTime.Instant as Instant
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.URL (URL)
import Data.URL as URL
import Data.Variant (Variant)
import Data.Variant as Variant
import Data.Variant.Extra as Variant.X
import Dog.API as Dog.API
import Dog.Breed (Breeds) as Dog
import Dog.Breed as Dog.Breed
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Effect.Now as Now
import Halogen (HalogenM)
import Halogen as Halogen
import Halogen.Aff as Halogen.Aff
import Halogen.HTML (HTML, button, div, h1, h2, img, span, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, disabled, src)
import Halogen.VDom.Driver as Halogen.Driver.VDom
import Record.Extra as Record.X
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as Event.Mouse

type ImageURL = URL
type ImagePageOffset = Int
data ImageShiftDir = ShiftLeft | ShiftRight

type State = Variant
  ( loading :: Unit
  , loaded ::
      { breeds :: Dog.Breeds (ImagePageOffset /\ Set ImageURL)
      , activeBreed :: Maybe Dog.Breed.Id
      }
  )

type Action = Variant
  ( init :: Unit
  , clickBreed :: MouseEvent /\ Dog.Breed.Id
  , clickImageShift :: MouseEvent /\ ImageShiftDir
  )

main :: Effect Unit
main = Halogen.Aff.runHalogenAff
  $ Halogen.Aff.awaitBody
      >>= Halogen.Driver.VDom.runUI component unit

component ::
  forall q i o m.
  MonadThrow Error m =>
  MonadAff m =>
  Halogen.Component q i o m
component =
  Halogen.mkComponent
    { initialState: const $ Variant.X.inj_ @"loading"
    , render
    , eval: Halogen.mkEval $ Halogen.defaultEval
        { handleAction = handleAction
        , initialize = Just $ Variant.X.inj_ @"init"
        }
    }

render :: forall w. State -> HTML w Action
render =
  let
    spinner =
      div
        [ class_ $ wrap
            $ "bg-stone-800 h-[8rem] w-[8rem] "
                <> "flex justify-center items-center "
                <> "rounded-[9999px]"
        ]
        [ span
            [ class_ $ wrap
                $ "text-stone-200 text-8xl animate-spin "
                    <> "icon--solar icon--solar--bone-bold"
            ]
            []
        ]

    page =
      div
        [ class_ $ wrap
            $ "bg-stone-100 text-stone-900 "
                <> "h-full w-full grid grid-cols-[32rem_auto]"
        ]

    navbar children =
      div
        [ class_ $ wrap
            $ "overflow-hidden h-full bg-stone-200 col-start-1"
        ]
        [ div [ class_ $ wrap "overflow-y-auto h-full flex flex-col gap-3 p-4" ]
            $ Array.concat
                [ [ h1
                      [ class_ $ wrap "font-black text-2xl text-stone-600 mb-2"
                      ]
                      [ text "ACME Co. Breed Browser" ]
                  ]
                , children
                ]
        ]

    content children =
      div
        [ class_ $ wrap "h-full w-full overflow-hidden col-start-2" ]
        children

    contentNoneSelected =
      content
        [ div [ class_ $ wrap "h-full w-full flex justify-center items-center" ]
            [ div
                [ class_ $ wrap
                    $ "text-stone-400 flex flex-row p-4 rounded-lg gap-4 "
                        <> "border-4 border-stone-200 items-center"
                ]
                [ span
                    [ class_ $ wrap
                        "text-6xl icon--solar icon--solar--arrow-left-bold"
                    ]
                    []
                , div [ class_ $ wrap "flex flex-col" ]
                    [ h1 [ class_ $ wrap "text-2xl font-black " ]
                        [ text "No Breed Selected" ]
                    , span [ class_ $ wrap "text-xl " ]
                        [ text "Select a breed on the left to view pictures!" ]
                    ]
                ]
            ]
        ]

    breedCard activeId =
      case _ of
        (Dog.Breed.BreedNode id _ children) ->
          div
            [ class_ $ wrap $ "flex flex-col gap-1 w-full" ]
            [ h2
                [ class_ $ wrap $ "text-stone-500 text-xl font-bold" ]
                [ text $ Dog.Breed.idDisplay id ]
            , div
                [ class_ $ wrap $ "flex flex-row gap-2 w-full pl-1" ]
                [ div [ class_ $ wrap "h-full w-[4px] bg-stone-400" ] []
                , div
                    [ class_ $ wrap "flex flex-col gap-2 w-full" ]
                    $ Array.NonEmpty.toArray
                    $ breedCard activeId <$> children
                ]
            ]
        (Dog.Breed.BreedLeaf id _) ->
          button
            [ class_
                $ wrap
                $ "text-left rounded-lg flex flex-col p-2 gap-2 "
                    <> "transition-[transform,background] duration-100 "
                    <>
                      ( if Just id == activeId then "bg-orange-300 "
                        else "bg-stone-300 "
                      )
                    <> "hover:bg-orange-300 active:bg-orange-400 "
                    <> "active:scale-[0.975] "
            , onClick \ev -> Variant.X.inj @"clickBreed" (ev /\ id)
            ]
            [ h2
                [ class_ $ wrap $ "text-2xl font-bold text-stone-800" ]
                [ text $ Dog.Breed.idDisplay id ]
            ]
  in
    Variant.match
      { loading:
          const
            $ page
                [ navbar
                    [ div
                        [ class_ $ wrap
                            "h-full w-full flex justify-center items-center"
                        ]
                        [ spinner
                        ]
                    ]
                , contentNoneSelected
                ]
      , loaded: \{ breeds, activeBreed } ->
          page
            [ navbar
                $ map (breedCard activeBreed)
                $ Array.NonEmpty.toArray
                $ breeds

            , fromMaybe contentNoneSelected do
                active <- activeBreed
                offset /\ images <- Dog.Breed.lookup active breeds

                let
                  shiftButton dir =
                    let
                      disabled' =
                        case dir of
                          ShiftLeft -> offset <= 0
                          ShiftRight -> (offset + 1) * 20 >= Set.size images
                      iconClass =
                        case dir of
                          ShiftLeft -> "icon--solar--arrow-left-bold"
                          ShiftRight -> "icon--solar--arrow-right-bold"
                      dirText =
                        case dir of
                          ShiftLeft -> "Back"
                          ShiftRight -> "Next"
                      icon = span
                        [ class_ $ wrap $ "text-6xl icon--solar " <> iconClass ]
                        []
                      text' = h2 [ class_ $ wrap "text-4xl font-bold " ]
                        [ text dirText ]
                      contents' = case dir of
                        ShiftLeft -> [ icon, text' ]
                        ShiftRight -> [ text', icon ]
                    in
                      button
                        [ class_
                            $ wrap
                            $
                              "justify-center items-center grow rounded-lg flex p-2 gap-4 "
                                <>
                                  "transition-[transform,background] duration-100 "
                                <> "text-stone-800 "
                                <> "disabled:bg-stone-200 "
                                <> "disabled:text-stone-400 "
                                <> "disabled:cursor-not-allowed "
                                <> "[&:not(:disabled)]:bg-orange-200 "
                                <> "[&:not(:disabled):hover]:bg-orange-300 "
                                <> "[&:not(:disabled):active]:bg-orange-400 "
                                <> "[&:not(:disabled):active]:scale-[0.975] "
                        , disabled disabled'
                        , onClick \ev -> Variant.X.inj @"clickImageShift"
                            (ev /\ dir)
                        ]
                        contents'

                pure $
                  if Set.size images == 0 then
                    content
                      [ div
                          [ class_ $ wrap
                              "h-full w-full flex items-center justify-center"
                          ]
                          [ spinner ]
                      ]
                  else
                    content
                      [ div
                          [ class_ $ wrap "h-full flex flex-col gap-4 p-4" ]
                          [ h1
                              [ class_ $ wrap
                                  "text-4xl font-black text-stone-800"
                              ]
                              [ text $ Dog.Breed.idDisplay active ]
                          , div [ class_ $ wrap "grow overflow-hidden" ]
                              [ div
                                  [ class_
                                      $ wrap
                                      $ "h-full w-full grid grid-flow-col "
                                          <> "grid-columns-[repeat(4,1fr)] "
                                          <> "grid-rows-[repeat(5,1fr)]"
                                  ]
                                  $ map
                                      ( \url ->
                                          img
                                            [ class_
                                                $ wrap
                                                $ "rounded-lg h-full w-full "
                                                    <> "object-contain"
                                            , src $ URL.toString url
                                            ]
                                      )
                                  $ Array.take 20
                                  $ Array.drop (offset * 20)
                                  $ Array.fromFoldable
                                  $ images
                              ]
                          , div
                              [ class_ $ wrap "w-full p-4 flex gap-4" ]
                              [ shiftButton ShiftLeft
                              , shiftButton ShiftRight
                              ]
                          ]
                      ]
            ]
      }

handleAction ::
  forall slots output m.
  MonadThrow Error m =>
  MonadAff m =>
  Action ->
  HalogenM State Action slots output m Unit
handleAction =
  Variant.match
    { init: const $
        let
          fetchBreeds =
            Dog.API.breeds
              >>= (liftEither <<< lmap error)

          initBreedState ::
            Dog.Breeds Unit -> Dog.Breeds (ImagePageOffset /\ Set ImageURL)
          initBreedState = map $ map $ const $ 0 /\ Set.empty

          putBreeds =
            Halogen.put
              <<< Variant.X.inj @"loaded"
              <<< (\breeds -> { breeds, activeBreed: Nothing })
        in
          ( fetchBreeds
              # minDuration (Milliseconds 1000.0)
              <#> initBreedState
          )
            >>= putBreeds

    , clickBreed: \(ev /\ id) ->
        let
          setActiveBreed =
            Halogen.modify_
              $ Variant.X.modify @"loaded"
              $ Record.X.set @"activeBreed"
              $ Just id
          activeBreedImageCount =
            Halogen.get
              <#> Variant.X.prj @"loaded"
              >>= liftMaybe (error "unreachable")
              <#> Record.X.get @"breeds"
              <#> Dog.Breed.lookup id
              >>= liftMaybe (error "unreachable")
              <#> snd
              <#> Set.size
          fetchImages =
            Dog.API.allImages id
              >>= (liftEither <<< lmap error)
              # liftAff
          putImages =
            Halogen.modify_
              <<< Variant.X.modify @"loaded"
              <<< Record.X.modify @"breeds"
              <<< flip Dog.Breed.put id
              <<< (0 /\ _)
          preventDefault =
            ev
              # Event.Mouse.toEvent
              # Event.preventDefault
              # liftEffect
        in
          preventDefault
            *> setActiveBreed
            *> whenM
              (activeBreedImageCount <#> (_ == 0))
              (fetchImages # minDuration (Milliseconds 1000.0) >>= putImages)

    , clickImageShift: \(ev /\ dir) ->
        let
          modifyPage =
            case dir of
              ShiftLeft -> (_ - 1)
              ShiftRight -> (_ + 1)
          preventDefault =
            ev
              # Event.Mouse.toEvent
              # Event.preventDefault
              # liftEffect
          getActiveBreedId =
            Halogen.get
              <#> Variant.X.prj @"loaded"
              >>= liftMaybe (error "unreachable")
              <#> Record.X.get @"activeBreed"
              >>= liftMaybe (error "unreachable")
          updateImagePage =
            Halogen.modify_
              <<< Variant.X.modify @"loaded"
              <<< Record.X.modify @"breeds"
              <<< Dog.Breed.update (lmap modifyPage)
        in
          preventDefault *> getActiveBreedId >>= updateImagePage
    }

-- | Avoid spinner flashes by adding a minimum amount of time
-- | doing async work `m` takes.
minDuration :: forall m a. MonadAff m => Milliseconds -> m a -> m a
minDuration min m = do
  start <- liftEffect Now.now
  projectedEnd <- Instant.unInstant start # (_ <> min) # Instant.instant
    # liftMaybe (error "unreachable")
    # liftEffect
  a <- m
  end <- liftEffect Now.now
  let delay = projectedEnd `Instant.diff` end
  when (delay > Milliseconds 0.0) (liftAff $ Aff.delay $ delay)
  pure a
