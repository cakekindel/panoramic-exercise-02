module ACMECorp.Dog.Web.Main where

import Prelude hiding (div)

import Control.Monad.Error.Class (class MonadThrow, liftEither, liftMaybe)
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Bifunctor (lmap)
import Data.DateTime.Instant as Instant
import Data.Either (Either, either)
import Data.Maybe (Maybe(..), isNothing)
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

-- STYLE:
-- Using Variants instead of named sum types for state, actions, routing, queries, etc.
--
-- If this is the first time you're seeing Variant, it is the sum type dual to
-- Records; a value of type `Record (foo :: String, bar :: String)` has ALL
-- fields of the row type passed to `Record`, where `Variant (foo :: String, bar :: String)`
-- has exactly ONE.
--
-- Using Variants for FRP state & actions is EXCELLENT for reducing
-- constructor name clutter and extensibility.
--
-- Adding a variant is as (or more) simple than a sum type, with the benefit
-- of not having to worry about name collisions.
--
-- This comes with costs, though:
-- - A new tool to learn with its own hairy edges
-- - Much more obscure type errors
-- - A little boilerplate when constructing and extracting the variants
-- - Variants that don't carry data still need to carry `Unit`, and handling them needs `const`
type State = Variant
  ( loading :: Unit
  , loaded ::
      { breeds :: Dog.Breeds (ImagePageOffset /\ Maybe (Set ImageURL))
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

-- STYLE:
-- A constant negotiation that needs to happen ESPECIALLY
-- in the rendering body of FRP components (not just halogen)
-- is "is this doing too much?"
--
-- The nature of HTML & CSS is, unfortunately, a very tightly
-- coupled layout hierachy that resists componentization to some
-- degree. I've chosen in this case to keep the body of `render`
-- as a single, very large, HTML expression rather than break it up.
--
-- In my experience, the Halogen philosophy of "Components are **state** boundaries"
-- helps a lot in preventing excessive carving of templates to the point of having to
-- track down hidden template dependencies across many files to debug layout issues
-- (looking at you, React.)
--
-- To my eye, this is around my upper limit for complexity of a single HTML expression
-- and would have organized things a bit differently in a production codebase.
--
-- Namely:
-- - Break common atomic components into separate (pure if possible) modules
--   to reduce styling clutter, eg. `ACMECorp.Dog.Web.Atom.Button`
-- - Have this Main module **JUST** be the routing & state entry point,
--   deferring to `Page` components based on route.
-- - Nested `let .. in ..` is a smell to me that this should be a top-level
--   function or simplified.
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
                , content []
                ]
      , loaded: \{ breeds, activeBreed } ->
          let
            navbarBreedCard activeId =
              let
                nodeWrapper =
                  div
                    [ class_ $ wrap $ "flex flex-col gap-1 w-full" ]
                nodeSubtitle id =
                  h2
                    [ class_ $ wrap $ "text-stone-500 text-xl font-bold" ]
                    [ text $ Dog.Breed.idDisplay id ]
                nodeIndentRow children =
                  div
                    [ class_ $ wrap $ "flex flex-row gap-2 w-full pl-1" ]
                    $ Array.concat
                        [ [ div [ class_ $ wrap "h-full w-[4px] bg-stone-400" ]
                              []
                          ]
                        , children
                        ]
                nodeChildren children =
                  div
                    [ class_ $ wrap "flex flex-col gap-2 w-full" ]
                    $ Array.NonEmpty.toArray
                    $ navbarBreedCard activeId <$> children
                leafButton id =
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
                case _ of
                  (Dog.Breed.BreedNode id _ children) ->
                    nodeWrapper
                      [ nodeSubtitle id
                      , nodeIndentRow [ nodeChildren children ]
                      ]
                  (Dog.Breed.BreedLeaf id _) -> leafButton id

            contentBreedLoading =
              content
                [ div
                    [ class_ $ wrap
                        "h-full w-full flex items-center justify-center"
                    ]
                    [ spinner ]
                ]

            contentChooseBreed =
              div
                [ class_ $ wrap "h-full w-full flex justify-center items-center"
                ]
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
                            [ text
                                "Select a breed on the left to view pictures!"
                            ]
                        ]
                    ]
                ]

            contentBreedImages activeBreed' offset images =
              let
                shiftButton dir =
                  let
                    disabled' ShiftLeft = offset <= 0
                    disabled' ShiftRight = (offset + 1) * 20 >= Set.size images

                    iconClass ShiftLeft = "icon--solar--arrow-left-bold"
                    iconClass ShiftRight = "icon--solar--arrow-right-bold"

                    textWrapper = h2 [ class_ $ wrap "text-4xl font-bold" ]
                      <<< pure
                      <<< text

                    text' ShiftLeft = textWrapper "Back"
                    text' ShiftRight = textWrapper "Next"

                    icon =
                      span
                        [ class_ $ wrap $ "text-6xl icon--solar " <> iconClass
                            dir
                        ]
                        []

                    contents' ShiftLeft = [ icon, text' dir ]
                    contents' ShiftRight = [ text' dir, icon ]
                  in
                    button
                      [ class_
                          $ wrap
                          $
                            "justify-center items-center grow rounded-lg "
                              <> "flex p-2 gap-4 duration-100 "
                              <> "transition-[transform,background] "
                              <> "text-stone-800 "
                              <> "disabled:bg-stone-200 "
                              <> "disabled:text-stone-400 "
                              <> "disabled:cursor-not-allowed "
                              <> "[&:not(:disabled)]:bg-orange-200 "
                              <> "[&:not(:disabled):hover]:bg-orange-300 "
                              <> "[&:not(:disabled):active]:bg-orange-400 "
                              <> "[&:not(:disabled):active]:scale-[0.975] "
                      , disabled $ disabled' dir
                      , onClick \ev -> Variant.X.inj @"clickImageShift"
                          (ev /\ dir)
                      ]
                      (contents' dir)
                images' =
                  Array.take 20
                    $ Array.drop (offset * 20)
                    $ Array.fromFoldable
                    $ images

                imageGrid children =
                  div [ class_ $ wrap "grow overflow-hidden" ]
                    [ div
                        [ class_
                            $ wrap
                            $ "h-full w-full grid grid-flow-col "
                                <> "grid-columns-[repeat(4,1fr)] "
                                <> "grid-rows-[repeat(5,1fr)]"
                        ]
                        children
                    ]

                image url =
                  img
                    [ class_
                        $ wrap
                        $ "rounded-lg h-full w-full "
                            <> "object-contain"
                    , src $ URL.toString url
                    ]

                title =
                  h1
                    [ class_ $ wrap
                        "text-4xl font-black text-stone-800"
                    ]
                    [ text $ Dog.Breed.idDisplay activeBreed' ]
                shiftControls =
                  div
                    [ class_ $ wrap "w-full p-4 flex gap-4" ]
                    [ shiftButton ShiftLeft
                    , shiftButton ShiftRight
                    ]
              in
                div
                  [ class_ $ wrap "h-full flex flex-col gap-4 p-4" ]
                  [ title
                  , imageGrid $ image <$> images'
                  , shiftControls
                  ]

            -- STYLE:
            -- Several dependent short-circuits here that want to exit early with
            -- varying content. I chose a pure Either do expression, but could
            -- easily be written as an applicative expression similar to the
            -- body of `handleAction`.
            content' =
              content
                $ pure
                $
                  ( either identity identity ::
                      Either (HTML _ _) (HTML _ _) -> HTML _ _
                  )
                $ do
                    active <- liftMaybe contentChooseBreed activeBreed
                    offset /\ images' <-
                      liftMaybe contentChooseBreed
                        $ Dog.Breed.lookup active breeds
                    images <- liftMaybe contentBreedLoading images'
                    pure $ contentBreedImages active offset images
          in
            page
              [ navbar
                  $ map (navbarBreedCard activeBreed)
                  $ Array.NonEmpty.toArray
                  $ breeds
              , content'
              ]
      }

-- STYLE:
-- At first this was written as a do block for handling each action,
-- but I refactored it at some point to use point-free applicative
-- syntax with named locals. This is a constant negotiation that happens
-- in my head as I'm writing, "would this be more expressive point free or
-- imperative?" and to my eye this is EXTREMELY expressive compared to before.
--
-- I would not block a pull request that had written this as do blocks, though :)
handleAction ::
  forall slots output m.
  MonadThrow Error m =>
  MonadAff m =>
  Action ->
  HalogenM State Action slots output m Unit
handleAction =
  -- STYLE:
  -- You may notice I mix LTR and RTL composition (e.g. <$> and <#>) occasionally.
  --
  -- This is a result of experience; I prefer RTL composition generally
  -- (`$`, `<$>`, `=<<`, `<<<`), however when writing a point-free expression mixing
  -- map and bind, RTL map `<$>` forcibly groups binds together:
  --
  -- `a =<< b <$> c =<< d` is equivalent to `(a =<< b) <$> (c =<< d)`
  -- whereas
  -- `d >>= c <#> b >>= a` is equivalent to `((d >>= c) <#> b) >>= a`
  --
  -- The latter lets me write point-free monadic chains that express
  -- the dependency on previous terms without writing parenthesis.
  --
  -- In general I will use RTL composition for pure expressions (because this
  -- preserves the order that you'd write the expressions with parenthesis) and
  -- LTR for monadic expressions to express the "and-then" nature of those expressions.
  Variant.match
    { init: const $
        let
          fetchBreeds =
            Dog.API.breeds
              >>= (liftEither <<< lmap error)

          initBreedState ::
            Dog.Breeds Unit ->
            Dog.Breeds (ImagePageOffset /\ Maybe (Set ImageURL))
          initBreedState = map $ map $ const $ 0 /\ Nothing

          putBreeds =
            Halogen.put
              <<< Variant.X.inj @"loaded"
              <<< (\breeds -> { breeds, activeBreed: Nothing })
        in
          ( minDuration (Milliseconds 1000.0) fetchBreeds
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
          shouldLoad =
            Halogen.get
              <#> Variant.X.prj @"loaded"
              >>= liftMaybe (error "unreachable")
              <#> Record.X.get @"breeds"
              <#> Dog.Breed.lookup id
              >>= liftMaybe (error "unreachable")
              <#> snd
              <#> isNothing
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
              <<< Just
          preventDefault =
            ev
              # Event.Mouse.toEvent
              # Event.preventDefault
              # liftEffect
        in
          preventDefault
            *> setActiveBreed
            *> whenM
              shouldLoad
              (minDuration (Milliseconds 1000.0) fetchImages >>= putImages)

    , clickImageShift: \(ev /\ dir) ->
        let
          incrOrDecr =
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
          updatePage =
            Halogen.modify_
              <<< Variant.X.modify @"loaded"
              <<< Record.X.modify @"breeds"
              <<< Dog.Breed.update (lmap incrOrDecr)
        in
          preventDefault *> getActiveBreedId >>= updatePage
    }

-- | Avoid spinner flashes by adding a minimum duration for async work `m`.
minDuration :: forall m a. MonadAff m => Milliseconds -> m a -> m a
minDuration min m = do
  start <- liftEffect Now.now
  projectedEnd <-
    Instant.unInstant start
      # (_ <> min)
      # Instant.instant
      # liftMaybe (error "unreachable")
      # liftEffect
  a <- m
  end <- liftEffect Now.now
  let delay = projectedEnd `Instant.diff` end
  when (delay > Milliseconds 0.0) (liftAff $ Aff.delay $ delay) $> a
