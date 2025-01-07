module Dog.Breed where

import Prelude

import Control.Alternative (empty)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Bifunctor (rmap)
import Data.Foldable
  ( class Foldable
  , all
  , findMap
  , foldMapDefaultL
  , foldl
  , foldr
  , foldrDefault
  )
import Data.FoldableWithIndex
  ( class FoldableWithIndex
  , findWithIndex
  , foldMapWithIndexDefaultL
  , foldlWithIndex
  , foldrWithIndexDefault
  )
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.FunctorWithIndex as Functor.Index
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))

type Breeds a = NonEmptyArray (Breed a)

-- NOTE:
--
-- Another valid representation would have been something like
-- a `Map BreedId (Array a)`, where an empty array would indicate this
-- breed is a leaf node.
--
-- This would allow O(log(n)) time lookups (O(1) if using HashMap)
-- at the cost of less expressive types and more plumbing code to keep
-- the visible API surface respecting the tree-like nature of this data.
--
-- If both the scale of data & importance of expressive type design were high,
-- this could also be converted to a BTree map representation or similar to get
-- the best of both worlds at the cost of code footprint.
data Breed a
  = BreedNode Id a (NonEmptyArray (Breed a))
  | BreedLeaf Id a

id :: forall a. Breed a -> Id
id (BreedNode id' _ _) = id'
id (BreedLeaf id' _) = id'

derive instance Generic (Breed a) _
derive instance Eq a => Eq (Breed a)
derive instance Ord a => Ord (Breed a)
instance Show a => Show (Breed a) where
  show a = genericShow a

instance Functor Breed where
  map f (BreedNode id' a as) = BreedNode id' (f a) (map f <$> as)
  map f (BreedLeaf id' a) = BreedLeaf id' (f a)

instance FunctorWithIndex Id Breed where
  mapWithIndex f (BreedNode id' a as) = BreedNode id' (f id' a)
    (Functor.Index.mapWithIndex f <$> as)
  mapWithIndex f (BreedLeaf id' a) = BreedLeaf id' (f id' a)

instance Foldable Breed where
  foldl f b (BreedNode _ a as) = foldl (foldl f) (f b a) as
  foldl f b (BreedLeaf _ a) = f b a
  foldr f b a = foldrDefault f b a
  foldMap f a = foldMapDefaultL f a

instance FoldableWithIndex Id Breed where
  foldlWithIndex f b (BreedNode id' a as) = foldl (foldlWithIndex f) (f id' b a)
    as
  foldlWithIndex f b (BreedLeaf id' a) = f id' b a
  foldrWithIndex f b a = foldrWithIndexDefault f b a
  foldMapWithIndex f a = foldMapWithIndexDefaultL f a

instance Traversable Breed where
  traverse f (BreedNode id' a as) =
    pure (BreedNode id')
      <*> (f a)
      <*> (traverse (traverse f) as)
  traverse f (BreedLeaf id' a) = BreedLeaf id' <$> f a
  sequence = sequenceDefault

-- | Find the data associated with the given id
lookup :: forall a. Id -> Breeds a -> Maybe a
lookup id' breeds =
  map _.value
    $ flip findMap breeds
    $ findWithIndex
    $ \id'' _ -> id'' == id'

-- | Modify the data associated with the given id
update :: forall a. (a -> a) -> Id -> Breeds a -> Breeds a
update f id' = mapWithIndex $ \id'' a -> if id'' == id' then f a else a

-- | Put a new state for the given id
put :: forall a. a -> Id -> Breeds a -> Breeds a
put a = update (const a)

-- | Modify the data associated with the given id
mapWithIndex :: forall a b. (Id -> a -> b) -> Breeds a -> Breeds b
mapWithIndex f = map $ Functor.Index.mapWithIndex f

-- | Insert an breed node into a tree of nodes.
-- |
-- | This function will insert the node into:
-- | 1. a new category from the deepest leaf node that is a direct ancestor
-- | 1. the deepest category node that is a direct ancestor
-- | 1. the tree as a new root node
-- |
-- | ### Examples
-- | **Syntax**
-- | * `a`: a leaf
-- | * `a > b`: a category with 1 child `b`
-- | * `a > [b, c]`: a category with 2 children.*
-- |
-- | ```text
-- | |---------------------|------------------------|-------------------------------|
-- | | Inserting...        | Into...                | Results in...                 |
-- | |---------------------|------------------------|-------------------------------|
-- | |  a > b > c > d > e  |  [a > b > c]           |  [a > b > c > d > e]          |
-- | |  a > d              |  [a > b > c]           |  [a > [b > c, d]]             |
-- | |  a > d > e          |  [a > b > c]           |  [a > [b > c, d > e]]         |
-- | |  a > b > c > f > g  |  [a > b > c > [d, e]]  |  [a > b > c > [d, e, f > g]]  |
-- | |  c                  |  [a > b]               |  [a > b, c]                   |
-- | |---------------------|------------------------|-------------------------------|
insert :: forall a. Id -> a -> Breeds a -> Breeds a
insert insertId insertData breeds =
  let
    -- given a (maybe) source id `from`, generate as deep a tree of singleton
    -- nodes as necessary until reaching the final id segment, placing it in a leaf.
    toInsert from =
      fromMaybe (BreedLeaf insertId insertData)
        $ do
            ids <- Array.NonEmpty.fromArray $ walk from insertId
            foldr
              ( \id' -> Just <<< maybe (BreedLeaf id' insertData)
                  (BreedNode id' insertData <<< pure)
              )
              Nothing
              ids

    insert' :: Maybe Id -> Breeds a -> Breeds a
    insert' from as =
      let
        appendIfNotInserted (true /\ as') = as'
        appendIfNotInserted (false /\ _) = as <> pure (toInsert from)

        foldTryInsert (inserted /\ as') = rmap (append as' <<< pure) <<<
          tryInsertInNode inserted

        maybeInserted =
          rmap (fromMaybe as <<< Array.NonEmpty.fromArray)
            $ foldl foldTryInsert (false /\ []) as
      in
        appendIfNotInserted maybeInserted

    tryInsertInNode :: Boolean -> Breed a -> Boolean /\ Breed a
    tryInsertInNode true a = true /\ a
    tryInsertInNode false b@(BreedLeaf id' a)
      | id' == insertId = true /\ b
      | insertId `isDirectDescendantOf` id' =
          true /\ BreedNode id' a (pure $ toInsert $ Just id')
      | otherwise = false /\ b
    tryInsertInNode false b@(BreedNode id' a as)
      | id' == insertId = true /\ b
      | insertId `isDirectDescendantOf` id' =
          true /\ BreedNode id' a (insert' (Just id') as)
      | otherwise = false /\ b
  in
    insert' Nothing breeds

-- | Construct a `Breeds` tree from a set of breed ids
-- |
-- | ### Examples
-- | **Syntax**
-- | * `a`: a leaf
-- | * `a > b`: a category with 1 child `b`
-- | * `a > [b, c]`: a category with 2 children.*
-- |
-- | `[a]` -> `[a]`
-- | `[a>b, a>c]` -> `[a>[b,c]]`
-- | `[a>b, a>b>d, a>c]` -> `[a>[b>d,c]]`
fromIds :: NonEmptyArray Id -> Breeds Unit
fromIds ids =
  let
    breeds = flip BreedLeaf unit <$> ids
    init = pure $ Array.NonEmpty.head breeds
  in
    foldl (\bs id' -> insert id' unit bs) init ids

newtype Id = Id (NonEmptyArray String)

derive instance Generic Id _
derive instance Newtype Id _
derive newtype instance Eq Id
derive newtype instance Ord Id
instance Show Id where
  show = genericShow

-- | Given ids `a` and `b`, find the longest shared prefix between
-- | the two.
-- |
-- | If the ids are equal, returns `Nothing`.
-- |
-- | ex:
-- | ```purescript
-- | commonAncestor [a]          [a, b, c, d] == Just [a]
-- | commonAncestor [a, b]       [a, b, c, d] == Just [a, b]
-- | commonAncestor [a, b, c]    [a, b, c, d] == Just [a, b, c]
-- | commonAncestor [a, b, c, e] [a, b, c, d] == Just [a, b, c]
-- | commonAncestor []           []           == Nothing
-- | commonAncestor [a]          [a]          == Nothing
-- | commonAncestor [b]          [a, b]       == Nothing
-- | commonAncestor []           [a, b]       == Nothing
-- | ```
commonAncestor :: Id -> Id -> Maybe Id
commonAncestor (Id as) (Id bs) =
  map wrap
    $ Array.NonEmpty.fromArray
    $ map fst
    $ Array.NonEmpty.takeWhile (\(a /\ b) -> a == b)
    $ Array.NonEmpty.zip as bs

-- | Given ids `a` and `b`, if `a` is a prefix of `b`, yields the set of intermediate
-- | ids walking from the ancestor node to the child node.
-- |
-- | ex:
-- | ```purescript
-- | walk []           [a, b, c, d] == [[a], [a, b], [a, b, c], [a, b, c, d]]
-- | walk [a]          [a, b, c, d] == [[a, b], [a, b, c], [a, b, c, d]]
-- | walk [a, b]       [a, b, c, d] == [[a, b, c], [a, b, c, d]]
-- | walk [a, b, c]    [a, b, c, d] == [[a, b, c]]
-- | walk [a, b, c, e] [a, b, c, d] == []
-- | walk []           []           == []
-- | walk [a]          [a]          == []
-- | walk [b]          [a, b]       == []
-- | walk []           [a, b]       == []
-- | ```
walk :: Maybe Id -> Id -> Array Id
walk mfrom b@(Id bs) =
  let
    commonCount = fromMaybe 0 do
      from <- mfrom
      common <- commonAncestor from b
      pure $ Array.NonEmpty.length $ unwrap common
  in
    if maybe false (flip isDirectDescendantOf b) mfrom then
      []
    else
      Array.range (commonCount + 1) (Array.NonEmpty.length bs)
        <#> (\to -> Array.NonEmpty.slice 0 to bs)
        <#> Array.NonEmpty.fromArray
        # Array.catMaybes
        <#> Id

-- | Is the first breed a direct descendant of the second?
isDirectDescendantOf :: Id -> Id -> Boolean
isDirectDescendantOf (Id as) (Id bs) =
  (Array.NonEmpty.length as > Array.NonEmpty.length bs)
    && all (\(a /\ b) -> a == b) (Array.NonEmpty.zip as bs)

-- | Construct a breed id from segments
idFromSegments :: NonEmptyArray String -> Id
idFromSegments = wrap <<< map (String.trim <<< String.toLower)

-- | Parse a breed id from a string with hyphen-separated hierarchical segments
idFromString :: String -> Maybe Id
idFromString =
  map idFromSegments
    <<< Array.NonEmpty.fromArray
    <<< String.split (String.Pattern "-")
    <<< String.trim

-- | Concatenate id segments with hyphens `-`
idToString :: Id -> String
idToString = Array.NonEmpty.intercalate "-" <<< unwrap

-- | Human-readable display string from id
idDisplay :: Id -> String
idDisplay id' = fromMaybe (idDisplay' $ unwrap id') (idDisplayOverride id')

-- | Convert id to URL path segments
idToPathSegments :: Id -> String
idToPathSegments (Id as) = Array.NonEmpty.intercalate "/" as

idDisplay' :: NonEmptyArray String -> String
idDisplay' =
  let
    capitalizeChars cs =
      fromMaybe []
        $ pure (\head tail -> [ String.toUpper head ] <> tail)
            <*> Array.head cs
            <*> Array.tail cs
    capitalize = Array.intercalate "" <<< capitalizeChars <<< String.split
      (String.Pattern "")
  in
    Array.NonEmpty.intercalate " " <<< Array.NonEmpty.reverse <<< map capitalize

idDisplayOverride :: Id -> Maybe String
idDisplayOverride (Id as) =
  if as == pure "danish" <> pure "swedish" then
    pure "Danish-Swedish Farm Dog"
  else if as == pure "pointer" <> pure "germanlonghair" then
    pure "German Longhaired Pointer"
  else if Array.NonEmpty.head as == "australian" then
    pure $ idDisplay' $ Array.NonEmpty.reverse as
  else
    empty
