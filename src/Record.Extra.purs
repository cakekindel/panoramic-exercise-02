-- | I am allergic to writing explicit proxy constructors
-- | now that visible type application exists <3
module Record.Extra where

import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons, class Lacks)
import Record as R
import Type.Prelude (Proxy(..))

-- | Wrapper of `Record.insert` using visible type application instead of `Proxy`.
insert ::
  forall @l @r1 @r2 a.
  IsSymbol l =>
  Lacks l r1 =>
  Cons l a r1 r2 =>
  a ->
  Record r1 ->
  Record r2
insert = R.insert (Proxy @l)

-- | Wrapper of `Record.modify` using visible type application instead of `Proxy`.
modify ::
  forall @l @r1 @r2 r a b.
  IsSymbol l =>
  Cons l a r r1 =>
  Cons l b r r2 =>
  (a -> b) ->
  Record r1 ->
  Record r2
modify = R.modify (Proxy @l)

-- | Wrapper of `Record.set` using visible type application instead of `Proxy`.
set ::
  forall @l r1 r2 r a b.
  IsSymbol l =>
  Cons l a r r1 =>
  Cons l b r r2 =>
  b ->
  Record r1 ->
  Record r2
set = R.set (Proxy @l)

-- | Wrapper of `Record.get` using visible type application instead of `Proxy`.
get ::
  forall @l r r' a.
  IsSymbol l =>
  Cons l a r' r =>
  Record r ->
  a
get = R.get (Proxy @l)

-- | Wrapper of `Record.delete` using visible type application instead of `Proxy`.
delete ::
  forall r1 r2 @l a.
  IsSymbol l =>
  Lacks l r1 =>
  Cons l a r1 r2 =>
  Record r2 ->
  Record r1
delete = R.delete (Proxy @l)

-- | Wrapper of `Record.rename` using visible type application instead of `Proxy`.
rename ::
  forall @prev @next ty input inter output.
  IsSymbol prev =>
  IsSymbol next =>
  Cons prev ty inter input =>
  Lacks prev inter =>
  Cons next ty inter output =>
  Lacks next inter =>
  Record input ->
  Record output
rename = R.rename (Proxy @prev) (Proxy @next)
