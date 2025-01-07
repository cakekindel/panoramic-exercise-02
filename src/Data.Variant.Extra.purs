module Data.Variant.Extra where

import Prelude

import Control.Alternative (class Alternative)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row (class Cons, class Union)
import Type.Prelude (Proxy(..))

inj :: forall @sym a r1 r2. Cons sym a r1 r2 => IsSymbol sym => a -> Variant r2
inj = V.inj (Proxy @sym)

inj_ :: forall @sym r1 r2. Cons sym Unit r1 r2 => IsSymbol sym => Variant r2
inj_ = V.inj (Proxy @sym) unit

prj ::
  forall @sym a r1 r2 f.
  Cons sym a r1 r2 =>
  IsSymbol sym =>
  Alternative f =>
  Variant r2 ->
  f a
prj = V.prj (Proxy @sym)

on ::
  forall @sym a b r1 r2.
  Cons sym a r1 r2 =>
  IsSymbol sym =>
  (a -> b) ->
  (Variant r1 -> b) ->
  Variant r2 ->
  b
on = V.on (Proxy @sym)

modify ::
  forall @sym a r1 r2 x.
  IsSymbol sym =>
  Cons sym a r1 r2 =>
  Union r1 x r2 =>
  (a -> a) ->
  Variant r2 ->
  Variant r2
modify f = V.overOne (Proxy @sym) f V.expand
