{-# LANGUAGE FlexibleInstances, AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables #-}

module Stream (
    false, true, (&&), (||), not,
    (<), (>), (<=), (>=), (==),
    (+), (-), (*), negate, signum, abs, if', filter',
    Stream, Int, Bool, input, constinput, skip, output, foreach, group, groupN, (>>>),
    execute, compile, compileToFile) where

import Data.List hiding (group)
import Prelude hiding ((<),(<=),(>),(>=),(==),(&&),(||),not)
import qualified Prelude

import Stream.AbstractSyntax
import Stream.Interpreter
import Stream.Compiler

instance Num (Elem Int) where
    a + b = App (App (Symbol "+" (+)) a) b
    a - b = App (App (Symbol "-"(-)) a) b
    a * b = App (App (Symbol "*" (*)) a) b
    negate a = App (Symbol "negate" negate) a
    signum a = App (Symbol "signum" signum) a
    abs a = App (Symbol "abs" abs) a
    fromInteger a = Symbol (show a) (fromInteger a)

infix 4 <
(<) :: Elem Int -> Elem Int -> Elem Bool
a < b = App (App (Symbol "<" (Prelude.<)) a) b

infix 4 <=
(<=) :: Elem Int -> Elem Int -> Elem Bool
a <= b = App (App (Symbol "<=" (Prelude.<=)) a) b

infix 4 >
(>) :: Elem Int -> Elem Int -> Elem Bool
a > b = App (App (Symbol ">" (Prelude.>)) a) b

infix 4 >=
(>=) :: Elem Int -> Elem Int -> Elem Bool
a >= b = App (App (Symbol ">=" (Prelude.>=)) a) b

infix 5 ==
(==) :: (StreamType a) => Elem a -> Elem a -> Elem Bool
a == b = App (App (Symbol "==" (Prelude.==)) a) b

false :: Elem Bool
false = Symbol "false" False

true :: Elem Bool
true = Symbol "true" True

infixr 3 &&
(&&) :: Elem Bool -> Elem Bool -> Elem Bool
a && b = App (App (Symbol "&&" (Prelude.&&)) a) b

infixr 2 ||
(||) :: Elem Bool -> Elem Bool -> Elem Bool
a || b = App (App (Symbol "||" (Prelude.||)) a) b

not :: Elem Bool -> Elem Bool
not a = App (Symbol "!" Prelude.not) a

input :: (StreamType a) => Stream a
input = Input

constinput :: (StreamType a) => a -> Stream a
constinput = ConstInput

output :: (StreamType a) => Stream a -> Stream Void
output = Output

skip :: (StreamType a) =>
    Stream a -> Stream a
skip = Skip

cond :: Bool -> a -> a -> a
cond True  a _ = a
cond False _ b = b

if' :: Elem Bool -> Elem Int -> Elem Int -> Elem Int
if' c a b = App (App (App (Symbol "if'" (cond)) c) a) b

filter' :: (StreamType a) =>
    (Elem a -> Elem Bool) -> Stream a -> Stream a
filter' = Filter

foreach :: (StreamType a, StreamType b) =>
    (Elem a -> Elem b) -> Stream a -> Stream b
foreach = ForEach

group :: (StreamType a, StreamType b) =>
    Int -> Elem b -> (Elem b -> Elem a -> Elem b) -> Stream a -> Stream b
group = Group

groupN :: Elem Int -> (Elem Int -> Elem Int -> Elem Int) -> Stream Int -> Stream Int
groupN = GroupN

infixl 1 >>>
(>>>) :: (StreamType a, StreamType b) =>
    Stream a -> (Stream a -> Stream b) -> Stream b
str1 >>> str2 = str2 str1
