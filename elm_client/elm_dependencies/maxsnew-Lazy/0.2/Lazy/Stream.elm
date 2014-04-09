module Lazy.Stream ( head, tail
                   , cons, cons', iterate, unfold, repeat, cycle
                   , map, apply, zip, zipWith, scanl
                   , take, drop, splitAt
                   , sampleOn
                   , filter, takeWhile, dropWhile, splitWith
                   ) where

{-| Library for Infinite Streams 

# Create
@docs cons, cons', iterate, unfold, repeat, cycle

# Observe
@docs head, tail, sampleOn, take, drop, splitAt

# Transform
@docs map, apply, zip, zipWith, scanl

# Converge
@docs filter, takeWhile, dropWhile, splitWith

-}

import Lazy (..)
import Signal ((<~), foldp, Signal)

data Stream a = S (Lazy (a, Stream a))

unS : Stream a -> (a, Stream a)
unS (S t) = force t

{-| Compute the first element of a Stream -}
head : Stream a -> a
head = fst . unS

{-| Compute the next piece of a Stream -}
tail : Stream a -> Stream a
tail = snd . unS

{-| Create a Stream -}
cons : a -> (() -> Stream a) -> Stream a
cons x txs = let mtxs = lazy txs in
  S . lazy <| \() ->
  (x, force mtxs)

{-| Lazily create a Stream. -}
cons' : (() -> (a, Stream a)) -> Stream a
cons' = S . lazy

{-| Create a stream of repeated applications of f to x:

    iterate f x = S.cons x (\() -> S.cons (f x) (\() -> S.cons (f (f x)) ...))
-}
iterate : (a -> a) -> a -> Stream a
iterate f x = cons' <| \() ->
  (x, iterate f (f x))

{-| Build a stream from a seed value. -}
unfold : (b -> (a, b)) -> b -> Stream a
unfold f s = let loop s = 
                   cons' <| \() ->
                   let (hd, s') = f s
                   in (hd, loop s')
             in loop s

{-| Create an infinite Stream where every head is the same. -}
repeat : a -> Stream a
repeat x = let go = cons x <| \() -> go
           in go

{-| Cycle through the elements of a nonempty list. -}           
cycle : a -> [a] -> Stream a
cycle x xs = let cycle' ys = case ys of
                   [] -> go
                   (y :: ys) -> cons y <| \() ->
                     cycle' ys
                 go = cons' <| \() ->
                   (x, cycle' xs)
             in go

{-| Apply a function to every element of a Stream. -}
map : (a -> b) -> Stream a -> Stream b
map f xs = cons' <| \() ->
  (f (head xs), map f (tail xs))

{-| Pairwise apply a stream of functions to a stream of inputs.
    Together with map, generalizes zipWith to n arguments.

    zipWith2 : (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
    zipWith2 f xs ys zs = f `S.map` xs `S.apply` ys `S.apply` zs
-}
apply : Stream (a -> b) -> Stream a -> Stream b
apply fs xs = zipWith (<|) fs xs

{-| Combine two streams, combining them into tuples pairwise. -}              
zip : Stream a -> Stream b -> Stream (a, b)
zip = zipWith (\x y -> (x,y))

{-| Combine two streams, combining them with the given function. -}
zipWith : (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f xs ys = cons' <| \() ->
  (f (head xs) (head ys),
   zipWith f (tail xs) (tail ys))

{-| Reduce a Stream from the left, building an infinite stream of reductions. -}
scanl : (a -> b -> b) -> b -> Stream a -> Stream b
scanl f init xs = cons' <| \() ->
  (init,
   scanl f (f (head xs) init) (tail xs))

{-| Compute the first n elements of a stream into a list. -}
take : Int -> Stream a -> [a]
take n xs = fst <| splitAt n xs

{-| Drop n elements from the front of a stream. -}
drop : Int -> Stream a -> Stream a
drop n xs = snd <| splitAt n xs

{-| Combination of take and drop.

    splitAt n xs == (take n xs, drop n xs)
-}
splitAt : Int -> Stream a -> ([a], Stream a)
splitAt n xs = case n of
  0 -> ([], xs)
  n -> let (heads, end) = splitAt (n - 1) (tail xs)
       in (head xs :: heads, end)

{-| Turn a stream into a signal of the elements of the stream,
    advancing through the stream whenever an event in the given signal is
    fired.
-}
sampleOn : Signal b -> Stream a -> Signal a
sampleOn sig str = let tails = foldp (\_ -> tail) str sig in
                   head <~ tails

{-| Filter the elements of a Stream according to a predicate.
    The produced Stream will go into an infinite loop under head or tail
    unless there are infinitely many terms in the Stream that satisfy the
    predicate.
-}
filter : (a -> Bool) -> Stream a -> Stream a
filter p xs = cons' <| (\() ->
  let (hd, tl) = unS xs in
  case p hd of
    True ->  (hd, filter p tl)
    False -> unS <| filter p tl)

{-| Take the longest prefix of a Stream for which a predicate holds on every element.
    This function only terminates if there is an element of the stream for
    which the predicate does not hold.
-}
takeWhile : (a -> Bool) -> Stream a -> [a]
takeWhile p xs = fst <| splitWith p xs

{-| Take the first tail of a Stream for which a predicate does not
    hold on its head. This function only terminates if there is an element
    of the stream for which the predicate does not hold.
-}
dropWhile : (a -> Bool) -> Stream a -> Stream a
dropWhile p xs = snd <| splitWith p xs

{-| Split a Stream into its longest prefix for which a predicate holds and
    the rest of the Stream. This function only terminates if there is an element
    of the stream for which the predicate does not hold.

    splitWith p xs == (takeWhile p xs, dropWhile p xs)
-}
splitWith : (a -> Bool) -> Stream a -> ([a], Stream a)
splitWith p xs = let (hd, tl) = unS xs in
  case p hd of
    True  -> let (taken, dropped) = splitWith p tl
             in (hd :: taken, dropped)
    False -> ([], xs)
