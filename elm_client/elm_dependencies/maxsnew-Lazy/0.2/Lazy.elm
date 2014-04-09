module Lazy ( force, lazy, map, apply, bind
            ) where

{-| Library for Lazy evaluation.

# Delay
@docs lazy

# Evaluate
@docs force

# Transform
@docs map, apply, bind
-}

data Lazy a = L { force : () -> a }

{-| Delay execution of a value -}
lazy : (() -> a) -> Lazy a
lazy t = L { force = t }

{-| Execute a lazy value. -}
force : Lazy a -> a
force (L r) = r.force ()

{-| Lazily apply a pure function to a lazy value. -}
map : (a -> b) -> Lazy a -> Lazy b
map f t = lazy <| \() ->
  f . force <| t

{-| Lazily apply a Lazy function to a Lazy value. -}
apply : Lazy (a -> b) -> Lazy a -> Lazy b
apply f x = lazy <| \() ->
  (force f) (force x)

{-| Lazily chain together Lazy computations. -}
bind : Lazy a -> (a -> Lazy b) -> Lazy b
bind x k = lazy <| \() ->
  force . k . force <| x
