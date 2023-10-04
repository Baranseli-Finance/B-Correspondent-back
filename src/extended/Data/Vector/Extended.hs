module Data.Vector.Extended
  ( module Data.Vector,
    unzip7,
    unzip8,
    unzip9,
    unzip10,
    unzip11,
    unzip12,
    unzip13,
    unzip14,
    unzip15,
    unzip16,
    unzip17
  )
where

import Data.Vector
import Data.Vector.Generic hiding (Vector)
import Prelude hiding (map)

unzip7 :: Vector (a, b, c, d, e, f, g) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f, Vector g)
unzip7 xs =
  ( Data.Vector.Generic.map (\(a, _, _, _, _, _, _) -> a) xs,
    Data.Vector.Generic.map (\(_, b, _, _, _, _, _) -> b) xs,
    Data.Vector.Generic.map (\(_, _, c, _, _, _, _) -> c) xs,
    Data.Vector.Generic.map (\(_, _, _, d, _, _, _) -> d) xs,
    Data.Vector.Generic.map (\(_, _, _, _, e, _, _) -> e) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, f, _) -> f) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, g) -> g) xs
  )

unzip8 :: Vector (a, b, c, d, e, f, g, h) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f, Vector g, Vector h)
unzip8 xs =
  ( Data.Vector.Generic.map (\(a, _, _, _, _, _, _, _) -> a) xs,
    Data.Vector.Generic.map (\(_, b, _, _, _, _, _, _) -> b) xs,
    Data.Vector.Generic.map (\(_, _, c, _, _, _, _, _) -> c) xs,
    Data.Vector.Generic.map (\(_, _, _, d, _, _, _, _) -> d) xs,
    Data.Vector.Generic.map (\(_, _, _, _, e, _, _, _) -> e) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, f, _, _) -> f) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, g, _) -> g) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, h) -> h) xs
  )

unzip9 :: Vector (a, b, c, d, e, f, g, h, k) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f, Vector g, Vector h, Vector k)
unzip9 xs =
  ( Data.Vector.Generic.map (\(a, _, _, _, _, _, _, _, _) -> a) xs,
    Data.Vector.Generic.map (\(_, b, _, _, _, _, _, _, _) -> b) xs,
    Data.Vector.Generic.map (\(_, _, c, _, _, _, _, _, _) -> c) xs,
    Data.Vector.Generic.map (\(_, _, _, d, _, _, _, _, _) -> d) xs,
    Data.Vector.Generic.map (\(_, _, _, _, e, _, _, _, _) -> e) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, f, _, _, _) -> f) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, g, _, _) -> g) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, h, _) -> h) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, k) -> k) xs
  )

unzip10 :: Vector (a, b, c, d, e, f, g, h, k, l) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f, Vector g, Vector h, Vector k, Vector l)
unzip10 xs =
  ( Data.Vector.Generic.map (\(a, _, _, _, _, _, _, _, _, _) -> a) xs,
    Data.Vector.Generic.map (\(_, b, _, _, _, _, _, _, _, _) -> b) xs,
    Data.Vector.Generic.map (\(_, _, c, _, _, _, _, _, _, _) -> c) xs,
    Data.Vector.Generic.map (\(_, _, _, d, _, _, _, _, _, _) -> d) xs,
    Data.Vector.Generic.map (\(_, _, _, _, e, _, _, _, _, _) -> e) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, f, _, _, _, _) -> f) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, g, _, _, _) -> g) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, h, _, _) -> h) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, k, _) -> k) xs,
     Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, l) -> l) xs
  )

unzip11 :: Vector (a, b, c, d, e, f, g, h, k, l, a1) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f, Vector g, Vector h, Vector k, Vector l, Vector a1)
unzip11 xs =
  ( Data.Vector.Generic.map (\(a, _, _, _, _, _, _, _, _, _, _) -> a) xs,
    Data.Vector.Generic.map (\(_, b, _, _, _, _, _, _, _, _, _) -> b) xs,
    Data.Vector.Generic.map (\(_, _, c, _, _, _, _, _, _, _, _) -> c) xs,
    Data.Vector.Generic.map (\(_, _, _, d, _, _, _, _, _, _, _) -> d) xs,
    Data.Vector.Generic.map (\(_, _, _, _, e, _, _, _, _, _, _) -> e) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, f, _, _, _, _, _) -> f) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, g, _, _, _, _) -> g) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, h, _, _, _) -> h) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, k, _, _) -> k) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, l, _) -> l) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, a1) -> a1) xs
  )

unzip12 :: Vector (a, b, c, d, e, f, g, h, k, l, a1, b1) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f, Vector g, Vector h, Vector k, Vector l, Vector a1, Vector b1)
unzip12 xs =
  ( Data.Vector.Generic.map (\(a, _, _, _, _, _, _, _, _, _, _, _) -> a) xs,
    Data.Vector.Generic.map (\(_, b, _, _, _, _, _, _, _, _, _, _) -> b) xs,
    Data.Vector.Generic.map (\(_, _, c, _, _, _, _, _, _, _, _, _) -> c) xs,
    Data.Vector.Generic.map (\(_, _, _, d, _, _, _, _, _, _, _, _) -> d) xs,
    Data.Vector.Generic.map (\(_, _, _, _, e, _, _, _, _, _, _, _) -> e) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, f, _, _, _, _, _, _) -> f) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, g, _, _, _, _, _) -> g) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, h, _, _, _, _) -> h) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, k, _, _, _) -> k) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, l, _, _) -> l) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, a1, _) -> a1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, b1) -> b1) xs
  )

unzip13 :: Vector (a, b, c, d, e, f, g, h, k, l, a1, b1, c1) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f, Vector g, Vector h, Vector k, Vector l, Vector a1, Vector b1, Vector c1)
unzip13 xs =
  ( Data.Vector.Generic.map (\(a, _, _, _, _, _, _, _, _, _, _, _, _) -> a) xs,
    Data.Vector.Generic.map (\(_, b, _, _, _, _, _, _, _, _, _, _, _) -> b) xs,
    Data.Vector.Generic.map (\(_, _, c, _, _, _, _, _, _, _, _, _, _) -> c) xs,
    Data.Vector.Generic.map (\(_, _, _, d, _, _, _, _, _, _, _, _, _) -> d) xs,
    Data.Vector.Generic.map (\(_, _, _, _, e, _, _, _, _, _, _, _, _) -> e) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, f, _, _, _, _, _, _, _) -> f) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, g, _, _, _, _, _, _) -> g) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, h, _, _, _, _, _) -> h) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, k, _, _, _, _) -> k) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, l, _, _, _) -> l) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, a1, _, _) -> a1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, b1, _) -> b1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, _, c1) -> c1) xs
  )

unzip14 :: Vector (a, b, c, d, e, f, g, h, k, l, a1, b1, c1, d1) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f, Vector g, Vector h, Vector k, Vector l, Vector a1, Vector b1, Vector c1, Vector d1)
unzip14 xs =
  ( Data.Vector.Generic.map (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) xs,
    Data.Vector.Generic.map (\(_, b, _, _, _, _, _, _, _, _, _, _, _, _) -> b) xs,
    Data.Vector.Generic.map (\(_, _, c, _, _, _, _, _, _, _, _, _, _, _) -> c) xs,
    Data.Vector.Generic.map (\(_, _, _, d, _, _, _, _, _, _, _, _, _, _) -> d) xs,
    Data.Vector.Generic.map (\(_, _, _, _, e, _, _, _, _, _, _, _, _, _) -> e) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, f, _, _, _, _, _, _, _, _) -> f) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, g, _, _, _, _, _, _, _) -> g) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, h, _, _, _, _, _, _) -> h) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, k, _, _, _, _, _) -> k) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, l, _, _, _, _) -> l) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, a1, _, _, _) -> a1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, b1, _, _) -> b1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, _, c1, _) -> c1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, _, _, d1) -> d1) xs
  )

unzip15 :: Vector (a, b, c, d, e, f, g, h, k, l, a1, b1, c1, d1, e1) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f, Vector g, Vector h, Vector k, Vector l, Vector a1, Vector b1, Vector c1, Vector d1, Vector e1)
unzip15 xs =
  ( Data.Vector.Generic.map (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) xs,
    Data.Vector.Generic.map (\(_, b, _, _, _, _, _, _, _, _, _, _, _, _, _) -> b) xs,
    Data.Vector.Generic.map (\(_, _, c, _, _, _, _, _, _, _, _, _, _, _, _) -> c) xs,
    Data.Vector.Generic.map (\(_, _, _, d, _, _, _, _, _, _, _, _, _, _, _) -> d) xs,
    Data.Vector.Generic.map (\(_, _, _, _, e, _, _, _, _, _, _, _, _, _, _) -> e) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, f, _, _, _, _, _, _, _, _, _) -> f) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, g, _, _, _, _, _, _, _, _) -> g) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, h, _, _, _, _, _, _, _) -> h) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, k, _, _, _, _, _, _) -> k) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, l, _, _, _, _, _) -> l) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, a1, _, _, _, _) -> a1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, b1, _, _, _) -> b1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, _, c1, _, _) -> c1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, _, _, d1, _) -> d1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, e1) -> e1) xs
  )

unzip16 :: Vector (a, b, c, d, e, f, g, h, k, l, a1, b1, c1, d1, e1, f1) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f, Vector g, Vector h, Vector k, Vector l, Vector a1, Vector b1, Vector c1, Vector d1, Vector e1, Vector f1)
unzip16 xs =
  ( Data.Vector.Generic.map (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) xs,
    Data.Vector.Generic.map (\(_, b, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> b) xs,
    Data.Vector.Generic.map (\(_, _, c, _, _, _, _, _, _, _, _, _, _, _, _, _) -> c) xs,
    Data.Vector.Generic.map (\(_, _, _, d, _, _, _, _, _, _, _, _, _, _, _, _) -> d) xs,
    Data.Vector.Generic.map (\(_, _, _, _, e, _, _, _, _, _, _, _, _, _, _, _) -> e) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, f, _, _, _, _, _, _, _, _, _, _) -> f) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, g, _, _, _, _, _, _, _, _, _) -> g) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, h, _, _, _, _, _, _, _, _) -> h) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, k, _, _, _, _, _, _, _) -> k) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, l, _, _, _, _, _, _) -> l) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, a1, _, _, _, _, _) -> a1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, b1, _, _, _, _) -> b1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, _, c1, _, _, _) -> c1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, _, _, d1, _, _) -> d1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, e1, _) -> e1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, f1) -> f1) xs
  )

unzip17 :: Vector (a, b, c, d, e, f, g, h, k, l, a1, b1, c1, d1, e1, f1, g1) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f, Vector g, Vector h, Vector k, Vector l, Vector a1, Vector b1, Vector c1, Vector d1, Vector e1, Vector f1, Vector g1)
unzip17 xs =
  ( Data.Vector.Generic.map (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) xs,
    Data.Vector.Generic.map (\(_, b, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> b) xs,
    Data.Vector.Generic.map (\(_, _, c, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> c) xs,
    Data.Vector.Generic.map (\(_, _, _, d, _, _, _, _, _, _, _, _, _, _, _, _, _) -> d) xs,
    Data.Vector.Generic.map (\(_, _, _, _, e, _, _, _, _, _, _, _, _, _, _, _, _) -> e) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, f, _, _, _, _, _, _, _, _, _, _, _) -> f) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, g, _, _, _, _, _, _, _, _, _, _) -> g) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, h, _, _, _, _, _, _, _, _, _) -> h) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, k, _, _, _, _, _, _, _, _) -> k) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, l, _, _, _, _, _, _, _) -> l) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, a1, _, _, _, _, _, _) -> a1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, b1, _, _, _, _, _) -> b1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, _, c1, _, _, _, _) -> c1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, _, _, d1, _, _, _) -> d1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, e1, _, _) -> e1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, f1, _) -> f1) xs,
    Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, g1) -> g1) xs
  )

