# questions/08

```haskell
ghci> patterns 1 [(0, 0)]
[[(-1,0),(0,0)],
 [(1,0),(0,0)],
 [(0,-1),(0,0)],
 [(0,1),(0,0)]]
```

```haskell
ghci> patterns 1 [(1, 0), (0, 0)]
[[(2,0),(1,0),(0,0)],
 [(1,-1),(1,0),(0,0)],
 [(1,1),(1,0),(0,0)]]
```

## パターンを列挙するのとパターンの数を数えるのを抽象化したい

```haskell
patterns :: Int -> Route -> [Route]
patterns 0 route = [route]
patterns i route = foldl (++) [] $ map (patterns (i - 1)) $ map (:route) $ nextPoss route
```

これ↑と

```haskell
count :: Int -> Route -> Int
count 0 _ = 1
count i route = foldl (+) 0 $ map (count (i - 1)) $ map (:route) $ nextPoss route
```

これ↑が似ているのでまとめたい。

### 再帰部の抽象化

Monoidモジュールをインポートする。

```
import Data.Monoid
```

`[Route]` は既にMonoidなのでそのままで。

`Int` は `Sum Int` にする。

```haskell
count :: Int -> Route -> Sum Int
count 0 _ = Sum 1
count i route = foldl (+) 0 $ map (count (i - 1)) $ map (:route) $ nextPoss route
```

Monoid のリストには `mconcat` が使える。

```haskell
patterns i route = mconcat $ map (count (i - 1)) $ map (:route) $ nextPoss route
count i route = mconcat $ map (count (i - 1)) $ map (:route) $ nextPoss route
```

再帰部が共通化できた。

### 基底部の抽象化

`[Route]` と `Sum Int` によって挙動が異なる関数を定義したい。

```haskell
class Monoid r => Routable r where
    runit :: Route -> r

instance Routable [Route] where
    runit route = [route]

instance Routable (Sum Int) where
    runit route = Sum 1
```

これで `[Route]` と `Sum Int` は `Routable` の型インスタンスとなる。
`Routable` の型インスタンスはMonoidでもある。

リストや型シノニムはそのままでは型インスタンスの引数にできないので

```haskell
{-# LANGUAGE FlexibleInstances #-}
```

する。

```
patterns 0 route = runit route
count 0 route = runit route
```

これで基底部も共通化できた。

### 統合

共通化された関数を作る。

```haskell
mroutes :: Routable r => Int -> Route -> r
mroutes 0 route = runit route
mroutes i route = mconcat $ map (mroutes (i - 1)) $ map (:route) $ nextPoss route
```

パターンを列挙する関数はこれ。

```haskell
patterns = mroutes :: Int -> Route -> [Route]
```

パターンの数を数える関数はこう。

```haskell
count = mroutes :: Int -> Route -> Sum Int
```

### まとめ

以上をまとめるとこのようなコードになる。

```haskell
{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid

type Pos = (Int, Int)
type Route = [Pos]

class Monoid r => Routable r where
    runit :: Route -> r

instance Routable [Route] where
    runit route = [route]

instance Routable (Sum Int) where
    runit route = Sum 1

mroutes :: Routable r => Int -> Route -> r
mroutes 0 route = runit route
mroutes i route = mconcat $ map (mroutes (i - 1)) $ map (:route) $ nextPoss route

patterns = mroutes :: Int -> Route -> [Route]
count = mroutes :: Int -> Route -> Sum Int
```
