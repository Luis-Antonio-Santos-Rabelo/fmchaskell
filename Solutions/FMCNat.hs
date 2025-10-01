{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O     = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    (==) :: Nat -> Nat -> Bool
    O   == O         = True
    S n == S m       = m == n
    _ == _           = False

instance Ord Nat where

    (<=) :: Nat -> Nat -> Bool
    O <= _     = True
    S n <= O   = False
    S n <= S m = n <= m



    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min :: Nat -> Nat -> Nat
    min (S n) (S m) = S(min n m)
    min _ _         = O



    max :: Nat -> Nat -> Nat
    max (S n) (S m) = S(max n m)
    max n O         = n
    max O n         = n


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O     = True
isZero (S _) = False



-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O     = O
pred (S n) = n



even :: Nat -> Bool
even O         = True
even (S O)     = False
even (S (S n)) = even n



odd :: Nat -> Bool
odd O         = False
odd (S O)     = True
odd (S (S n)) = odd n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O   = n
n <+> S m = S (n <+> m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus = (<->)

(<->) :: Nat -> Nat -> Nat
n   <-> O   = n
O   <-> n   = n
S n <-> S m = n <-> m




-- multiplication
times :: Nat -> Nat -> Nat
times = (<*>)

(<*>) :: Nat -> Nat -> Nat
n <*> O   = O
n <*> S m = n <*> m <+> n




-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow = (<^>)

exp :: Nat -> Nat -> Nat
exp = (<^>)

(<^>) :: Nat -> Nat -> Nat
n <^> O   = S O
n <^> S m = n <^> m <*> n




-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> O = undefined
n </> m =
    if n <= m then O else S O <+> ((n <-> m) </> m)

-- remainder
(<%>) :: Nat -> Nat -> Nat
_ <%> O   = undefined
n <%> m   =
    if n <= m then O else n <-> ((n </> m) <*> m)

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (n, m) = (n </> m, n <%> m)

-- divides
(<|>) :: Nat -> Nat -> Bool
n <|>  m = isZero (m <%> n)

divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist = (|-|)

(|-|) :: Nat -> Nat -> Nat
n |-| m = 
    if n <= m then m <-> n else n <-> m

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = S n <*> factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O     = O
sg (S n) = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O n = undefined
lo n O = undefined
lo (S O) n = undefined
lo n m = 
    if m <= n then O else S O <+> lo n (m </> n)


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat 0 = O
toNat n = S (toNat (n - 1))

fromNat :: Integral a => Nat -> a
fromNat O     = 0
fromNat (S n) = fromNat n + 1 


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = O
      | otherwise = S (fromInteger (x - 1))

