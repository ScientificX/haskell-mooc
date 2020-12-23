import qualified Data.Map as Map
import Data.List 

--from LYAH

data Car = Car { company :: String
               , model :: String
               , year :: Int} deriving (Show)

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "str" ++ c    --type destructuring
-- we are going to define a custom typeclass trivial that basically steals of Eq

data Trivial =
  Trivial'

{-This Eq is the typeclass being derived frrom and Trivial is the new typeclass Trivial' is the argument to the type Trivial and the behaviour of this argumetn based on the typeclass it just derived from is implemented after the where statement -}

instance Eq Trivial where 
  Trivial' == Trivial' = True

{-A bit less trivial!-}

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving Show 

data Date =
  Date DayOfWeek Int
  deriving Show

{-Since these are not prebaked datatypes in Haskell, they have no typeclass instances at all. As they stand, there is nothing you can do
with them because no operations are defined for them. Let’s fix that.-}


instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
    (Date weekday' dayOfMonth') = weekday == weekday' && dayOfMonth == dayOfMonth'

{-NOTE avoid partial fuunctions -- functions that do not adequately cover all cases and scenarios and throw nasty errors -- DONT BE LAZY a way to avoid this is setting -Wall flag in ghci  -}

{-We are going to implement a type and a typeclass for The identity function and shows how to restrict teh values that can be acepted in a typeclass definition using a typeclass and how to handle type variables-}

data Identity a = 
  Identity a

-- instance Eq (Identity a) where
--   (==) (Identity b) (Identity b') = b == b'

{-If you run this read the errrors-}

-- a fix

instance Eq a => Eq (Identity a) where
  (==) (Identity b) (Identity b') = b == b'

--Exercises 

data TisAnInteger =
  TisAn Integer
  deriving Show

instance Eq TisAnInteger where
  (==) (TisAn integerd) (TisAn integerd') = integerd' == integerd

data TwoIntegers =
  Two Integer Integer
  deriving Show

instance Eq TwoIntegers where
  (==) (Two num1 num2) (Two num1' num2') = num1 == num1' && num2 == num2'

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt num) (TisAnInt num1) = num1 == num 
  (==) (TisAString str) (TisAString str1) = str1 == str

-- data Pair a =
--   Pair a a

-- instance Eq a => Eq (Pair a a) where
--   (==) (Pair x x) (Pair y y) = x == y

data Tuple a b =
  Tuple a b
  deriving Show

instance (Eq a, Eq b) =>  Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a =
  ThisOne a
  | ThatOne a


instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a') (ThatOne b') = a' == b'

data EitherOr a b =
  Hello a
  | Goodbye b
  deriving Show

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello b) (Hello b') = b' == b
  (==) (Goodbye a') (Goodbye a) = a' == a
  
  {-Take note of default typeclasses sometimes ghc evalueates typeclassed into types-}
{-
Num -> Integer
Real Integer
Enum Integer
Integral Integer
Fractional Double
RealFrac Double
Floating Double
RealFloat Double
-}

{-When reviewing this create an updated version of daysoftheweek using Ord instance instead of Eq, build whatever comparisons you have in mind The data definition should still derive  Eq, Why?-}

{-is a superclass of Ord.
Usually, you want the minimally sufficient set of constraints on all
your functions-}

{--would the following code typecheck why / why not
check' :: a -> a -> Bool
check' a a' = a == a'
-}

{-
1. C
2. A
3. A
4. c /
5. d /

-}

{-Syntax for anonymous functions in haskell-}
-- y :: [Int]
-- let y = map (\x -> x * 3) [1,2,4] 

