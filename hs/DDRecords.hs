{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.Foldable

-- Exercise 2.74, page 250

-- part a
class DivisionFile f r | f -> r where
  getRecord :: EmployeeRecord r => f -> String -> Maybe r

-- We implement generic employee files and records using typeclasses.
-- Each new division will implement EmployeeRecord and DivisionFile instances.


-- part b
class EmployeeRecord r where
  getSalary :: r -> Double


-- an example implementation for the Sales division
-- records are a tuple of names and salaries and files are lists of records

newtype SalesRecord = SalesRecord (String,Double)
  deriving Show

instance EmployeeRecord SalesRecord where
  getSalary (SalesRecord (_,salary)) = salary

newtype SalesDivisionFile = SalesDivisionFile [SalesRecord]
  deriving Show

instance DivisionFile SalesDivisionFile SalesRecord where
  getRecord (SalesDivisionFile rs) name =
    case rs of
      []                                  -> Nothing
      r@(SalesRecord (n,s)):_ | n == name -> Just r
      _:rrs                               -> getRecord (SalesDivisionFile rrs) name


-- part c
-- This function can't be easily implemented in Haskell, because the actual type
-- of the returned record will be different depending on which division the
-- employee is in. Also, we can't have a normal list containing different kinds
-- of division files in each element... This problem lends itself more easily to
-- a sum type (explicit type tags), rather than a typeclass.
--
-- This function just implements a version where we have a list of multiple
-- division files of the same type.
findEmployeeRecord :: (DivisionFile f r, EmployeeRecord r) => String -> [f] -> Maybe r
findEmployeeRecord name = asum . map (\f -> getRecord f name)


-- part d
-- The changes that must be made are to have each division implement the
-- DivisionFile and EmployeeRecord typeclass.
