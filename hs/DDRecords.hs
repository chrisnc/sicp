{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module DDRecords
  ( Address
  , Salary
  , EmployeeName
  , HasAddress(..)
  , HasSalary(..)
  , HasEmployeeRecords(..)
  , EmployeeRecord(..)
  , DivisionFile(..)
  , findEmployeeRecord
  ) where

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map

-- Exercise 2.74, page 250

-- Preliminary work to get this to be nice in Haskell.
-- We implement generic employee files and records using typeclasses.
-- Each new division will define two types, one type for employee records that
-- must implement both HasAddress and HasSalary, and another type for the
-- division file that implements getEmployeeRecord.
-- To put both records and files into the same kind of data structure, we will
-- wrap them in the EmployeeRecord and DivisionFile types, which implement
-- the appropriate methods and hide the underlying implementation with
-- existential quantification.

type Address = String
type Salary = Int
type EmployeeName = String

class HasAddress r where
  getAddress :: r -> Address

class HasSalary r where
  getSalary :: r -> Salary

data EmployeeRecord = forall r. (HasAddress r, HasSalary r, Show r) => EmployeeRecord r
deriving instance Show EmployeeRecord

-- the instances for EmployeeRecord just dispatch to the underlying type
instance HasAddress EmployeeRecord where
  getAddress (EmployeeRecord r) = getAddress r

instance HasSalary EmployeeRecord where
  getSalary (EmployeeRecord r) = getSalary r


class HasEmployeeRecords f where
  getEmployeeRecord :: EmployeeName -> f -> Maybe EmployeeRecord

data DivisionFile = forall f. (HasEmployeeRecords f, Show f) => DivisionFile f
deriving instance Show DivisionFile

instance HasEmployeeRecords DivisionFile where
  getEmployeeRecord name (DivisionFile f) = getEmployeeRecord name f


-- the employee record for the computer division will be a newtype wrapper around a tuple
newtype ComputerEmployeeRecord = ComputerEmployeeRecord (Salary, Address)
  deriving Show

instance HasSalary ComputerEmployeeRecord where
  getSalary (ComputerEmployeeRecord (s, _)) = s

instance HasAddress ComputerEmployeeRecord where
  getAddress (ComputerEmployeeRecord (_, a)) = a

-- the employee file for the computer division will be a Map
type ComputerDivisionFile = Map EmployeeName ComputerEmployeeRecord

instance HasEmployeeRecords ComputerDivisionFile where
  getEmployeeRecord name f = EmployeeRecord <$> (Map.lookup name f)

-- the employee record for the auto division will be a Haskell record type
data AutoEmployeeRecord = AutoEmployeeRecord
  { autoSalary :: Salary
  , autoAddress :: Address
  }
  deriving Show

instance HasSalary AutoEmployeeRecord where
  getSalary = autoSalary

instance HasAddress AutoEmployeeRecord where
  getAddress = autoAddress

-- the employee file for the auto division will be a list of (name,record) tuples
type AutoDivisionFile = [(EmployeeName, AutoEmployeeRecord)]

instance HasEmployeeRecords AutoDivisionFile where
  getEmployeeRecord name f = EmployeeRecord . snd <$> find ((== name) . fst) f

-- part a
-- The getEmployeeRecord function will work on any DivisionFile.
-- Each division just needs to define a type and provide an instance of
-- HasEmployeeRecords for it, and then wrap that type with the DivisionFile
-- constructor.

-- part b
-- The getSalary function will work on any EmployeeRecord.
-- Each division needs to define a type and provide an instance of HasSalary
-- for it, and then wrap that type with the EmployeeRecord constructor, and
-- then HQ will be able to handle records in a generic way.

-- part c
findEmployeeRecord :: EmployeeName -> [DivisionFile] -> Maybe EmployeeRecord
findEmployeeRecord name files = asum (getEmployeeRecord name <$> files)

-- putting it all together
computerFile :: DivisionFile
computerFile = DivisionFile (Map.singleton "Bill" (ComputerEmployeeRecord (123, "foo")))

autoFile :: DivisionFile
autoFile = DivisionFile ([("Bob", AutoEmployeeRecord 123 "foo")])

divisionFiles :: [DivisionFile]
divisionFiles = [computerFile, autoFile]

main :: IO ()
main = do
  print (findEmployeeRecord "Bill" divisionFiles)
  print (findEmployeeRecord "Bob" divisionFiles)
  print (findEmployeeRecord "Andy" divisionFiles)
  print (getSalary <$> findEmployeeRecord "Bob" divisionFiles)

-- part d
-- When Insatiable takes over a new company, that company's divisions must
-- expose their personnel information systems by implementing the getSalary and
-- getAddress methods for their employee records, and implement the
-- getEmployeeRecord methods for their file type. Then, when passing the
-- records or files to the central system, the new divisions must wrap them in
-- the EmployeeRecord and DivisionFile constructors.
