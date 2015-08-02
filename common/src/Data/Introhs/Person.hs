module Data.Introhs.Person (module Data.Introhs.Person) where

data Person = Person String Int
    deriving (Eq, Show)

personName :: Person -> String
personName (Person name _) = name

personAge :: Person -> Int
personAge (Person _ age) = age
