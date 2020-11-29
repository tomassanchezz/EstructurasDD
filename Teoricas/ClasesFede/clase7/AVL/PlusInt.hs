module PlusInt where

data PlusInt a = PI { int :: Int,
                      value :: a
                    }

instance (Show a) => Show (PlusInt a) where
  show (PI _ a) = show a
