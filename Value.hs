module Value where

data Value
    = Number Double
    | Integer Int
    deriving (Show, Eq)