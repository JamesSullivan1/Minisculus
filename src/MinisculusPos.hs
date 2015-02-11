{- Defines the token position data type. -}

module MinisculusPos where

{- Internal Representation of token positions -}
data Pos    = Pos Line Column   deriving (Eq)
data Line   = Line Int          deriving (Eq)
data Column = Column Int        deriving (Eq)

instance Show Pos where
    show (Pos l c)  = show l ++ ":" ++ show c
instance Show Line where
    show (Line n)   = show n
instance Show Column where
    show (Column n) = show n


