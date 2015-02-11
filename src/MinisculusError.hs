{- Simple error handling engine. -}

module MinisculusError where

import MinisculusPos

lexingError msg =
    error $ "Lexical Error - " ++ msg
    
parsingError p msg =
    error $ "Parsing Error - " ++ msg ++ " at " ++ show p

