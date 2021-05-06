module BioCore.Strand

import Data.List
import Data.Strings

%default total


--Strand--

public export
data Strand = Plus
            | Minus

public export
Eq Strand where
  (==) Plus Plus   = True
  (==) Minus Minus = True
  (==) _     _     = False 
  (/=) x     y     = not (x == y)

----------
