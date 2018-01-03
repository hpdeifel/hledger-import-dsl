module Hledger.Data.Lenses.Helper (makeLensesL) where

import           Lens.Micro.Platform
import           Language.Haskell.TH

makeLensesL :: Name -> DecsQ
makeLensesL = makeLensesWith (lensRules & lensField .~ addL)
  where addL _ _ n = [TopName (mkName (nameBase n ++ "L"))]
