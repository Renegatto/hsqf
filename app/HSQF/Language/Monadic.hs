module HSQF.Language.Monadic ((>>=), (>>), fail) where

import HSQF.Language.Common (PConstant (pconstant), Term, ptraceError)
import Prelude (String, id, (.))

(>>=) :: (x -> Term c s a) -> x -> Term c s a
(>>=) = id

(>>) :: (x -> Term c s a) -> x -> Term c s a
(>>) = id

fail :: String -> Term c s a
fail = ptraceError . pconstant
