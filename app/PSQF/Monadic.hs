module PSQF.Monadic ((>>=),(>>),fail) where
import Prelude (String,id,(.))
import PSQF.Definition

(>>=) :: (x -> Term c s a) -> x -> Term c s a
(>>=) = id

(>>) :: (x -> Term c s a) -> x -> Term c s a
(>>) = id

fail :: String -> Term c s a
fail = ptraceError . pconstant

