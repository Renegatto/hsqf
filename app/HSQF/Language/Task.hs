{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module HSQF.Language.Task (PTask (getTask), ptask) where

import HSQF.Language.Definition (PType, Scope (Expr), Term (MkTerm))

type PTask :: PType -> PType
newtype PTask a s = MkPTask {getTask :: forall s0. Term Expr s0 a}

-- | Task differs on a procedure only by the scope
ptask :: (forall s. Term c s a) -> Term c s (PTask a)
ptask (MkTerm t) = MkTerm t