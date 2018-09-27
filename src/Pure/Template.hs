{-# LANGUAGE FlexibleInstances #-}
module Pure.Template (module Pure.Template, i) where

import Data.String.Interpolate

import Language.Haskell.Meta

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.String

-- | This module plumbs together haskell-src-meta/template-haskell with string 
-- interpolation to enable interpolating haskell source templates. This module 
-- provides an instance of `IsString` for `Q [Dec]`.
--
-- Warning: Use of this templating capability can dangerously obscure code.
--
-- Usage example:
--
-- File1: 
-- 
-- > myNewtype name ty =
-- >   [i|newtype #{name} = Mk#{name} #{ty} deriving (Show,Generic,Eq,Ord,ToJSON,FromJSON)|]
--
-- File2:
-- 
-- > $(myNewtype "Test" "Int")
--
-- TODO: Find a way to include imports in templates. It's possible to parse
--       to a `Module`, but I'm not sure there is a way to inject imports 
--       into a use-site module.  This closed trac ticket suggests it is 
--       not possible: https://ghc.haskell.org/trac/ghc/ticket/1475

instance IsString (Q [Dec]) where
    fromString = either fail (return . concatMap toDecs) . parseHsDecls