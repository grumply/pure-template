{-# LANGUAGE FlexibleInstances #-}
module Pure.Template (module Pure.Template, i, Q, Dec, parseModeWithExtensions, defaultExtensions, parseDecls, ToTxt(..), FromTxt(..), parse) where

import Pure.Data.Txt
import Pure.Data.Txt.Interpolate

import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Parser hiding (parse)
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax

import Language.Haskell.Meta

import Language.Haskell.TH (Q,Dec)

import Data.String
import Data.List as List

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

parseModeWithExtensions :: [KnownExtension] -> ParseMode
parseModeWithExtensions exts = ParseMode [] Haskell2010 (EnableExtension <$> exts) False False Nothing False

defaultExtensions :: [KnownExtension]
defaultExtensions = 
    [QuasiQuotes
    ,TemplateHaskell
    ,RankNTypes
    ,MultiParamTypeClasses
    ,FlexibleContexts
    ,ViewPatterns
    ,PatternSynonyms
    ,TypeFamilies
    ,TypeOperators
    ]

parseDecls :: [KnownExtension] -> String -> Either String [Decl SrcSpanInfo]
parseDecls exts = either Left (Right . moduleDecls) 
                . parseResultToEither 
                . parseModuleWithMode (parseModeWithExtensions exts)

parse :: [KnownExtension] -> Txt -> Q [Dec]
parse exts = either fail (return . List.concatMap toDecs) . parseDecls exts . fromTxt

instance FromTxt (Q [Dec]) where
    fromTxt = Pure.Template.parse defaultExtensions

instance IsString (Q [Dec]) where
    fromString = Pure.Template.parse defaultExtensions . toTxt