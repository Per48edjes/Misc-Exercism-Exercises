{-# LANGUAGE ImportQualifiedPost #-}

module Isogram (isIsogram) where

import Data.Char (isAlpha)
import Data.Maybe (isJust)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T

isIsogram :: Text -> Bool
isIsogram = isJust . dupeCheck S.empty . cleanUp
  where
    cleanUp = T.filter isAlpha . T.toLower
    lookUp s c = if c `elem` s then Nothing else Just $ S.insert c s
    dupeCheck s t
        | t == T.empty = Just s
        | otherwise = do
            (c, t') <- T.uncons t
            s' <- lookUp s c
            dupeCheck s' t'
