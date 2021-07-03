{-# LANGUAGE TemplateHaskell #-}

module Helpers where

import Language.Haskell.TH
import Control.Monad

-- GHC maximum is 62, but making this smaller makes generated code easier to read
maxTupleSize :: Int
maxTupleSize = 5

names :: Int -> Int -> [Name]
names a b = map (mkName . ('x':) . show) [a..b]

namedTupleP :: Int -> Pat
namedTupleP n = TupP . map VarP $ names 1 n

headN :: Int -> Q Exp
headN n = do
  pure $ LamE [namedTupleP n] (VarE $ mkName "x1")

rangeOverTuples :: Int -> String -> (Int -> Q Exp) -> Q [Dec]
rangeOverTuples startingTupleSize funcName funcForTupleSize =
  forM [startingTupleSize..maxTupleSize] $ \tupleSize -> do
    currentFunc <- funcForTupleSize tupleSize
    let name = mkName $ funcName ++ show tupleSize
    pure $ FunD name [Clause [] (NormalB currentFunc) []]

tailN :: Int -> Q Exp
tailN n = do
  pure $ LamE [namedTupleP n] (tupleE $ names 2 n)

tupleE :: [Name] -> Exp
tupleE = TupE . map (Just . VarE)

initN :: Int -> Q Exp
initN n = do
  pure $ LamE [namedTupleP n] (tupleE $ names 1 (n-1))

lastN :: Int -> Q Exp
lastN n = do
  pure $ LamE [namedTupleP n] (VarE . mkName $ "x" ++ show n)

lengthN :: Int -> Q Exp
lengthN n = do
  pure . LamE [VarP $ mkName "_"] . LitE . IntegerL $ toInteger n

nullN :: Int -> Q Exp
nullN n =
  pure . LamE [namedTupleP n] . ConE $
    if n == 0
    then 'True
    else 'False

mapN :: Int -> Q Exp
mapN n = do
  let f = mkName "f"
      args = [VarP f, namedTupleP n]
      applyFunc x = AppE (VarE f) (VarE x)
  pure $ LamE args $ TupE . map (Just . applyFunc) $ names 1 n
