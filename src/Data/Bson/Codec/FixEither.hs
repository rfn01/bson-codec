{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Bson.Codec.FixEither where

import Data.String (IsString, fromString)

instance IsString str => MonadFail (Either str) where
    fail :: String -> Either str a
    fail = Left . fromString
    {-# INLINE fail #-}
