module Data.Bson.Codec
  ( BsonCodec
  , field
  , fieldOmit
  , wrapped
  , encode
  , decode
  , (=.)
  ) where

import Data.Bson.Codec.Core

import Control.Monad.Writer.CPS (Writer, execWriter, writer)
import Data.Bson (Document, (=:), (=?), Val, look, lookup, cast)
import Data.Profunctor

-- FIXME How to handle sum types?

-- Bson is typically machine generated.  Perhaps a simple first-error
-- approach is fine for starters.  Could look at validation-selective
-- or monad-validate in the future.

type BsonCodec a = Codec (ExceptT Text (Reader Document)) (Writer Document) a a

-- uses Null for Nothing
field :: forall a. Val a => Text -> BsonCodec a
field l = Codec r w
  where
    r = ExceptT $ reader (lookup l)
    w a = writer (a, [l =: a])

-- omits Nothing fields, but can parse Null or omitted
fieldOmit :: forall a. Val a => Text -> BsonCodec (Maybe a)
fieldOmit l = Codec r w
  where
    r = ExceptT $ reader (\d ->
                            case look l d of
                              Nothing -> Right Nothing
                              Just a -> cast a
                         )
    w a = writer (a, l =? a)

-- handles newtypes, but you have to annotate the call to field or wrapped
wrapped :: (Coercible a b, Coercible b a) => BsonCodec a -> BsonCodec b
wrapped = dimap coerce coerce

encode :: BsonCodec a -> a -> Document
encode Codec {..} = execWriter . codecWriter

decode :: BsonCodec a -> Document -> Either Text a
decode Codec {..} = runReader (runExceptT codecReader)

infixl 5 =.
(=.) :: (Functor r, Functor w) => Codec r w y a -> (x -> y) -> Codec r w x a
(=.) = flip lmap
