module Data.Bson.Codec
  ( BsonCodec
  , field
  , fieldOmit
  , wrapped
  , fixed
  , prism
  , (=.)
  , encode
  , decode
  , ValueCodec
  , label
  , doc
  , array
  , viaVal
  , as
  ) where

import Data.Bson.Codec.Core

import Control.Applicative (empty)
import Control.Monad (guard)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Reader (Reader, reader, runReader)
import Data.Bson.Codec.FixEither ()
import Data.Coerce (Coercible, coerce)
import Data.Either.Combinators (maybeToRight)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last(..))
import Data.Text (Text)

import Control.Monad.Trans.Writer.CPS (WriterT, execWriterT, writerT)
import Control.Monad.Trans.Writer.CPS (Writer, execWriter, writer)
import Data.Bson (Document, (=:), (=?), Value, Val, Label)
import Data.Bson qualified as B
import Data.Profunctor (lmap, dimap)

-- Bson is typically machine generated.  Perhaps a simple first-error
-- approach is fine for starters.  Could look at validation-selective
-- or monad-validate in the future.

type BsonCodec a = Codec (ExceptT Text (Reader Document)) (WriterT Document Maybe) a a

-- simple field based on B.Val (where Nothing becomes B.Null)
field :: forall a. Val a => Label -> BsonCodec a
field l = Codec r w
  where
    r = ExceptT $ reader (B.lookup l)
    w a = writerT $ pure (a, [l =: a])

-- omits Nothing fields, but can parse Null or omitted
-- needs to distinguish "does not exists" from "wrong type"
fieldOmit :: forall a. Val a => Label -> BsonCodec (Maybe a)
fieldOmit l = Codec r w
  where
    r = ExceptT $ reader (\d ->
                            case B.look l d of
                              Nothing -> Right Nothing
                              Just a -> B.cast a
                         )
    w a = writerT $ pure (a, l =? a)

-- handles newtypes, but means field needs type annotations
wrapped :: (Coercible a b, Coercible b a) => BsonCodec a -> BsonCodec b
wrapped = dimap coerce coerce

-- fixed text field that is added and verified
fixed :: forall a. Label -> Text -> Codec (ExceptT Text (Reader Document)) (WriterT Document Maybe) a Text
fixed l v = do
  t <- const v =. field @Text l
  guard (t == v)
  pure t

-- not quite dimap, because we skip codecWriter completely if project fails
prism :: forall a b. (b -> a) -> (a -> Maybe b) -> BsonCodec b -> BsonCodec a
prism inject project Codec{..} = Codec r w
  where
    r = fmap inject codecReader
    w = (\case
               Nothing -> empty
               Just b -> fmap inject (codecWriter b)) . project

-- like argument order matching lmap
-- like that it looks record constructor `name = ...`
-- .= is used by aeson & lens
infixl 5 =.
(=.) :: (Functor r, Functor w) => (x -> y) -> Codec r w y a -> Codec r w x a
(=.) = lmap

encode :: BsonCodec a -> a -> Document
encode Codec {..} = fromMaybe [] . execWriterT . codecWriter

decode :: BsonCodec a -> Document -> Either Text a
decode Codec {..} = runReader (runExceptT codecReader)

-- The bson library uses the B.Val typeclass to convert from B.Value
-- to a haskell type.  This nicely captures maybes, nested objects,
-- lists of values, lists of objects, and the various combinations.
-- We can tie into this by adding B.Val instances, and those B.Val
-- instances can be built using Codec and encode/decode.  However, I
-- would like to offer the same functionality without requiring B.Val
-- instances.

-- One of the issues is (B.Doc Document) vs Document.  BsonCodec is
-- in terms of a raw Document because the outer most things is a raw
-- Document.  We need a ValueCodec to connect things to a B.Value.

type ValueCodec a = Codec (ExceptT Text (Reader B.Value)) (Writer (Last Value)) a a

encodeValue :: ValueCodec a -> a -> Maybe Value
encodeValue Codec {..} = getLast . execWriter . codecWriter

decodeValue :: ValueCodec a -> Value -> Either Text a
decodeValue Codec {..} = runReader (runExceptT codecReader)

-- pluck Value from a Document
label :: Label -> ValueCodec a -> BsonCodec a
label l inner = Codec r w
  where
    r = ExceptT $ reader (\d ->
                            case B.look l d of
                              Nothing -> Left $ "label " <> l <> " not found"
                              Just v -> decodeValue inner v
                         )
    w a = writerT $ pure (a, [l =: encodeValue inner a])

-- Doc to Document
doc :: BsonCodec a -> ValueCodec a
doc inner = Codec r w
  where
    r = ExceptT $ reader (\case -- v
                             B.Doc d -> decode inner $ d
                             _ -> Left "expected Doc")
    w a = writer $ (a, Last . pure . B.Doc . encode inner $ a)

-- Array to [B.Value]
array :: ValueCodec a -> ValueCodec [a]
array inner = Codec r w
  where
    r = ExceptT $ reader (\case -- v
                             B.Array v -> traverse (decodeValue inner) v
                             _ -> Left "expected Array")
    w a = writer $ (a, Last . fmap B.Array . traverse (encodeValue inner) $ a)

viaVal :: Val a => ValueCodec a
viaVal = Codec r w
  where
    r = ExceptT $ reader $ B.cast
    w a = writer $ (a, Last . pure . B.val $ a)

as :: (a -> Maybe Value) -> (Value -> Maybe a) -> ValueCodec a
as inject project = Codec r w
  where
    r = ExceptT $ reader $ maybeToRight ("as failed to project" :: Text) . project
    w a = writer $ (a, Last . inject $ a)
