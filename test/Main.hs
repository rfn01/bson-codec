module Main where

import Data.Bson.Codec

import Test.Hspec
import Data.Bson qualified as B
import Data.Bson (Field((:=)))


--------------------------------------------------------------------------------
data Simple = Simple {sText :: !Text, sInt :: !Int64} deriving stock (Eq, Show)

simpleCodec :: BsonCodec Simple
simpleCodec = Simple
  <$> field "text" =. sText
  <*> field "int" =. sInt

simpleSpec :: Spec
simpleSpec = do
  describe "simple" $ do
    let
      h = Simple "one" 1
      d = ["text" := B.String "one", "int" := B.Int64 1]
    it "can encode" $ do
      encode simpleCodec h `shouldBe` d

    it "can decode" $ do
      decode simpleCodec d `shouldBe` Right h

    it "can fail decode" $ do
      decode simpleCodec ["text" := B.String "one", "int" := B.String "1"]
        `shouldSatisfy` isLeft

    it "does fail null" $ do
      decode simpleCodec ["text" := B.Null, "int" := B.Int64 1]
        `shouldSatisfy` isLeft

--------------------------------------------------------------------------------
data Wrapped = Wrapped {wText :: !Text, wInt :: !I} deriving stock (Eq, Show)

newtype I = I Int64 deriving newtype (Eq, Show)

wrappedCodec :: BsonCodec Wrapped
wrappedCodec = Wrapped <$> field "text" =. wText <*> wrapped (field @Int64 "int") =. wInt

wrappedSpec :: Spec
wrappedSpec = do
  describe "wrapped" $ do
    let
      h = Wrapped "one" (I 1)
      d = ["text" := B.String "one", "int" := B.Int64 1]
    it "can encode" $ do
      encode wrappedCodec h `shouldBe` d

    it "can decode" $ do
      decode wrappedCodec d `shouldBe` Right h

    it "can fail decode" $ do
      decode wrappedCodec ["text" := B.String "one", "int" := B.String "1"]
        `shouldSatisfy` isLeft

--------------------------------------------------------------------------------
data WMaybe = WMaybe {mText :: !Text, mJust :: !(Maybe Int64), mNothing :: !(Maybe Int64)} deriving stock (Eq, Show)

nullableCodec :: BsonCodec WMaybe
nullableCodec = WMaybe <$> field "text" =. mText <*> field "just" =. mJust <*> field "nothing" =. mNothing

-- fine to have two codecs for same type, but can't have to B.Val instances :-(
omittableCodec :: BsonCodec WMaybe
omittableCodec = WMaybe <$> field "text" =. mText <*> fieldOmit "just" =. mJust <*> fieldOmit "nothing" =. mNothing

maybeSpec :: Spec
maybeSpec = do
  describe "maybe" $ do
    let
      h = WMaybe "one" (Just 1) Nothing
      n = ["text" := B.String "one", "just" := B.Int64 1, "nothing" := B.Null]
      o = ["text" := B.String "one" , "just" := B.Int64 1]
      
    describe "nullable" $ do
      it "can encode" $ do
        encode nullableCodec h `shouldBe` n

      it "can decode" $ do
        decode nullableCodec n `shouldBe` Right h

      -- here it is an error if the field does not exist
      it "can not decode omitted" $ do
        decode nullableCodec o `shouldSatisfy` isLeft

      it "can fail decode" $ do
        decode nullableCodec ["text" := B.String "one", "just" := B.String "1", "nothing" := B.Null]
          `shouldSatisfy` isLeft

    describe "omittable" $ do
      it "can encode" $ do
        encode omittableCodec h `shouldBe` o

      it "can decode" $ do
        decode omittableCodec o `shouldBe` Right h

      -- this is sort of like backwards compatability - is that reasonable?
      it "can decode null" $ do
        decode omittableCodec n `shouldBe` Right h

      it "can fail decode" $ do
        decode omittableCodec ["text" := B.String "one", "just" := B.String "1"]
          `shouldSatisfy` isLeft

--------------------------------------------------------------------------------
data RawList = RawList {rlText :: !Text, rlInts :: ![Int64]} deriving stock (Eq, Show)

rawlistCodec :: BsonCodec RawList
rawlistCodec = RawList <$> field "text" =. rlText <*> field "ints" =. rlInts

rawlistSpec :: Spec
rawlistSpec = do
  describe "rawlist" $ do
    let
      h = RawList "one" [1,2,3]
      d = ["text" := B.String "one", "ints" := B.Array [B.Int64 1, B.Int64 2, B.Int64 3]]
    it "can encode" $ do
      encode rawlistCodec h `shouldBe` d

    it "can decode" $ do
      decode rawlistCodec d `shouldBe` Right h

--------------------------------------------------------------------------------
data WrappedList = WrappedList {wlText :: !Text, wlInts :: ![J]} deriving stock (Eq, Show)

newtype J = J Int64 deriving newtype (Eq, Show)

-- J has to have a B.Val instance
instance B.Val J where
  val (J j) = B.Int64 j
  cast' (B.Int64 j) = Just (J j)
  cast' _ = Nothing

wrappedlistCodec :: BsonCodec WrappedList
wrappedlistCodec = WrappedList <$> field "text" =. wlText <*> field @[J] "ints" =. wlInts

wrappedlistSpec :: Spec
wrappedlistSpec = do
  describe "wrappedlist" $ do
    let
      h = WrappedList "one" [J 1,J 2,J 3]
      d = ["text" := B.String "one", "ints" := B.Array [B.Int64 1, B.Int64 2, B.Int64 3]]
    it "can encode" $ do
      encode wrappedlistCodec h `shouldBe` d

    it "can decode" $ do
      decode wrappedlistCodec d `shouldBe` Right h

--------------------------------------------------------------------------------
data NestOuter = NestOuter {noText :: !Text, noIn :: !NestInner} deriving stock (Eq, Show)

data NestInner = NestInner {niText :: !Text, niInt :: !Int64} deriving stock (Eq, Show)

nestOuterCodec :: BsonCodec NestOuter
nestOuterCodec = NestOuter <$> field "text" =. noText <*> field "inner" =. noIn

nestInnerCodec :: BsonCodec NestInner
nestInnerCodec = NestInner <$> field "text" =. niText <*> field "int" =. niInt

-- NestInner has to have a B.Val instance
instance B.Val NestInner where
  val = B.Doc . encode nestInnerCodec
  cast' (B.Doc d) = rightToMaybe $ decode nestInnerCodec d
  cast' _ = Nothing

nestedSpec :: Spec
nestedSpec = do
  describe "nested" $ do
    let
      hi = NestInner "two" 2
      ho = NestOuter "one" hi
      d = ["text" := B.String "one", "inner" := B.Doc ["text" := B.String "two", "int" := B.Int64 2]]
    it "can encode" $ do
      encode nestOuterCodec ho `shouldBe` d

    it "can decode" $ do
      decode nestOuterCodec d `shouldBe` Right ho

--------------------------------------------------------------------------------
data ListOuter = ListOuter {loText :: !Text, loIns :: ![NestInner]} deriving stock (Eq, Show)

listOuterCodec :: BsonCodec ListOuter
listOuterCodec = ListOuter <$> field "text" =. loText <*> field "inners" =. loIns

nestedListSpec :: Spec
nestedListSpec = do
  describe "nested list" $ do
    let
      hi = [NestInner "two" 2, NestInner "three" 3]
      ho = ListOuter "one" hi
      d = ["text" := B.String "one"
          , "inners" := B.Array [ B.Doc ["text" := B.String "two", "int" := B.Int64 2]
                                  , B.Doc ["text" := B.String "three", "int" := B.Int64 3]
                                  ]]
    it "can encode" $ do
      encode listOuterCodec ho `shouldBe` d

    it "can decode" $ do
      decode listOuterCodec d `shouldBe` Right ho

--------------------------------------------------------------------------------
data Foo = Foo { fText :: !Text, fInt :: !Int64 } deriving stock (Eq, Show)

fooCodec :: BsonCodec Foo
fooCodec =do
  t <- field @Text "type" =. const "foo"
  guard (t == "foo")
  Foo
    <$> field "text" =. fText
    <*> field "int" =. fInt

fooSpec :: Spec
fooSpec = do
  describe "WIP static" $ do
    let
      h = Foo "one" 1
      d = [ "type" := B.String "foo"
          , "text" := B.String "one"
          , "int" := B.Int64 1]
    it "can encode" $ do
      encode fooCodec h `shouldBe` d

    it "can decode" $ do
      decode fooCodec d `shouldBe` Right h

data Bar = BarA Text Int64 | BarB Int64 Text deriving stock (Eq, Show)

data ProjectBarA = ProjectBarA { pbaText :: !Text, pbaInt :: !Int64 } deriving stock (Eq, Show)
data ProjectBarB = ProjectBarB { pbbInt :: !Int64, pbbText :: !Text } deriving stock (Eq, Show)

projectBarA :: Bar -> Maybe ProjectBarA
projectBarA (BarA t i) = Just $ ProjectBarA t i
projectBarA _          = Nothing

projectBarB :: Bar -> Maybe ProjectBarB
projectBarB (BarB i t) = Just $ ProjectBarB i t
projectBarB _          = Nothing

barCodec :: BsonCodec Bar
barCodec =do
  t <- field @Text "type" =. \case
    BarA {} -> "a"
    BarB {} -> "b"
  case t of
    "a" -> BarA
           <$> field "texta" =. (\case
                                    BarA t _ -> t
                                    _ -> error "invalid"
                                )
           <*> field "inta" =. (\case
                                   BarA _ i -> i
                                   _ -> error "invalid"
                               )
    "b" -> BarB <$> field "intb" =. const 1 <*> field "textb" =. const "one"

-- it's interesting that it works, but it's ugly.
-- perhsps some way to map a non-record type into k/v pairs.
-- perhaps some way to use <|> for read/write
-- perhaps a combiner to help with fixed values that fails if it doesn't match

barSpec :: Spec
barSpec = do
  describe "WIP sum types" $ do
    let
      h = BarA "one" 1
      d = [ "type" := B.String "a"
          , "texta" := B.String "one"
          , "inta" := B.Int64 1]
    it "can encode" $ do
      encode barCodec h `shouldBe` d

    it "can decode" $ do
      decode barCodec d `shouldBe` Right h



main :: IO ()
main = hspec $ do
  simpleSpec
  wrappedSpec
  maybeSpec
  rawlistSpec
  wrappedlistSpec
  nestedSpec
  nestedListSpec
  fooSpec
  barSpec
