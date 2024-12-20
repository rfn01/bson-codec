module Simple (simpleSpec) where

import Data.Bson.Codec

import Test.Hspec
import Data.Bson qualified as B
import Data.Bson (Field((:=)))

-- tests with field, fieldOmit, and B.Val instances

--------------------------------------------------------------------------------
-- no frills

data Easy = Easy {eText :: !Text, eInt :: !Int64} deriving stock (Eq, Show)

easyCodec :: BsonCodec Easy
easyCodec = Easy
  <$> eText =. field "text"
  <*> eInt  =. field "int"

easySpec :: Spec
easySpec = do
  describe "easy" $ do
    let
      h = Easy "one" 1
      d = ["text" := B.String "one", "int" := B.Int64 1]
    it "can encode" $ do
      encode easyCodec h `shouldBe` d

    it "can decode" $ do
      decode easyCodec d `shouldBe` Right h

    it "can fail decode" $ do
      decode easyCodec ["text" := B.String "one", "int" := B.String "1"]
        `shouldSatisfy` isLeft

    it "does fail null" $ do
      decode easyCodec ["text" := B.Null, "int" := B.Int64 1]
        `shouldSatisfy` isLeft

--------------------------------------------------------------------------------
-- there are two ways to encode Nothing

data WMaybe = WMaybe {mText :: !Text, mJust :: !(Maybe Int64), mNothing :: !(Maybe Int64)} deriving stock (Eq, Show)

nullCodec :: BsonCodec WMaybe
nullCodec = WMaybe <$> mText =. field "text" <*> mJust =. field "just" <*> mNothing =. field "nothing"

-- fine to have two codecs for same type, but can't have to B.Val instances
omitCodec :: BsonCodec WMaybe
omitCodec = WMaybe <$> mText =. field "text" <*> mJust =. fieldOmit "just" <*> mNothing =. fieldOmit "nothing"

maybeSpec :: Spec
maybeSpec = do
  describe "maybe" $ do
    let
      h = WMaybe "one" (Just 1) Nothing
      n = ["text" := B.String "one", "just" := B.Int64 1, "nothing" := B.Null]
      o = ["text" := B.String "one" , "just" := B.Int64 1]
      
    describe "null" $ do
      it "can encode" $ do
        encode nullCodec h `shouldBe` n

      it "can decode" $ do
        decode nullCodec n `shouldBe` Right h

      -- here it is an error if the field does not exist
      it "can not decode omitted" $ do
        decode nullCodec o `shouldSatisfy` isLeft

      it "can fail decode" $ do
        decode nullCodec ["text" := B.String "one", "just" := B.String "1", "nothing" := B.Null]
          `shouldSatisfy` isLeft

    describe "omit" $ do
      it "can encode" $ do
        encode omitCodec h `shouldBe` o

      it "can decode" $ do
        decode omitCodec o `shouldBe` Right h

      -- this is sort of like backwards compatability - is that reasonable?
      it "can decode null" $ do
        decode omitCodec n `shouldBe` Right h

      it "can fail decode" $ do
        decode omitCodec ["text" := B.String "one", "just" := B.String "1"]
          `shouldSatisfy` isLeft

--------------------------------------------------------------------------------
-- simple list of native type

data SimpleList = SimpleList {rlText :: !Text, rlInts :: ![Int64]} deriving stock (Eq, Show)

simplelistCodec :: BsonCodec SimpleList
simplelistCodec = SimpleList <$> rlText =. field "text" <*> rlInts =. field "ints"

simpleListSpec :: Spec
simpleListSpec = do
  describe "simplelist" $ do
    let
      h = SimpleList "one" [1,2,3]
      d = ["text" := B.String "one", "ints" := B.Array [B.Int64 1, B.Int64 2, B.Int64 3]]
    it "can encode" $ do
      encode simplelistCodec h `shouldBe` d

    it "can decode" $ do
      decode simplelistCodec d `shouldBe` Right h

--------------------------------------------------------------------------------
-- simple newtypes - have to annotate field 

data Wrap = Wrap {wText :: !Text, wInt :: !I} deriving stock (Eq, Show)

newtype I = I Int64 deriving newtype (Eq, Show)

wrapCodec :: BsonCodec Wrap
wrapCodec = Wrap <$> wText =. field "text" <*> wInt =. wrapped (field @Int64 "int")

wrapSpec :: Spec
wrapSpec = do
  describe "wrapped" $ do
    let
      h = Wrap "one" (I 1)
      d = ["text" := B.String "one", "int" := B.Int64 1]
    it "can encode" $ do
      encode wrapCodec h `shouldBe` d

    it "can decode" $ do
      decode wrapCodec d `shouldBe` Right h

    it "can fail decode" $ do
      decode wrapCodec ["text" := B.String "one", "int" := B.String "1"]
        `shouldSatisfy` isLeft

--------------------------------------------------------------------------------
-- nested with B.Val

data NestOuter = NestOuter {noText :: !Text, noIn :: !NestInner} deriving stock (Eq, Show)

data NestInner = NestInner {niText :: !Text, niInt :: !Int64} deriving stock (Eq, Show)

nestOuterCodec :: BsonCodec NestOuter
nestOuterCodec = NestOuter <$> noText =. field "text" <*> noIn =. field "inner"
-- asDocument "inner" nestInnerCodec

nestInnerCodec :: BsonCodec NestInner
nestInnerCodec = NestInner <$> niText =. field "text" <*> niInt =. field "int"

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

-- and same but as a list using same B.Val instance

data ListOuter = ListOuter {loText :: !Text, loIns :: ![NestInner]} deriving stock (Eq, Show)

listOuterCodec :: BsonCodec ListOuter
listOuterCodec = ListOuter <$> loText =. field "text" <*> loIns =. field "inners"

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


simpleSpec :: Spec
simpleSpec = describe "simple" $ do
  easySpec
  maybeSpec
  simpleListSpec
  wrapSpec
  nestedSpec
  nestedListSpec
