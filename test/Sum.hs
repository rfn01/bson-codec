module Sum (sumSpec) where

import Data.Bson.Codec

import Test.Hspec
import Data.Bson qualified as B
import Data.Bson (Field((:=)))

-- sum types and related

--------------------------------------------------------------------------------
-- fixed values

data Foo = Foo { fText :: !Text, fInt :: !Int64 } deriving stock (Eq, Show)

fooCodec :: BsonCodec Foo
fooCodec =
  fixed "type" "foo" *> (Foo
                         <$> fText =. field "text"
                         <*> fInt =. field "int")

fooSpec :: Spec
fooSpec = do
  describe "fixed" $ do
    let
      h = Foo "one" 1
      d = [ "type" := B.String "foo"
          , "text" := B.String "one"
          , "int" := B.Int64 1]
    it "can encode" $ do
      encode fooCodec h `shouldBe` d

    it "can decode" $ do
      decode fooCodec d `shouldBe` Right h

    it "fails decode mismatch" $ do
      decode fooCodec [ "type" := B.String "wrong"
                      , "text" := B.String "one"
                      , "int" := B.Int64 1]
        `shouldSatisfy` isLeft

--------------------------------------------------------------------------------
-- tagged sum types

data Bar = BarA Text Int64 | BarB Text deriving stock (Eq, Show)

-- it feels like a lot to make these project types
data PBarA = PBarA { pbaText :: !Text, pbaInt :: !Int64 } deriving stock (Eq, Show)

projectBarA :: Bar -> Maybe PBarA
projectBarA (BarA t i) = Just $ PBarA t i
projectBarA _          = Nothing

injectBarA :: PBarA -> Bar
injectBarA (PBarA t i) = BarA t i

data PBarB = PBarB { pbbText :: !Text } deriving stock (Eq, Show)

projectBarB :: Bar -> Maybe PBarB
projectBarB (BarB t) = Just $ PBarB t
projectBarB _          = Nothing

injectBarB :: PBarB -> Bar
injectBarB (PBarB t) = BarB t

-- fixed is a guard, but any distinct field names are enough
barCodec :: BsonCodec Bar
barCodec = prism injectBarA projectBarA (fixed "type" "a" >> (PBarA <$> pbaText =. field "texta" <*> pbaInt =. field "inta"))
       <|> prism injectBarB projectBarB (fixed "type" "b" >> (PBarB <$> pbbText =. field "textb"))

barSpec :: Spec
barSpec = do
  describe "sum types" $ do
    describe "a" $ do
      let
        h = BarA "one" 1
        d = [ "type" := B.String "a"
            , "texta" := B.String "one"
            , "inta" := B.Int64 1]
      it "can encode" $ do
        encode barCodec h `shouldBe` d

      it "can decode" $ do
        decode barCodec d `shouldBe` Right h

      it "fails decode tag mismatch" $ do
        decode fooCodec [ "type" := B.String "wrong"
                        , "texta" := B.String "one"
                        , "inta" := B.Int64 1]
          `shouldSatisfy` isLeft

      it "fails decode missing field" $ do
        decode fooCodec [ "type" := B.String "a"
                        , "texta" := B.String "one" ]
          `shouldSatisfy` isLeft

    describe "b" $ do
      let
        h = BarB "two"
        d = [ "type" := B.String "b"
            , "textb" := B.String "two" ]
      it "can encode" $ do
        encode barCodec h `shouldBe` d

      it "can decode" $ do
        decode barCodec d `shouldBe` Right h

sumSpec :: Spec
sumSpec = describe "sum" $ do
  fooSpec
  barSpec
