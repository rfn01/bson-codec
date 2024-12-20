module Value (valueSpec) where

import Data.Bson.Codec

import Test.Hspec
import Data.Bson qualified as B
import Data.Bson (Field((:=)))


--------------------------------------------------------------------------------
-- nested document withOUT B.Val

data NestOuter = NestOuter {noText :: !Text, noIn :: !NestInner} deriving stock (Eq, Show)

data NestInner = NestInner {niText :: !Text, niInt :: !Int64} deriving stock (Eq, Show)

outerCodec :: BsonCodec NestOuter
outerCodec = NestOuter <$> noText =. label "text" viaVal <*> noIn =. label "inner" (doc innerCodec)
-- slightly silly use of viaVal, as `field "test"` would have worked just as well

innerCodec :: BsonCodec NestInner
innerCodec = NestInner <$> niText =. field "text" <*> niInt =. field "int"

nestedSpec :: Spec
nestedSpec = do
  describe "nested" $ do
    let
      hi = NestInner "two" 2
      ho = NestOuter "one" hi
      d = ["text" := B.String "one", "inner" := B.Doc ["text" := B.String "two", "int" := B.Int64 2]]

    it "can encode" $ do
      encode outerCodec ho `shouldBe` d

    it "can decode" $ do
      decode outerCodec d `shouldBe` Right ho

-- same for array

data ListOuter = ListOuter {loText :: !Text, loIns :: ![NestInner]} deriving stock (Eq, Show)

listOuterCodec :: BsonCodec ListOuter
listOuterCodec = ListOuter <$> loText =. field "text" <*> loIns =. label "inners" (array (doc innerCodec))

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
-- as - bring your own conversation function

data Color = Red | Green | Blue deriving stock (Eq, Show, Read)

data AsColor = AsColor {aText :: !Text, aColor :: !Color} deriving stock (Eq, Show)

asColor :: ValueCodec Color
asColor =  as inject project
  where 
    inject :: Color -> Maybe B.Value
    inject = pure . B.String . show

    project :: B.Value -> Maybe Color
    project v = (case v of
                    B.String t -> Just t
                    _ -> Nothing)
                >>= readMaybe . toString

asColorCodec :: BsonCodec AsColor
asColorCodec = AsColor <$> aText =. field "text" <*> aColor =. label "color" asColor

asColorSpec :: Spec
asColorSpec = do
  describe "as" $ do
    let
      h = AsColor "one" Blue
      d = ["text" := B.String "one", "color" := B.String "Blue"]

    it "can encode" $ do
      encode asColorCodec h `shouldBe` d

    it "can decode" $ do
      decode asColorCodec d `shouldBe` Right h

    it "can fail to decode" $ do
      decode asColorCodec ["text" := B.String "one", "color" := B.String "blue"]
        `shouldSatisfy` isLeft


valueSpec :: Spec
valueSpec = describe "value" $ do
  nestedSpec
  nestedListSpec
  asColorSpec
