module Test where

import Debug.Trace 
import Data.Argonaut
-- import Data.Argonaut.Core  
import qualified Data.Argonaut.Core as C
import qualified Data.Argonaut.Encode as E
import qualified Data.Argonaut.Decode as D
import Data.Identity
import Data.Either
import Data.Maybe
import qualified Data.StrMap as M
import Data.Tuple

data Foo = Foo
    { foo :: String
    , bar :: Number
    }

instance showFoo :: Show Foo where
    show (Foo f) = "Foo(" ++ show f.foo ++ ", " ++ show f.bar ++ ")"

instance decodeFoo :: D.DecodeJson Foo where
    decodeJson json = maybe (Left "not json for foo") Right $ do
      obj <- C.toObject json
      foo <- (M.lookup "foo" obj >>= C.toString)
      bar <- (M.lookup "bar" obj >>= C.toNumber)
      pure (Foo {foo: foo, bar: bar})

instance encodeFoo :: E.EncodeJson Foo where
  encodeJson (Foo {foo = f, bar = b}) =  fromObject $ M.fromList [Tuple "foo" $ C.fromString f, Tuple "bar" $ C.fromNumber b]

--    encodeJson :: a -> Json
main = do 
  trace $ bah

bah :: String
bah = """
{
  "userId": 8927524,
  "profile": {
    "name":   "Mary Jane",
    "age":    29,
    "gender": "female"
  },
  "comments": [{
    "id":       "F2372BAC",
    "text":     "I concur.",
    "replyTo":  [9817361, "F8ACD164F"],
    "time":     "2015-02-03"
  }, {
    "id":       "GH732AFC",
    "replyTo":  [9654726, "A44124F"],
    "time":     "2015-03-01"
  }]
}
"""

