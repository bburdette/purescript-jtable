module Test where

import Debug.Trace 
import Data.Argonaut
import qualified Data.Argonaut.Core as C
import qualified Data.Argonaut.Encode as E
import qualified Data.Argonaut.Decode as D
import Data.Identity
import Data.Either
import Data.Maybe
import qualified Data.StrMap as M
import Data.Tuple
import Data.Array
import qualified Data.Argonaut.JSemantic as Jc

main = do
  let ebahson = jsonParser bah
      eheads = calcheadings <$> ebahson
  trace $ "heads: " ++ show eheads

  -- eobject = toObject <$> ebahson
  -- trace $ "eobject: " ++ show eobject
  -- trace "---------------------------------------" 

calcheadings :: C.Json -> [[String]]
calcheadings js = 
  calcJsonHeads [] js

calcJsonHeads :: [String] -> C.Json -> [[String]]
calcJsonHeads path json = 
  let cp = (const [path]) in 
  C.foldJson (const [path]) (const [path]) (const [path]) (const [path]) (calcJAheadings path) (calcJOheadings path) json

calcJOheadings :: [String] -> C.JObject -> [[String]]
calcJOheadings path jo =  
  M.fold meh [] jo
  where
    meh z str json = 
      let moopath :: [String]
          moopath = path ++ [str]
        in 
          z ++ calcJsonHeads moopath json 

calcJAheadings :: [String] -> C.JArray -> [[String]]
calcJAheadings path ja =  
  concat (map (calcJsonHeads path) ja)



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


