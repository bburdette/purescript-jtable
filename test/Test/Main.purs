module Test where

import Debug.Trace 
import Data.Argonaut
import qualified Data.Argonaut.Core as C
import qualified Data.Argonaut.Encode as E
import qualified Data.Argonaut.Decode as D
import Data.Identity
import Data.Either
import Data.Maybe
import qualified Data.StrMap as SM
import Data.Tuple
import qualified Data.Array as A
import Data.Foldable
import qualified Data.Argonaut.JSemantic as Jc
import Data.Argonaut.JCursor
import Data.Argonaut.JSemantic
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad

import Text.Smolder.HTML (html, head, meta, link, title, body, h1, p, table, td, th, tr)
import Text.Smolder.HTML.Attributes (lang, charset, httpEquiv, content, name, rel, href)
import Text.Smolder.Markup (text, (!), Markup(..))
import Text.Smolder.Renderer.String (render)

----------------------------------------------

main :: forall eff. Control.Monad.Eff.Eff (trace :: Debug.Trace.Trace | eff) Prelude.Unit
main = 
  case (jsonParser bah) of 
    Right json -> do
      let eheads = calcheadings json
      trace $ "heads: " ++ show eheads
      let jcsrs = headsToJcs eheads
      trace $ "jcursors: " ++ show jcsrs
      let blah = flattjs json
      trace $ show blah
      let stacked = stackVals blah
      trace $ show stacked
      let mahdoc = makeTestDoc
      trace $ show mahdoc
      let jcsrlist = makeJcsrList blah
      trace $ show ((\jc -> (Tuple jc (jcdepth jc))) <$> jcsrlist)
    Left err -> do 
      trace "error" 

makeTestDoc = html ! lang "en" $ do 
  head $ do
    meta ! charset "utf-8"
    meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
    title $ text "OMG HAI LOL"
    meta ! name "description" ! content "YES OMG HAI LOL"
    meta ! name "viewport" ! content "width=device-width"
    link ! rel "stylesheet" ! href "css/screen.css"
  body $ do
    h1 $ text "OMG HAI LOL"
    p $ text "This is clearly the best HTML DSL ever invented."
    makeTable ["a", "b", "c"]

makeTable :: [String] -> Markup
makeTable strings = do
  table $ do 
    tr $ th $ text "meh" 
    makeEntries strings

makeEntries :: [String] -> Markup
makeEntries [] = return unit 
makeEntries (s:ss) = do
  tr $ td $ text s
  makeEntries ss

headsToJcs :: [[String]] -> [JCursor]
headsToJcs heads = 
  headsToJc <$> heads

headsToJc :: [String] -> JCursor
headsToJc [] = JCursorTop
headsToJc (str:strs) = 
  JField str (headsToJc strs)

-- trace "---------------------------------------" 

data JSVal = Jsn Number | Jsb Boolean | Jss String

instance showJSVal :: Show JSVal where
  show (Jsn a) = show a
  show (Jsb a) = show a
  show (Jss a) = show a

data JSValStuff = JSValStuff {
  val :: JSVal,
  smantic :: JSemantic
  }

instance showJSValStuff :: Show JSValStuff where
  show (JSValStuff { val: v, smantic: s }) = "{ " ++ show v ++ ", " ++ show s ++ "}"

jsvnull :: C.JNull -> Maybe JSValStuff
jsvnull _ = Nothing 

jsvboolean :: C.JBoolean -> Maybe JSValStuff
jsvboolean b = Just $ JSValStuff { val: Jsb b, smantic: Bool }

jsvnumber :: C.JNumber -> Maybe JSValStuff
jsvnumber n = Just $ JSValStuff { val: Jsn n, smantic: Integral }

jsvstring :: C.JString -> Maybe JSValStuff
jsvstring s = Just $ JSValStuff { val: Jss s, smantic: Text }

makejsv :: forall a. [String] -> (a -> Maybe JSValStuff) -> 
                    a -> [(Tuple JCursor (Maybe JSValStuff))]
makejsv path converter thing = 
  [(Tuple (headsToJc path) (converter thing))] 

flattjs :: C.Json -> [(Tuple JCursor (Maybe JSValStuff))]
flattjs js = 
  flattJson [] js

flattJson :: [String] -> C.Json -> [(Tuple JCursor (Maybe JSValStuff))]
flattJson path json = 
  C.foldJson (makejsv path jsvnull)
             (makejsv path jsvboolean)
             (makejsv path jsvnumber)
             (makejsv path jsvstring)
             (flattJA path) 
             (flattJO path) 
             json

flattJO :: [String] -> C.JObject -> [(Tuple JCursor (Maybe JSValStuff))]
flattJO path jo =  
  SM.fold meh [] jo
  where
    meh z str json = 
      let moopath :: [String]
          moopath = path ++ [str]
        in 
          z ++ flattJson moopath json 

flattJA :: [String] -> C.JArray -> [(Tuple JCursor (Maybe JSValStuff))]
flattJA path ja =  
  A.concat (A.map (flattJson path) ja)

stackVals :: [(Tuple JCursor (Maybe JSValStuff))] -> M.Map JCursor [(Maybe JSValStuff)]
stackVals tupes = 
  foldr doitt M.empty tupes
  where doitt :: (Tuple JCursor (Maybe JSValStuff)) 
               -> M.Map JCursor [(Maybe JSValStuff)] 
               -> M.Map JCursor [(Maybe JSValStuff)] 
        doitt (Tuple jcsr jsvs) map = 
          let val = M.lookup jcsr map 
            in case val of 
              Nothing -> M.insert jcsr [jsvs] map
              Just val -> M.insert jcsr (val ++ [jsvs]) map
 
-- make list of JCursors in order of appearance, without duplicates.  
makeJcsrList :: [(Tuple JCursor (Maybe JSValStuff))] -> [JCursor] 
makeJcsrList tupelist = makeJcsrList' tupelist S.empty

makeJcsrList' :: [(Tuple JCursor (Maybe JSValStuff))] -> S.Set JCursor -> [JCursor] 
makeJcsrList' [] _ = []
makeJcsrList' ((Tuple jcsr _):ts) jcset = 
  if S.member jcsr jcset 
    then (makeJcsrList' ts jcset)
    else (jcsr : (makeJcsrList' ts (S.insert jcsr jcset)))
 
jcdepth :: JCursor -> Number
jcdepth JCursorTop = 0
jcdepth (JField _ jcsr) = 1 + (jcdepth jcsr)

----------------------------

calcheadings :: C.Json -> [[String]]
calcheadings js = 
  calcJsonHeads [] js

calcJsonHeads :: [String] -> C.Json -> [[String]]
calcJsonHeads path json = 
  let cp = (const [path]) in 
  C.foldJson (const [path]) (const [path]) (const [path]) (const [path]) (calcJAHeadings path) (calcJOHeadings path) json

calcJOHeadings :: [String] -> C.JObject -> [[String]]
calcJOHeadings path jo =  
  SM.fold meh [] jo
  where
    meh z str json = 
      let moopath :: [String]
          moopath = path ++ [str]
        in 
          z ++ calcJsonHeads moopath json 

calcJAHeadings :: [String] -> C.JArray -> [[String]]
calcJAHeadings path ja =  
  A.concat (A.map (calcJsonHeads path) ja)


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


