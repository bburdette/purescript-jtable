module Data.Argonaut.JSemantic where

-- Data.Argonaut.JSemantic

data JSemantic =       -- this could be moved to Data.Argonaut
    Integral    |
    Fractional  |
    Date        |
    DateTime    |
    Time        |
    Interval    |
    Text        |
    Bool        |
    Percent     |
    Currency    |
    NA

instance showJSemantic :: Show JSemantic where
  show Integral = "Integral"
  show Fractional = "Fractional"
  show Date = "Date"
  show DateTime = "DateTime"
  show Time = "Time"
  show Interval = "Interval"
  show Text = "Text"
  show Bool = "Bool"
  show Percent = "Percent"
  show Currency = "Currency"
  show NA = "NA"

