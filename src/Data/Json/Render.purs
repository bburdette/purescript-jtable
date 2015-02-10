module Render where

type Level = Number

{-
data ColumnOrdering = InOrdering | CustomOrdering (JCursor -> JCursor -> Ordering)

-- Table style does not create the structure of a table, it only styles
-- a table that's already been created.
data TableStyle = 
  TableStyle { 
    table   :: Level      -> -- is this a top-level table? or a nested table?
               Markup     -> -- the unstyled <table> element
               Markup,       -- the styled <table> element

    td      :: JSemantic  -> -- what type of cell (for purposes of formatting only!)
               Markup     -> -- the unstyled <td> element
               Markup,       -- the styled <td> element

    th      :: JCursor    -> -- the path associated with the header
               Markup     -> -- the unstyled <th> element
               Markup,       -- the styled <th> element

    tr      :: Markup     -> -- the unstyled <tr> element
               Markup }      -- the styled <tr> element

renderJTable :: TableStyle      -> -- the style to apply to the table
                ColumnOrdering  -> -- how to order the columns
                [Json]          -> -- the array of JSON values
                Markup             -- the final HTML markup for the table

-}
