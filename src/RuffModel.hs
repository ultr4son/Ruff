module RuffModel where

  data Reference = It | Named String deriving(Show)
  data LiteralValue = Number Float | Text String | Form [FormEntry] | Null deriving(Show)
  data FormEntry = FormEntry LiteralValue FormEntryType String deriving(Show)
  data Function = Function String [FArg] Reference deriving(Show)
  data FArg = Literal LiteralValue | Reference Reference deriving(Show)
  data Get = Get{getLocation::Reference, accessor::Function} deriving (Show)
  data Put = Put{writer::Function, value::Reference, putLocation::Reference} deriving (Show)
  data Statement = G Get Statement | P Put deriving(Show)

  data FormEntryType = TextEntryT | NumberEntryT | FormEntryT deriving(Show)

  entryEmpty::FormEntryType -> LiteralValue
  entryEmpty TextEntryT = Text ""
  entryEmpty NumberEntryT = Number 0
  entryEmpty FormEntryT = Form []
