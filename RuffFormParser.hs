module RuffFormParser where
  import Control.Monad (void)
  import Text.Megaparsec hiding (space)
  import Text.Megaparsec.Expr
  import Text.Megaparsec.String -- input stream is of type ‘String’
  import qualified Text.Megaparsec.Lexer as L
  import RuffModel

  skipBlockComment = L.skipBlockComment "(" ")"
  space = L.space (void spaceChar) skipBlockComment skipBlockComment
  symbol = L.symbol space
  symbol' = L.symbol' space
  lexeme = L.lexeme space

  formLiteral::Parser LiteralValue
  formLiteral = do
    symbol "{"
    entries <- sepBy formEntry (symbol ".")
    symbol "}"
    return entries

  typeText::Parser FormEntryType
  typeText = do
    symbol' "text"
    return TextEntryT

  typeNumber::Parser FormEntryType
  typeNumber = do
    symbol' "number"
    return NumberEntryT

  typeForm::Parser FormEntryType
  typeForm = do
    symbol' "form"
    return FormEntryT

  formEntryType::Parser FormEntryType
  formEntryType = do
    t <- (typeText <|> typeNumber <|> typeForm)
    return t

  formEntry::Parser FormEntry
  formEntry = do
    name <- lexeme (someTill alphaNumChar (string ":"))
    t <- formEntryType
    v <- (formEntryValue <|> (return (entryEmpty t)))
    return (FormEntry v t name)

  formEntryValue::FormEntryType -> Parser LiteralValue
  formEntryValue t = do
    symbol ","
    v <- numericalLiteral <|> textLiteral <|> formLiteral
    return v
