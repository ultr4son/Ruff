module RuffParser where
  import Control.Monad (void)
  import Text.Megaparsec hiding (space)
  import Text.Megaparsec.Expr
  import Text.Megaparsec.String -- input stream is of type ‘String’
  import qualified Text.Megaparsec.Lexer as L
  import RuffModel
  import Data.Scientific

  skipBlockComment = L.skipBlockComment "(" ")"
  space = L.space (void spaceChar <|> void char "!") skipBlockComment skipBlockComment
  symbol = L.symbol space
  symbol' = L.symbol' space
  lexeme = L.lexeme space

  --Parse a form literal in the format {name:Type,value;...}
  formLiteral::Parser LiteralValue
  formLiteral = do
    symbol "{"
    entries <- sepEndBy formEntry ((symbol ";") >> (notFollowedBy numberChar))
    symbol "}"
    return (Form entries)

  --Parse text type declaration
  typeText::Parser FormEntryType
  typeText = do
    symbol' "text"
    return TextEntryT

  --Parse numerical type declaration
  typeNumber::Parser FormEntryType
  typeNumber = do
    symbol' "number"
    return NumberEntryT

  --Pars form type declaration
  typeForm::Parser FormEntryType
  typeForm = do
    symbol' "form"
    return FormEntryT

  --Get type declaration
  formEntryType::Parser FormEntryType
  formEntryType = do
    t <- (typeText <|> typeNumber <|> typeForm)
    return t

  --Parse single form entry in format name:type,value
  formEntry::Parser FormEntry
  formEntry = do
    name <- lexeme (someTill alphaNumChar (string ":"))
    t <- formEntryType
    v <- (formEntryValue t) <|> (return (entryEmpty t))
    return (FormEntry v t name)

  formEntryValue::FormEntryType -> Parser LiteralValue
  formEntryValue t =
    let p = case t of
          TextEntryT -> textLiteral
          FormEntryT -> formLiteral
          NumberEntryT -> numericalLiteral
    in do
      symbol ","
      v <- p
      return v

  --Parse function
  function::Parser Function
  function = do
    optional ((symbol' "get") >> notFollowedBy fArgs)
    f <- lexeme (someTill alphaNumChar (spaceChar))
    args <- fArgs <|> (return [])
    result <- functionResulting <|> (return It)
    return (Function f args result)

  --Parse optional result name
  functionResulting::Parser Reference
  functionResulting = do
    symbol' "named"
    name <- reference
    return name

  --Parse number
  numericalLiteral::Parser LiteralValue
  numericalLiteral = do
    n <- lexeme (L.number)
    return (Number (toRealFloat n))

  --Parse text
  textLiteral::Parser LiteralValue
  textLiteral = do
    char '"'
    t <- lexeme (manyTill L.charLiteral (char '"'))
    return (Text t)

  --Parse literal number, text, form
  literalArg::Parser FArg
  literalArg = do
    v <- numericalLiteral <|> textLiteral <|> formLiteral
    return (Literal v)

  --Parse reference as argument
  refArg::Parser FArg
  refArg = do
    v <- reference
    return (Reference v)

  --Parse argument reference or literal
  functionArgument::Parser FArg
  functionArgument = do
    arg <- refArg <|> literalArg
    return arg

  --Parse function arguments separated by ","
  fArgs::Parser [FArg]
  fArgs = do
    symbol' "with"
    as <- sepBy functionArgument (symbol ",")
    return as

  --Parse it
  itRef::Parser Reference
  itRef = do
    string' "it"
    spaceChar
    space
    return It

  --Parse named reference starting with letter
  namedRef::Parser Reference
  namedRef = do
    l <- letterChar
    n <- lexeme (manyTill alphaNumChar (end))
    return (Named (l:n))

  end::Parser String
  end = string " " <|> lookAhead (symbol ".")

  reference::Parser Reference
  reference = do
    r <- (try itRef) <|> namedRef
    return r

  get::Parser Get
  get = do
    symbol' "from"
    location <- reference
    accessor <- function
    return (Get location accessor)

  put::Parser Put
  put = do
    writer <- function
    value <- reference
    optional ((symbol' "in") <|> (symbol' "on") <|> (symbol' "to"))
    optional (symbol' "the")
    location <- reference
    return (Put writer value location)

  statementGet::Parser Statement
  statementGet = do
    g <- get
    symbol' "and"
    rest <- statement
    return (G g rest)

  statementPut::Parser Statement
  statementPut = do
    p <- put
    symbol "."
    return (P p)

  statement::Parser Statement
  statement = statementGet <|> statementPut

  ruff::Parser [Statement]
  ruff = do
    s <- many statementGet
    return s
