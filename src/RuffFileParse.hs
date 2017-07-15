module RuffFileParse(parseFile) where
	import Text.Megaparsec
	import Text.Megaparsec.String

	parseFile::(Show a) => String->Parser a->IO()
	parseFile path parser= do
		text <- readFile path
		let result = parse (dbg "parser" parser) path text in
			case result of
				(Left err) -> putStrLn (parseErrorPretty err)
				(Right r) -> print r

