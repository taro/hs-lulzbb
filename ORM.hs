module ORM where
import Data.Char (toLower, toUpper)
import Data.List (intercalate)

data ColType = 
	ColReference String |
	ColInteger |
	ColString Integer |
	ColText |
	ColDatetime

type Table = (String, [(String, ColType)])

capitalize :: String -> String
capitalize "" = ""
capitalize s = (toUpper . head) s : tail s

getReferences :: Table -> [(String, String)]
getReferences (_, cols) =
	map formatRef $ filter onlyRefs cols
		where
			onlyRefs x = case x of
				(_, ColReference _) -> True
				_ -> False
			formatRef (c, ColReference r) =
				(c, r)

typeSql :: ColType -> String
typeSql (ColReference _) = "INTEGER"
typeSql (ColInteger) = "INTEGER"
typeSql (ColString x) = "VARCHAR(" ++ show x ++ ")"
typeSql (ColText) = "TEXT"
typeSql (ColDatetime) = "TIMESTAMP WITHOUT TIME ZONE"

typeHs :: ColType -> String
typeHs (ColReference x) = "Maybe " ++ capitalize x
typeHs (ColInteger) = "Integer"
typeHs (ColString _) = "String"
typeHs (ColText) = "String"
typeHs (ColDatetime) = "Integer"

columnSql :: String -> (String, ColType) -> String
columnSql tName (colName, colType) = 
	tName ++ capitalize colName ++ " " ++ typeSql colType

columnHs :: String -> (String, ColType) -> String
columnHs tName (colName, colType) =
	tName ++ capitalize colName ++ " :: " ++ typeHs colType	

createTableSql :: Table -> String
createTableSql (tableName, cols) = 
	"CREATE TABLE " ++ tableName ++ " (\n\t" ++ pkey ++ columns ++ "\n);"
		where 
			pkey = tableName ++ "Id INTEGER PRIMARY KEY,\n\t"
			columns = intercalate ",\n\t" $ map (columnSql tableName) cols

createRecordHs :: Table -> String
createRecordHs (tableName, cols) =
	"data " ++ cTableName ++ " = " ++ cTableName ++ " {\n\t" ++ pkey ++ columns ++ "\n}"
		where
			cTableName = capitalize tableName
			pkey = tableName ++ "Id :: Integer,\n\t"
			columns = intercalate ",\n\t" $ map (columnHs tableName) cols

parserColumn :: String -> (String, ColType) -> String
parserColumn tName (colName, ColReference _) = 
	tName ++ capitalize colName ++ " = parseSql' pfx sql"
parserColumn tName (colName, ColInteger) = 
	tName ++ capitalize colName ++ " = coerseSql 0 pfx \"" ++ colName ++ "\" sql"
parserColumn tName (colName, ColString _) =
	tName ++ capitalize colName ++ " = coerseSql \"\" pfx \"" ++ colName ++ "\" sql"
parserColumn tName (colName, ColText) =
	tName ++ capitalize colName ++ " = coerseSql \"\" pfx \"" ++ colName ++ "\" sql"
parserColumn tName (colName, ColDatetime) =
	tName ++ capitalize colName ++ " = coerseSql 0 pfx \"" ++ colName ++ "\" sql"

createParserHs :: Table -> String
createParserHs (tableName, cols) =
	let cTableName = capitalize tableName in
	"parseSql' :: String -> Data.Map String SqlValue -> Maybe " ++ cTableName ++ "\nparseSql' pfx sql = \n\tcase lookup (pfx ++ \"" ++ tableName ++ "Id\" of\n\t\tNothing -> Nothing\n\t\t_ -> Just $ " ++ cTableName ++ " {\n\t\t\t" ++ columns ++ " }"
		where columns = intercalate ",\n\t\t\t" $ map (parserColumn tableName) cols
