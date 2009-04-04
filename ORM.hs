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
