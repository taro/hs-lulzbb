module ORM where
import Data.Char (toLower)
import Data.List (intercalate)

data ColType = 
	ColReference String |
	ColInteger |
	ColString Integer |
	ColText |
	ColDatetime

type Table = (String, [(String, ColType)])

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
typeSql (ColDatetime) = "TIMEZONE WITHOUT TIME STAMP"

columnSql :: String -> (String, ColType) -> String
columnSql tName (colName, colType) = 
	tName ++ "_" ++ colName ++ " " ++ typeSql colType

createTableSql :: Table -> String
createTableSql (tableName, cols) = 
	"CREATE TABLE " ++ tableName ++ " (\n\t" ++ pkey ++ columns ++ "\n);"
		where 
			pkey = tableName ++ "_id INTEGER PRIMARY KEY,\n\t"
			columns = intercalate ",\n\t" $ map (columnSql tableName) cols
