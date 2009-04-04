module ORM where
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Database.HDBC (fromSql)

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

coerseSql d p k sql = maybe d fromSql $ Map.lookup (p ++ k) sql

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

getReferences :: Table -> [(String, String)]
getReferences (_, cols) =
	map formatRef $ filter onlyRefs cols
		where
			onlyRefs x = case x of
				(_, ColReference _) -> True
				_ -> False
			formatRef (c, ColReference r) =
				(c, r)

createRefSql :: Table -> String
createRefSql t@(tableName, _) = 
	intercalate "\n" $ map refSql $ getReferences t
		where refSql (colName, refTable) = "ALTER TABLE " ++ tableName ++ " ADD FOREIGN KEY (" ++ tableName ++ capitalize colName ++ ") REFERENCES " ++ refTable ++ ";"	

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
	"instance DbRecord " ++ cTableName ++ " where\n\tparseSql' pfx sql = \n\t\tcase Map.lookup (pfx ++ \"" ++ tableName ++ "Id\") sql of\n\t\t\tNothing -> Nothing\n\t\t\t_ -> Just $ " ++ cTableName ++ " {\n\t\t\t\t" ++ columns ++ " }"
		where columns = intercalate ",\n\t\t\t\t" $ map (parserColumn tableName) $ ("id", ColInteger) : cols

dumpSchema :: [Table] -> String
dumpSchema tbls = (unlines . map ($ tbls)) [
	intercalate "\n\n" . map createTableSql,
	intercalate "\n" . map createRefSql]

dumpRecordHs :: [Table] -> String
dumpRecordHs = intercalate "\n\n" . map createRecordHs

dumpParserHs :: [Table] -> String
dumpParserHs = intercalate "\n\n" . map createParserHs

dumpSchemaHs :: String -> [Table] -> String
dumpSchemaHs siteName tbls =
	header ++ dumpRecordHs tbls ++ "\n\n" ++ dumpParserHs tbls
		where header =
			unlines [
				"module " ++ siteName ++ ".ORM where",
				"import Database.HDBC (SqlValue)",
				"import qualified Data.Map as Map",
				"import ORM",
				"",
				"parseSql :: DbRecord a => Map.Map String SqlValue -> Maybe a",
				"parseSql = parseSql' \"\"",
				"",
				"class DbRecord a where",
				"\tparseSql' :: String -> Map.Map String SqlValue -> Maybe a",
				""]
