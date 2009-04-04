module ORM (
	ColType (..), 
	Table, 
	coerseSql, 
	dumpSchema,
	dumpSchemaHs
	) where

import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Database.HDBC (fromSql)

{-|
	Type definitions for all of the SQL types. To add a new one, you
	have to add it here, then create a corresponding typeHs and typeSql
	function (I guess I should enforce that statically).
-}
data ColType = 
	{-| A reference to another table. Creates a foreign key. -}
	ColReference String |
	{-| A vanilla INTEGER -}
	ColInteger |
	{-| A VARCHAR(x) -}
	ColString Integer |
	{-| A TEXT field -}
	ColText |
	{-| A timestamp. Kind of on the fence about handling these -- since
	    HDBC has some issues with TIMESTAMP WITHOUT TIME ZONE, ugh -}
	ColDatetime

{-|
	Type definition for a table. Eventually this should be wrapped up
	in a Yaml parser or something, but for now you have to define your
	tables in Haskell somewhere.

	A table is a (TableName, [(ColumnName, ColumnType)]).

	An "Id" column (for the primary key) is generated automatically.
-}
type Table = (String, [(String, ColType)])

{-|
	Capitalizes the first letter in a string. Should rip this part out
	and replace it with some more sophisticated table sanitization stuff.
-}
capitalize :: String -> String
capitalize "" = ""
capitalize s = (toUpper . head) s : tail s

{-|
	Helper function used by the generated code to extract a typed value
	from a Map (String, SqlValue), or use a default if the key doesn't
	exist.
-}
coerseSql d p k sql = maybe d fromSql $ Map.lookup (p ++ k) sql

{-|
	Converts the ColType into the corresponding SQL type notation.
-}
typeSql :: ColType -> String
typeSql (ColReference _) = "INTEGER"
typeSql (ColInteger) = "INTEGER"
typeSql (ColString x) = "VARCHAR(" ++ show x ++ ")"
typeSql (ColText) = "TEXT"
typeSql (ColDatetime) = "TIMESTAMP WITHOUT TIME ZONE"

{-|
	Converts the ColType into the corresponding Haskell type notation.
-}
typeHs :: ColType -> String
typeHs (ColReference x) = "Maybe " ++ capitalize x
typeHs (ColInteger) = "Integer"
typeHs (ColString _) = "String"
typeHs (ColText) = "String"
typeHs (ColDatetime) = "Integer"

{-|
	Takes the table name and a column and outputs the SQL fragment for
	the CREATE TABLE statement.
-}
columnSql :: String -> (String, ColType) -> String
columnSql tName (colName, colType) = 
	tName ++ capitalize colName ++ " " ++ typeSql colType

{-|
	Takes the table name and a column and outputs the Haskell fragment
	for the column's entry in the table record type.
-}
columnHs :: String -> (String, ColType) -> String
columnHs tName (colName, colType) =
	tName ++ capitalize colName ++ " :: " ++ typeHs colType	

{-|
	Takes the table and outputs the whole CREATE TABLE statement used
	to generate the proper schema in SQL.
-}
createTableSql :: Table -> String
createTableSql (tableName, cols) = 
	"CREATE TABLE " ++ tableName ++ " (\n\t" ++ pkey ++ columns ++ "\n);"
		where 
			pkey = tableName ++ "Id INTEGER PRIMARY KEY,\n\t"
			columns = intercalate ",\n\t" $ map (columnSql tableName) cols

{-|
	Filters out all of the ColReference columns of the table, and returns
	a [(ColumnName, ReferencedTable)].
-}
getReferences :: Table -> [(String, String)]
getReferences (_, cols) =
	map formatRef $ filter onlyRefs cols
		where
			onlyRefs x = case x of
				(_, ColReference _) -> True
				_ -> False
			formatRef (c, ColReference r) =
				(c, r)

{-|
	Creates the SQL to add FOREIGN KEY constraints to all of the columns
	which have a ColReference type.
-}
createRefSql :: Table -> String
createRefSql t@(tableName, _) = 
	intercalate "\n" $ map refSql $ getReferences t
		where refSql (colName, refTable) = "ALTER TABLE " ++ tableName ++ " ADD FOREIGN KEY (" ++ tableName ++ capitalize colName ++ ") REFERENCES " ++ refTable ++ ";"	

{-|
	Creates the Haskell code for a DbRecord which exposes all the fields
	of the supplied table.
-}
createRecordHs :: Table -> String
createRecordHs (tableName, cols) =
	"data " ++ cTableName ++ " = " ++ cTableName ++ " {\n\t" ++ pkey ++ columns ++ "\n}"
		where
			cTableName = capitalize tableName
			pkey = tableName ++ "Id :: Integer,\n\t"
			columns = intercalate ",\n\t" $ map (columnHs tableName) cols

{-|
	Given a table name and a column, outputs a row of Haskell code to
	convert a Map (String, SqlValue) into the proper type. Will recursively
	call parseSql' for ColReferences.
-}
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

{-|
	Takes the table and returns a Haskell fragment which defines the
	parseSql' function for record type which corresponds to the table.
-}
createParserHs :: Table -> String
createParserHs (tableName, cols) =
	let cTableName = capitalize tableName in
	"instance DbRecord " ++ cTableName ++ " where\n\tparseSql' pfx sql = \n\t\tcase Map.lookup (pfx ++ \"" ++ tableName ++ "Id\") sql of\n\t\t\tNothing -> Nothing\n\t\t\t_ -> Just $ " ++ cTableName ++ " {\n\t\t\t\t" ++ columns ++ " }"
		where columns = intercalate ",\n\t\t\t\t" $ map (parserColumn tableName) $ ("id", ColInteger) : cols

{-|
	Takes a list of tables and returns the complete SQL listing to 
	produce the schema. (CREATE TABLE and ADD FOREIGN KEY).
-}
dumpSchema :: [Table] -> String
dumpSchema tbls = (unlines . map ($ tbls)) [
	intercalate "\n\n" . map createTableSql,
	intercalate "\n" . map createRefSql]

{-|
	Converts a list of tables into a bunch of Haskell record definitions.
-}
dumpRecordHs :: [Table] -> String
dumpRecordHs = intercalate "\n\n" . map createRecordHs

{-|
	Converts a list of tables into the corresponding parseSql'
	instances for each of the record types.
-}
dumpParserHs :: [Table] -> String
dumpParserHs = intercalate "\n\n" . map createParserHs

{-|
	Converts the list of tables into a complete Haskell source file which
	defines all the records (and functionality to map those records out
	of a Map (String, SqlValue)) for the tables.

	First argument is the SiteName, which is used to name the module.
-}
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
