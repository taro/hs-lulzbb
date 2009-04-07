module ORM (
	ColType (..), 
	Table, 
	coerseSql, 
	dumpSchema,
	dumpSchemaHs
	) where

import Control.Arrow (second)
import Data.Char (toLower, toUpper, isUpper)
import Data.List (intercalate, unfoldr)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Database.HDBC (fromSql)
import Text.Regex.Posix ((=~))

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
	Uncapitalizes the first letter in a string.
-}
uncapitalize :: String -> String
uncapitalize "" = ""
uncapitalize s = (toLower . head) s : tail s

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
	Helper function to transform the Haskell names to SQL names. I like to
	use camelCase in Haskell, but PostgreSQL is case insensitive, so it's kind
	of ugh. This might help a bit kekeke.
-}
columnNameSql :: String -> String -> String
columnNameSql tableName =
	intercalate "_" .  map uncapitalize . (tableName :) . (=~ "((^|[A-Z])[a-z]*)") 

{-|
	Takes the table name and a column and outputs the SQL fragment for
the CREATE TABLE statement.
-}
columnSql :: String -> (String, ColType) -> String
columnSql tName (colName, colType) = 
	columnNameSql tName colName ++ " " ++ typeSql colType

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
createTableSql :: String -> Table -> String
createTableSql pfx (tableName, cols) = 
	unlines [
		"DROP TABLE IF EXISTS \"" ++ pfx ++ tableName ++ "\";",
		"CREATE TABLE \"" ++ pfx ++ tableName ++ "\" (\n\t" ++ pkey ++ columns ++ "\n);"
		]
		where 
			pkey = columnNameSql tableName "Id" ++ " INTEGER PRIMARY KEY,\n\t"
			columns = intercalate ",\n\t" $ map (columnSql $ tableName) cols

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
createRefSql :: String -> Table -> String
createRefSql pfx t@(tableName, _) = 
	intercalate "\n" $ map refSql $ getReferences t
		where refSql (colName, refTable) = "ALTER TABLE \"" ++ pfx ++ tableName ++ "\" ADD FOREIGN KEY (" ++ columnNameSql tableName colName ++ ") REFERENCES \"" ++ pfx ++ refTable ++ "\";"	

{-|
	Creates the SQL to CREATE SEQUENCE and set that sequence as the default
	value for the primary key of the table. This only really works for
	PostgreSQL -- will have to override (somehow lol?) for shit databases.
-}
createSeqSql :: String -> Table -> String
createSeqSql pfx (tableName, _) =
	let seqName = columnNameSql "seq" (tableName ++ "Id") in
	unlines [
		"DROP SEQUENCE IF EXISTS " ++ seqName ++ ";",
		"CREATE SEQUENCE " ++ seqName ++ ";",
		"ALTER TABLE \"" ++ pfx ++ tableName ++ "\" ALTER COLUMN " ++ columnNameSql tableName "Id" ++ " SET DEFAULT NEXTVAL('" ++ seqName ++ "');"
	]

{-|
	Creates the Haskell code for a DbRecord which exposes all the fields
	of the supplied table.
-}
createRecordHs :: Table -> String
createRecordHs (tableName, cols) =
	"data " ++ cTableName ++ " = " ++ cTableName ++ " {\n\t" ++ pkey ++ columns ++ "\n} deriving (Show)"
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
	tName ++ capitalize colName ++ " = coerseSql 0 pfx \"" ++ columnNameSql tName colName ++ "\" sql"
parserColumn tName (colName, ColString _) =
	tName ++ capitalize colName ++ " = coerseSql \"\" pfx \"" ++ columnNameSql tName colName ++ "\" sql"
parserColumn tName (colName, ColText) =
	tName ++ capitalize colName ++ " = coerseSql \"\" pfx \"" ++ columnNameSql tName colName ++ "\" sql"
parserColumn tName (colName, ColDatetime) =
	tName ++ capitalize colName ++ " = coerseSql 0 pfx \"" ++ columnNameSql tName colName ++ "\" sql"

{-|
	Takes the table and returns a Haskell fragment which defines the
	parseSql' function for record type which corresponds to the table.
-}
createParserHs :: Table -> String
createParserHs (tableName, cols) =
	let cTableName = capitalize tableName in
	"instance DbRecord " ++ cTableName ++ " where\n\tparseSql' pfx sql = \n\t\tcase Map.lookup (pfx ++ \"" ++ columnNameSql tableName "Id" ++ "\") sql of\n\t\t\tNothing -> Nothing\n\t\t\t_ -> Just $ " ++ cTableName ++ " {\n\t\t\t\t" ++ columns ++ " }"
		where columns = intercalate ",\n\t\t\t\t" $ map (parserColumn tableName) $ ("id", ColInteger) : cols

{-|
	Creates encode functions for each record type so all the records
	can be dumped into a JSON-encoded string.
-}
createJsonEncoderHs :: Table -> String
createJsonEncoderHs (tableName, cols) =
	let cTableName = capitalize tableName in
	"instance JSON " ++ cTableName ++ " where\n\tshowJSON x = makeObj [\n\t\t" ++ fields ++ "]\n\treadJSON = undefined"
		where 
			fields = intercalate ",\n\t\t" $ map encoderField $ ("id", ColInteger) : cols
			encoderField (col, _) = "(\"" ++ tableName ++ capitalize col ++ "\", showJSON $ " ++ tableName ++ capitalize col ++ " x)"


{-|
	Takes a list of tables and returns the complete SQL listing to 
	produce the schema. (CREATE TABLE and ADD FOREIGN KEY). First
	argument is a prefix which is appended to all table names.
-}
dumpSchema :: String -> [Table] -> String
dumpSchema pfx tbls = (unlines . map ($ tbls)) [
	intercalate "\n\n" . map (createTableSql pfx),
	intercalate "\n" . map (createSeqSql pfx),
	intercalate "\n" . map (createRefSql pfx)]

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
	Crafts JSON encoders from a list of tables.
-}
dumpJsonEncoderHs :: [Table] -> String
dumpJsonEncoderHs = intercalate "\n\n" . map createJsonEncoderHs

{-|
	Converts the list of tables into a complete Haskell source file which
	defines all the records (and functionality to map those records out
	of a Map (String, SqlValue)) for the tables.

	First argument is the SiteName, which is used to name the module.
-}
dumpSchemaHs :: String -> [Table] -> String
dumpSchemaHs siteName tbls =
	intercalate "\n\n" [
		header,
		dumpRecordHs tbls,
		dumpParserHs tbls,
		dumpJsonEncoderHs tbls
		] where
			header = unlines [
				"module " ++ siteName ++ ".ORM where",
				"import Database.HDBC (SqlValue)",
				"import qualified Data.Map as Map",
				"import ORM",
				"import Text.Json (encode, toJSObject)",
				"",
				"parseSql :: DbRecord a => Map.Map String SqlValue -> Maybe a",
				"parseSql = parseSql' \"\"",
				"",
				"class DbRecord a where",
				"\tparseSql' :: String -> Map.Map String SqlValue -> Maybe a",
				""]
