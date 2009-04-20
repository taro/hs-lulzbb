{-# LANGUAGE TemplateHaskell #-}
module ORM2 where

import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Regex.Posix

{-|
	Type definitions for all of the SQL types. To add a new one, you
	have to add it here, then create a corresponding genTypeHs and genTypeSql
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

{-| Helper function which capitalizes the first letter of a string -}
capitalize :: String -> String
capitalize "" = ""
capitalize s = (toUpper . head) s : tail s

{-| Helper function which uncapitalizes the first letter of a string -}
uncapitalize :: String -> String
uncapitalize "" = ""
uncapitalize s = (toLower . head) s : tail s

{-| Takes a ColType and transforms it into an SQL fragment representing the 
	corresponding SQL type -}
genTypeSql :: ColType -> String
genTypeSql (ColReference _) = "INTEGER"
genTypeSql (ColInteger) = "INTEGER"
genTypeSql (ColString x) = "VARCHAR(" ++ show x ++ ")"
genTypeSql (ColText) = "TEXT"
genTypeSql (ColDatetime) = "TIMESTAMP WITHOUT TIME ZONE"

{-| Takes a ColType and transforms it into a Type fragment -}
genTypeHs :: ColType -> Type
genTypeHs (ColReference x) = AppT (ConT $ mkName "Maybe") (VarT $ mkName $ capitalize x)
genTypeHs (ColInteger) = VarT $ mkName "Integer"
genTypeHs (ColString _) = VarT $ mkName "String"
genTypeHs (ColText) = VarT $ mkName "String"
genTypeHs (ColDatetime) = VarT $ mkName "LocalTime"

{-| Takes the table name and a column name and returns an SQL fragment
	representing the standardized name of the column -}
genColNameSql :: String -> String -> String
genColNameSql tableName =
	intercalate "_" . map uncapitalize . (tableName :) . (=~ "((^|[A-Z])[a-z]*)")

{-| Takes the table name and a column name, returns a Name fragment
	representing the Haskell representation of the corresponding field -}
genColNameHs :: String -> String -> Name
genColNameHs tableName colName =
	mkName $ (uncapitalize tableName) ++ capitalize colName	

{-| Transforms the table name into the SQL equivalent -}
genTableNameSql :: String -> String
genTableNameSql = uncapitalize

{-| Transofrms the table name into a Name fragment -}
genTableNameHs :: String -> Name
genTableNameHs = mkName . capitalize

{-|
	Takes the table name and a column definition and returns a string
	SQL fragment which defines the column (as per CREATE TABLE syntax.
-}
genColumnSql :: String -> (String, ColType) -> String
genColumnSql tName (colName, colType) =
	genColNameSql tName colName ++ " " ++ genTypeSql colType

{-|
	Uses the table name and column definition to create a VarStrictType
	fragment, which is used in the RecD (record declaration).
-}
genColumnHs :: String -> (String, ColType) -> VarStrictType
genColumnHs tName (colName, colType) =
	(genColNameHs tName colName, IsStrict, genTypeHs colType)

{-|
	Takes a table prefix and returns the CREATE TABLE statements required
	to instanciate the table in an RDBMS. Automatically injects an "id"
	column to act as a primary key.
-}
genTableSql :: String -> Table -> String
genTableSql pfx (rawTableName, cols) =
	unlines [
		"DROP TABLE IF EXISTS \"" ++ pfx ++ tableName ++ "\";",
		"CREATE TABLE \"" ++ pfx ++ tableName ++ "\" (\n\t" ++ pkey ++ columns ++ "\n);"
	]
	where
		tableName = genTableNameSql rawTableName
		pkey = genColNameSql tableName "Id" ++ " INTEGER PRIMARY KEY,\n\t"
		columns = intercalate ",\n\t" $ map (genColumnSql tableName) cols

{-| Filters out all the ColReference entries, returns (colName, refTable) -}
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
	Takes a table prefix and a table; looks through all the foreign
	references and returns a sequence of SQL statements to add foreign
	key constraints to the table.
-}
genRefSql :: String -> Table -> String
genRefSql pfx t@(rawTableName, _) =
	unlines $ map refSql $ getReferences t
	where 
		tableName = "\"" ++ pfx ++ rawTableName ++ "\""	
		refSql (colName, refTable) =
			"ALTER TABLE " ++ tableName ++ 
			" ADD FOREIGN KEY (" ++ genColNameSql rawTableName colName ++ ")" ++ 
			" REFERENCES " ++ tableName ++ ";"

{-|
	Takes a table prefix and a table; returns an SQL statement to create
	a sequence for the table's primary key and to set that sequence value
	as the primary key's default.
-}
genSeqSql :: String -> Table -> String
genSeqSql pfx (rawTableName, _) =
	unlines [
		"DROP SEQUENCE IF EXISTS " ++ seqName ++ ";",
		"CREATE SEQUENCE " ++ seqName ++ ";",
		"ALTER TABLE \"" ++ pfx ++ rawTableName ++ "\"" ++
		" ALTER COLUMN " ++ genColNameSql rawTableName "Id" ++
		" SET DEFAULT NEXTVAL('" ++ seqName ++ "');"
	]
	where
		seqName = genColNameSql "seq" (rawTableName ++ "Id")

{-|
	Takes a table prefix (XXX: Why?) and a table definition, returns
	a declaration of the corresponding record which explicitly types
	all of the fields, ie

	data Forum = Forum {forumId :: Integer
	                   ,forumName :: String} etc
-}
genTableHs :: String -> Table -> Dec
genTableHs _ (rawTableName, cols) =
	DataD [] tableName [] [rcon] [mkName "Show"] 
	where
		tableName = genTableNameHs rawTableName
		rcon = RecC tableName $ map (genColumnHs rawTableName) cols

{-|
	Returns a declaration for a function, recId, which simply aliases
	the accessor function for the table's primary key.
-}
genRecIdHs :: Table -> Dec
genRecIdHs (rawTableName, _) =
	FunD funName [clause]
	where
		funName = mkName "recId"
		colName = genColNameHs rawTableName "Id"
		clause = Clause [] (NormalB (ConE colName)) []

{-| 
	Helper function which generates a template snippet for
		fieldName = coerseSql !default pfx "!sqlName" sql :: !type
	which is used by all of the non-ColReference rows to generate code.
-}
genParseColHs_ :: String -> String -> Exp -> Type -> FieldExp
genParseColHs_ tName colName colDefault colType =
	(genColNameHs tName colName, signedExpr)
	where
		signedExpr = SigE callExpr exprType
		fun = VarE $ mkName "coerceSql"
		args = [
			colDefault,
			VarE $ mkName "pfx",
			LitE $ StringL $ genColNameSql tName colName,
			VarE $ mkName "sql"
			]
		callExpr = foldl AppE fun args
		exprType = colType

{-|
	Function which converts a single TableName, Column to the Haskell
	snippet which pulls the value from an SQL result set.
-}
genParseColHs :: String -> (String, ColType) -> FieldExp

{-| fieldName = Nothing :: Maybe recordType -}
genParseColHs tName (colName, colType@(ColReference _)) =
	(genColNameHs tName colName, signedExpr)
	where
		signedExpr = SigE expr exprType
		expr = ConE $ mkName "Nothing"
		exprType = AppT (ConT $ mkName "Maybe") $ VarT $ genTableNameHs tName

{-| fieldName = coerseSql 0 pfx "sqlName" sql :: Integer -}
genParseColHs tName (colName, colType@(ColInteger)) =
	genParseColHs_ tName colName colDefault colType
	where
		colDefault = LitE $ IntegerL 0
		colType = ConT $ mkName "Integer"

{-| fieldName = coerseSql "" pfx "sqlName" sql :: String -}
genParseColHs tName (colName, colType@(ColText)) =
	genParseColHs_ tName colName colDefault colType
	where
		colDefault = LitE $ StringL ""
		colType = ConT $ mkName "String"

{-| Pass through to the handler for ColText since they're the same -}
genParseColHs tName (colName, colType@(ColString _)) =
	genParseColHs tName (colName, ColText)

{-| fieldName = coerseSql undefined pfx "sqlName" sql :: LocalTime -}
genParseColHs tName (colName, colType@(ColDatetime)) =
	genParseColHs_ tName colName colDefault colType
	where
		colDefault = VarE $ mkName "undefined"
		colType = ConT $ mkName "LocalTime"

{-|
	Returns a declaration for the parseSql' function, which looks like

		parseSql' = \ pfx sql ->
			case Data.Map.lookup (pfx ++ ID_FIELD) sql of
				Nothing -> Nothing
				_ -> Just $ REC_CONSTRUCTOR {
					SQL_PARSERS
					}

	It's a complete mess (XXX: possibly not needlessly so with [| stuff |]
-}
genParseSqlHs :: Table -> Dec
genParseSqlHs (rawTableName, cols) = 
	FunD funName [Clause [] (NormalB eFun) []]
	where
		funName = mkName "parseSql'"
		-- \ pfx sql -> eCase
		eFun = LamE [VarP $ mkName "pfx", VarP $ mkName "sql"] eCase
		-- case eLookup of
		eCase = CaseE eLookup [matchNothing, matchElse]
		-- Data.Map.lookup 
		eLookup = AppE (VarE $ mkName "Data.Map.lookup") eIdColName
		-- pfx ++ 
		eIdColName' = AppE (VarE $ mkName "++") $ VarE $ mkName "pfx"
		-- "table_id"
		eIdColName = AppE eIdColName' $ LitE $ StringL $ genColNameSql rawTableName "id"
		-- Nothing -> eNothing
		matchNothing = Match (ConP (mkName "Nothing") []) (NormalB eNothing) []
		-- _ -> eElse
		matchElse = Match WildP (NormalB eElse) []
		-- Nothing
		eNothing = ConE $ mkName "Nothing"
		-- Just $ TableName {...}
		eElse = AppE (ConE $ mkName "Just") $ RecConE (mkName $ genTableNameSql rawTableName) $ map (genParseColHs rawTableName) cols

{-|
	Takes a table definition and returns a declaration which makes the
	corresponding record an instance of DbRecord, which implements both
	recId and parseSql'.
-}
genInstanceDec :: Table -> Dec
genInstanceDec t@(tName, _) = 
	InstanceD [] (AppT cDbRecord vRecord) fs
	where
		cDbRecord = ConT $ mkName "DbRecord"
		vRecord = VarT $ genTableNameHs tName
		fs = map ($ t) [
			genRecIdHs,
			genParseSqlHs
			]

