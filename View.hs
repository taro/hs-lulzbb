module View (
	suggestions,
	getTemplate
	) where

import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Text.Regex.Posix ((=~))
import Text.StringTemplate (getStringTemplate, STGroup, StringTemplate)

formatSuggPart :: String -> String
formatSuggPart = 
	strip . map toLower
		where strip = concat . (=~ "[a-z]")

suggestions :: String -> String -> String -> Integer -> [String]
suggestions mod act sec id =
	let sid = show id in
	let lmod = map toLower mod in
	let lact = map toLower act in
	let lsec = map toLower sec in
	[
		lmod ++ "-" ++ lact ++ "-" ++ lsec ++ "-" ++ sid ++ ".tpl",
		lmod ++ "-" ++ lact ++ "-" ++ lsec ++ ".tpl",
		lmod ++ "-" ++ lsec ++ "-" ++ sid ++ ".tpl",
		lmod ++ "-" ++ lsec ++ ".tpl",
		lsec ++ "-" ++ sid ++ ".tpl",
		lsec ++ "-" ++ ".tpl"
	]

getTemplate :: STGroup String -> [String] -> StringTemplate String
getTemplate tpls suggs = 
	let tpls' = catMaybes $ map (\x -> getStringTemplate x tpls) suggs in
	case tpls' of
		[] -> error $ "No matching template in " ++ show suggs
		_ -> tpls' !! 0
