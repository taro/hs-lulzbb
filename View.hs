module View (
	suggestions,
	getTemplate
	) where

import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Text.Regex.Posix ((=~))
import Text.StringTemplate (getStringTemplate, STGroup, StringTemplate)

{-|
	Function which converts the input string to lowercase, then
	strips out all of the non [a-z_] characters to make template
	names easier to handle.
-}
formatSuggPart :: String -> String
formatSuggPart = 
	strip . map toLower
		where strip = concat . (=~ "[_a-z]")

{-|
	Takes the names of the module, action, section and ID of the 
	piece being rendered and returns a list of possible template
	names for rendering the object.
-}
suggestions :: String -> String -> String -> Integer -> [String]
suggestions mod act sec id =
	let sid = show id in
	let lmod = map toLower mod in
	let lact = map toLower act in
	let lsec = map toLower sec in
	[
		lmod ++ "-" ++ lact ++ "-" ++ lsec ++ "-" ++ sid,
		lmod ++ "-" ++ lact ++ "-" ++ lsec,
		lmod ++ "-" ++ lsec ++ "-" ++ sid,
		lmod ++ "-" ++ lsec,
		lsec ++ "-" ++ sid,
		lsec
	]

{-|
	Takes a STGroup and a list of template suggestions and returns a 
	single StringTemplate (the first one in the suggestion list with
	a corresponding template. If there is no matching template it 
	will raise an error.
-}
getTemplate :: STGroup String -> [String] -> StringTemplate String
getTemplate tpls suggs = 
	let tpls' = catMaybes $ map (\x -> getStringTemplate x tpls) suggs in
	case tpls' of
		[] -> error $ "No matching template in " ++ show suggs
		_ -> tpls' !! 0
