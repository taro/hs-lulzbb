{-# LANGUAGE ExistentialQuantification #-}
module Routing (
	parseRoutes, 
	dumpRoutes, 
	route, 
	ActionTable, 
	UrlTree, 
	Route(..), 
	RouteParameters) where 

import Control.Exception (throw, Exception (ErrorCall))
import Data.Char (toLower, toUpper)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.Regex.Posix ((=~))

{-|
	Data type which defines a routing tree. Each node in the tree
	can be one of three types --
	
		Literal: Must match the string exactly.
		Parameter: Can be anything, added to parameter array
		Branch: Can be one of several literals, possibly a Parameter
			if not a Literal, or possibly the end of the Route.
	
	The RouteEnd contains the parameter associative array contained
	in the route definition.
-}
data Route = 
	  Literal (String, Route)
	| Parameter (String, Route)
	| Branch ([Route], Maybe Route, Maybe Route) -- [Literals, Parameter, RouteEnd]
	| RouteEnd (Maybe RouteParameters)
	deriving Show

{-|
	Associative array to hold the 'params' data contained within
	each routing definition.
-}
type RouteParameters = [(String, String)]

{-| Simple alias -}
type UrlTree = Route

{-| 
	The application exposes a set of functions which can be of any
	type. These functions are all dumped by the Routing compiler into
	the `rawActionTable'. The application code is responsible for 
	converting each of these raw actions into an Action (by currying
	them) to generate an ActionTable to pass to `route'.
-}
type Action = RouteParameters -> IO String

{-| A mapping of (moduleName, actionName) -> Action. -}
type ActionTable = Map.Map (String, String) Action

{-|
	Hardcoded list of acceptable formats (marked as a .X at the end of
	the request string). TODO: Make this configurable.
-}
formats :: [String]
formats = ["xml", "json", "html"]

{-|
	Format to use if none is given. TODO: Make this configurable.
-}
defaultFormat :: String
defaultFormat = "html"

{-| 
	Was too lazy to write a fully-fledged parser so used a simple regex
	to parse the YAML instead. Gets the job done, for the most part.
-}
regexRoute = "[^:]+:\\s*\r?\n  url:[ \t]+([^\r\n]+)\\s*\r?\n  param:[ \t]+\\{([^}]*)\\}"

{-|
	Takes the contents of a routing YAML file and parses it into a list
	of strings, each of which contain a routing definition.
-}
splitInput :: String -> [String]
splitInput = (=~ regexRoute)

{-|
	Takes a single routing definition and returns the url and parameter
	sections of it, which can be further parsed.
-}
splitRoute :: String -> (String, String)
splitRoute s = (x, y) where
	(_, _, _, (x : y : _)) = s =~ regexRoute :: (String, String, String, [String])

{-|
	Takes the URL portion returned by splitRoute (and the route end
	parameters parsed out by splitRouteParams) and generates a route
	node from it.
-}
splitRouteUrl :: RouteParameters -> String -> Route
splitRouteUrl rp s = foldr f (RouteEnd $ Just rp) parts where
	parts = s =~ "[^/]+" :: [String]
	f x a = case (x !! 0) of
		':' -> Parameter ((drop 1 x), a)
		'*' -> Parameter ("", a)
		_ -> Literal (x, a)

{-|
	Takes the Params portin returned by splitRoute and returns
	an associative array of all the key-value pairs.
-}
splitRouteParams :: String -> [(String, String)]
splitRouteParams s = map f x where
	x = s =~ "[^:]+:\\s* [^,]+" :: [String]
	f x = let [y, z] = x =~ "[^:, \t\r\n]+" in (y, z)
	
{-|
	Takes the contents of a routing YAML file, splits it up and parses
	each route definition. Returns the list of all route definitions
	contained within the string.
-}
parseRoutes :: String -> [Route]
parseRoutes = map f . map splitRoute . splitInput where
	f (x, y) = splitRouteUrl (splitRouteParams y) x

{-|
	parseRoutes returns a list of Routes, but we want to get that into
	a single tree (so that we can parse URLs as they arrive). As such,
	we can define a method to merge each type of node with each other
	type (to create a single node), which can then be used to fold the
	list of Routes into a single Route recursively. That's what this does.

	There are certain situations where it would be invalid to merge
	two nodes because it would create an ambiguity (ie, you can't have
	two different parameters at the same place in different routes, etc).
	In these situations an error is thrown. Since this is executed at
	compile time, it's a build error so it isn't a big deal.
-}
mergeRoutes :: Route -> Route -> Route
{- 
	If two literals are the same, we can reduce them to one node and
	fold all further nodes together. If they're different, we need to
	branch.
-}
mergeRoutes x@(Literal (x', xs)) y@(Literal (y', ys)) =
	if x' == y'
		then Literal (x', mergeRoutes xs ys)
		else Branch ([x, y], Nothing, Nothing)

{-
	A literal and a parameter is just a branch (if not the literal,
	then take the parameter.
-}
mergeRoutes x@(Literal _) y@(Parameter _) = mergeRoutes y x
mergeRoutes x@(Parameter _) y@(Literal _) = 
	Branch ([y], Just x, Nothing)

{-
	A literal and a branch combine by folding the literal into the 
	branch conditions. If the literal is already an option in the branch,
	it's further nodes get merged into the existing node, otherwise it
	just gets tossed in the Branch's literal list.
-}
mergeRoutes x@(Literal _) y@(Branch _) = mergeRoutes y x
mergeRoutes x@(Branch (xs, xp, xn)) y@(Literal (y', ys)) = 
	case List.find (\(Literal (x', _)) -> x' == y') xs of
		Nothing -> Branch (y : xs, xp, xn) -- new possible literal
		Just z@(Literal _) -> Branch (new : old, xp, xn) where
			new = mergeRoutes z y -- L+L on the same literal
			old = filter (\(Literal (x', _)) -> x' /= y') xs
		Just _ -> throw $ ErrorCall "mergeRoutes: Branch contains a non-literal literal?"

{- A literal and an end get merged into a "literal or an end" branch. -}
mergeRoutes x@(Literal _) y@(RouteEnd _) = mergeRoutes y x
mergeRoutes x@(RouteEnd rp) y@(Literal _) =
	case rp of
		Nothing -> y
		Just _ -> Branch ([y], Nothing, Just x)

{- 
	Two different parameters means there are two different routes which
	have the same URL structure. This is only allowed in the case where
	one of them terminates immediately (because we can discern it by
	the RouteEnd), which occurs in situations like

	/:x
	/:x/:y

	etc.
-}
mergeRoutes (Parameter (xs, xr)) (Parameter (ys, yr)) | xs == ys =
	Parameter (xs, mergeRoutes xr yr)

mergeRoutes (Parameter _) (Parameter _) =
	throw $ ErrorCall "mergeRoutes: Unable to merge two Parameters."

{-
	The Parameter gets folded into the Branch, unless the Branch already
	has a potential Parameter (in which case it would be ambiguous)
-}
mergeRoutes x@(Parameter _) y@(Branch _) = mergeRoutes y x
mergeRoutes x@(Branch (xl, xp, xn)) y@(Parameter _) =
	case xp of
		Nothing -> Branch (xl, Just y, xn)
		Just xr -> Branch (xl, Just $ mergeRoutes xr y, xn)

{-
	Parameter and RouteEnd get folded together into a Branch.
-}
mergeRoutes x@(Parameter _) y@(RouteEnd _) = mergeRoutes y x
mergeRoutes x@(RouteEnd rp) y@(Parameter _) =
	case rp of
		Nothing -> y
		Just _ -> Branch ([], Just y, Just x)

{-
	To fold two branches together, we reduce the branch into a list
	of Routes, which we can then fold together with the first operand.
-}
mergeRoutes x@(Branch (xl, xp, xn)) (Branch (yl, yp, yn)) =
	foldl mergeRoutes x $ yl ++ yp' ++ yn' where
		yp' = case yp of
			Nothing -> []
			Just x -> [x]
		yn' = case yn of
			Nothing -> []
			Just x -> [x]

{-
	Branch and RouteEnd get folded together easily. It's ambiguous
	if there's already a RouteEnd with options though.
-}
mergeRoutes x@(Branch _) y@(RouteEnd _) = mergeRoutes y x
mergeRoutes re@(RouteEnd rp) y@(Branch (yl, yp, yn)) =
	case (rp, yn) of
		(Nothing, Nothing) -> y
		(Just x, Nothing) -> Branch (yl, yp, Just re)
		(Nothing, Just x) -> Branch (yl, yp, yn)
		(Just _, Just _) -> throw $ ErrorCall "mergeRoutes: Unable to merge RouteEnd into a Branch which already has a RouteEnd bound."

{-
	Two RouteEnds are ambiguous if they both have options, otherwise
	one just overrides the other.
-}
mergeRoutes x@(RouteEnd x') y@(RouteEnd y') = 
	case (x', y') of
		(Nothing, Nothing) -> x
		(Just _, Nothing) -> x
		(Nothing, Just _) -> y
		(Just _, Just _) -> throw $ ErrorCall "mergeRoutes: Unable to merge two competing RouteEnds."

{-| Helper function which makes the first character upper case -}
formatModule :: String -> String
formatModule [] = []
formatModule (x : xs) = (toUpper x) : xs

{-| Helper function which makes the first character lower case -}
formatAction :: String -> String
formatAction [] = []
formatAction (x : xs) = (toLower x) : xs

{-| 
	Takes a Route tree and returns all of the possible (module, action)
	pairs that can be encountered in the route. Obviously, if you have
	module/action parameters they won't be picked up and some dirty
	work will have to be done to get them to compile in nicely.
-}
getEndpoints :: Route -> [(String, String)]
getEndpoints (Literal (_, r)) = getEndpoints r
getEndpoints (Parameter (_, r)) = getEndpoints r
getEndpoints (Branch (rs, mr, nr)) = 
	(concat . map getEndpoints) rs ++ mrs ++ nrs where
		mrs = case mr of
			Just r -> getEndpoints r
			Nothing -> []
		nrs = case nr of
			Just r -> getEndpoints r
			Nothing -> []
getEndpoints (RouteEnd Nothing) = []
getEndpoints (RouteEnd (Just p)) = 
	[(formatModule $ get "module" p, formatAction $ get "action" p)] where
		get x = fromMaybe "index" . lookup x

{-|
	Takes a site name and a list of routes. Grabs all of the possible
	endpoints from the routes, and returns a list of import statements
	to import in all of the modules needed.
-}
dumpRouteImports :: String -> [Route] -> String
dumpRouteImports sn rs =
	(concat . List.nub . List.sort . map f) (filter (/= "") modules) where
		f x = "import qualified " ++ sn ++ "." ++ x ++ "\n"
		modules = (map fst . concat . map getEndpoints) rs

{-|
	Takes a site name and a list of routes. Dumps a mapping of
	(module, action) to the qualified module/action name.
-}
dumpActionTable :: String -> [Route] -> String
dumpActionTable sn rs = "[\n\t" ++ (List.intercalate ",\n\t" . map (\(m, a) -> "((\"" ++ m ++ "\", \"" ++ a ++ "\"), " ++ sn ++ "." ++ m ++ "." ++ a ++ ")")) ((List.nub . concat . map getEndpoints) rs) ++ "]\n"

{-| Merges a list of Routes and returns a serialized version -}
dumpUrlTree :: [Route] -> String
dumpUrlTree rs = show $ foldl mergeRoutes (Branch ([], Nothing, Nothing)) rs

{-| 
	Takes a site name and a list of routes, then returns a string 
	containing the Haskell code implementing a routing table for
	the routes.
-}
dumpRoutes :: String -> [Route] -> String
dumpRoutes sn rs = concat [
	"{-# LANGUAGE NoMonomorphismRestriction #-}\n",
	"module " ++ sn ++ ".RoutingTable where\n",
	dumpRouteImports sn rs, 
	"import Routing (Route (..))\n",
	"\nrawActionTable = " ++ dumpActionTable sn rs,
	"\nurlTree = " ++ dumpUrlTree rs]

{-| Simple string split -}
split :: Eq a => a -> [a] -> [[a]]
split delim = List.unfoldr (\x -> 
	case (takeWhile (delim /=) x) of
		[] | length x == 0 -> Nothing 
		[] -> Just ([], drop 1 x) 
		y | length(y) + 1 >= length(x) -> Just (y, drop (length y) x)
		y -> Just (y, drop (length y + 1) x)) 

{-|
	Takes a UrlTree (ie, a Route tree) and a list of URL components.
	The tree is then recursively walked, building and saving any
	parameters along the way, until the URL is exhausted or there is
	no valid traversal left. 

	If the traversal successfully reaches a RouteEnd and the url
	list is exhausted, the RouteEnd's parameters are merged with the
	parameters generated by the traversal.

	If there is no valid traversal, it should return Nothing (but
	currently throws an Exception for debugging purposes.)
-}
routeUrl :: UrlTree -> [String] -> Maybe [(String, String)]
routeUrl (Literal _) [] = 
	throw $ ErrorCall "routeUrl: Unable to satisfy Literal."

routeUrl (Literal (x, rs)) (x' : xs) =
	if x == x'
		then routeUrl rs xs
		else Nothing

routeUrl (Parameter ("", rs)) [] = routeUrl rs [] -- Wildcard
routeUrl (Parameter ("", rs)) (_ : xs) = routeUrl rs xs

routeUrl (Parameter _) [] = 
	throw $ ErrorCall "routeUrl: Unable to satisfy Parameter."

routeUrl (Parameter (x, rs)) (x' : xs) =
	case routeUrl rs xs of
		Nothing -> Nothing
		Just ps -> Just (ps ++ [(x, x')])

routeUrl (Branch (_, _, nr)) [] =
	case nr of
		Nothing -> Nothing
		Just (RouteEnd ep) -> ep
		Just _ -> throw $ ErrorCall "routeUrl: Branch has a non-RouteEnd element in a RouteEnd slot!"

routeUrl (Branch (rs, er, nr)) xl@(x' : xs) =
	case (List.find f rs) of
		Just (Literal (_, r)) -> routeUrl r xs
		Nothing -> case er of
			Just r -> routeUrl r xl -- passing xs eats, so don't
			Nothing -> case nr of
				Just (RouteEnd ps) -> ps
				_ -> throw $ ErrorCall "routeUrl: Unable to satisfy Branch."
		where
			f (Literal (x, _)) = x == x'
			f _ = False

routeUrl (RouteEnd x) [] = x
routeUrl (RouteEnd x) _ = 
	throw $ ErrorCall "routeUrl: Unmatched trailing URL pieces."

{-|
	route is the bread and butter of the Routing component. It 
	takes the UrlTree generated during the compilation phase and
	an ActionTable (which is generated by the application by currying
	the outputted rawApplicationTable), along with a URL. 

	If there is a valid module/action, it returns the corresponding action.

	If there is a valid module, it uses the 'index' action.

	If there is neither, it uses the site's index action.
-}
route ::  UrlTree -> ActionTable -> String -> (IO String, String)
route ut at url = 
	-- chop off a trailing '/', if one exists
	let url' = if (not $ null url) && (last url == '/')
		then init url
		else url
			in

	-- add a leading '/', unless one exists
	let url'' = if (null url') || (head url' /= '/')
		then '/' : url'
		else url' 
			in

	-- see if there's a format at the end of it
	let (format, url''') = case url'' =~ "\\.[^./]+$" of
		"" -> (defaultFormat, url'')
		x -> (drop 1 x, take (length url'' - length x) url'') in

	-- split the url parts apart and route the URL
	let urlParts = split '/' $ dropWhile ((==) '/') url''' in
	let params = ("format", format) : (fromMaybe [] $ routeUrl ut urlParts) in

	-- normalize the bits and pieces of the route parameters and resolve
	let mod = formatModule $ fromMaybe "" $ lookup "module" params in
	let act = formatAction $ fromMaybe "index" $ lookup "action" params in
	let action = Map.lookup (mod, act) at :: Maybe Action in
	
	case action of
		Just x -> (x params, format)
		_ -> throw $ ErrorCall $ "route: No action `" ++ act ++ "' exists for module `" ++ mod ++ "' (while routing `" ++ url'' ++ "')"
