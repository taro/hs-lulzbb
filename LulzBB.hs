module Main where
import Control.Exception (throw, handle, Exception (NoMethodError, ErrorCall))
import Control.Monad (liftM)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Database.HDBC (commit, disconnect, IConnection (..))
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import LulzBB.Main
import Network.CGI
import Network.URI (uriPath)
import Routing
import System.Directory (setCurrentDirectory)

main :: IO ()
main = do
	-- TODO: Un-hardcode this
	setCurrentDirectory "/usr/home/hark/dev/hs-lulzbb/"
	db <- liftIO $ connectPostgreSQL "user=hark"
	at <- getActionTable db "LulzBB/Templates/"
	ut <- getUrlTree

	runCGI $ handleErrors $ handler db ut at
		`catchCGI` (\e -> (liftIO $ disconnect db) >> throwCGI e)

resolveFormatMime :: String -> String
resolveFormatMime format = fromMaybe "text/html" $ lookup format [
	("html", "text/html"),
	("xml", "text/xml"),
	("rss", "application/rss+xml"),
	("json", "application/json")
	]

handler :: IConnection a => a -> UrlTree -> ActionTable -> CGI CGIResult
handler db ut at = do
	-- Set a default Content-Type (for errors and stuff)
	setHeader "Content-Type" "text/plain"

	-- TODO: Un-hardcode this
	-- TODO: Make sure this is /-terminated
	let baseurl = "/lulzbb/"
	
	-- Resolve the action
	fullurl <- liftM uriPath requestURI

	if not $ isPrefixOf baseurl fullurl
		then throwCGI $ NoMethodError $ unlines [
			"uri isn't in baseurl.",
			"uri: " ++ fullurl, 
			"baseurl: " ++ baseurl ]
		else return ()

	let req = (drop (length baseurl) fullurl)
	let (action, format) = route ut at req

	-- Execute the action
	res <- liftIO action 

	{- 
		Set the proper headers, but only after the action has
		been executed (so errors will be returned as text/plain)
	-}
	setHeader "Content-Type" $ resolveFormatMime format

	-- Commit the database changes
	liftIO $ commit db
	
	-- Output the data
	output res
