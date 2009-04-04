module Main where
import Control.Exception (handle, Exception (NoMethodError))
import Control.Monad (liftM)
import Data.List (isPrefixOf)
import Database.HDBC (commit, disconnect, IConnection (..))
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Network.CGI
import Network.URI (uriPath)
import LulzBB.Main
import Routing

main :: IO ()
main = do
	-- TODO: Un-hardcode this
	db <- liftIO $ connectPostgreSQL "user=hark"
	at <- getActionTable db
	ut <- getUrlTree

	runCGI $ handleErrors $ handler db ut at
		`catchCGI` (\e -> (liftIO $ disconnect db) >> throwCGI e)

handler :: IConnection a => a -> UrlTree -> ActionTable -> CGI CGIResult
handler db ut at = do
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
	let action = route ut at req

	-- Execute the action
	res <- liftIO action

	-- Commit the database changes
	liftIO $ commit db
	
	-- Output the data
	output res
