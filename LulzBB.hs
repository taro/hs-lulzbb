module Main where
import Control.Exception (handle, Exception (NoMethodError))
import Control.Monad (liftM)
import Data.List (isPrefixOf)
import Network.CGI
import Network.URI (uriPath)
import LulzBB.Main
import Routing

main :: IO ()
main = do
	at <- getActionTable
	ut <- getUrlTree
	runCGI $ handleErrors $ handler ut at

handler :: UrlTree -> ActionTable -> CGI CGIResult
handler ut at = do
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
	output res
