module Main where
import Control.Monad (liftM)
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
	
	-- Resolve the action
	req <- liftM uriPath requestURI
	let action = route ut at req

	-- Execute the action
	res <- liftIO action
	output res
