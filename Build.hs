import Control.Exception
import Routing
import System (getArgs, exitFailure)

main = do
	args <- getArgs

	case length args of
		1 -> return ()
		_ -> do
			putStrLn $ "Usage: ./Build SiteName"
			exitFailure

	let siteName = args !! 0

	routingFile <- readFile $ siteName ++ "/routing.yml"
	let routes = parseRoutes routingFile

	case length routes of
		0 -> do
			putStrLn $ "ERROR: No routes were parsed from the file?"
			exitFailure
		_ -> return ()

	writeFile (siteName ++ "/RoutingTable.hs") $ dumpRoutes siteName routes
