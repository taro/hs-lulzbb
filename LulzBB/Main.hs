module LulzBB.Main where
import Database.HDBC
import qualified Data.Map as Map
import LulzBB.RoutingTable
import Routing
import Text.StringTemplate (directoryGroup, STGroup (..))

getActionTable :: IConnection a => a -> String -> IO ActionTable
getActionTable db tplPath = do
	tpls <- directoryGroup tplPath :: IO (STGroup String)
	return $ Map.fromList $ map (\(k, f) -> (k, f db tpls)) rawActionTable

getUrlTree :: IO UrlTree
getUrlTree = 
	return urlTree
