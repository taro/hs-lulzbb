module LulzBB.Main where
import Database.HDBC
import qualified Data.Map as Map
import LulzBB.RoutingTable
import Routing

getActionTable :: IConnection a => a -> IO ActionTable
getActionTable db = do
	return $ Map.fromList $ map (\(k, f) -> (k, f db)) rawActionTable

getUrlTree :: IO UrlTree
getUrlTree = 
	return urlTree
