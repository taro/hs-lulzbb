module LulzBB.Main where
import qualified Data.Map as Map
import LulzBB.RoutingTable
import Routing

getActionTable :: IO ActionTable
getActionTable = do
	return $ Map.fromList $ rawActionTable

getUrlTree :: IO UrlTree
getUrlTree = 
	return urlTree
