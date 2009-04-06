module LulzBB.Forum where
import Control.Monad (liftM)
import Database.HDBC
import LulzBB.ORM
import Routing (RouteParameters)

list :: IConnection a => a -> RouteParameters -> IO String
list db params = do
	stm <- prepare db "SELECT * FROM s_forum"
	execute stm []
	fs <- liftM (map parseSql) $ fetchAllRowsMap stm :: IO [Maybe Forum]
	return $ show fs

view :: IConnection a => a -> RouteParameters -> IO String
view db params = return "view forum"

post :: IConnection a => a -> RouteParameters -> IO String
post db params = return "post forum"

