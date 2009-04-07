module LulzBB.Forum where
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Database.HDBC
import LulzBB.ORM
import Routing (RouteParameters)
import Text.JSON (encode)

list :: IConnection a => a -> RouteParameters -> IO String
list db params = do
	stm <- prepare db "SELECT * FROM s_forum"
	execute stm []
	fs <- liftM (map (fromMaybe undefined . parseSql)) $ fetchAllRowsMap stm :: IO [Forum]

	case lookup "format" params of
		Just "json" -> return $ encode fs
		_ -> undefined

view :: IConnection a => a -> RouteParameters -> IO String
view db params = return "view forum"

post :: IConnection a => a -> RouteParameters -> IO String
post db params = return "post forum"

