module LulzBB.Index where
import Database.HDBC
import Routing (RouteParameters)

view :: IConnection a => a -> RouteParameters -> IO String
view db params = return "asd"
