module LulzBB.Forum where
import Database.HDBC
import Routing (RouteParameters)

view :: IConnection a => a -> RouteParameters -> IO String
view db params = return "view forum"

post :: IConnection a => a -> RouteParameters -> IO String
post db params = return "post forum"

