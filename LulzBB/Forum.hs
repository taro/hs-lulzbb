module LulzBB.Forum where
import Routing (RouteParameters)

view :: RouteParameters -> IO String
view params = return "view forum"

post :: RouteParameters -> IO String
post params = return "post forum"

