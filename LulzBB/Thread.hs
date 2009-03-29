module LulzBB.Thread where
import Routing (RouteParameters)

view :: RouteParameters -> IO String
view params = return "view thread"

post :: RouteParameters -> IO String
post params = return "post thread"

