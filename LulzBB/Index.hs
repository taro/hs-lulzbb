module LulzBB.Index where
import Routing (RouteParameters)

view :: RouteParameters -> IO String
view params = return "view index"
