module LulzBB.Thread where
import Database.HDBC
import Routing (RouteParameters)
import Text.StringTemplate

view db tpls params = return "view thread"

post db tpls params = return "post thread"

