module LulzBB.Forum (
	list,
	view,
	post
	) where

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Database.HDBC
import LulzBB.ORM
import Routing (RouteParameters)
import Text.JSON (encode)
import Text.StringTemplate
import View

list db tpls params = do
	stm <- prepare db "SELECT * FROM s_forum"
	execute stm []
	fs <- liftM (map (fromMaybe undefined . parseSql)) $ fetchAllRowsMap stm :: IO [Forum]

	case lookup "format" params of
		Just "json" -> return $ encode fs
		_ -> do
			-- Pass each forum through the appropriate template
			let suggs = suggestions "Forum" "list" "forum"
			let fs' = map (xformDbRecord tpls suggs) fs

			-- Pass the results through the page template
			return $ concat fs'

view db tpls params = return "view forum"

post db tpls params = return "post forum"

