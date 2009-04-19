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
	fs <- liftM (map (fromMaybe (error "Unable to parse post") . parseSql)) $ fetchAllRowsMap stm :: IO [Forum]

	case lookup "format" params of
		Just "json" -> return $ encode fs
		_ -> do
			-- Pass each forum through the appropriate template
			let suggs = suggestions "Forum" "list" "forum"
			let fs' = map (xformDbRecord tpls suggs) fs

			-- Pass the results through the page template
			let Just tpl = getStringTemplate "page-forum-list" tpls

			let attrs = [
				("forums", fs')
				]

			return $ toString $ setManyAttrib attrs tpl

view db tpls params = do
	let fid = maybe 0 read $ lookup "forum" params
	let page = maybe 0 read $ lookup "page" params
	let perPage = 30

	stm <- prepare db "SELECT * FROM s_post p INNER JOIN s_rev r ON p.post_rev = r.rev_id WHERE p.post_forum = ? AND p.post_parent IS NULL ORDER BY post_created DESC LIMIT ? OFFSET ?"
	execute stm [iToSql fid, iToSql perPage, iToSql page]
	res <- fetchAllRowsMap stm

	let ps = map (fromMaybe (error "Unable to parse post") . parseSql) res :: [Post]
	let ps' = zipWith (\p s -> p {postRev = parseSql s}) ps res

	case lookup "format" params of
		Just "json" -> return $ encode ps'

post db tpls params = return "post forum"

