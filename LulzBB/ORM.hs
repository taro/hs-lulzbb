module LulzBB.ORM where
import Database.HDBC (SqlValue)
import qualified Data.Map as Map
import ORM
import Text.JSON (JSON(..), makeObj)

parseSql :: DbRecord a => Map.Map String SqlValue -> Maybe a
parseSql = parseSql' ""

class DbRecord a where
	parseSql' :: String -> Map.Map String SqlValue -> Maybe a



data Forum = Forum {
	forumId :: Integer,
	forumName :: String,
	forumDesc :: String
} deriving (Show)

data Post = Post {
	postId :: Integer,
	postForum :: Maybe Forum,
	postParent :: Maybe Post,
	postAuthor :: Maybe User,
	postCreated :: Integer,
	postRev :: Maybe Rev
} deriving (Show)

data Rev = Rev {
	revId :: Integer,
	revPost :: Maybe Post,
	revUpdated :: Integer,
	revAuthor :: Maybe User,
	revAuthorIp :: String,
	revSubject :: String,
	revBody :: String
} deriving (Show)

data User = User {
	userId :: Integer,
	userName :: String,
	userDisplayName :: String,
	userPassword :: String,
	userCreated :: Integer,
	userLastVisit :: Integer
} deriving (Show)

instance DbRecord Forum where
	parseSql' pfx sql = 
		case Map.lookup (pfx ++ "forum_id") sql of
			Nothing -> Nothing
			_ -> Just $ Forum {
				forumId = coerseSql 0 pfx "forum_id" sql,
				forumName = coerseSql "" pfx "forum_name" sql,
				forumDesc = coerseSql "" pfx "forum_desc" sql }

instance DbRecord Post where
	parseSql' pfx sql = 
		case Map.lookup (pfx ++ "post_id") sql of
			Nothing -> Nothing
			_ -> Just $ Post {
				postId = coerseSql 0 pfx "post_id" sql,
				postForum = parseSql' pfx sql,
				postParent = parseSql' pfx sql,
				postAuthor = parseSql' pfx sql,
				postCreated = coerseSql 0 pfx "post_created" sql,
				postRev = parseSql' pfx sql }

instance DbRecord Rev where
	parseSql' pfx sql = 
		case Map.lookup (pfx ++ "rev_id") sql of
			Nothing -> Nothing
			_ -> Just $ Rev {
				revId = coerseSql 0 pfx "rev_id" sql,
				revPost = parseSql' pfx sql,
				revUpdated = coerseSql 0 pfx "rev_updated" sql,
				revAuthor = parseSql' pfx sql,
				revAuthorIp = coerseSql "" pfx "rev_author_ip" sql,
				revSubject = coerseSql "" pfx "rev_subject" sql,
				revBody = coerseSql "" pfx "rev_body" sql }

instance DbRecord User where
	parseSql' pfx sql = 
		case Map.lookup (pfx ++ "user_id") sql of
			Nothing -> Nothing
			_ -> Just $ User {
				userId = coerseSql 0 pfx "user_id" sql,
				userName = coerseSql "" pfx "user_name" sql,
				userDisplayName = coerseSql "" pfx "user_display_name" sql,
				userPassword = coerseSql "" pfx "user_password" sql,
				userCreated = coerseSql 0 pfx "user_created" sql,
				userLastVisit = coerseSql 0 pfx "user_last_visit" sql }

instance JSON Forum where
	showJSON x = makeObj [
		("forumId", showJSON $ forumId x),
		("forumName", showJSON $ forumName x),
		("forumDesc", showJSON $ forumDesc x)]
	readJSON = undefined

instance JSON Post where
	showJSON x = makeObj [
		("postId", showJSON $ postId x),
		("postForum", showJSON $ postForum x),
		("postParent", showJSON $ postParent x),
		("postAuthor", showJSON $ postAuthor x),
		("postCreated", showJSON $ postCreated x),
		("postRev", showJSON $ postRev x)]
	readJSON = undefined

instance JSON Rev where
	showJSON x = makeObj [
		("revId", showJSON $ revId x),
		("revPost", showJSON $ revPost x),
		("revUpdated", showJSON $ revUpdated x),
		("revAuthor", showJSON $ revAuthor x),
		("revAuthorIp", showJSON $ revAuthorIp x),
		("revSubject", showJSON $ revSubject x),
		("revBody", showJSON $ revBody x)]
	readJSON = undefined

instance JSON User where
	showJSON x = makeObj [
		("userId", showJSON $ userId x),
		("userName", showJSON $ userName x),
		("userDisplayName", showJSON $ userDisplayName x),
		("userPassword", showJSON $ userPassword x),
		("userCreated", showJSON $ userCreated x),
		("userLastVisit", showJSON $ userLastVisit x)]
	readJSON = undefined