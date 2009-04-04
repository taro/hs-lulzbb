module LulzBB.ORM where
import Database.HDBC (SqlValue)
import qualified Data.Map as Map
import ORM

parseSql :: DbRecord a => Map.Map String SqlValue -> Maybe a
parseSql = parseSql' ""

class DbRecord a where
	parseSql' :: String -> Map.Map String SqlValue -> Maybe a

data Forum = Forum {
	forumId :: Integer,
	forumName :: String,
	forumDesc :: String
} 

data Post = Post {
	postId :: Integer,
	postForum :: Maybe Forum,
	postParent :: Maybe Post,
	postAuthor :: Maybe User,
	postCreated :: Integer,
	postRev :: Maybe Rev
}

data Rev = Rev {
	revId :: Integer,
	revPost :: Maybe Post,
	revUpdated :: Integer,
	revAuthor :: Maybe User,
	revAuthorIp :: String,
	revSubject :: String,
	revBody :: String
}

data User = User {
	userId :: Integer,
	userName :: String,
	userDisplayName :: String,
	userPassword :: String,
	userCreated :: Integer,
	userLastVisit :: Integer
}

instance DbRecord Forum where
	parseSql' pfx sql = 
		case Map.lookup (pfx ++ "forumId") sql of
			Nothing -> Nothing
			_ -> Just $ Forum {
				forumId = coerseSql 0 pfx "id" sql,
				forumName = coerseSql "" pfx "name" sql,
				forumDesc = coerseSql "" pfx "desc" sql }

instance DbRecord Post where
	parseSql' pfx sql = 
		case Map.lookup (pfx ++ "postId") sql of
			Nothing -> Nothing
			_ -> Just $ Post {
				postId = coerseSql 0 pfx "id" sql,
				postForum = parseSql' pfx sql,
				postParent = parseSql' pfx sql,
				postAuthor = parseSql' pfx sql,
				postCreated = coerseSql 0 pfx "created" sql,
				postRev = parseSql' pfx sql }

instance DbRecord Rev where
	parseSql' pfx sql = 
		case Map.lookup (pfx ++ "revId") sql of
			Nothing -> Nothing
			_ -> Just $ Rev {
				revId = coerseSql 0 pfx "id" sql,
				revPost = parseSql' pfx sql,
				revUpdated = coerseSql 0 pfx "updated" sql,
				revAuthor = parseSql' pfx sql,
				revAuthorIp = coerseSql "" pfx "authorIp" sql,
				revSubject = coerseSql "" pfx "subject" sql,
				revBody = coerseSql "" pfx "body" sql }

instance DbRecord User where
	parseSql' pfx sql = 
		case Map.lookup (pfx ++ "userId") sql of
			Nothing -> Nothing
			_ -> Just $ User {
				userId = coerseSql 0 pfx "id" sql,
				userName = coerseSql "" pfx "name" sql,
				userDisplayName = coerseSql "" pfx "displayName" sql,
				userPassword = coerseSql "" pfx "password" sql,
				userCreated = coerseSql 0 pfx "created" sql,
				userLastVisit = coerseSql 0 pfx "lastVisit" sql }
