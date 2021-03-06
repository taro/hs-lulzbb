module LulzBB.Schema where
import ORM

tables :: [Table]
tables = [
	("forum", [
		("name", ColText),
		("desc", ColText)
	]),
	("post", [
		("forum", ColReference "forum"),
		("parent", ColReference "post"),
		("author", ColReference "user"),
		("created", ColDatetime),
		("rev", ColReference "rev")
	]),
	("rev", [
		("post", ColReference "post"),
		("updated", ColDatetime),
		("author", ColReference "user"),
		("authorIp", ColString 16),
		("subject", ColText),
		("body", ColText)
	]),
	("user", [
		("name", ColString 255),
		("displayName", ColString 255),
		("password", ColString 32),
		("created", ColDatetime),
		("lastVisit", ColDatetime)
	])
	]
