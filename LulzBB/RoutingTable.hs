{-# LANGUAGE NoMonomorphismRestriction #-}
module LulzBB.RoutingTable where
import qualified LulzBB.Forum
import qualified LulzBB.Index
import qualified LulzBB.Thread
import Routing (Route (..))

rawActionTable = [
	(("Index", "view"), LulzBB.Index.view),
	(("Forum", "view"), LulzBB.Forum.view),
	(("Forum", "post"), LulzBB.Forum.post),
	(("Thread", "view"), LulzBB.Thread.view),
	(("Thread", "post"), LulzBB.Thread.post)]

urlTree = Branch ([],Just (Parameter ("forum",Branch ([Literal ("post",RouteEnd (Just [("module","Forum"),("action","post")]))],Just (Parameter ("thread",Branch ([Literal ("post",RouteEnd (Just [("module","Thread"),("action","post")]))],Just (Parameter ("page",RouteEnd (Just [("module","Thread"),("action","view")]))),Just (RouteEnd (Just [("module","Thread"),("action","view"),("page","0")]))))),Just (RouteEnd (Just [("module","Forum"),("action","view")]))))),Just (RouteEnd (Just [("module","Index"),("action","view")])))