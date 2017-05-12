{-# LANGUAGE OverloadedStrings #-}

module API.Teams (tests) where

import API.Util (Galley, Brig, test, zUser)
import Bilge
import Bilge.Assert
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Conversion
import Data.List1
import Data.Maybe
import Galley.Types.Teams
import Test.Tasty
import Test.Tasty.HUnit

import qualified API.Util as Util
import qualified Data.Set as Set

tests :: Galley -> Brig -> Manager -> TestTree
tests g b m = testGroup "Teams API"
    [ test m "create team" (testCreateTeam g b)
    , test m "create team with members" (testCreateTeamWithMembers g b)
    , test m "add new team member" (testAddTeamMember g b)
    , test m "remove team member" (testRemoveTeamMember g b)
    ]

testCreateTeam :: Galley -> Brig -> Http ()
testCreateTeam g b = do
    owner <- Util.randomUser b
    tid   <- Util.createTeam "foo" owner [] g
    team  <- Util.getTeam owner tid g
    liftIO $ assertEqual "owner" owner (team^.teamCreator)

testCreateTeamWithMembers :: Galley -> Brig -> Http ()
testCreateTeamWithMembers g b = do
    owner <- Util.randomUser b
    user1 <- Util.randomUser b
    user2 <- Util.randomUser b
    let pp = Util.symmPermissions [CreateConversation, AddConversationMember]
    let m1 = newTeamMember user1 pp
    let m2 = newTeamMember user2 pp
    Util.connectUsers b owner (list1 user1 [user2])
    tid <- Util.createTeam "foo" owner [m1, m2] g
    mem <- Util.getTeamMembers owner tid g
    liftIO $ assertEqual "members"
        (Set.fromList [newTeamMember owner fullPermissions, m1, m2])
        (Set.fromList (mem^.teamMembers))

testAddTeamMember :: Galley -> Brig -> Http ()
testAddTeamMember g b = do
    owner <- Util.randomUser b
    let p1 = Util.symmPermissions [CreateConversation, AddConversationMember]
    let p2 = Util.symmPermissions [CreateConversation, AddConversationMember, AddTeamMember]
    mem1 <- flip newTeamMember p1 <$> Util.randomUser b
    mem2 <- flip newTeamMember p2 <$> Util.randomUser b
    Util.connectUsers b owner (list1 (mem1^.userId) [mem2^.userId])
    tid <- Util.createTeam "foo" owner [mem1, mem2] g

    mem3 <- flip newTeamMember p1 <$> Util.randomUser b
    let payload = json (newNewTeamMember mem3)

    -- `mem1` lacks permission to add new team members
    post (g . paths ["teams", toByteString' tid, "members"] . zUser (mem1^.userId) . payload) !!!
        const 403 === statusCode

    -- `mem2` has `AddTeamMember` permission
    post (g . paths ["teams", toByteString' tid, "members"] . zUser (mem2^.userId) . payload) !!!
        const 200 === statusCode

testRemoveTeamMember :: Galley -> Brig -> Http ()
testRemoveTeamMember g b = do
    owner <- Util.randomUser b
    let p1 = Util.symmPermissions [AddConversationMember]
    let p2 = Util.symmPermissions [AddConversationMember, RemoveTeamMember]
    mem1 <- flip newTeamMember p1 <$> Util.randomUser b
    mem2 <- flip newTeamMember p2 <$> Util.randomUser b
    Util.connectUsers b owner (list1 (mem1^.userId) [mem2^.userId])
    tid <- Util.createTeam "foo" owner [mem1, mem2] g

    -- `mem1` lacks permission to remove team members
    delete ( g
           . paths ["teams", toByteString' tid, "members", toByteString' (mem2^.userId)]
           . zUser (mem1^.userId)
           ) !!! const 403 === statusCode

    -- `mem2` has `RemoveTeamMember` permission
    delete ( g
           . paths ["teams", toByteString' tid, "members", toByteString' (mem1^.userId)]
           . zUser (mem2^.userId)
           ) !!! const 200 === statusCode

