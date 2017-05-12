{-# LANGUAGE OverloadedStrings #-}

module API.Teams (tests) where

import API.Util (Galley, Brig, test, zUser)
import Bilge
import Bilge.Assert
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Foldable (for_)
import Data.List1
import Data.Maybe
import Galley.Types
import Galley.Types.Teams
import Test.Tasty
import Test.Tasty.HUnit

import qualified API.Util as Util
import qualified Data.Set as Set
import qualified Network.Wai.Utilities.Error as Error

tests :: Galley -> Brig -> Manager -> TestTree
tests g b m = testGroup "Teams API"
    [ test m "create team" (testCreateTeam g b)
    , test m "create team with members" (testCreateTeamWithMembers g b)
    , test m "add new team member" (testAddTeamMember g b)
    , test m "remove team member" (testRemoveTeamMember g b)
    , test m "add team conversation" (testAddTeamConv g b)
    , test m "add managed team conversation ignores given users" (testAddTeamConvWithUsers g b)
    , test m "add team member to conversation without connection" (testAddTeamMemberToConv g b)
    ]

testCreateTeam :: Galley -> Brig -> Http ()
testCreateTeam g b = do
    owner <- Util.randomUser b
    tid   <- Util.createTeam g "foo" owner []
    team  <- Util.getTeam g owner tid
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
    tid <- Util.createTeam g "foo" owner [m1, m2]
    mem <- Util.getTeamMembers g owner tid
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
    tid <- Util.createTeam g "foo" owner [mem1, mem2]

    mem3 <- flip newTeamMember p1 <$> Util.randomUser b
    let payload = json (newNewTeamMember mem3)

    -- `mem1` lacks permission to add new team members
    post (g . paths ["teams", toByteString' tid, "members"] . zUser (mem1^.userId) . payload) !!!
        const 403 === statusCode

    -- `mem2` has `AddTeamMember` permission
    Util.addTeamMember g (mem2^.userId) tid mem3

testRemoveTeamMember :: Galley -> Brig -> Http ()
testRemoveTeamMember g b = do
    owner <- Util.randomUser b
    let p1 = Util.symmPermissions [AddConversationMember]
    let p2 = Util.symmPermissions [AddConversationMember, RemoveTeamMember]
    mem1 <- flip newTeamMember p1 <$> Util.randomUser b
    mem2 <- flip newTeamMember p2 <$> Util.randomUser b
    Util.connectUsers b owner (list1 (mem1^.userId) [mem2^.userId])
    tid <- Util.createTeam g "foo" owner [mem1, mem2]

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

testAddTeamConv :: Galley -> Brig -> Http ()
testAddTeamConv g b = do
    owner  <- Util.randomUser b
    extern <- Util.randomUser b

    let p = Util.symmPermissions [AddConversationMember]
    member <- flip newTeamMember p <$> Util.randomUser b

    Util.connectUsers b owner (list1 (member^.userId) [extern])
    tid <- Util.createTeam g "foo" owner []

    -- Managed conversation:
    cid1 <- Util.createTeamConv g owner (ConvTeamInfo tid True) [] (Just "gossip")
    -- Regular conversation:
    cid2 <- Util.createTeamConv g owner (ConvTeamInfo tid False) [extern] (Just "blaa")

    Util.addTeamMember g owner tid member

    -- New team members are added automatically to managed conversations ...
    Util.assertConvMember g (member^.userId) cid1
    -- ... but not to regular ones.
    Util.assertNotConvMember g (member^.userId) cid2

    -- Managed team conversations get all team members added implicitly.
    cid3 <- Util.createTeamConv g owner (ConvTeamInfo tid True) [] (Just "blup")
    for_ [owner, member^.userId] $ \u ->
        Util.assertConvMember g u cid3

    -- Non team members are never added implicitly.
    for_ [cid1, cid3] $
        Util.assertNotConvMember g extern

testAddTeamConvWithUsers :: Galley -> Brig -> Http ()
testAddTeamConvWithUsers g b = do
    owner  <- Util.randomUser b
    extern <- Util.randomUser b
    Util.connectUsers b owner (list1 extern [])
    tid <- Util.createTeam g "foo" owner []
    -- Create managed team conversation and erroneously specify external users.
    cid <- Util.createTeamConv g owner (ConvTeamInfo tid True) [extern] (Just "gossip")
    -- External users have been ignored.
    Util.assertNotConvMember g extern cid
    -- Team members are present.
    Util.assertConvMember g owner cid

testAddTeamMemberToConv :: Galley -> Brig -> Http ()
testAddTeamMemberToConv g b = do
    owner <- Util.randomUser b
    let p = Util.symmPermissions [AddConversationMember]
    mem1 <- flip newTeamMember p <$> Util.randomUser b
    mem2 <- flip newTeamMember p <$> Util.randomUser b
    mem3 <- flip newTeamMember (Util.symmPermissions []) <$> Util.randomUser b

    Util.connectUsers b owner (list1 (mem1^.userId) [mem2^.userId, mem3^.userId])
    tid <- Util.createTeam g "foo" owner [mem1, mem2, mem3]

    -- Team owner creates new regular team conversation:
    cid <- Util.createTeamConv g owner (ConvTeamInfo tid False) [] (Just "blaa")

    -- Team member 1 (who is *not* a member of the new conversation)
    -- can add other team members without requiring a user connection
    -- thanks to both being team members and member 1 having the permission
    -- `AddConversationMember`.
    Util.assertNotConvMember g (mem1^.userId) cid
    Util.postMembers g (mem1^.userId) (list1 (mem2^.userId) []) cid !!! const 200 === statusCode
    Util.assertConvMember g (mem2^.userId) cid

    -- OTOH, team member 3 can not add another team member since it
    -- lacks the required permission
    Util.assertNotConvMember g (mem3^.userId) cid
    Util.postMembers g (mem3^.userId) (list1 (mem1^.userId) []) cid !!! do
        const 403                === statusCode
        const "operation-denied" === (Error.label . Util.decodeBody' "error label")

