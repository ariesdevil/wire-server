{-# LANGUAGE OverloadedStrings #-}

module API.Teams (tests) where

import API.Util (Galley, Brig, test)
import Bilge
import Control.Monad
import Data.List1
import Data.Maybe
import Galley.Types.Teams
import Test.Tasty

import qualified API.Util as Util
import qualified Data.Set as Set

tests :: Galley -> Brig -> Manager -> TestTree
tests g b m = testGroup "Teams API"
    [ test m "create team" (testCreateTeam g b)
    , test m "create team with members" (testCreateTeamWithMembers g b)
    ]

testCreateTeam :: Galley -> Brig -> Http ()
testCreateTeam g b = do
    owner <- Util.randomUser b
    void $ Util.createTeam "foo" owner [] g

testCreateTeamWithMembers :: Galley -> Brig -> Http ()
testCreateTeamWithMembers g b = do
    owner <- Util.randomUser b
    u1 <- Util.randomUser b
    u2 <- Util.randomUser b
    let pp = let p = Set.fromList [CreateConversation, AddConversationMember] in
             newPermissions p p
    let m1 = newTeamMember u1 (fromJust pp)
    let m2 = newTeamMember u2 (fromJust pp)
    Util.connectUsers b owner (list1 u1 [u2])
    void $ Util.createTeam "foo" owner [m1, m2] g

