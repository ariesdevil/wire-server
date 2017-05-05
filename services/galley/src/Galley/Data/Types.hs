{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Galley.Data.Types
    ( Conversation (..)
    , isSelfConv
    , isO2OConv
    , isTeamConv
    , isManagedTeamConv
    , convTeamId
    ) where

import Data.Id
import Data.List1
import Data.Text
import Data.Maybe (isJust)
import Galley.Types (ConvType (..), Access, Member (..), ConvTeamInfo (..))

data Conversation = Conversation
    { convId      :: ConvId
    , convType    :: ConvType
    , convCreator :: UserId
    , convName    :: Maybe Text
    , convAccess  :: List1 Access
    , convMembers :: [Member]
    , convTeam    :: Maybe ConvTeamInfo
    } deriving (Eq, Show)

isSelfConv :: Conversation -> Bool
isSelfConv = (SelfConv ==) . convType

isO2OConv :: Conversation -> Bool
isO2OConv = (One2OneConv ==) . convType

isTeamConv :: Conversation -> Bool
isTeamConv = isJust . convTeam

isManagedTeamConv :: Conversation -> Bool
isManagedTeamConv = maybe False cnvManaged . convTeam

convTeamId :: Conversation -> Maybe TeamId
convTeamId = fmap cnvTeamId . convTeam
