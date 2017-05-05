{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Galley.Types.Teams
    ( Team
    , newTeam
    , teamId
    , teamCreator
    , teamName
    , teamIcon
    , teamIconKey

    , TeamMember
    , newTeamMember
    , userId
    , permissions

    , NewTeam
    , newTeamName
    , newTeamIcon
    , newTeamIconKey
    , newTeamMembers

    , Permissions
    , newPermissions
    , self
    , copy

    , Perm (..)
    , permToInt
    , permsToInt
    , intToPerm
    , intToPerms
    ) where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Bits (testBit, (.|.))
import Data.Id (TeamId, UserId)
import Data.Json.Util
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word

import qualified Data.Set as Set

data Team = Team
    { _teamId      :: TeamId
    , _teamCreator :: UserId
    , _teamName    :: Text
    , _teamIcon    :: Text
    , _teamIconKey :: Maybe Text
    }

newTeam :: TeamId -> UserId -> Text -> Text -> Team
newTeam tid uid nme ico = Team tid uid nme ico Nothing

data TeamMember = TeamMember
    { _userId      :: UserId
    , _permissions :: Permissions
    }

newTeamMember :: UserId -> Permissions -> TeamMember
newTeamMember = TeamMember

data NewTeam = NewTeam
    { _newTeamName    :: Text
    , _newTeamIcon    :: Text
    , _newTeamIconKey :: Maybe Text
    , _newTeamMembers :: [TeamMember]
    }

data Permissions = Permissions
    { _self :: Set Perm
    , _copy :: Set Perm
    } deriving (Eq, Ord, Show)

data Perm =
      CreateConversation
    | DeleteConversation
    | AddTeamMember
    | RemoveTeamMember
    | AddConversationMember
    | RemoveConversationMember
    | GetBilling
    | SetBilling
    | SetTeamData
    deriving (Eq, Ord, Show)

makeLenses ''Team
makeLenses ''TeamMember
makeLenses ''NewTeam
makeLenses ''Permissions

newPermissions :: Set Perm -> Set Perm -> Maybe Permissions
newPermissions a b
    | b `Set.isSubsetOf` a = Just (Permissions a b)
    | otherwise            = Nothing

permToInt :: Perm -> Word64
permToInt CreateConversation       = 0x001
permToInt DeleteConversation       = 0x002
permToInt AddTeamMember            = 0x004
permToInt RemoveTeamMember         = 0x008
permToInt AddConversationMember    = 0x010
permToInt RemoveConversationMember = 0x020
permToInt GetBilling               = 0x040
permToInt SetBilling               = 0x080
permToInt SetTeamData              = 0x100

intToPerm :: Word64 -> Maybe Perm
intToPerm 0x001 = Just CreateConversation
intToPerm 0x002 = Just DeleteConversation
intToPerm 0x004 = Just AddTeamMember
intToPerm 0x008 = Just RemoveTeamMember
intToPerm 0x010 = Just AddConversationMember
intToPerm 0x020 = Just RemoveConversationMember
intToPerm 0x040 = Just GetBilling
intToPerm 0x080 = Just SetBilling
intToPerm 0x100 = Just SetTeamData
intToPerm _     = Nothing

intToPerms :: Word64 -> Set Perm
intToPerms n =
    let perms = [ 2^i | i <- [0 .. 62], n `testBit` i ] in
    Set.fromList (mapMaybe intToPerm perms)

permsToInt :: Set Perm -> Word64
permsToInt = Set.foldr' (\p n -> n .|. permToInt p) 0

instance ToJSON Team where
    toJSON t = object
        $ "id"       .= _teamId t
        # "creator"  .= _teamCreator t
        # "name"     .= _teamName t
        # "icon"     .= _teamIcon t
        # "icon_key" .= _teamIconKey t
        # []

instance FromJSON Team where
    parseJSON = withObject "team" $ \o -> do
        Team <$> o .:  "id"
             <*> o .:  "creator"
             <*> o .:  "name"
             <*> o .:  "icon"
             <*> o .:? "icon_key"

instance ToJSON TeamMember where
    toJSON m = object
        $ "user"        .= _userId m
        # "permissions" .= _permissions m
        # []

instance FromJSON TeamMember where
    parseJSON = withObject "team-member" $ \o -> do
        TeamMember <$> o .:  "user"
                   <*> o .:  "permissions"

instance ToJSON NewTeam where
    toJSON t = object
        $ "name"     .= _newTeamName t
        # "icon"     .= _newTeamIcon t
        # "icon_key" .= _newTeamIconKey t
        # "members"  .= _newTeamMembers t
        # []

instance FromJSON NewTeam where
    parseJSON = withObject "new-team" $ \o -> do
        NewTeam <$> o .:  "name"
                <*> o .:  "icon"
                <*> o .:? "icon_key"
                <*> o .:  "members"

instance ToJSON Permissions where
    toJSON p = object
        $ "self" .= permsToInt (_self p)
        # "copy" .= permsToInt (_copy p)
        # []

instance FromJSON Permissions where
    parseJSON = withObject "permissions" $ \o -> do
        s <- intToPerms <$> o .: "self"
        d <- intToPerms <$> o .: "copy"
        case newPermissions s d of
            Nothing -> fail "invalid permissions"
            Just ps -> pure ps
