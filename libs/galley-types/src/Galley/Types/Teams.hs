{-# LANGUAGE DataKinds         #-}
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

    , TeamList
    , newTeamList
    , teamListTeams
    , teamListHasMore

    , TeamMember
    , newTeamMember
    , userId
    , permissions
    , teamMemberJson

    , TeamMemberList
    , newTeamMemberList
    , teamMembers
    , teamMemberListJson

    , TeamConversation
    , newTeamConversation
    , conversationId

    , TeamConversationList
    , newTeamConversationList
    , teamConversations

    , Permissions
    , newPermissions
    , hasPermission
    , self
    , copy

    , Perm (..)
    , permToInt
    , permsToInt
    , intToPerm
    , intToPerms

    , NewTeam
    , newNewTeam
    , newTeamName
    , newTeamIcon
    , newTeamIconKey
    , newTeamMembers

    , UpdateTeamMembers
    , newUpdateTeamMembers
    , addTeamMembers
    , delTeamMembers

    ) where

import Control.Lens (makeLenses, (^.))
import Data.Aeson
import Data.Bits (testBit, (.|.))
import Data.Id (TeamId, ConvId, UserId)
import Data.Json.Util
import Data.Maybe (mapMaybe)
import Data.Range
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
    } deriving Show

data TeamList = TeamList
    { _teamListTeams   :: [Team]
    , _teamListHasMore :: Bool
    } deriving Show

data TeamMember = TeamMember
    { _userId      :: UserId
    , _permissions :: Permissions
    }

newtype TeamMemberList = TeamMemberList
    { _teamMembers :: [TeamMember]
    }

newtype TeamConversation = TeamConversation
    { _conversationId :: ConvId
    }

newtype TeamConversationList = TeamConversationList
    { _teamConversations :: [TeamConversation]
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
    | GetMemberPermissions
    | GetTeamConversations
    deriving (Eq, Ord, Show)

data NewTeam = NewTeam
    { _newTeamName    :: Range 1 256 Text
    , _newTeamIcon    :: Range 1 256 Text
    , _newTeamIconKey :: Maybe (Range 1 256 Text)
    , _newTeamMembers :: Maybe (Range 1 128 [TeamMember])
    }

data UpdateTeamMembers = UpdateTeamMembers
    { _addTeamMembers :: Maybe (Range 1 128 [TeamMember])
    , _delTeamMembers :: Maybe (Range 1 128 [UserId])
    }

newTeam :: TeamId -> UserId -> Text -> Text -> Team
newTeam tid uid nme ico = Team tid uid nme ico Nothing

newTeamList :: [Team] -> Bool -> TeamList
newTeamList = TeamList

newTeamMember :: UserId -> Permissions -> TeamMember
newTeamMember = TeamMember

newTeamMemberList :: [TeamMember] -> TeamMemberList
newTeamMemberList = TeamMemberList

newTeamConversation :: ConvId -> TeamConversation
newTeamConversation = TeamConversation

newTeamConversationList :: [TeamConversation] -> TeamConversationList
newTeamConversationList = TeamConversationList

newNewTeam :: Range 1 256 Text -> Range 1 256 Text -> NewTeam
newNewTeam nme ico = NewTeam nme ico Nothing Nothing

newUpdateTeamMembers
    :: Maybe (Range 1 128 [TeamMember])
    -> Maybe (Range 1 128 [UserId])
    -> Maybe UpdateTeamMembers
newUpdateTeamMembers Nothing Nothing = Nothing
newUpdateTeamMembers add     remove  = Just (UpdateTeamMembers add remove)

makeLenses ''Team
makeLenses ''TeamList
makeLenses ''TeamMember
makeLenses ''TeamMemberList
makeLenses ''TeamConversation
makeLenses ''TeamConversationList
makeLenses ''Permissions
makeLenses ''NewTeam
makeLenses ''UpdateTeamMembers

newPermissions :: Set Perm -> Set Perm -> Maybe Permissions
newPermissions a b
    | b `Set.isSubsetOf` a = Just (Permissions a b)
    | otherwise            = Nothing

hasPermission :: TeamMember -> Perm -> Bool
hasPermission tm p = p `Set.member` (tm^.permissions.self)

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
permToInt GetMemberPermissions     = 0x200
permToInt GetTeamConversations     = 0x400

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
intToPerm 0x200 = Just GetMemberPermissions
intToPerm 0x400 = Just GetTeamConversations
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

instance ToJSON TeamList where
    toJSON t = object
        $ "teams"    .= _teamListTeams t
        # "has_more" .= _teamListHasMore t
        # []

instance FromJSON TeamList where
    parseJSON = withObject "teamlist" $ \o -> do
        TeamList <$> o .: "teams"
                 <*> o .: "has_more"

teamMemberJson :: Bool -> TeamMember -> Value
teamMemberJson False m = object [ "user" .= _userId m ]
teamMemberJson True  m = object [ "user" .= _userId m, "permissions" .= _permissions m ]

teamMemberListJson :: Bool -> TeamMemberList -> Value
teamMemberListJson withPerm l =
    object [ "members" .= map (teamMemberJson withPerm) (_teamMembers l) ]

instance FromJSON TeamMember where
    parseJSON = withObject "team-member" $ \o -> do
        TeamMember <$> o .:  "user"
                   <*> o .:  "permissions"

instance FromJSON TeamMemberList where
    parseJSON = withObject "team member list" $ \o -> do
        TeamMemberList <$> o .: "members"

instance ToJSON TeamConversation where
    toJSON t = object ["conversation" .= _conversationId t]

instance FromJSON TeamConversation where
    parseJSON = withObject "team conversation" $ \o -> do
        TeamConversation <$> o .: "conversation"

instance ToJSON TeamConversationList where
    toJSON t = object ["conversations" .= _teamConversations t]

instance FromJSON TeamConversationList where
    parseJSON = withObject "team conversation list" $ \o -> do
        TeamConversationList <$> o .: "conversations"

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

instance ToJSON NewTeam where
    toJSON t = object
        $ "name"     .= fromRange (_newTeamName t)
        # "icon"     .= fromRange (_newTeamIcon t)
        # "icon_key" .= (fromRange <$> _newTeamIconKey t)
        # "members"  .= (map (teamMemberJson True) . fromRange <$> _newTeamMembers t)
        # []

instance FromJSON NewTeam where
    parseJSON = withObject "new-team" $ \o -> do
        name <- o .:  "name"
        icon <- o .:  "icon"
        key  <- o .:? "icon_key"
        mems <- o .:? "members"
        either fail pure $
            NewTeam <$> checkedEitherMsg "name" name
                    <*> checkedEitherMsg "icon" icon
                    <*> maybe (pure Nothing) (fmap Just . checkedEitherMsg "icon_key") key
                    <*> maybe (pure Nothing) (fmap Just . checkedEitherMsg "members") mems

instance ToJSON UpdateTeamMembers where
    toJSON t = object
        $ "add"     .= (map (teamMemberJson True) . fromRange <$> _addTeamMembers t)
        # "remove"  .= (fromRange <$> _delTeamMembers t)
        # []

instance FromJSON UpdateTeamMembers where
    parseJSON = withObject "update team members" $ \o -> do
        add <- o .:? "add"
        del <- o .:? "remove"
        utm <- either fail pure $ UpdateTeamMembers
            <$> maybe (pure Nothing) (fmap Just . checkedEitherMsg "add") add
            <*> maybe (pure Nothing) (fmap Just . checkedEitherMsg "remove") del
        if null (_addTeamMembers utm) && null (_delTeamMembers utm) then
            fail "At least one of {'add', 'remove'} must be present."
        else
            pure utm

