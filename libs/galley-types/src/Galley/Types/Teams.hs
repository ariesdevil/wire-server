{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Galley.Types.Teams
    ( Team
    , teamId
    , teamName
    , teamIcon
    , teamIconKey

    , TeamMember
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
    , delegate

    , Perm (..)
    , permToInt
    , permsToInt
    , intToPerm
    , intToPerms
    ) where

import Control.Lens (makeLenses)
import Control.Monad (when)
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
    , _teamName    :: Text
    , _teamIcon    :: Text
    , _teamIconKey :: Maybe Text
    }

data TeamMember = TeamMember
    { _userId      :: UserId
    , _permissions :: Permissions
    }

data NewTeam = NewTeam
    { _newTeamName    :: Text
    , _newTeamIcon    :: Text
    , _newTeamIconKey :: Maybe Text
    , _newTeamMembers :: [TeamMember]
    }

data Permissions = Permissions
    { _self     :: Set Perm
    , _delegate :: Set Perm
    } deriving (Eq, Ord, Show)

data Perm =
      CreateConversation
    | DeleteConversation
    | AddTeamMember
    | RemoveTeamMember
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
permToInt CreateConversation = 0x01
permToInt DeleteConversation = 0x02
permToInt AddTeamMember      = 0x04
permToInt RemoveTeamMember   = 0x08
permToInt GetBilling         = 0x10
permToInt SetBilling         = 0x20
permToInt SetTeamData        = 0x40

intToPerm :: Word64 -> Maybe Perm
intToPerm 0x01 = Just CreateConversation
intToPerm 0x02 = Just DeleteConversation
intToPerm 0x04 = Just AddTeamMember
intToPerm 0x08 = Just RemoveTeamMember
intToPerm 0x10 = Just GetBilling
intToPerm 0x20 = Just SetBilling
intToPerm 0x40 = Just SetTeamData
intToPerm _    = Nothing

intToPerms :: Word64 -> Set Perm
intToPerms n =
    let perms = [ 2^i | i <- [0 .. 63], n `testBit` i ] in
    Set.fromList (mapMaybe intToPerm perms)

permsToInt :: Set Perm -> Word64
permsToInt = Set.foldr' (\p n -> n .|. permToInt p) 0

instance ToJSON Team where
    toJSON t = object
        $ "id"       .= _teamId t
        # "name"     .= _teamName t
        # "icon"     .= _teamIcon t
        # "icon_key" .= _teamIconKey t
        # []

instance FromJSON Team where
    parseJSON = withObject "team" $ \o -> do
        Team <$> o .:  "id"
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
        $ "self"     .= permsToInt (_self p)
        # "delegate" .= permsToInt (_delegate p)
        # []

instance FromJSON Permissions where
    parseJSON = withObject "permissions" $ \o -> do
        s <- intToPerms <$> o .: "self"
        d <- intToPerms <$> o .: "delegate"
        when (Set.null s) $ fail "empty permissions encountered"
        case newPermissions s d of
            Nothing -> fail "invalid permissions"
            Just ps -> pure ps
