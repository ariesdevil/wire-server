{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Galley.API.Teams
    ( getTeam
    , getManyTeams
    , createTeam
    , deleteTeam
    , updateTeam
    , getTeamMembers
    , updateTeamMembers
    , getTeamConvs
    , updateTeamConvs
    ) where

import Cassandra (result, hasMore)
import Control.Monad (unless)
import Control.Monad.Catch
import Data.ByteString.Conversion
import Data.Int
import Data.Id
import Data.Maybe (catMaybes)
import Data.Range
import Data.Traversable (mapM)
import Galley.App
import Galley.API.Error
import Galley.API.Util
import Galley.Types.Teams
import Network.Wai
import Network.Wai.Predicate hiding (setStatus, result)
import Network.Wai.Utilities
import Prelude hiding (head, mapM)

import qualified Galley.Data as Data

getTeam :: UserId ::: TeamId ::: JSON -> Galley Response
getTeam (zusr::: tid ::: _) =
    maybe (throwM teamNotFound) (pure . json) =<< lookupTeam zusr tid

getManyTeams :: UserId ::: Maybe (Either (Range 1 32 (List TeamId)) TeamId) ::: Range 1 500 Int32 ::: JSON -> Galley Response
getManyTeams (zusr ::: range ::: size ::: _) =
    withTeamIds zusr range size $ \more ids -> do
        teams <- mapM (lookupTeam zusr) ids
        pure (json $ newTeamList (catMaybes teams) more)

lookupTeam :: UserId -> TeamId -> Galley (Maybe Team)
lookupTeam zusr tid = do
    m <- Data.teamMembers tid
    unless (zusr `isTeamMember` m) $
        throwM teamNotFound
    Data.team tid

createTeam :: UserId ::: Request ::: JSON -> Galley Response
createTeam (zusr::: req ::: _) = undefined

deleteTeam :: UserId ::: TeamId ::: JSON -> Galley Response
deleteTeam (zusr::: tid ::: _) = undefined

updateTeam :: UserId ::: TeamId ::: Request ::: JSON -> Galley Response
updateTeam (zusr::: tid ::: req ::: _) = undefined

getTeamMembers :: UserId ::: TeamId ::: JSON -> Galley Response
getTeamMembers (zusr::: tid ::: _) = undefined

updateTeamMembers :: UserId ::: TeamId ::: Request ::: JSON -> Galley Response
updateTeamMembers (zusr::: tid ::: req ::: _) = undefined

getTeamConvs :: UserId ::: TeamId ::: JSON -> Galley Response
getTeamConvs (zusr::: tid ::: _) = undefined

updateTeamConvs :: UserId ::: TeamId ::: Request ::: JSON -> Galley Response
updateTeamConvs (zusr::: tid ::: req ::: _) = undefined

-- Internal -----------------------------------------------------------------

-- | Invoke the given continuation 'k' with a list of team IDs
-- which are looked up based on:
--
-- * just limited by size
-- * an (exclusive) starting point (team ID) and size
-- * a list of team IDs
--
-- The last case returns those team IDs which have an associated
-- user. Additionally 'k' is passed in a 'hasMore' indication (which is
-- always false if the third lookup-case is used).
withTeamIds :: UserId
            -> Maybe (Either (Range 1 32 (List TeamId)) TeamId)
            -> Range 1 500 Int32
            -> (Bool -> [TeamId] -> Galley Response)
            -> Galley Response
withTeamIds usr range size k = case range of
    Nothing        -> do
        Data.ResultSet r <- Data.teamIdsFrom usr Nothing (rcast size)
        k (hasMore r) (result r)

    Just (Right c) -> do
        Data.ResultSet r <- Data.teamIdsFrom usr (Just c) (rcast size)
        k (hasMore r) (result r)

    Just (Left cc) -> do
        ids <- Data.teamIdsOf usr cc
        k False ids
{-# INLINE withTeamIds #-}
