{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V21 (migration) where

import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 21 "Add teams" $ do
    schema' [r|
        CREATE TABLE team (
            team     uuid PRIMARY KEY,
            creator  uuid,
            name     text,
            icon     text,
            icon_key text,
        ) WITH compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
            AND gc_grace_seconds = 864000;
        |]

    schema' [r|
        CREATE TABLE team_conv (
            team  uuid,
            conv  uuid
            PRIMARY KEY (team, conv)
        ) WITH CLUSTERING ORDER BY (conv ASC)
            AND compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
            AND gc_grace_seconds = 864000;
        |]

    schema' [r|
        CREATE TABLE team_member (
            team  uuid,
            user  uuid,
            perm  bigint
            PRIMARY KEY (team, user)
        ) WITH CLUSTERING ORDER BY (user ASC)
            AND compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
            AND gc_grace_seconds = 864000;
        |]

    schema' [r|
        CREATE TABLE user_teams (
            user  uuid,
            team  uuid,
            PRIMARY KEY (user, team)
        ) WITH CLUSTERING ORDER BY (team ASC)
            AND compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
            AND gc_grace_seconds = 864000;
        |]

    schema' [r| ALTER TABLE conversation ADD team uuid; |]
    schema' [r| ALTER TABLE conversation ADD managed boolean; |]

