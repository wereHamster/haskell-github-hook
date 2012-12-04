{-|

  This module contains data definitions for the payload that the GitHub
  post-receive hook sends. All the data definitions are instances of
  'FromJSON', so you can parse the payload with 'Aeson'.

  Here is an Snap handler which processes the payload.

  > {-# LANGUAGE OverloadedStrings #-}
  >
  > import GitHub.Hook
  > import Data.Aeson (decode)
  >
  > hook :: Snap ()
  > hook = do
  >     payload <- getParam "payload"
  >     case payload of
  >         Nothing -> pass
  >         Just x  -> do
  >             case (decode $ fromChunks [ x ]) of
  >                 Nothing -> pass
  >                 Just x  -> -- Do something with x :: Payload

-}

module GitHub.Hook (Payload(..), Commit(..), Identity(..), Repository(..)) where

import Data.Aeson

import Control.Applicative
import Control.Monad


{-|

  The Identity represents a user or committer. This data is attached to each
  commit.

-}
data Identity = Identity
  { identityEmail :: String
    -- ^ The email address associated with the identity. This is taken
    --   straight from the git commit.

  , identityName :: String
    -- ^ The name associated with the identity. This is taken straight from
    --   the git commit.

  , identityUsername :: String
    -- ^ The github username. FIXME: Not sure if it's always present.

  } deriving (Show)


instance FromJSON Identity where
    parseJSON (Object x) =
        Identity            <$>
        (x .: "email")      <*>
        (x .: "name")       <*>
        (x .: "username")

    parseJSON _          = mzero



{-|

  The commit includes all the data from the git commit, plus some more.

-}
data Commit = Commit
  { commitId :: String
    -- ^ The commit SHA1 in hex-representation.

  , commitAuthor :: Identity
    -- ^ The commit auhor identity (the person who wrote the patch).

  , commitCommitter :: Identity
    -- ^ The commit committer identity (the person who committed the changes).

  } deriving (Show)


instance FromJSON Commit where
    parseJSON (Object x) =
        Commit              <$>
        (x .: "id")         <*>
        (x .: "author")     <*>
        (x .: "committer")

    parseJSON _          = mzero



{-|

  Information about the repository.

-}
data Repository = Repository
  { repositoryName :: String
    -- ^ The repository name.

  , repositoryOwner :: String
    -- ^ The username of the repository owner.

  } deriving (Show)


instance FromJSON Repository where
    parseJSON (Object x) =
        Repository                       <$>
        (x .: "owner" >>= (.: "name"))   <*>
        (x .: "name")

    parseJSON _          = mzero



{-|

  The 'Payload' is the top-level datastructure sent by the GitHub post-receive
  hook. It contains info about one ref that was updated by a git-push.

-}

data Payload = Payload
  { payloadRepository :: Repository
    -- ^ The repository which was affected by the push.

  , payloadRef :: String
    -- ^ The fully qualified ref name  which was affected by the push.

  , payloadBefore :: String
    -- ^ The ref SHA before the update. Is a null-SHA if the ref did not exist
    --   before the push.

  , payloadAfter :: String
    -- ^ The ref SHA after the update. Is a null-SHA if the push deleted the
    --   ref.

  , payloadCommits :: [ Commit ]
    -- ^ List of commits uploaded by the push. If no new commits have been
    --   uploaded to github, this list will be empty.

  } deriving (Show)


instance FromJSON Payload where
    parseJSON (Object x) =
        Payload                <$>
        (x .: "repository")    <*>
        (x .: "ref")           <*>
        (x .: "before")        <*>
        (x .: "after")         <*>
        (x .: "commits")

    parseJSON _          = mzero
