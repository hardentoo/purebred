{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | module for integrating notmuch within purebred
module Storage.Notmuch where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (void, MonadError, throwError)
import qualified Data.ByteString as B
import Data.Traversable (traverse)
import Data.List (union, notElem)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Vector as Vec
import System.Process (readProcess)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Types
import Control.Lens (view, over, set)

import Notmuch
import Notmuch.Search
import Notmuch.Util (bracketT)

import Error


-- | creates a vector of parsed mails from a not much search
-- Note, that at this point in time only free form searches are supported. Also,
-- we filter out the tag which we use to mark mails as new mails
getMessages
  :: (MonadError Error m, MonadIO m)
  => T.Text
  -> NotmuchSettings FilePath
  -> m (Vec.Vector NotmuchMail)
getMessages s settings =
  bracketT (databaseOpenReadOnly (view nmDatabase settings)) databaseDestroy go
  where go db = do
              msgs <- query db (FreeForm $ T.unpack s) >>= messages
              mails <- liftIO $ mapM messageToMail msgs
              pure $ Vec.fromList mails

-- | creates a vector of parsed mails which are tips of each thread
--
getThreads
  :: (HasThreads Query, MonadError Error m, MonadIO m)
  => T.Text
  -> NotmuchSettings FilePath
  -> m (Vec.Vector NotmuchThread)
getThreads s settings =
  bracketT (databaseOpenReadOnly (view nmDatabase settings)) databaseDestroy go
  where
    go db = do
        ts <- query db (FreeForm $ T.unpack s) >>= threads
        t <- liftIO $ traverse threadToThread ts
        pure $ Vec.fromList t

getThreadMessages
  :: (MonadError Error m, MonadIO m)
  => FilePath
  -> NotmuchThread
  -> m (Vec.Vector NotmuchMail)
getThreadMessages fp t =
  bracketT (databaseOpenReadOnly fp) databaseDestroy go
  where go db = do
          msgs <- getThread db (view thId t) >>= messages
          mails <- liftIO $ traverse messageToMail msgs
          pure $ Vec.fromList mails

mailFilepath
  :: (MonadError Error m, MonadIO m)
  => NotmuchMail -> FilePath -> m FilePath
mailFilepath m dbpath =
  bracketT (databaseOpenReadOnly dbpath) databaseDestroy go
  where
    go db = getMessage db (view mailId m) >>= messageFilename

setNotmuchMailTags
  :: (MonadError Error m, MonadIO m)
  => FilePath
  -> NotmuchMail
  -> m NotmuchMail
setNotmuchMailTags dbpath m = do
  nmtags <- toNotmuchTags (view mailTags m)
  bracketT (databaseOpen dbpath) databaseDestroy (tagsToMessage nmtags (view mailId m))
  pure m

setNotmuchThreadTags
  :: (MonadError Error m, MonadIO m)
  => FilePath
  -> NotmuchThread
  -> m NotmuchThread
setNotmuchThreadTags dbpath t = do
  tgs <- toNotmuchTags (view thTags t)
  mgs <- getThreadMessages dbpath t
  void $ bracketT (databaseOpen dbpath) databaseDestroy (go tgs mgs)
  pure t
    where go xs msgs db = traverse (\x -> tagsToMessage xs (view mailId x) db) msgs

tagsToMessage
  :: (MonadError Error m, MonadIO m)
  => [Tag] -> B.ByteString -> Database RW -> m ()
tagsToMessage xs id' db = getMessage db id' >>= messageSetTags xs

-- | Get message by message ID, throwing MessageNotFound if not found
--
getMessage
  :: (MonadError Error m, MonadIO m)
  => Database mode -> B.ByteString -> m (Message 0 mode)
getMessage db msgId =
  findMessage db msgId
  >>= maybe (throwError (MessageNotFound msgId)) pure

class ManageTags a where
  setTags :: a -> [T.Text] -> a
  addTags :: a -> [T.Text] -> a
  removeTags :: a -> [T.Text] -> a
  getTags :: a -> [T.Text]
  writeToNotmuch :: (MonadError Error m, MonadIO m) => FilePath -> a -> m a

instance ManageTags NotmuchMail where
  setTags m ts = set mailTags ts m
  addTags m ts = over mailTags (`union` ts) m
  removeTags m ts = over mailTags (filter (`notElem` ts)) m
  getTags = view mailTags
  writeToNotmuch = setNotmuchMailTags

instance ManageTags NotmuchThread where
  setTags m ts = set thTags ts m
  addTags m ts = over thTags (`union` ts) m
  removeTags m ts = over thTags (filter (`notElem` ts)) m
  getTags = view thTags
  writeToNotmuch = setNotmuchThreadTags

toNotmuchTags :: MonadError Error m => [T.Text] -> m [Tag]
toNotmuchTags = traverse (mkTag' . encodeUtf8)
  where mkTag' s = maybe (throwError (InvalidTag s)) pure $ mkTag s

messageToMail
    :: HasTags (Message n a)
    => Message n a
    -> IO NotmuchMail
messageToMail m = do
    tgs <- tags m
    let tgs' = decodeUtf8 . getTag <$> tgs
    NotmuchMail
      <$> (decodeUtf8 . fromMaybe "" <$> messageHeader "Subject" m)
      <*> (decodeUtf8 . fromMaybe "" <$> messageHeader "From" m)
      <*> messageDate m
      <*> pure tgs'
      <*> messageId m

getThread
  :: (MonadError Error m, MonadIO m)
  => Database mode -> B.ByteString -> m (Thread mode)
getThread db tid = do
  t <- query db (Thread tid) >>= threads
  maybe (throwError (ThreadNotFound tid)) pure (listToMaybe t)

threadToThread
  :: (HasTags (Thread a), HasThread (Thread a))
  => Thread a
  -> IO NotmuchThread
threadToThread m = do
    tgs <- tags m
    auth <- threadAuthors m
    let tgs' = decodeUtf8 . getTag <$> tgs
    NotmuchThread
      <$> (decodeUtf8 <$> threadSubject m)
      <*> (pure $ view matchedAuthors auth)
      <*> threadNewestDate m
      <*> pure tgs'
      <*> threadTotalMessages m
      <*> threadId m

getDatabasePath :: IO FilePath
getDatabasePath = getFromNotmuchConfig "database.path"

getFromNotmuchConfig :: String -> IO String
getFromNotmuchConfig key = do
  let cmd = "notmuch"
  let args = ["config", "get", key]
  stdout <- readProcess cmd args []
  pure $ filter (/= '\n') stdout

mailIsNew :: T.Text -> [T.Text] -> Bool
mailIsNew ignoredTag tgs = ignoredTag `elem` tgs
