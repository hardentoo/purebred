{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Actions (
  Scrollable(..)
  , quit
  , focus
  , done
  , abort
  , noop
  , displayMail
  , displayThreadMails
  , setUnread
  , mailIndexUp
  , mailIndexDown
  , switchComposeEditor
  , replyMail
  , scrollUp
  , scrollDown
  , toggleHeaders
  , initialCompose
  , continue
  , chain
  , chain'
  , setTags
  , addTags
  , removeTags
  , reloadList
  ) where

import qualified Brick.Main as Brick
       (continue, halt, vScrollPage, viewportScroll, ViewportScroll)
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Network.Mail.Mime (Address(..), renderSendMail, simpleMail')
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Text (splitOn, strip, intercalate, unlines, Text)
import Data.Text.Lazy.IO (readFile)
import Prelude hiding (readFile, unlines)
import Control.Applicative ((<|>))
import Control.Lens (set, over, view, _Just, (&), Getting)
import Control.Lens.Fold ((^?!))
import Control.Monad ((>=>))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text.Zipper (currentLine, gotoEOL, textZipper)
import qualified Storage.Notmuch as Notmuch
       (getThreadMessages, getThreads, addTags, setTags, removeTags,
        ManageTags(..))
import Storage.ParsedMail (parseMail, getTo, getFrom, getSubject)
import Types
import Error

class Scrollable (n :: Mode) where
  makeViewportScroller :: Proxy n -> Brick.ViewportScroll Name

instance Scrollable 'ViewMail where
  makeViewportScroller _ = Brick.viewportScroll ScrollingMailView

instance Scrollable 'Help where
  makeViewportScroller _ = Brick.viewportScroll ScrollingHelpView


class ModeTransition (s :: Mode) (d :: Mode) where

instance ModeTransition s s where

instance ModeTransition 'ManageTags 'BrowseMail where

instance ModeTransition 'ManageTags 'BrowseThreads where

instance ModeTransition 'BrowseThreads 'SearchMail where

instance ModeTransition 'BrowseMail 'ManageTags where

instance ModeTransition 'BrowseThreads 'ManageTags where

instance ModeTransition 'ViewMail 'BrowseMail where

instance ModeTransition 'BrowseThreads 'BrowseMail where

instance ModeTransition 'BrowseMail 'BrowseThreads  where

instance ModeTransition 'SearchMail 'BrowseThreads  where

instance ModeTransition 'BrowseThreads 'GatherHeaders where

instance ModeTransition 'BrowseMail 'GatherHeaders where

instance ModeTransition 'GatherHeaders 'BrowseThreads where

instance ModeTransition 'ComposeEditor 'BrowseThreads where

instance ModeTransition 'Help 'BrowseThreads where

instance ModeTransition s 'Help where  -- help can be reached from any mode

-- | An action - typically completed by a key press (e.g. Enter) - and it's
-- contents are used to be applied to an action.
--
-- For example: the user changes
-- the notmuch search terms to find a particular mail. To apply his changes, he
-- 'completes' his text entered by pressing Enter.
--
-- Another example is sending e-mail. So the complete action for the
-- ComposeEditor is sent, since that's at the end of the composition process.
--
class Completable (m :: Mode) where
  complete :: Proxy m -> AppState -> T.EventM Name AppState

instance Completable 'SearchMail where
  complete _ = applySearch

instance Completable 'ComposeEditor where
  complete _ = sendMail

instance Completable 'ManageTags where
  complete _ = \s -> do
    s' <- liftIO $ applyEditorMailTags s
    restoreNMSearch s'

-- | Generalisation of reset actions, whether they reset editors back to their
-- initial state or throw away composed, but not yet sent mails.
--
class Resetable (m :: Mode) where
  reset :: Proxy m -> AppState -> T.EventM Name AppState

instance Resetable 'ComposeEditor where
  reset _ = pure . set asCompose initialCompose

instance Resetable 'ManageTags where
  reset _ = \s -> let terms = textZipper [(view (asConfig . confNotmuch . nmSearch) s)] Nothing
                  in pure $ s &
                     set (asMailIndex . miSearchEditor . E.editContentsL) (terms)
                     . over (asMailIndex . miSearchEditor) (E.applyEdit gotoEOL)

-- | Generalisation of focus changes between widgets on the same "view"
-- expressed with the mode in the application state.
--
class Focusable (m :: Mode) where
  switchFocus :: Proxy m -> AppState -> T.EventM Name AppState

instance Focusable 'SearchMail where
  switchFocus _ = pure . over (asMailIndex . miSearchEditor) (E.applyEdit gotoEOL)

instance Focusable 'ManageTags where
  switchFocus _ = pure . prepareTagEditMode

instance Focusable 'BrowseMail where
  switchFocus _ = pure

instance Focusable 'BrowseThreads where
  switchFocus _ = pure . set asAppMode BrowseThreads

instance Focusable 'GatherHeaders where
  switchFocus _ = pure . set asAppMode GatherHeaders

instance Focusable 'Help where
  switchFocus _ = pure . set asAppMode Help

-- | Problem: How to chain actions, which operate not on the same mode, but a
-- mode switched by the previous action?
class HasMode (a :: Mode) where
  mode :: Proxy a -> Mode

-- promote the type to a value we can use for chaining actions
instance HasMode 'BrowseMail where
  mode _ = BrowseMail

instance HasMode 'SearchMail where
  mode _ = SearchMail

instance HasMode 'ViewMail where
  mode _ = ViewMail

instance HasMode 'ManageTags where
  mode _ = ManageTags

instance HasMode 'BrowseThreads where
  mode _ = BrowseThreads

instance HasMode 'GatherHeaders where
  mode _ = GatherHeaders

instance HasMode 'Help where
  mode _ = Help

quit :: Action ctx (T.Next AppState)
quit = Action "quit the application" Brick.halt

continue :: Action ctx (T.Next AppState)
continue = Action "" Brick.continue

chain :: Action ctx AppState -> Action ctx a -> Action ctx a
chain (Action d1 f1) (Action d2 f2) =
  Action (if null d2 then d1 else d1 <> " and then " <> d2) (f1 >=> f2)

chain'
    :: forall ctx ctx' a.
       (HasMode ctx', ModeTransition ctx ctx')
    => Action ctx AppState
    -> Action ctx' a
    -> Action ctx a
chain' (Action d1 f1) (Action d2 f2) =
  Action (if null d2 then d1 else d1 <> " and then " <> d2) (f1 >=> switchMode >=> f2)
  where
    switchMode = pure . set asAppMode (mode (Proxy :: Proxy ctx'))

done :: forall a. Completable a => Action a AppState
done = Action "apply" (complete (Proxy :: Proxy a))

abort :: forall a. Resetable a => Action a AppState
abort = Action "cancel" (reset (Proxy :: Proxy a))

focus :: forall a. (HasMode a, Focusable a) => Action a AppState
focus = Action ("switch mode to " <> show (mode (Proxy :: Proxy a))) (switchFocus (Proxy :: Proxy a))

-- | A no-op action which just returns the current AppState
-- This action can be used at the start of an Action chain where an immediate
-- mode switch is required
noop :: Action ctx AppState
noop = Action "" pure

scrollUp :: forall ctx. (Scrollable ctx) => Action ctx AppState
scrollUp = Action
  { _aDescription = "scrolling up"
  , _aAction = (\s -> Brick.vScrollPage (makeViewportScroller (Proxy :: Proxy ctx)) T.Up >> pure s)
  }

scrollDown :: forall ctx. (Scrollable ctx) => Action ctx AppState
scrollDown = Action
  { _aDescription = "scrolling down"
  , _aAction = (\s -> Brick.vScrollPage (makeViewportScroller (Proxy :: Proxy ctx)) T.Down >> pure s)
  }

displayMail :: Action ctx AppState
displayMail =
    Action
    { _aDescription = "display an e-mail"
    , _aAction = \s -> liftIO $ updateStateWithParsedMail s
                       >>= updateReadState Notmuch.removeTags
    }

displayThreadMails :: Action 'BrowseThreads AppState
displayThreadMails =
    Action
    { _aDescription = "display an e-mail for threads"
    , _aAction = liftIO . setMailsForThread
    }

setUnread :: Action 'BrowseMail AppState
setUnread =
    Action
    { _aDescription = "toggle unread"
    , _aAction = (liftIO . updateReadState Notmuch.addTags)
    }

mailIndexUp :: Action m AppState
mailIndexUp =
    Action
    { _aDescription = "mail index up one e-mail"
    , _aAction = \s -> case view asAppMode s of
        BrowseMail -> pure $ over (asMailIndex . miListOfMails) L.listMoveUp s
        _ -> pure $ over (asMailIndex . miListOfThreads) L.listMoveUp s
    }

mailIndexDown :: Action m AppState
mailIndexDown =
    Action
    { _aDescription = "mail index down one e-mail"
    , _aAction = \s -> case view asAppMode s of
        BrowseMail -> pure $ over (asMailIndex . miListOfMails) L.listMoveDown s
        _ -> pure $ over (asMailIndex. miListOfThreads) L.listMoveDown s
    }

switchComposeEditor :: Action 'BrowseThreads AppState
switchComposeEditor =
    Action
    { _aDescription = "switch to compose editor"
    , _aAction = \s -> case view (asCompose . cTmpFile) s of
                          Just _ -> pure $ set asAppMode ComposeEditor s
                          Nothing -> pure s
    }

replyMail :: Action 'BrowseMail AppState
replyMail =
    Action
    { _aDescription = "reply to an e-mail"
    , _aAction = replyToMail
    }

toggleHeaders :: Action 'ViewMail AppState
toggleHeaders = Action
  { _aDescription = "toggle mail headers"
  , _aAction = pure . go
  }
  where
    go :: AppState -> AppState
    go s = case view (asMailView . mvHeadersState) s of
      Filtered -> set (asMailView . mvHeadersState) ShowAll s
      ShowAll -> set (asMailView . mvHeadersState) Filtered s

setTags :: [Text] -> Action ctx AppState
setTags ts =
    Action
    { _aDescription = "apply given tags"
    , _aAction = (\s -> case view asAppMode s of
                     BrowseMail -> liftIO . selectedItemHelper (asMailIndex . miListOfMails) s
                                   $ \m -> applyItemTags m ts Notmuch.setTags s
                     _ -> liftIO . selectedItemHelper (asMailIndex . miListOfThreads) s
                          $ \m -> applyItemTags m ts Notmuch.setTags s)
    }

addTags :: [Text] -> Action ctx AppState
addTags ts =
    Action
    { _aDescription = "add given tags"
    , _aAction = (\s -> case view asAppMode s of
                     BrowseMail -> liftIO . selectedItemHelper (asMailIndex . miListOfMails) s
                                   $ \m -> applyItemTags m ts Notmuch.addTags s
                     _ -> liftIO . selectedItemHelper (asMailIndex . miListOfThreads) s
                          $ \m -> applyItemTags m ts Notmuch.addTags s)
    }

removeTags :: [Text] -> Action ctx AppState
removeTags ts =
    Action
    { _aDescription = "remove given tags"
    , _aAction = (\s -> case view asAppMode s of
                     BrowseMail -> liftIO . selectedItemHelper (asMailIndex . miListOfMails) s
                                   $ \m -> applyItemTags m ts Notmuch.removeTags s
                     _ -> liftIO . selectedItemHelper (asMailIndex . miListOfThreads) s
                          $ \m -> applyItemTags m ts Notmuch.removeTags s)
    }

reloadList :: Action 'BrowseThreads AppState
reloadList = Action "reload list of threads" applySearch

-- Function definitions for actions
--
applySearch :: AppState -> T.EventM Name AppState
applySearch s = runExceptT (Notmuch.getThreads searchterms (view (asConfig . confNotmuch) s))
                >>= pure . ($ s) . either setError (updateList)
   where searchterms = currentLine $ view (asMailIndex . miSearchEditor . E.editContentsL) s
         updateList vec s' =
           let current = view (asMailIndex . miListOfThreads . L.listSelectedL) s' <|> Just 0
           in over (asMailIndex . miListOfThreads) (L.listReplace vec current) s'

setMailsForThread :: AppState -> IO AppState
setMailsForThread s = selectedItemHelper (asMailIndex . miListOfThreads) s $ \t ->
  let dbpath = view (asConfig . confNotmuch . nmDatabase) s
      updateThreadMails vec = over (asMailIndex . miListOfMails) (L.listReplace vec (Just 0))
  in either setError updateThreadMails <$> runExceptT (Notmuch.getThreadMessages dbpath t)

selectedItemHelper
    :: Applicative f
    => Getting (L.List n t) AppState (L.List n t)
    -> AppState
    -> (t -> f (AppState -> AppState))
    -> f AppState
selectedItemHelper l s func =
  ($ s) <$> case L.listSelectedElement (view l s) of
  Just (_, m) -> func m
  Nothing -> pure $ setError (GenericError "No item selected.")

class ListItemSetter a where
  updateListItem :: a -> AppState -> AppState

instance ListItemSetter NotmuchMail where
  updateListItem m = over (asMailIndex . miListOfMails) (\x -> L.listModify (const m) x)

instance ListItemSetter NotmuchThread where
  updateListItem m = over (asMailIndex . miListOfThreads) (\x -> L.listModify (const m) x)

applyItemTags
    :: (ListItemSetter a, Notmuch.ManageTags a, MonadIO f)
    => a
    -> [Text]
    -> (a -> [Text] -> a)
    -> AppState
    -> f (AppState -> AppState)
applyItemTags m ts op s =
  let dbpath = view (asConfig . confNotmuch . nmDatabase) s
  in either setError updateListItem <$> runExceptT (Notmuch.writeToNotmuch dbpath (op m ts))

applyEditorMailTags :: AppState -> IO AppState
applyEditorMailTags s = case view asAppMode s of
  BrowseMail -> selectedItemHelper (asMailIndex . miListOfMails) s $ \m -> applyEditorMailTags' m s
  _ -> selectedItemHelper (asMailIndex . miListOfThreads) s $ \m -> applyEditorMailTags' m s

applyEditorMailTags' :: (ListItemSetter a, Notmuch.ManageTags a) => a -> AppState -> IO (AppState -> AppState)
applyEditorMailTags' m s =
  let contents = (unlines $ E.getEditContents $ view (asMailIndex . miSearchEditor) s)
      tags = strip <$> splitOn "," contents
  in applyItemTags m tags Notmuch.setTags s

restoreNMSearch :: AppState -> T.EventM Name AppState
restoreNMSearch s =
  let z = textZipper [(view (asConfig . confNotmuch . nmSearch) s)] Nothing
  in pure $ set (asMailIndex . miSearchEditor . E.editContentsL) z s

updateStateWithParsedMail :: AppState -> IO AppState
updateStateWithParsedMail s = selectedItemHelper (asMailIndex . miListOfMails) s $ \m ->
        either
            (\e -> setError e . set asAppMode BrowseMail)
            (\pmail -> set (asMailView . mvMail) (Just pmail) . set asAppMode ViewMail)
            <$> runExceptT (parseMail m (view (asConfig . confNotmuch . nmDatabase) s))

updateReadState :: (NotmuchMail -> [Text] -> NotmuchMail) -> AppState -> IO AppState
updateReadState op s =
  let newTag = view (asConfig . confNotmuch . nmNewTag) s
  in selectedItemHelper (asMailIndex . miListOfMails) s $ \m -> applyItemTags m [newTag] op s

setError :: Error -> AppState -> AppState
setError = set asError . Just

prepareTagEditMode :: AppState -> AppState
prepareTagEditMode s = case view asAppMode s of
  BrowseMail -> maybe s (setTagEditor s) (L.listSelectedElement (view (asMailIndex . miListOfMails) s))
  _ -> maybe s (setTagEditor s) (L.listSelectedElement (view (asMailIndex . miListOfThreads) s))

setTagEditor :: Notmuch.ManageTags a => AppState -> (Int, a) -> AppState
setTagEditor s (_, m) =
    let tags = intercalate "," $ Notmuch.getTags m
        contents = currentLine $ view (asMailIndex . miSearchEditor . E.editContentsL) s
    in s
       & set (asMailIndex . miSearchEditor) (E.editor ManageTagsEditor (Just 1) tags)
       -- shelf the current search state
       . set (asConfig . confNotmuch . nmSearch) contents
       . over (asMailIndex . miSearchEditor) (E.applyEdit gotoEOL)

replyToMail :: AppState -> T.EventM Name AppState
replyToMail s =
  pure . ($ s)
  =<< case L.listSelectedElement (view (asMailIndex . miListOfMails) s) of
    Just (_, m) -> either handleErr handleMail
                   <$> runExceptT (parseMail m (view (asConfig . confNotmuch . nmDatabase) s))
    Nothing -> pure id
  where
    handleErr e = set asAppMode BrowseMail . setError e
    handleMail pmail =
      set (asCompose . cTo) (E.editor GatherHeadersTo Nothing $ getFrom pmail)
      . set (asCompose . cFrom) (E.editor GatherHeadersFrom Nothing $ getTo pmail)
      . set (asCompose . cSubject)
        (E.editor GatherHeadersSubject Nothing ("Re: " <> getSubject pmail))
      . set (asCompose . cFocus) AskFrom
      . set asAppMode GatherHeaders

sendMail :: AppState -> T.EventM Name AppState
sendMail s = do
    -- XXX if something has removed the tmpfile for whatever reason we go b00m :(
    body <- liftIO $ readFile (view (asCompose . cTmpFile) s ^?! _Just)
    let to =
            Address
                Nothing
                (unlines $ E.getEditContents $ view (asCompose . cTo) s)
    let from =
            Address
                Nothing
                (unlines $ E.getEditContents $ view (asCompose . cFrom) s)
    let m =
            simpleMail'
                to
                from
                (unlines $ E.getEditContents $ view (asCompose . cSubject) s)
                body
    liftIO $ renderSendMail m
    pure $ set asCompose initialCompose s

initialCompose :: Compose
initialCompose =
    Compose
        Nothing
        AskFrom
        (E.editor GatherHeadersFrom Nothing "")
        (E.editor GatherHeadersTo Nothing "")
        (E.editor GatherHeadersSubject Nothing "")
