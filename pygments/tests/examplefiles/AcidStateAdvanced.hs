{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TemplateHaskell
  , TypeFamilies, FlexibleInstances #-}
module Main where
import Control.Applicative  (Applicative, Alternative, (<$>))
import Control.Exception.Lifted    (bracket)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad        (MonadPlus, mplus)
import Control.Monad.Reader (MonadReader, ReaderT(..), ask)
import Control.Monad.Trans  (MonadIO(..))
import Data.Acid            ( AcidState(..), EventState(..), EventResult(..)
                            , Query(..), QueryEvent(..), Update(..), UpdateEvent(..)
                            , IsAcidic(..), makeAcidic, openLocalState
                            )
import Data.Acid.Local      ( createCheckpointAndClose
                            , openLocalStateFrom
                            )
import Data.Acid.Advanced   (query', update')
import Data.Maybe           (fromMaybe)
import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)
import Data.Data            (Data, Typeable)
import Data.Lens            ((%=), (!=))
import Data.Lens.Template   (makeLens)
import Data.Text.Lazy       (Text)
import Happstack.Server     ( Happstack, HasRqData, Method(GET, POST), Request(rqMethod)
                            , Response
                            , ServerPartT(..), WebMonad, FilterMonad, ServerMonad
                            , askRq, decodeBody, dir, defaultBodyPolicy, lookText
                            , mapServerPartT, nullConf, nullDir, ok, simpleHTTP
                            , toResponse
                            )
import Prelude hiding       (head, id)
import System.FilePath      ((</>))
import Text.Blaze           ((!))
import Text.Blaze.Html4.Strict (body, head, html, input, form, label, p, title, toHtml)
import Text.Blaze.Html4.Strict.Attributes (action, enctype, for, id, method, name, type_, value)
class HasAcidState m st where
   getAcidState :: m (AcidState st)
query :: forall event m. 
         ( Functor m
         , MonadIO m
         , QueryEvent event
         , HasAcidState m (EventState event)
         ) => 
         event
      -> m (EventResult event)
query event =
    do as <- getAcidState
       query' (as :: AcidState (EventState event)) event
update :: forall event m. 
          ( Functor m
          , MonadIO m
          , UpdateEvent event
          , HasAcidState m (EventState event)
          ) => 
          event 
       -> m (EventResult event)
update event =
    do as <- getAcidState
       update' (as :: AcidState (EventState event)) event
-- | bracket the opening and close of the `AcidState` handle. 

-- automatically creates a checkpoint on close
withLocalState :: (MonadBaseControl IO m, MonadIO m, IsAcidic st, Typeable st) => 
                  Maybe FilePath           -- ^ path to state directory
                 -> st                     -- ^ initial state value
                 -> (AcidState st -> m a) -- ^ function which uses the `AcidState` handle
                 -> m a
withLocalState mPath initialState =
    bracket (liftIO $ (maybe openLocalState openLocalStateFrom mPath) initialState)
            (liftIO . createCheckpointAndClose)
-- State that stores a hit count

data CountState = CountState { _count :: Integer }
                deriving (Eq, Ord, Data, Typeable, Show)

$(deriveSafeCopy 0 'base ''CountState)
$(makeLens ''CountState)

initialCountState :: CountState
initialCountState = CountState { _count = 0 }

incCount :: Update CountState Integer
incCount = count %= succ

$(makeAcidic ''CountState ['incCount])
-- State that stores a greeting
data GreetingState = GreetingState {  _greeting :: Text }
                deriving (Eq, Ord, Data, Typeable, Show)

$(deriveSafeCopy 0 'base ''GreetingState)
$(makeLens ''GreetingState)

initialGreetingState :: GreetingState
initialGreetingState = GreetingState { _greeting = "Hello" }

getGreeting :: Query GreetingState Text
getGreeting = _greeting <$> ask

setGreeting :: Text -> Update GreetingState Text
setGreeting txt = greeting != txt

$(makeAcidic ''GreetingState ['getGreeting, 'setGreeting])
data Acid = Acid { acidCountState    :: AcidState CountState
                 , acidGreetingState :: AcidState GreetingState
                 }

withAcid :: Maybe FilePath -> (Acid -> IO a) -> IO a
withAcid mBasePath action =
    let basePath = fromMaybe "_state" mBasePath
    in withLocalState (Just $ basePath </> "count")    initialCountState    $ \c ->
       withLocalState (Just $ basePath </> "greeting") initialGreetingState $ \g ->
           action (Acid c g)
newtype App a = App { unApp :: ServerPartT (ReaderT Acid IO) a }
    deriving ( Functor, Alternative, Applicative, Monad, MonadPlus, MonadIO
               , HasRqData, ServerMonad ,WebMonad Response, FilterMonad Response
               , Happstack, MonadReader Acid)

runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) = mapServerPartT (flip runReaderT acid) sp
instance HasAcidState App CountState where
    getAcidState = acidCountState    <$> ask 

instance HasAcidState App GreetingState where
    getAcidState = acidGreetingState <$> ask
page :: App Response
page =
    do nullDir
       g <- greet
       c <- update IncCount -- ^ a CountState event
       ok $ toResponse $
          html $ do
            head $ do
              title "acid-state demo"
            body $ do
              form ! action "/" ! method "POST" ! enctype "multipart/form-data" $ do
                label "new message: " ! for "msg"
                input ! type_ "text" ! id "msg" ! name "greeting"
                input ! type_ "submit" ! value "update message"
              p $ toHtml g
              p $ do "This page has been loaded " 
                     toHtml c
                     " time(s)."
    where
    greet =
        do m <- rqMethod <$> askRq
           case m of
             POST -> 
                 do decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
                    newGreeting <- lookText "greeting"
                    update (SetGreeting newGreeting)   -- ^ a GreetingState event
                    return newGreeting
             GET  -> 
                 do query GetGreeting                  -- ^ a GreetingState event
main :: IO ()
main =
    withAcid Nothing $ \acid ->
        simpleHTTP nullConf $ runApp acid page
newtype FooState = FooState { foo :: Text }
    deriving (Eq, Ord, Data, Typeable, SafeCopy)

initialFooState :: FooState
initialFooState = FooState { foo = "foo" }

askFoo :: Query FooState Text
askFoo = foo <$> ask

$(makeAcidic ''FooState ['askFoo])
fooPlugin :: (Happstack m, HasAcidState m FooState) => m Response
fooPlugin =
    dir "foo" $ do
       txt <- query AskFoo
       ok $ toResponse txt
data Acid' = Acid' { acidCountState'    :: AcidState CountState
                   , acidGreetingState' :: AcidState GreetingState
                   , acidFooState'      :: AcidState FooState
                   }
withAcid' :: Maybe FilePath -> (Acid' -> IO a) -> IO a
withAcid' mBasePath action =
    let basePath = fromMaybe "_state" mBasePath
    in withLocalState (Just $ basePath </> "count")    initialCountState    $ \c ->
       withLocalState (Just $ basePath </> "greeting") initialGreetingState $ \g ->
       withLocalState (Just $ basePath </> "foo")      initialFooState      $ \f ->
           action (Acid' c g f)
newtype App' a = App' { unApp' :: ServerPartT (ReaderT Acid' IO) a }
    deriving ( Functor, Alternative, Applicative, Monad, MonadPlus, MonadIO
               , HasRqData, ServerMonad ,WebMonad Response, FilterMonad Response
               , Happstack, MonadReader Acid')

instance HasAcidState App' FooState where
    getAcidState = acidFooState' <$> ask
fooAppPlugin :: App' Response
fooAppPlugin = fooPlugin
fooReaderPlugin :: ReaderT (AcidState FooState) (ServerPartT IO) Response
fooReaderPlugin = fooPlugin
instance HasAcidState (ReaderT (AcidState FooState) (ServerPartT IO)) FooState where
    getAcidState = ask
withFooPlugin :: (MonadIO m, MonadBaseControl IO m) => 
                 FilePath                          -- ^ path to state directory
              -> (ServerPartT IO Response -> m a)  -- ^ function that uses fooPlugin
              -> m a
withFooPlugin basePath f =
       do withLocalState (Just $ basePath </> "foo") initialFooState $ \fooState -> 
              f $ runReaderT fooReaderPlugin fooState
main' :: IO ()
main' = 
    withFooPlugin "_state" $ \fooPlugin' ->
        withAcid Nothing $ \acid ->
            simpleHTTP nullConf $ fooPlugin' `mplus` runApp acid page
