{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Example.Monad where
    
import Control.Concurrent       (forkIO)
import Control.Concurrent.Async (Async,async,wait)
import Control.Concurrent.Chan  (Chan,newChan,readChan,writeChan)
import Control.Concurrent.MVar  (MVar,modifyMVar,modifyMVar_,newMVar,putMVar,takeMVar,tryReadMVar,tryTakeMVar)
import Control.Exception        (catch,finally,try)
import Control.Monad            (join,void)
import Data.Either              (either,lefts,rights)
import Data.Map.Strict          (Map)
import Data.Maybe               (fromMaybe)
import Data.Serialize           (runGet)
import Data.Typeable            (Typeable)
import Control.Monad.IO.Class   (MonadIO,liftIO)
import Example.BaseTypes
--import Example.Responses
import Prelude                  (Applicative,Bool(..),Either(..),Eq,Functor,IO,Maybe(..),Monad,Ord,Show
                                ,($),(.),(<$>),(<*>),(>>),(>>=)
                                ,fail,fmap,maybe,pure,return,undefined)

-- Type to get to compile for now
data ParsedResponse e = SomeResponse (Map e ExampleData) deriving (Eq,Ord,Show)

instance ExampleResponseType ParsedResponse where
  exampleresponseget _ _ = undefined
  exampleresponseput _ _ = undefined
    
---- ExampleMonad's function is to allow for asyncronous and concurrent processing of requests, rawresponses, parseddata, and storage of runtime information
newtype ExampleMonad e a  = ExampleMonad (ExampleMonadEnvironment e -> IO (Either (ExampleError e) a))

--- Deriving the Functor instance as it applies to ExampleMonad    
instance Functor (ExampleMonad m) where
    fmap f (ExampleMonad g) = ExampleMonad $ \ environment -> fmap f <$> g environment

--- Deriving the Applicative instance as it applies to the ExampleMonad
instance Applicative (ExampleMonad m) where
    pure x = ExampleMonad $ \ _ -> pure (Right x)
    ExampleMonad ff <*> ExampleMonad fx =
        ExampleMonad $ \ environment -> do
          ef <- ff environment
          case ef of
            Left err -> pure (Left err)
            Right f -> do
              ex <- fx environment
              case ex of
                Left err -> pure (Left err)
                Right x  -> pure (Right $ f x)

--- Deriving the Monad instance for ExampleMonad
instance Monad (ExampleMonad m) where
    ExampleMonad f >>= g = ExampleMonad $ \ environment -> do
                             ex <- f environment
                             case ex of
                               Left err -> pure (Left err)
                               Right x  -> do
                                 let ExampleMonad h = g x
                                 h environment
    fail = throwExampleError . ExampleFail 

-- Derving the MonadIO instance for the ExampleMonad
instance MonadIO (ExampleMonad m) where
    liftIO io = ExampleMonad $ \ _ -> catch (Right <$> io)
                                           $ pure . Left . ExampleIOError

--- Function for throwing an error within the ExampleMonad to force termination
throwExampleError :: ExampleError e -> ExampleMonad e a
throwExampleError err = ExampleMonad $ \ _ -> pure (Left err)                                             


---- Type defining the environment while inside the ExampleMonad. Here the e is referencing the supplied indexing from the external application
data ExampleMonadEnvironment e = ExampleMonadEnvironment { requestChannel   :: Chan (Maybe (ByteStringWithIndices e))
                                                         , responseChannel  :: Chan (Maybe (ByteStringWithIndices e))
                                                         , exampleResponses :: MVar [Either (ExampleError e) (ParsedResponse e)]
                                                         , exampleEnvClose  :: MVar (IO())
                                                         }

---- Type for interfacing between the ExampleMonad and the rest of the program
data ExampleLink e a = ExampleLink { exampleLinkAsync     :: Async a
                                   , exampleLinkRequests  :: MVar (Maybe (Chan (Maybe (ByteStringWithIndices e))))
                                   , exampleLinkResponses :: MVar (Maybe (Chan (Maybe (ByteStringWithIndices e))))
                                   , exampleLinkConfig    :: ExampleConfiguration e
                                   , exampleLinkClose     :: MVar (IO())
                                   }

------ Function that initializes the ExampleMonad
runExampleMonad :: (Ord e, Show e) => ExampleConfiguration e -> ExampleMonad e a ->  IO (ExampleLink e (Either (ExampleError e) a))
runExampleMonad exampleconfig (ExampleMonad fcn)  = do
  requestchannel  <- newChan
  responsechannel <- newChan
  requestv <- newMVar $ Just requestchannel
  responsev <- newMVar $ Just responsechannel
  exampleresponses <- newMVar []
  close <- newMVar $ return ()
  let exampleenvironment = ExampleMonadEnvironment { requestChannel  = requestchannel
                                                   , responseChannel = responsechannel
                                                   , exampleResponses = exampleresponses
                                                   , exampleEnvClose  = close
                                                   }
  examplemonadasync <- async $ finally (fcn exampleenvironment) $ do
    writeChan requestchannel Nothing
    writeChan responsechannel Nothing
  void $ forkIO $ parseIncomingLoop exampleconfig responsechannel exampleresponses 
  pure ExampleLink { exampleLinkAsync     = examplemonadasync
                   , exampleLinkRequests  = requestv
                   , exampleLinkResponses = responsev
                   , exampleLinkConfig    = exampleconfig
                   , exampleLinkClose     = close
                   }

------ This is the async function that attempts to take a bytestring from the response channel and attempts to parse it into a message. The message is then placed into the response container.
parseIncomingLoop :: (Ord e,Show e) => ExampleConfiguration e -> Chan (Maybe (ByteStringWithIndices e)) -> MVar [Either (ExampleError e) (ParsedResponse e)] -> IO ()
parseIncomingLoop exampleconfig responsechannel exampleresponses =
  loop
    where
      loop = do
        maybebytestringwithindices <- readChan responsechannel
        case maybebytestringwithindices of
          Nothing -> pure ()
          Just (ByteStringWithIndices responsebs seqindices) -> do
            updateMVar $ either (Left . ExampleParseError seqindices) Right
                                (runGet (exampleresponseget exampleconfig seqindices) responsebs)
            loop
            where
              updateMVar eitherErrorOrResponse = modifyMVar_ exampleresponses $ pure . (eitherErrorOrResponse:)


----- Function for terminating the ExampleMonad by throwing the ExampleAbortError
abortExampleMonad :: ExampleMonad e (ExampleError e)
abortExampleMonad = throwExampleError ExampleAbort

----- Function for queing request messages into the ExampleMonad
feedRequestByteString :: Maybe (ByteStringWithIndices e) -> ExampleMonad e ()
feedRequestByteString maybebytestringwithindices =
    ExampleMonad $ \ environment -> do
      writeChan (requestChannel environment) maybebytestringwithindices
      pure (Right ())

---- Function that takes the ExampleLink to the Monad and attempts to retrieve a request, but if none is available, ends the channel.
retrieveExampleRequestByteStringWithIndices :: ExampleLink e a -> IO (Maybe (ByteStringWithIndices e))
retrieveExampleRequestByteStringWithIndices examplelink = do
  let requestv = exampleLinkRequests examplelink
  maybechannel <- takeMVar requestv
  case maybechannel of
    Nothing -> putMVar requestv Nothing >> pure Nothing
    Just channel -> do
      maybebytestringwithinterval <- readChan channel
      case maybebytestringwithinterval of
        Nothing -> putMVar requestv Nothing >> pure Nothing
        Just bytestringwithinterval -> putMVar requestv maybechannel >> pure (Just bytestringwithinterval)

--- Function that takes the ExampeLink to the Monad and attempts to place a response bytestring, and returns True if completed or false if no channel is found
feedExampleResponseByteString :: ExampleLink e a -> ByteStringWithIndices e -> IO Bool
feedExampleResponseByteString examplelink bytestringinterval = do
  let responsevar = exampleLinkResponses examplelink
  maybechannel <- takeMVar responsevar
  let unlock = putMVar responsevar maybechannel
  case maybechannel of
    Just channel -> writeChan channel (Just bytestringinterval) >> unlock >> pure True
    _ -> unlock >> pure False

--- Function that acesses the ResponseList and performs some function on the list and returns the result.
withExampleResponses :: ([Either (ExampleError e) (ParsedResponse e)] -> ([Either (ExampleError e) (ParsedResponse e)], a)) -> ExampleMonad e a
withExampleResponses fcn =
  ExampleMonad $ \ environment -> do
    x  <- modifyMVar (exampleResponses environment) $ pure . fcn
    pure (Right x)

-- Function that returns the first Response from the ResponseList, and putting the rest of the Responses back into the Monad
retrieveAnyExampleResponse :: ExampleMonad e (Maybe (Either (ExampleError e) (ParsedResponse e)))
retrieveAnyExampleResponse = withExampleResponses $ \ exampleresponses ->
  case exampleresponses of
    [] -> ([], Nothing)
    x : xs -> (xs, Just x)

-- Function that retrieves all Errors from the ResponseList, and returns those errors while putting all good response back to the Monad
retrieveErrors :: ExampleMonad e (Maybe [ExampleError e])
retrieveErrors = withExampleResponses $ \ exampleresponses ->
  case lefts exampleresponses of
    [] -> (exampleresponses, Nothing)
    errs -> (Right <$> rights exampleresponses, Just errs)

-- Function that retrieves all valid Resposes from the ResponseList, and putting all errors back to the Monad            
retrieveResponses :: ExampleMonad e (Maybe [ParsedResponse e])
retrieveResponses = withExampleResponses $ \ exampleresponses ->
  case rights exampleresponses of
    [] -> (exampleresponses, Nothing)
    responses -> (Left <$> lefts exampleresponses, Just responses)

-- Function that waits for the Link async function to return and handles Exceptions as a Left and Results as Right
getExampleLinkResult :: (Show e,Typeable e) => ExampleLink e (Either (ExampleError e) a) -> IO (Either (ExampleError e) a)
getExampleLinkResult = fmap join . try . wait . exampleLinkAsync

-- Function that allows access to the ExampleMonad environment
getExampleMonadEnvironment :: ExampleMonad e (ExampleMonadEnvironment e)
getExampleMonadEnvironment = ExampleMonad $ \env -> return $ Right env


------------------------ CloseAction is intended to ensure IO functions are terminated inside ExampleLink ----------------------------------
exampleClose :: ExampleMonad e ()
exampleClose = do
  env <- getExampleMonadEnvironment
  liftIO $ do
    closeAction <- tryReadMVar $ exampleEnvClose env
    fromMaybe (return ()) closeAction

addExampleClose :: IO () -> ExampleMonad e ()
addExampleClose newCloseAction = do
  env <- getExampleMonadEnvironment
  let closeExample = exampleEnvClose env
  liftIO $ do
    closeAction <- tryTakeMVar closeExample
    maybe (putMVar closeExample newCloseAction)
          (\ oldCloseAction -> putMVar closeExample $ oldCloseAction >> newCloseAction)
          closeAction

runExampleLinkClose :: ExampleLink e a -> IO ()
runExampleLinkClose link =
  modifyMVar_ closeMVar $ \ closeAction -> do
    closeAction
    return $ return ()
  where
    closeMVar = exampleLinkClose link

addExampleLinkClose :: ExampleLink e a -> IO () -> IO ()
addExampleLinkClose link newCloseAction =
  modifyMVar_ closeMVar $ \ closeAction ->
    return $ closeAction >> newCloseAction
  where
    closeMVar = exampleLinkClose link
