module Main where

import Control.Monad.RWS
import qualified Data.SortedList as SL
import qualified System.Random.MWC as MWC
import System.Random.MWC.Distributions

type Time = Double

data AlohaSimConfig = AlohaSimConfig {
  simDuration             :: Time,
  frameDuration           :: Time,
  meanAttemptsPerFrame    :: Double
}

data AlohaSimLog = AlohaSimLog {
  numFramesTransmitted    :: Int,
  numFramesReceived       :: Int,
  numCollisions           :: Int
} deriving (Show)

instance Monoid AlohaSimLog where
  mempty = logEmpty
  log1 `mappend` log2 = AlohaSimLog {
    numFramesTransmitted  = numFramesTransmitted  log1 + numFramesTransmitted   log2,
    numFramesReceived     = numFramesReceived     log1 + numFramesReceived      log2,
    numCollisions         = numCollisions         log1 + numCollisions          log2
  }

logEmpty = AlohaSimLog {
  numFramesTransmitted    = 0,
  numFramesReceived       = 0,
  numCollisions           = 0
}

oneFrameTransmitted       = logEmpty { numFramesTransmitted   = 1 }
oneFrameReceived          = logEmpty { numFramesReceived      = 1 }
oneCollision              = logEmpty { numCollisions          = 1 }

type Event = RWST AlohaSimConfig AlohaSimLog AlohaSimState IO ()

data TimedEvent = TE Time Event

instance Eq TimedEvent where
  (TE t1 _) == (TE t2 _) = t1 == t2

instance Ord TimedEvent where
  (TE t1 _) `compare` (TE t2 _) = t1 `compare` t2

type EventQueue = SL.SortedList TimedEvent

data AlohaSimState = AlohaSimState {
  currentSimulationTime   :: Time,
  numFrameTransmissions   :: Int,
  eventQueue              :: EventQueue,
  randomNumberGenerator   :: MWC.GenIO
}

scheduleNextFrameArrival :: Event
scheduleNextFrameArrival = do
  gen <- gets randomNumberGenerator
  load <- asks meanAttemptsPerFrame
  tf <- asks frameDuration
  dt <- liftIO $ exponential (load*tf) gen
  t <- gets currentSimulationTime
  modify . scheduleEvent $ TE (t+dt) beginFrameTransmission

beginFrameTransmission :: Event
beginFrameTransmission = do
  tell oneFrameTransmitted
  t <- gets currentSimulationTime
  liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Frame transmission begins"
  numft <- gets numFrameTransmissions
  case numft == 0 of
    True -> return ()
    False -> do
      tell oneCollision
      liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Frame collision occurred"
  modify incrFrameTransmissions
  tf <- asks frameDuration
  modify . scheduleEvent $ TE (t+tf) endFrameTransmission
  scheduleNextFrameArrival

endFrameTransmission :: Event
endFrameTransmission = do
  t <- gets currentSimulationTime
  liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Frame transmission ends"
  modify decrFrameTransmissions
  

incrFrameTransmissions :: AlohaSimState -> AlohaSimState
incrFrameTransmissions st = st { numFrameTransmissions = (+) 1 $ numFrameTransmissions st }

decrFrameTransmissions :: AlohaSimState -> AlohaSimState
decrFrameTransmissions st = st { numFrameTransmissions = (+) (-1) $ numFrameTransmissions st }

scheduleEvent :: TimedEvent -> AlohaSimState -> AlohaSimState
scheduleEvent te st = st { eventQueue = SL.insert te $ eventQueue st }

alohaSim :: Event
alohaSim = do
  q <- gets eventQueue        -- Get the current eventQueue
  case SL.uncons q of         -- Check if the eventQueue is empty
    Nothing -> return ()
    Just (TE t event, q') -> do
      endt <- asks simDuration  -- Get the simulation duration
      case (t > endt) of        -- Check if earliest event time exceeds simulation duration
        True -> return ()
        False -> do
          modify (\st -> st { currentSimulationTime = t, eventQueue = q' })
          event     -- Execute earliest event
          alohaSim    -- Recursive call to continue processing events


main :: IO ()
main = do
  gen <- MWC.create
  let initialState = AlohaSimState {
        currentSimulationTime   = 0.0,
        numFrameTransmissions   = 0,
        eventQueue              = SL.singleton $ TE 0.0 scheduleNextFrameArrival,
        randomNumberGenerator   = gen
      }
      config = AlohaSimConfig {
        simDuration             = 20.0,
        frameDuration           = 1.0,
        meanAttemptsPerFrame    = 0.5
      }

  (s, log) <- execRWST alohaSim config initialState
  putStrLn $ show log
