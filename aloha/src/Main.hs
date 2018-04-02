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
  frameBeginTimes         :: [Time],
  eventQueue              :: EventQueue,
  randomNumberGenerator   :: MWC.GenIO
}

scheduleNextFrameArrival :: Event
scheduleNextFrameArrival = do
  gen <- gets randomNumberGenerator
  load <- asks meanAttemptsPerFrame
  tf <- asks frameDuration
  dt <- liftIO $ exponential (load/tf) gen
  t <- gets currentSimulationTime
  modify . scheduleEvent $ TE (t+dt) beginFrameTransmission

beginFrameTransmission :: Event
beginFrameTransmission = do
  tell oneFrameTransmitted
  t <- gets currentSimulationTime
  liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Frame transmission begins"
  modify $ addFrameBeginTime t
  tf <- asks frameDuration
  modify . scheduleEvent $ TE (t+tf) endFrameTransmission
  scheduleNextFrameArrival

endFrameTransmission :: Event
endFrameTransmission = do
  t <- gets currentSimulationTime
  liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Frame transmission ends"
  fbts <- gets frameBeginTimes
  tf <- asks frameDuration
  case (isCollisionAtFrameEndTime fbts tf t) of
    True -> do
      tell oneCollision
      liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Frame collision occurred"
    False -> do
      tell oneFrameReceived
      liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Frame received successfully"
  modify $ filterFrameBeginTimes t tf

scheduleEvent :: TimedEvent -> AlohaSimState -> AlohaSimState
scheduleEvent te st = st { eventQueue = SL.insert te $ eventQueue st }

addFrameBeginTime :: Time -> AlohaSimState -> AlohaSimState
addFrameBeginTime t st = st { frameBeginTimes = [t] ++ (frameBeginTimes st) }

filterFrameBeginTimes :: Time -> Time -> AlohaSimState -> AlohaSimState
filterFrameBeginTimes t tf st = st { frameBeginTimes = fbt } where
  f = \t1 -> t1 > t-2*tf
  fbt = filter f $ frameBeginTimes st

isCollisionAtFrameEndTime :: [Time] -> Time -> Time -> Bool
isCollisionAtFrameEndTime fbts tf fet = any collision fbts where
  collision = \fbt -> (fbt >= fet-2*tf) && (fbt <= fet) && (fbt /= fet-tf)
-- We assume two packets will never arrive at the same time in specifying the last condition

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
        frameBeginTimes         = [],
        eventQueue              = SL.singleton $ TE 0.0 scheduleNextFrameArrival,
        randomNumberGenerator   = gen
      }
      config = AlohaSimConfig {
        simDuration             = 1000.0,
        frameDuration           = 1.0,
        meanAttemptsPerFrame    = 0.5
      }

  (s, results) <- execRWST alohaSim config initialState
  putStrLn $ show results
  let g = meanAttemptsPerFrame config
  putStrLn $"\nTheoretical throughput for load G = " ++ (show g) ++ " is Ge^{-2G} = " ++ (show $ g*(exp (-2*g)))
  let maxFramesPossible = (/) <$> simDuration <*> frameDuration $ config
  putStrLn $ "Simulated throughput = " ++ (show $ (fromIntegral $ numFramesReceived results)/maxFramesPossible)
