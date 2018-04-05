module Main where

import Control.Monad.RWS
import qualified Data.SortedList as SL
import qualified System.Random.MWC as MWC
import Data.Random
import Data.Random.Distribution.Poisson

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
  numFramesInCurrentSlot  :: Int,
  eventQueue              :: EventQueue,
  randomNumberGenerator   :: MWC.GenIO
}

beginSlotEvent :: Event
beginSlotEvent = do
  t <- gets currentSimulationTime
  tf <- asks frameDuration
  liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Slot begins"
  gen <- gets randomNumberGenerator
  load <- asks meanAttemptsPerFrame
  numFrames <- liftIO $ sampleFrom gen (poisson load)
  tell logEmpty { numFramesTransmitted = numFrames }
  modify $ setNumFramesInCurrentSlot numFrames
  modify . scheduleEvent $ TE (t+tf) endSlotEvent

endSlotEvent :: Event
endSlotEvent = do
  t <- gets currentSimulationTime
  numFrames <- gets numFramesInCurrentSlot
  case numFrames of
    0 -> liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Slot ends with no frame transmissions"
    1 -> do
      tell oneFrameReceived
      liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Slot ends with successful frame transmission"
    _ -> do
      tell oneCollision
      liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Slot ends with frame collisions"
  beginSlotEvent

scheduleEvent :: TimedEvent -> AlohaSimState -> AlohaSimState
scheduleEvent te st = st { eventQueue = SL.insert te $ eventQueue st }

setNumFramesInCurrentSlot :: Int -> AlohaSimState -> AlohaSimState
setNumFramesInCurrentSlot n st = st { numFramesInCurrentSlot = n }

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
        numFramesInCurrentSlot  = 0,
        eventQueue              = SL.singleton $ TE 0.0 beginSlotEvent,
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
  putStrLn $"\nTheoretical throughput for load G = " ++ (show g) ++ " is Ge^{-G} = " ++ (show $ g*(exp (-g)))
  let maxFramesPossible = (/) <$> simDuration <*> frameDuration $ config
  putStrLn $ "Simulated throughput = " ++ (show $ (fromIntegral $ numFramesReceived results)/maxFramesPossible)
