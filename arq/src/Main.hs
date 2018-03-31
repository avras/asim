module Main where

import Control.Monad.RWS
import qualified Data.SortedList as SL

type Time = Double
type Probability = Double

data ARQSimConfig = ARQSimConfig {
  simDuration             :: Time,
  packetErrorProbability  :: Probability,
  ackErrorProbability     :: Probability,
  timeoutDuration         :: Time,
  forwardPropDelay        :: Time,
  reversePropDelay        :: Time
}

data ARQSimLog = ARQSimLog {
  numPacketsTransmitted   :: Int,
  numACKTimeouts          :: Int,
  numACKsTransmitted      :: Int
} deriving (Show)

instance Monoid ARQSimLog where
  mempty = logEmpty
  log1 `mappend` log2 = ARQSimLog {
      numPacketsTransmitted = numPacketsTransmitted log1 + numPacketsTransmitted  log2,
      numACKTimeouts        = numACKTimeouts        log1 + numACKTimeouts         log2,
      numACKsTransmitted    = numACKsTransmitted    log1 + numACKsTransmitted     log2
    }

logEmpty = ARQSimLog {
  numPacketsTransmitted = 0,
  numACKTimeouts        = 0,
  numACKsTransmitted    = 0
}

onePacketTransmitted  = logEmpty { numPacketsTransmitted  = 1 }
onePacketTimeout      = logEmpty { numACKTimeouts         = 1 }
oneACKTransmitted     = logEmpty { numACKsTransmitted     = 1 }

data SeqNum = Zero | One
  deriving (Eq, Show)

snflip :: SeqNum -> SeqNum
snflip Zero = One
snflip One = Zero


data ARQTransmitterState = ARQTransmitterState {
  expectedACKSeqNum       :: SeqNum,
  ackTimeoutEvent         :: Maybe TimedEvent
}

data ARQReceiverState = ARQReceiverState {
  expectedPacketSeqNum    :: SeqNum
}

data TimedEvent = TE Time Event

instance Eq TimedEvent where
  (TE t1 _) == (TE t2 _) = t1 == t2

instance Ord TimedEvent where
  (TE t1 _) `compare` (TE t2 _) = t1 `compare` t2

type EventQueue = SL.SortedList TimedEvent

data ARQSimState = ARQSimState {
  currentSimulationTime   :: Time,
  currentPacketSeqNum     :: SeqNum,
  transmitterState        :: ARQTransmitterState,
  currentACKSeqNum        :: SeqNum,
  receiverState           :: ARQReceiverState,
  eventQueue              :: EventQueue
}

initialTransmitterState = ARQTransmitterState {
  expectedACKSeqNum       = One,
  ackTimeoutEvent         = Nothing
}

initialReceiverState = ARQReceiverState {
  expectedPacketSeqNum    = Zero
}

initialState = ARQSimState {
  currentSimulationTime   = 0.0,
  currentPacketSeqNum     = Zero,
  transmitterState        = initialTransmitterState,
  currentACKSeqNum        = Zero,
  receiverState           = initialReceiverState,
  eventQueue              = SL.singleton $ TE 0.0 transmitPacket
}

type Event = RWST ARQSimConfig ARQSimLog ARQSimState IO ()

setTransmitterState :: ARQTransmitterState -> ARQSimState -> ARQSimState
setTransmitterState txst st = st { transmitterState = txst }

setReceiverState :: ARQReceiverState -> ARQSimState -> ARQSimState
setReceiverState rxst st = st { receiverState = rxst }

transmitPacket :: Event
transmitPacket = do
  tell onePacketTransmitted
  t <- gets currentSimulationTime
  liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Packet transmitted"
  fd <- asks forwardPropDelay
  modify . scheduleEvent $ TE (t+fd) receivePacket
  td <- asks timeoutDuration
  let timeoutEv = TE (t+td) ackTimeout
  modify $ scheduleEvent timeoutEv
  txst <- gets transmitterState
  modify . setTransmitterState $ txst { ackTimeoutEvent = Just timeoutEv }


receivePacket :: Event
receivePacket = do
  t <- gets currentSimulationTime
  liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Packet received"
  psn <- gets currentPacketSeqNum
  rxst <- gets receiverState
  let epsn = expectedPacketSeqNum rxst
  case psn == epsn of
    True -> do
      liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Sequence number in received packet = " 
        ++ (show psn) ++ ". Expected packet sequence number = " ++ (show epsn) ++ "."
      modify flipACKSeqNum
      modify . setReceiverState $ rxst { expectedPacketSeqNum = snflip epsn }
      transmitACK
    False ->
      liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: ERROR: Sequence number in received packet = " 
        ++ (show psn) ++ ". Expected packet sequence number = " ++ (show epsn) ++ "."

ackTimeout :: Event
ackTimeout = do
  tell onePacketTimeout
  t <- gets currentSimulationTime
  liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: ACK timeout occurred"
  transmitPacket

transmitACK :: Event
transmitACK = do
  tell oneACKTransmitted
  t <- gets currentSimulationTime
  liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: ACK transmitted"
  rd <- asks reversePropDelay
  modify . scheduleEvent $ TE (t+rd) receiveACK

receiveACK :: Event
receiveACK = do
  t <- gets currentSimulationTime
  liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: ACK received"
  asn <- gets currentACKSeqNum
  txst <- gets transmitterState 
  let easn = expectedACKSeqNum txst
  case easn == asn of
    True -> do
      liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Sequence number in received ACK = "
        ++ (show asn) ++ ". Expected ACK sequence number = " ++ (show easn) ++ "."
      modify flipPacketSeqNum
      modify . setTransmitterState $ txst { expectedACKSeqNum = snflip easn }
      case ackTimeoutEvent txst of
        Nothing -> transmitPacket
        Just timeoutEv -> do
          modify $ cancelEvent timeoutEv
          transmitPacket
    False -> do
      liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: ERROR: Sequence number in received ACK = "
        ++ (show asn) ++ ". Expected ACK sequence number = " ++ (show easn) ++ "."

scheduleEvent :: TimedEvent -> ARQSimState -> ARQSimState
scheduleEvent te st = st { eventQueue = SL.insert te $ eventQueue st }

cancelEvent :: TimedEvent -> ARQSimState -> ARQSimState
cancelEvent te st = st { eventQueue = SL.delete te $ eventQueue st }

flipPacketSeqNum :: ARQSimState -> ARQSimState
flipPacketSeqNum st = st { currentPacketSeqNum = snflip $ currentPacketSeqNum st }

flipACKSeqNum :: ARQSimState -> ARQSimState
flipACKSeqNum st = st { currentACKSeqNum = snflip $ currentACKSeqNum st }

arqSim :: Event
arqSim = do
  q <- gets eventQueue      -- Get the current eventQueue
  case SL.uncons q of         -- Check if the eventQueue is empty
    Nothing -> return ()
    Just (TE t event, q') -> do
      endt <- asks simDuration  -- Get the simulation duration
      case (t > endt) of        -- Check if earliest event time exceeds simulation duration
        True -> return ()
        False -> do
          modify (\st -> st { currentSimulationTime = t, eventQueue = q' })
          event     -- Execute earliest event
          arqSim    -- Recursive call to continue processing events


main :: IO ()
main = do
  let config = ARQSimConfig {
    simDuration             = 5.0,
    packetErrorProbability  = 0.5,
    ackErrorProbability     = 0.0,
    timeoutDuration         = 3.0,
    forwardPropDelay        = 1.0,
    reversePropDelay        = 1.0
  }

  (s, log) <- execRWST arqSim config initialState
  putStrLn $ show log
