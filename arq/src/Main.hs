module Main where

import Control.Monad.RWS
import Data.PQueue.Prio.Min

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
  numACKsTransmitted      :: Int,
  numDupPacketsReceived   :: Int
} deriving (Show)

instance Monoid ARQSimLog where
  mempty = logEmpty
  log1 `mappend` log2 = ARQSimLog {
      numPacketsTransmitted = numPacketsTransmitted log1 + numPacketsTransmitted log2,
      numACKTimeouts = numACKTimeouts log1 + numACKTimeouts log2,
      numACKsTransmitted = numACKsTransmitted log1 + numACKsTransmitted log2,
      numDupPacketsReceived = numDupPacketsReceived log1 + numDupPacketsReceived log2
    }

logEmpty = ARQSimLog {
  numPacketsTransmitted = 0,
  numACKTimeouts = 0,
  numACKsTransmitted = 0,
  numDupPacketsReceived = 0
}

onePacketTransmitted = logEmpty { numPacketsTransmitted = 1 }
onePacketTimeout = logEmpty { numACKTimeouts = 1 }
oneACKTransmitted = logEmpty { numACKsTransmitted = 1 }
oneDupPacketReceived = logEmpty { numDupPacketsReceived = 1 }

data SeqNum = Zero | One

data ARQTransmitterState = ARQTransmitterState {
  currentPacketSeqNum     :: SeqNum,
  packetTimeoutEvent      :: Event
}

data ARQReceiverState = ARQReceiverState {
  currentAckSeqNum        :: SeqNum,
  ackTimeoutEvent         :: Event
}

data ARQSimState = ARQSimState {
  currentSimulationTime   :: Time,
  transmitterState        :: ARQTransmitterState,
  receiverState           :: ARQReceiverState,
  eventQueue              :: MinPQueue Time Event
}

initialTransmitterState = ARQTransmitterState {
  currentPacketSeqNum = Zero,
  packetTimeoutEvent = return ()
}

initialReceiverState = ARQReceiverState {
  currentAckSeqNum = One,
  ackTimeoutEvent = return ()
}

initialState = ARQSimState {
  currentSimulationTime = 0.0,
  transmitterState = initialTransmitterState,
  receiverState = initialReceiverState,
  eventQueue = empty
}

type Event = RWST ARQSimConfig ARQSimLog ARQSimState IO ()

transmitPacket :: Event
transmitPacket = do
  tell onePacketTransmitted
  t <- gets currentSimulationTime
  liftIO . putStrLn $ "t = " ++ (show t) ++ " seconds: Packet transmitted"

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

arqSim :: Event
arqSim = do
  transmitPacket
  transmitPacket
  ackTimeout
  transmitACK

main :: IO ()
main = do
  let config = ARQSimConfig {
    simDuration = 5.0,
    packetErrorProbability = 0.5,
    ackErrorProbability = 0.0,
    timeoutDuration = 3.0,
    forwardPropDelay = 1.0,
    reversePropDelay = 1.0
  }

  (s, log) <- execRWST arqSim config initialState
  putStrLn $ show log
