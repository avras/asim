module Main where

import Control.Monad.RWS

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
  numPacketTimeouts       :: Int,
  numACKsTransmitted      :: Int,
  numDupPacketsReceived   :: Int
}

logEmpty = ARQSimLog {
  numPacketsTransmitted = 0,
  numPacketTimeouts = 0,
  numACKsTransmitted = 0,
  numDupPacketsReceived = 0
}

instance Monoid ARQSimLog where
  mempty = logEmpty
  log1 `mappend` log2 = ARQSimLog {
      numPacketsTransmitted = numPacketsTransmitted log1 + numPacketsTransmitted log2,
      numPacketTimeouts = numPacketTimeouts log1 + numPacketTimeouts log2,
      numACKsTransmitted = numACKsTransmitted log1 + numACKsTransmitted log2,
      numDupPacketsReceived = numDupPacketsReceived log1 + numDupPacketsReceived log2
    }

data SeqNum = Zero | One
newtype Event a = Event (ARQSimState -> (a, ARQSimState))

data ARQTransmitterState = ARQTransmitterState {
  currentPacketSeqNum     :: SeqNum,
  packetTimeoutEvent      :: Event () 
}

data ARQReceiverState = ARQReceiverState {
  currentAckSeqNum        :: SeqNum,
  ackTimeoutEvent         :: Event () 
}

data ARQSimState = ARQSimState {
  currentSimulationTime   :: Time,
  transmitterState        :: ARQTransmitterState,
  receiverState           :: ARQReceiverState
}

defaultTransmitterState = ARQTransmitterState {
  currentPacketSeqNum = Zero,
  packetTimeoutEvent = Event (\s -> ((), s))
}

defaultReceiverState = ARQReceiverState {
  currentAckSeqNum = One,
  ackTimeoutEvent = Event (\s -> ((), s))
}

defaultState = ARQSimState {
  currentSimulationTime = 0.0,
  transmitterState = defaultTransmitterState,
  receiverState = defaultReceiverState
}

type ARQSim a = RWST ARQSimConfig ARQSimLog ARQSimState IO a

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
               
  putStrLn "hello world"
