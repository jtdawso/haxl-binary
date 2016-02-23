{-# LANGUAGE GADTs #-} 
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE KindSignatures #-} 
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE FlexibleInstances #-} 

{-|
Module:      Control.Remote.Monad.Binary
Copyright:   (C) 2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Justin Dawson
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Binary  where
import           Control.Monad (void)
import           Control.Natural
import qualified Control.Remote.Monad.Packet.Weak as WP
import qualified Control.Remote.Monad.Packet.Strong as SP
import qualified Control.Remote.Monad.Packet.Applicative as AP
import           Control.Remote.Monad.Binary.Types
import           Data.Binary
import qualified Data.ByteString.Lazy as BS


-- ##### Weak Packet  ######
sendWeakBinary :: (Binary a, Binary c, BinaryX p) => (SendAPI ~> IO) -> WP.WeakPacket c p a -> IO a
sendWeakBinary f pkt = do 
                        r <- f (Sync (encodeWeakPacket  pkt))
                        return $ decodeWeakPacketResult pkt r 
                                             


receiveWeakSendAPI :: (Binary c, BinaryX p) => BinaryNatTrans (WP.WeakPacket c p)  IO -> (SendAPI ~> IO)
receiveWeakSendAPI (BinaryNatTrans f) (Sync c) = do 
                     case decode c of 
                       (T v) -> do  
                                  r <- f v
                                  return $ encodeWeakPacketResult v r

-- ##### Strong Packet ######

sendStrongBinary :: (Binary a, Binary c, BinaryX p) => (SendAPI ~> IO) -> SP.StrongPacket c p a -> IO a
sendStrongBinary f pkt = do
                        r <- f (Sync (encodeStrongPacket pkt))
                        return $ decodeStrongPacketResult pkt r 

receiveStrongSendAPI :: (Binary c, BinaryX p) => BinaryNatTrans (SP.StrongPacket c p)  IO -> (SendAPI ~> IO)
receiveStrongSendAPI (BinaryNatTrans f) (Sync c) = do
                                        case decode c of
                                          (T v ) -> do
                                                    r <- f v
                                                    return $ encodeStrongPacketResult v r

-- ##### Applicative Packet ######
{-
sendApplicativeBinary :: (Binary a, Binary c, BinaryX p) => (SendAPI ~> IO) -> AP.ApplicativePacket c p a -> IO a
sendApplicativeBinary f pkt = undefined 

receiveApplicativeSendAPI :: (Binary c, BinaryX p) => BinaryNatTrans (AP.ApplicativePacket c p)  IO -> (SendAPI ~> IO)
receiveApplicativeSendAPI (BinaryNatTrans f) (Sync c) = undefined 
-}
