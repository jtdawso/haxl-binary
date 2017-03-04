{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}


{-|                                             
 - Module:      Control.Remote.Haxl.Binary.Types 
 - Copyright:   (C) 2017, The University of Kansas
 - License:     BSD-style (see the file LICENSE)
 - Maintainer:  Justin Dawson                      
 - Stability:   Alpha                              
 - Portability: GHC                                
 - -}


 module Control.Remote.Haxl.Binary.Types 
 (
   encodePacket
 , decodePacketResult
 , BinaryQ(..)
 , Fmap(..)
 , SendAPI(..)
 , HaxlBinaryException(..)
 )
 where

import qualified Control.Remote.Packet.Weak as WP
import qualified Control.Remote.Packet.Applicative as AP
import Data.ByteString.Lazy
import Data.Binary
import Data.Binary.Put (runPut)
import Data.Binary.Get (runGet)
import Control.Exception
import Data.Typeable
import Debug.Trace
-------------

data SendAPI :: * -> * where
    Sync  :: ByteString -> SendAPI ByteString

data Fmap f a where
   Fmap :: (a -> b) -> f a -> Fmap f b

instance Functor (Fmap f) where
  fmap f (Fmap g h) = Fmap (f . g) h 


class BinaryQ q  where
  putQ    :: q a -> Put            -- ^ encode a query/question as a packet
  getQ    :: Get (Fmap q Put)  -- ^ decode to the packet, which contains a way of encoding the answer
  interpQ :: q a -> Get a          -- ^ interprete the answer, in the context of the original query (type).

interpQ' :: (BinaryQ q, Exception e, Binary e) => q a -> Get (Either e a)
interpQ' pkt = trace "interpQ'" $
               do i <- get
                  case i :: Word8 of
                      0 -> do
                              res <- interpQ pkt
                              return $ Right res
                      _ -> do e <- get 
                              return $ Left e

-------------
-- ############## Weak Packet BinaryQ Instance #######################

instance (BinaryQ q) => BinaryQ (WP.WeakPacket q) where
  putQ = putWeakPacket

  getQ = do
       i <- get
       case i :: Word8 of
          1 -> do 
                (Fmap f q) <- getQ
                return $ Fmap f (WP.Query  q)
          _ -> error "ERROR: function getQ unable to parse WeakPacket"

  interpQ (WP.Query q)  =  interpQ q 

putWeakPacket :: (BinaryQ q) => WP.WeakPacket q a -> Put
putWeakPacket (WP.Query q)= do
   put (1 :: Word8)
   putQ q


---- ############ Applicative Packet BinaryQ Instance #####################
instance (BinaryQ q) => BinaryQ (AP.ApplicativePacket q) where
  putQ = putApplicativePacket 

  getQ = do
      i <- get
      case i :: Word8 of
         1 ->do 
                Fmap f q <- getQ
                return $Fmap f $ AP.Query $ q

         2 -> do Fmap f1 q1 <- getQ
                 Fmap f2 q2 <- getQ
                 return $ Fmap (\(a,b)-> f1 a >> f2 b )$ AP.Zip (\ a b -> (a,b)) q1 q2
         3 -> return $ Fmap (\()-> return ()) $ AP.Pure ()

         _ -> error "ERROR: getQ unable to parse ApplicativePacket"

  interpQ (AP.Query q) = interpQ q
  interpQ (AP.Zip f x y)   = f <$> interpQ x <*> interpQ y
  interpQ (AP.Pure a)      = return a


putApplicativePacket :: (BinaryQ q) => AP.ApplicativePacket q a -> Put
putApplicativePacket (AP.Query q) = do
    put (1 :: Word8)
    putQ q
putApplicativePacket (AP.Zip  _ a b) = do
    put (2 :: Word8)
    putApplicativePacket a
    putApplicativePacket b
putApplicativePacket (AP.Pure _) = do
    put (3 :: Word8)


-------------------------------------------------
encodePacket :: (BinaryQ f) =>  f a -> ByteString 
encodePacket pkt = runPut (putQ pkt)

decodePacketResult :: (BinaryQ f) => f a -> ByteString -> Either HaxlBinaryException a
decodePacketResult pkt = runGet (interpQ' pkt)

data HaxlBinaryException = HaxlBinaryException String
   deriving (Show, Typeable)

instance Exception HaxlBinaryException


instance Binary HaxlBinaryException where
    put (HaxlBinaryException s) = do put (220:: Word8)
                                     put s

    get = do i <-get
             case i :: Word8 of
               220 -> do s <- get
                         return $ HaxlBinaryException s
               _   -> error "unable to parse HaxlBinaryException"

