{-# LANGUAGE GADTs #-} 
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE KindSignatures #-} 
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE ScopedTypeVariables #-} 

{-|
Module:      Control.Remote.Monad.Transport
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Justin Dawson
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Transport  where


import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import           Control.Remote.Monad.Binary (SendAPI(..))
import           Control.Natural

import           Data.Binary
import           Data.Binary.Get (runGet)
import           Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

import           Network.Transport

transportClient :: Transport -> EndPointAddress -> IO (SendAPI :~> IO)
transportClient t remote = do
  ep0 <- newEndPoint t
  case ep0 of
    Left err -> error $ show err
    Right ep -> transportEndPointClient ep remote

transportEndPointClient :: EndPoint -> EndPointAddress -> IO (SendAPI :~> IO)
transportEndPointClient local remote = do
  conn0 <- connect local remote ReliableOrdered defaultConnectHints
  let newUniq :: IO Int
      newUniq = return 4
-- list of pending replies
  replyTable :: TVar [Reply] <- newTVarIO []
  let loop = do
        event <- receive local
        case event of

          Received _ bss -> do
              case decode $ LBS.fromChunks bss of
                  reply -> atomically $ do
                          replies <- readTVar replyTable
                          writeTVar replyTable (reply : replies)
              loop

          EndPointClosed -> return ()
          _ -> loop
  forkIO $ loop
  case conn0 of
    Left err -> error $ show err
    Right conn -> return $ Nat $ \ (Sync bs) -> do
                    tag <- newUniq
                    rep0 <- send conn [ LBS.toStrict $ encode $ Outgoing tag (address local) bs ]
                    case rep0 of
                      Left err -> error $ show err
                      Right rep -> do
                        -- now we need to wait for the reply. Because we are a monad.
                        atomically $ do
                                replies <- readTVar replyTable
                                case [ bs | Reply n bs <- replies, n == tag ] of
                                  [bs] -> do writeTVar replyTable [ Reply n bs | Reply n bs <- replies, n /= tag ]
                                             return bs
                                  _    -> retry

transportServer :: Transport -> (SendAPI :~> IO) -> IO ()
transportServer = undefined

data Outgoing where
    Outgoing :: Int -> EndPointAddress -> LBS.ByteString -> Outgoing

instance Binary Outgoing where
  put (Outgoing tag epa bs) = put (tag,epa,bs)  
  get = (\ (tag,epa,bs) -> Outgoing tag epa bs) <$> get

data Reply where
    Reply :: Int -> LBS.ByteString -> Reply

instance Binary Reply where
  put (Reply tag bs) = put (tag,bs)  
  get = (\ (tag,bs) -> Reply tag bs) <$> get
  