{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
module Types where
 
import           Control.Remote.Haxl
import           Control.Remote.Haxl.Binary
import           Data.Binary
import           Debug.Trace

data Query :: * -> * where
  Pop  :: Query Int          -- POP
  Push :: Int -> Query ()   -- PUSH

instance BinaryQ Query where
   getQ = do i <- get
             case i :: Word8 of
               0 -> return $ Fmap put Pop
               1 -> do
                     n <- get
                     return $ Fmap put (Push n)
               _ -> error "Unable to parse Query"
   putQ (Pop)= put (0 :: Word8) 
   putQ (Push n) = do
         put (1 :: Word8)
         put n
         
   interpQ (Pop) = get
   interpQ (Push _n) =pure ()

push :: Int -> RemoteHaxlMonad Query ()
push n = query $ Push n

pop :: RemoteHaxlMonad Query Int
pop = query $ Pop
