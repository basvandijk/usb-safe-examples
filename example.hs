{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- From initializing the usb library to performing I/O with an endpoint
-- in 10 easy steps!
--
--------------------------------------------------------------------------------


module Main where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude       ( fromInteger )
import System.Exit   ( exitFailure )
import System.IO     ( IO, putStrLn, hPutStrLn, stderr )
import Data.List     ( map, find, head, unlines )
import Data.Function ( ($) )
import Data.Bool     ( Bool )
import Data.Maybe    ( Maybe(Nothing, Just) )
import Control.Monad ( (>>=), fail, (>>), when )
import Text.Printf   ( printf )
import Text.Show     ( show )

-- from bytestring:
import qualified Data.ByteString as B ( ByteString, length, unpack )

-- from transformers:
import Control.Monad.IO.Class ( liftIO )

-- from base-unicode-symbols:
import Prelude.Unicode       ( (⋅) )
import Data.Function.Unicode ( (∘) )
import Data.Eq.Unicode       ( (≡) )
import Data.Bool.Unicode     ( (∧) )

-- from usb:
import System.USB.Initialization ( newCtx )
import System.USB.Enumeration    ( Device, getDevices )
import System.USB.Descriptors    ( deviceVendorId, deviceProductId
                                 , endpointMaxPacketSize, maxPacketSize
                                 )
-- from usb-safe:
import System.USB.Safe ( getDesc
                       , withDevice
                       , withDetachedKernelDriver
                       , useActiveConfig
                       , getInterfaces
                       , withInterface
                       , getAlternates
                       , setAlternate
                       , getEndpoints
                       , TransferDirection ( In )
                       , TransferType      ( Interrupt )
                       , readEndpoint
                       )


--------------------------------------------------------------------------------
-- Program
--------------------------------------------------------------------------------

main ∷ IO ()
main = do
  -- 1) First we need to initialize the usb library:
  ctx ← newCtx

  -- 2) Next we would like to know which USB devices are currently attached to
  --    our system:
  devs ← getDevices ctx

  -- 3) Now lets find one of the devices to communicate with. I currently have a
  --    'Microsoft Wheel Mouse Optical' attached, so lets try finding that one:
  case find isMSWheelMouseOptical devs of
    Nothing → do hPutStrLn stderr "Can't find Microsoft Wheel Mouse Optical!"
                 exitFailure
    Just dev →
      -- 4) If we found the device we were looking for we need to open it to be
      --    able to perform I/O. Opening of a resource like a device can only be
      --    done in a region. When the region terminates all opened resources
      --    are automatically closed. So now we need to create a region and open
      --    a device in it. Both of these steps can be performed at once with:
      withDevice dev $ \devHndl →

        -- 5) My OS has currently loaded a driver for the opened mouse. This
        --    prevents us from doing any I/O with it, so we first need to detach
        --    the kernel driver. After we're done I would like to use my mouse
        --    again so we need to attach the driver again. This is all nicely
        --    abstracted by:
        withDetachedKernelDriver devHndl 0 $

          -- 6) USB devices have one ore more configurations that specify
          --    certain settings of the device. Only one configuration can be
          --    active at any given time. Before you can do any I/O with a
          --    device you first need to set a specific configuration. It may be
          --    the case that a configuration has already been set in which case
          --    you don't need to set it again. If you want to use the currently
          --    active configuration use:
          useActiveConfig devHndl $ \confHndl →

            -- 7) A configuration specifies one or more interfaces. Before you
            --    can do any I/O you have to claim an interface to instruct the
            --    OS that, for now, nobody else will use that interface. Then,
            --    when you're done, you need to release it so that others can
            --    use that interface. Our mouse has only one interface so lets
            --    claim that one. Just as with opening devices we can use the
            --    following to accomplish this:
            withInterface (head $ getInterfaces confHndl) $ \ifHndl →

              -- 8) Just like a device has one ore more configurations, an
              --    interface has one ore more alternates. Before you can do any
              --    I/O you first have to set an alternate. Our mouse has only
              --    one alternate so lets set that one:
              setAlternate (head $ getAlternates ifHndl) $ \altHndl → do

                -- 9) I/O in USB is done from and to so called endpoints. An
                --    alternate can have one ore more of these endpoints. An
                --    endpoint has two important properties: its transfer
                --    direction which can be either In or Out and its transfer
                --    type which can be Control, Isochronous, Bulk or
                --    Interrupt. These properties define what kind of I/O
                --    operations are supported for that endpoint. Our mouse has
                --    only one Interrupt endpoint with an In transfer
                --    direction. You can get a handle to it using the following:
                let [interruptInEndp] = getEndpoints altHndl In Interrupt

                -- We specify the number of bytes to read to be a multiple of
                -- the maximum packet size of the respected endpoint so that an
                -- 'OverflowException' won't be thrown.
                let nrOfBytesToRead = 10 ⋅ ( maxPacketSize
                                           $ endpointMaxPacketSize
                                           $ getDesc interruptInEndp
                                           )

                -- The timeout in milliseconds specifies the maximum allowed time
                -- for the read operation. (Use 0 for no timeout)
                let timeout = 3000

                _ ← liftIO $ printf "Reading %i bytes during a maximum of %i ms...\n"
                                    nrOfBytesToRead timeout

                -- 10) Now we are finally able to perform I/O and in this case
                --     we are only able to read because we have an In endpoint:
                (bs, timedOut) ← readEndpoint interruptInEndp
                                              timeout
                                              nrOfBytesToRead

                -- The computation returns the bytes that were read in a
                -- ByteString and an indication if the operation timed out:
                when timedOut $ liftIO $ putStrLn "Reading timed out!"
                liftIO $ do _ ← printf "Read %i bytes:\n" $ B.length bs
                            printBytes bs

-- | Determines if the given device is a 'Microsoft Wheel Mouse Optical'
isMSWheelMouseOptical ∷ Device → Bool
isMSWheelMouseOptical dev = deviceVendorId  devDesc ≡ microsoftVID
                          ∧ deviceProductId devDesc ≡ msWheelMouseOpticalPID
    where
      microsoftVID = 0x045e
      msWheelMouseOpticalPID = 0x0040
      devDesc = getDesc dev

-- | Print the numerical value of each byte on a new line.
printBytes ∷ B.ByteString → IO ()
printBytes = putStrLn ∘ unlines ∘ map show ∘ B.unpack


-- The End ---------------------------------------------------------------------
