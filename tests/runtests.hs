module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans

import System.IO
import System.Posix.IO

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Monad.BinaryProtocol

makePipe :: IO (Handle,Handle)
makePipe = do
    (read_fd, write_fd) <- createPipe
    read_handle <- fdToHandle read_fd
    write_handle <- fdToHandle write_fd
    return (read_handle, write_handle)

makeSendTest value = do
    (read_handle,write_handle) <- makePipe
    result <- runProtocol ( do
        send value
        flush
        receive
      ) read_handle write_handle
    assertEqual "Was the correct value received?" value result

test_send_unit = makeSendTest ()
test_send_number = makeSendTest (3 :: Int)
test_send_list_of_numbers = makeSendTest [(3 :: Int),4,5,6]

makeExchangeTest correct_result protocol1 protocol2 = do
    result_mvar <- newEmptyMVar
    (read_handle_1,write_handle_2) <- makePipe
    (read_handle_2,write_handle_1) <- makePipe
    forkIO $ runProtocol (protocol1 result_mvar) read_handle_1 write_handle_1
    forkIO $ runProtocol (protocol2 result_mvar) read_handle_2 write_handle_2
    result <- readMVar result_mvar
    assertEqual "Was the correct result computed?" correct_result result

test_addition = makeExchangeTest (3 :: Int)
  (\result_mvar -> do
    send (1 :: Int)
    flush
    receive >>= liftIO . putMVar result_mvar
  )
  (\_ -> do
    a <- receive
    send (a + (2 :: Int))
  )

tests =
    [    testGroup "unidirectional communications"
         [    testCase "send unit" test_send_unit
         ,    testCase "send number" test_send_number
         ,    testCase "send list of numbers" test_send_list_of_numbers
         ]
    ,    testGroup "bidirectional communications"
         [    testCase "addition" test_addition
         ]
    ]

main = defaultMain tests
