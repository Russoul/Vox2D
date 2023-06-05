module Vox2D.Data.StrBuffer

import Data.Buffer

import System

import System.File.ReadWrite
import System.File.Error
import System.File.Buffer

public export
record StrBuffer where
  constructor MkStrBuffer
  get : Buffer
  ||| Actual size (= byte offset to the next free byte)
  offset : Int

export
allocStrBuffer : Int -> IO StrBuffer
allocStrBuffer initialSize =
  do
     (Just buf) <- newBuffer initialSize
        | _ => do
           putStrLn "Could not allocate buffer"
           exitFailure
     pure (MkStrBuffer buf 0)

export
appendStr :
      StrBuffer
   -> String
   -> IO StrBuffer
appendStr strbuf str =
  do
     let strlen = stringByteLength str
     raw <- ensureSize strbuf.get strbuf.offset strlen
     setString raw strbuf.offset str
     pure (MkStrBuffer raw (strbuf.offset + strlen))

   where
     ensureSize : Buffer -> Int -> Int -> IO Buffer
     ensureSize buf offset strlen =
        let bufLen = !(rawSize buf) in
            if offset + strlen > bufLen
               then do
                  (Just buf) <- resizeBuffer buf (max (2 * bufLen) (offset + strlen))
                    | _ => do
                       putStrLn "Could not allocate buffer"
                       exitFailure
                  pure buf
            else
               pure buf

export
writeToFile : StrBuffer -> (filename : String) -> IO ()
writeToFile buf filename = do
  Right () <- writeBufferToFile filename buf.get buf.offset
    | Left err => do
       putStrLn (show err)
       exitFailure
  pure ()
