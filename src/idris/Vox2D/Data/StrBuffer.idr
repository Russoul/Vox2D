module Vox2D.Data.StrBuffer

import Data.Buffer

import System

import System.File.ReadWrite
import System.File.Error
import System.File.Buffer

||| A native dynamic byte-buffer (Buffer) together with an actual size in bytes (offset)
public export
record StrBuffer where
  constructor MkStrBuffer
  get : Buffer
  ||| Actual size (= byte offset to the next allocated unused byte)
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

export
readFromFile : (filename : String) -> IO StrBuffer
readFromFile filename = do
  Right b <- createBufferFromFile filename
    | Left err => die (show err)
  pure (MkStrBuffer b !(rawSize b))

public export
readString : StrBuffer -> IO String
readString buf = getString buf.get 0 buf.offset
