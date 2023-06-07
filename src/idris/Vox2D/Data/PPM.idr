module Vox2D.Data.PPM

import Data.List
import Data.Buffer

import Vox2D.Text.Parser.Fork
import Vox2D.Text.Parser.CharUtil

import System.File.ReadWrite
import System.File.Error
import System

import Vox2D.Data.StrBuffer

||| A representation of the contents of a plain PPM image file
public export
record PPM where
  constructor MkPPM
  width : Bits32
  height : Bits32
  scale : Bits16
  -- TODO: Switch this to Bits16 buffer
  img : List Bits16

public export
at : PPM -> Nat -> Nat -> Maybe (Bits16, Bits16, Bits16)
at (MkPPM width height scale img) u v = H img u v
 where
   H : List Bits16 -> Nat -> Nat -> Maybe (Bits16, Bits16, Bits16)
   H (r :: g :: b :: _) 0 0 = Just (r, g, b)
   H _ 0 0 = Nothing
   H rest 0 (S v) = H rest (cast width) v
   H (r :: g :: b :: rest) (S u) v = H rest u v
   H _ (S u) v = Nothing

public export
white : Bits32 -> Bits32 -> PPM
white w h = MkPPM w h 255 (replicate (cast $ w * h * 3) 255)

export
toStrBuffer : PPM -> IO StrBuffer
toStrBuffer (MkPPM w h s img) = do
  buf <- allocStrBuffer 1024
  buf <- appendStr buf "P3\n"
  buf <- appendStr buf "\{show w} \{show h}\n"
  buf <- appendStr buf "\{show s}\n"
  writeImg img buf
 where
  handle : IO (Either FileError a) -> IO a
  handle io = do
    case !io of
      Left err => do
       putStrLn (show err)
       exitFailure
      Right ok => pure ok

  writePixel : List Bits16 -> StrBuffer -> IO (List Bits16, StrBuffer)
  writePixel (r :: g :: b :: rest) buf = do
    buf <- appendStr buf " \{show r} \{show g} \{show b}"
    pure (rest, buf)
  writePixel _ _ = do
    putStrLn "Invalid image"
    exitFailure

  writeLineH : (todoPixels : Nat) -> List Bits16 -> StrBuffer -> IO (List Bits16, StrBuffer)
  writeLineH 0 rest buf = do
    buf <- appendStr buf "\n"
    pure (rest, buf)
  writeLineH (S k) rest buf = do
    (rest, buf) <- writePixel rest buf
    writeLineH k rest buf

  writeLine : List Bits16 -> StrBuffer -> IO (List Bits16, StrBuffer)
  writeLine rest buf = do
    writeLineH (cast w) rest buf

  writeImgH : (todoLines : Nat) -> List Bits16 -> StrBuffer -> IO StrBuffer
  writeImgH 0 [] buf = pure buf
  writeImgH 0 (_ :: _) buf = do
    die "Invalid image"
  writeImgH (S k) rest buf = do
    (rest, buf) <- writeLine rest buf
    writeImgH k rest buf

  writeImg : List Bits16 -> StrBuffer -> IO StrBuffer
  writeImg img buf = writeImgH (cast h) img buf

export
parsePPM : Rule PPM
parsePPM = do
  mbWhitespace
  strLike "P3"
  whitespace
  width <- nat
  whitespace
  height <- nat
  whitespace
  max <- nat
  whitespace
  dat <- sepBy whitespace (nat <&> cast)
  mbWhitespace
  pure (MkPPM (cast width) (cast height) (cast max) dat)

export
fromStrBuffer : StrBuffer -> IO PPM
fromStrBuffer buf = do
  str <- readString buf
  let Right ok = parseFull' parsePPM str
    | Left err =>
        die err
  pure ok
