module Audio
  ( readPitch,
    freq,
    pitchFreq,
    pitch,
    saveFile,
  )
where

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BS
import qualified Data.Char as C
import Options.Applicative as OA
import qualified System.Process as SP
import qualified Text.Read as TR
import qualified Text.Read.Lex as C

type Amplitude = Float

type Hz = Float

type SampleRate = Float

type Time = Float

type Wave = [Float]

data Semitone = C | Cs | D | Eb | E | F | Fs | G | Gs | A | Bb | B
  deriving (Eq, Ord, Enum, Bounded)

instance Show Semitone where
  show C = "C"
  show Cs = "C#"
  show D = "D"
  show Eb = "Eb"
  show E = "E"
  show F = "F"
  show Fs = "F#"
  show G = "G"
  show Gs = "G#"
  show A = "A"
  show Bb = "Bb"
  show B = "B"

newtype ParserError = ParserError String deriving (Show)

readSemitone :: String -> Either ParserError Semitone
readSemitone "C" = Right C
readSemitone "C#" = Right Cs
readSemitone "D" = Right D
readSemitone "Eb" = Right Eb
readSemitone "E" = Right E
readSemitone "F" = Right F
readSemitone "F#" = Right Fs
readSemitone "G" = Right G
readSemitone "G#" = Right Gs
readSemitone "A" = Right A
readSemitone "Bb" = Right Bb
readSemitone "B" = Right B
readSemitone s = Left . ParserError $ s

type Octave = Int

readOctave :: String -> Either ParserError Octave
readOctave s = case TR.readMaybe s of
  Nothing -> Left . ParserError $ s
  Just n -> Right n

data Pitch = Pitch Semitone Octave deriving (Show, Eq)

readPitch :: String -> Either ParserError Pitch
readPitch s = do
  let semitoneStr = takeWhile (liftA2 (||) C.isLetter (== '#')) s
  let octaveStr = dropWhile (liftA2 (||) C.isLetter (== '#')) s
  semitone <- readSemitone semitoneStr
  octave <- readOctave octaveStr
  return $ Pitch semitone octave

freq :: Hz -> Amplitude -> SampleRate -> Time -> Wave
freq hz a sr t = map ((* a) . sin . (* (hz * 2 * pi / sr))) [0 .. sr * t]

step :: Pitch -> Pitch -> Int
step (Pitch n o) (Pitch n' o') = n'' + o'' * octaveSize
  where
    n'' = fromEnum n' - fromEnum n
    o'' = o' - o
    octaveSize = 1 + fromEnum (maxBound :: Semitone)

pitchStandard :: Pitch
pitchStandard = Pitch A 4

pitchStandardFreq :: Hz
pitchStandardFreq = 440

pitchFreq :: Pitch -> Hz
pitchFreq p = f * (a ** fromIntegral n)
  where
    n = step pitchStandard p
    f = pitchStandardFreq
    a = 2 ** (1 / 12)

pitch :: Pitch -> Amplitude -> SampleRate -> Time -> Wave
pitch p = freq (pitchFreq p)

saveFile :: FilePath -> [Wave] -> IO ()
saveFile filePath =
  BS.writeFile filePath
    . BSB.toLazyByteString
    . mconcat
    . map BSB.floatLE
    . mconcat
