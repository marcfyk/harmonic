module Audio
  ( ADSR (ADSR),
    readPitch,
    freq,
    pitchFreq,
    pitch,
    scale,
    octave,
    saveFile,
  )
where

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BS
import qualified Data.Char as C
import Options.Applicative as OA
import qualified Text.Read as TR

type Amplitude = Float

type Hz = Float

type SampleRate = Float

type Wave = [Float]

data ADSR
  = ADSR
      AttackT
      DecayT
      SustainT
      SustainL
      ReleaseT
  deriving (Show, Eq)

type AttackT = Float

type DecayT = Float

type SustainT = Float

type SustainL = Float

type ReleaseT = Float

adsrWeights :: AttackT -> DecayT -> SustainT -> SustainL -> ReleaseT -> [Float]
adsrWeights at dt st sl rt = mconcat [attack, decay, sustain, release]
  where
    attack = take (round at) . map (/ at) $ [0 ..]
    decay = take (round dt) . map (\x -> 1 + negate (1 - sl) * x / dt) $ [0 ..]
    sustain = replicate (round st) sl
    release = take (round rt) . map (\x -> sl + negate sl * x / rt) $ [0 ..]

freq :: Hz -> Amplitude -> SampleRate -> ADSR -> Wave
freq hz a sr (ADSR at dt st sl rt) = zipWith (*) w weights
  where
    ts = [0 .. sr * sum [at, dt, st, sl, rt]]
    s = hz * 2 * pi / sr
    w = map (sin . (* s)) ts
    adsr = adsrWeights (at * sr) (dt * sr) (st * sr) sl (rt * sr)
    weights = map (* a) adsr

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

type Octave = Integer

readOctave :: String -> Either ParserError Octave
readOctave s = case TR.readMaybe s of
  Nothing -> Left . ParserError $ s
  Just n -> Right n

data Pitch = Pitch Semitone Octave deriving (Show, Eq)

readPitch :: String -> Either ParserError Pitch
readPitch p = do
  let semitoneStr = takeWhile (liftA2 (||) C.isLetter (== '#')) p
  let octaveStr = dropWhile (liftA2 (||) C.isLetter (== '#')) p
  s <- readSemitone semitoneStr
  o <- readOctave octaveStr
  return $ Pitch s o

step :: Pitch -> Pitch -> Integer
step (Pitch n o) (Pitch n' o') = n'' + o'' * octaveSize
  where
    n'' = toInteger $ fromEnum n' - fromEnum n
    o'' = o' - o
    octaveSize = toInteger $ 1 + fromEnum (maxBound :: Semitone)

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

pitch :: Pitch -> Amplitude -> SampleRate -> ADSR -> Wave
pitch p = freq (pitchFreq p)

scale :: [Semitone]
scale = [minBound :: Semitone .. maxBound :: Semitone]

octave :: Octave -> [Pitch]
octave o = [Pitch s o | s <- scale]

saveFile :: FilePath -> [Wave] -> IO ()
saveFile filePath =
  BS.writeFile filePath
    . BSB.toLazyByteString
    . mconcat
    . map BSB.floatLE
    . mconcat
