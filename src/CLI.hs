module CLI (run) where

import qualified Audio as A
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BS
import qualified Options.Applicative as OA
import qualified System.Process as P

newtype Opts = Opts Command

data Command
  = Freq FilePath Hz SampleRate ADSR
  | Pitch FilePath Pitch SampleRate ADSR
  deriving (Show, Eq)

type Hz = Float

type SampleRate = Float

type Pitch = String

type AttackT = Float

type DecayT = Float

type SustainT = Float

type SustainL = Float

type ReleaseT = Float

data ADSR
  = ADSR
      AttackT
      DecayT
      SustainT
      SustainL
      ReleaseT
  deriving (Show, Eq)

run :: IO ()
run = do
  Opts command <- runParser
  runCommand command

runParser :: IO Opts
runParser = OA.execParser optsParser
  where
    optsParser :: OA.ParserInfo Opts
    optsParser =
      OA.info
        (commandParser OA.<**> OA.helper)
        (OA.fullDesc <> OA.progDesc "" <> OA.header "")

    commandParser :: OA.Parser Opts
    commandParser =
      Opts
        <$> OA.subparser
          ( freqCommand
              <> pitchCommand
          )

    freqCommand :: OA.Mod OA.CommandFields Command
    freqCommand =
      OA.command
        "freq"
        ( OA.info
            (parser OA.<**> OA.helper)
            ( OA.fullDesc
                <> OA.progDesc ""
                <> OA.header ""
            )
        )
      where
        parser :: OA.Parser Command
        parser =
          Freq
            <$> outputFileParser
            <*> hzParser
            <*> sampleRateParser
            <*> adsrParser

    pitchCommand :: OA.Mod OA.CommandFields Command
    pitchCommand =
      OA.command
        "pitch"
        ( OA.info
            (parser OA.<**> OA.helper)
            ( OA.fullDesc
                <> OA.progDesc ""
                <> OA.header ""
            )
        )
      where
        parser :: OA.Parser Command
        parser =
          Pitch
            <$> outputFileParser
            <*> pitchParser
            <*> sampleRateParser
            <*> adsrParser

    outputFileParser :: OA.Parser FilePath
    outputFileParser =
      OA.strOption
        ( OA.long "out"
            <> OA.metavar "OUTPUT_FILE"
            <> OA.help "output file"
        )

    hzParser :: OA.Parser Hz
    hzParser =
      OA.option
        OA.auto
        ( OA.long "hz"
            <> OA.metavar "FREQUENCY"
            <> OA.help "frequency"
        )

    sampleRateParser :: OA.Parser SampleRate
    sampleRateParser =
      OA.option
        OA.auto
        ( OA.long "sample-rate"
            <> OA.long "sr"
            <> OA.metavar "SAMPLE_RATE"
            <> OA.help "sample rate"
        )

    pitchParser :: OA.Parser Pitch
    pitchParser =
      OA.strOption
        ( OA.long "pitch"
            <> OA.metavar "PITCH"
            <> OA.help "pitch"
        )

    adsrParser :: OA.Parser ADSR
    adsrParser =
      ADSR
        <$> attackTParser
        <*> decayTParser
        <*> sustainTParser
        <*> sustainLParser
        <*> releaseTParser
      where
        attackTParser :: OA.Parser AttackT
        attackTParser =
          OA.option
            OA.auto
            ( OA.long "at"
                <> OA.metavar "ATTACK_TIME"
                <> OA.help "attack time"
            )
        decayTParser :: OA.Parser DecayT
        decayTParser =
          OA.option
            OA.auto
            ( OA.long "dt"
                <> OA.metavar "DECAY_TIME"
                <> OA.help "decay time"
            )
        sustainTParser :: OA.Parser SustainT
        sustainTParser =
          OA.option
            OA.auto
            ( OA.long "st"
                <> OA.metavar "SUSTAIN_TIME"
                <> OA.help "sustain time"
            )
        sustainLParser :: OA.Parser SustainL
        sustainLParser =
          OA.option
            OA.auto
            ( OA.long "sl"
                <> OA.metavar "SUSTAIN_LEVEL"
                <> OA.help "sustain level"
            )
        releaseTParser :: OA.Parser ReleaseT
        releaseTParser =
          OA.option
            OA.auto
            ( OA.long "rt"
                <> OA.metavar "RELEASE_TIME"
                <> OA.help "release time"
            )

runCommand :: Command -> IO ()
runCommand (Freq filePath hz sr (ADSR at dt st sl rt)) = do
  let adsr = A.ADSR at dt st sl rt
  let w = A.freq hz 1 sr adsr
  A.saveFile filePath [w]
runCommand (Pitch filePath p sr (ADSR at dt st sl rt)) = do
  let pitchResult = A.readPitch p
  case pitchResult of
    Left err -> print err
    Right pitch -> do
      let adsr = A.ADSR at dt st sl rt
      let w = A.pitch pitch 0.2 sr adsr
      A.saveFile filePath [w]
