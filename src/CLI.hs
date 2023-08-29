module CLI (run) where

import qualified Audio as A
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BS
import qualified Options.Applicative as OA
import qualified System.Process as P

newtype Opts = Opts Command

data Command
  = Freq FilePath Hz SampleRate Duration
  | Pitch FilePath Pitch SampleRate Duration
  deriving (Show, Eq)

type Hz = Float

type SampleRate = Float

type Duration = Float

type Pitch = String

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
            <*> durationParser

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
            <*> durationParser

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

    durationParser :: OA.Parser Duration
    durationParser =
      OA.option
        OA.auto
        ( OA.long "duration"
            <> OA.short 'd'
            <> OA.metavar "DURATION"
            <> OA.help "duration"
        )

    pitchParser :: OA.Parser Pitch
    pitchParser =
      OA.strOption
        ( OA.long "pitch"
            <> OA.metavar "PITCH"
            <> OA.help "pitch"
        )

runCommand :: Command -> IO ()
runCommand (Freq filePath hz sr d) = do
  let w = A.freq hz 1 sr d
  A.saveFile filePath [w]
runCommand (Pitch filePath p sr d) = do
  let pitchResult = A.readPitch p
  case pitchResult of
    Left err -> print err
    Right pitch -> do
      print pitch
      print (A.pitchFreq pitch)
      let w = A.pitch pitch 0.2 sr d
      A.saveFile filePath [w]
