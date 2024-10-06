# Harmonic

A CLI to generate simple audio files based on frequency, pitch, and octave,
with configurable parameters such sample rate, and ADSR parameters.

The CLI provides a help command that lists the following commands and the
arguments each command can take.

```shell
$ harmonic --help
Usage: harmonic COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  freq
  pitch
  octave
```

Each command also has its respective help flag to display more precise information
on the arguments that command can take.

```shell
$ harmonic --help freq
Usage: harmonic octave --out OUTPUT_FILE --octave OCTAVE
                       (--sample-rate|--sr SAMPLE_RATE) --at ATTACK_TIME
                       --dt DECAY_TIME --st SUSTAIN_TIME --sl SUSTAIN_LEVEL
                       --rt RELEASE_TIME

Available options:
  --out OUTPUT_FILE        output file
  --octave OCTAVE          octave
  --sample-rate,--sr SAMPLE_RATE
                           sample rate
  --at ATTACK_TIME         attack time
  --dt DECAY_TIME          decay time
  --st SUSTAIN_TIME        sustain time
  --sl SUSTAIN_LEVEL       sustain level
  --rt RELEASE_TIME        release time
  -h,--help                Show this help text
```
