# Chip8 VM

A chip-8 virtual machine written in OCaml.

## Features

- All opcodes and flag behaviour verified against [Chip8 specific test roms](https://github.com/Timendus/chip8-test-suite).
- Most quirks (other than clipping, which is WIP)
- Configurable colours & render scale
- A rudementary debugger
- Audio

## Requirements

- SDL2
- OCaml 5

## How to build

```bash
$ opam install . --deps-only --with-test
# run tests
$ dune build
$ dune test
# build release and install in env
$ dune build --release
$ dune install
```

## Where to find ROMs?

I found a bunch at the [Chip8 Archive](https://johnearnest.github.io/chip8Archive/).

## Attributions

- Originally inspired by [caml8](https://github.com/linoscope/caml8), then I
did my own thing.
- [This excellent guide](https://tobiasvl.github.io/blog/write-a-chip-8-emulator)
- [Another more practical guide](https://multigesture.net/articles/how-to-write-an-emulator-chip-8-interpreter/)
- [awesome-chip8](https://github.com/tobiasvl/awesome-chip-8)

## Known Issues

- Older ROMs may cause VM to hang

## License

GPLv3
