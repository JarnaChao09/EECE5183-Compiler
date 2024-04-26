# EECE 5183 Compiler

A compiler of a Pascal-like derivative for EECE 5183 Compiler Theory and Practice at the University of Cincinnati

## Installation

### Requirements

- The [Crystal language](https://crystal-lang.org/)
    - Version >= 1.11.0
    - The compiler is built entirely in Crystal with **zero** dependencies
- LLVM 17.0.6
    - Crystal comes with pre-built bindings to LLVM. See [getting the right LLVM](#getting-the-right-llvm) section for more details.
- Clang 17.0.6
    - `clang` is used as the linker and final compilation step to go from LLVM IR `.ll` files to a native executable.
    - `clang` must be discoverable on the system path.

## How to Install

- To install, run `shards build compiler` and the compiler executable will be found in the `bin` directory. 
    - Similarly, `shards run compiler -- [arguments to executable]` can be used to directly build and run the executable at the same time.

## Usage

The usage of the executable is as follows:

```
Welcome to the compiler for EECE 5183
Usage: compiler [subcommand] [options] [program-file]
    compile                          subcommand: Compile a file
    run                              subcommand: Compile and Execute a file using LLVM JIT
    -emit-ir, --emit-llvm-ir         Emits generated LLVM IR
    -dump-ir, --dump-llvm-ir         Dumps generated LLVM IR
    -v, --version                    Show Version
    -h, --help                       Show Help
```

## Getting the right LLVM

As of writing (04/26/2024), most major package managers (apt, pacman, snap) have crystal being pre-compiled with LLVM 15.0.7. For this project, LLVM 17.0.6 was the primary LLVM version tested. Mileage may vary depending on the compiled LLVM version. 

The one package manager that is currently up-to-date on the LLVM version being built with Crystal is Homebrew/Linuxbrew. However, recently the Homebrew/Linuxbrew bottle for Crystal has been updated to use LLVM 18.1.4 which Crystal current fails to build correctly with. 

> The reason for this is because LLVM 18 no longer requires `llvm_ext.o` to be linked and therefore will no longer build the object file during installation. However, Crystal and shards have not been updated to fix the utilized linking path, therefore they fail to build on the latest bottle from Homebrew/Linuxbrew.

There are currently two options to get the correct LLVM version:
1. Compile Crystal from source at [the GitHub](https://github.com/crystal-lang/crystal) using the correct LLVM version.
2. Pour from an old Homebrew/Linuxbrew formula to recieve the correct built binary.

## Development

As this project is for a university assignment, development will not be encouraged until after the grade has been given for the project. Please check with the main contributor to see if it is ok to start contributing to the project.

All development occurred on Crystal version 1.11.2 on a 13 inch M1 Macbook Pro 2020 edition. Crystal was installed using homebrew with LLVM version 17.0.6 and clang version 17.0.6. A docker file will be provided (soon) to be used for all future development.

## Contributing

1. Fork it (<https://github.com/JarnaChao09/EECE5183-Compiler>)
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request

## Contributors

- [JarnaChao09](https://github.com/JarnaChao09/) - creator and maintainer
