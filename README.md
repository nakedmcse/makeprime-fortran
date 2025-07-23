# makeprime-fortran
A FORTRAN port of Fudmotins prime generator.

## Overview
This tool creates a prime number for a given number of digits.  It is a FORTRAN port of the original [C++ version](https://github.com/Fudmottin/makeprime).

## Compiling
To compile, you will need to use the following make command:
`make all`

This will build the makeprime executable which can then be copied to you local bin folder.

You can clean up the build directory afterwards using `make clean`

## Usage

```
makeprime <number of digits> [options]
```

Number of digits must be greater than 3.

Options:
- `--twin`: Find consecutive prime numbers
- `--random`: Use a random starting point
- `--help`: Print usage

### Example
```
makeprime 100 --twin
```
