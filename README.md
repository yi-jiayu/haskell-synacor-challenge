# Synacor Challenge in Haskell

A(n incomplete) solution to the [Synacor Challenge](https://challenge.synacor.com/)
written in Haskell.

## Description

The challenge website reveals little about what needs to be done, only
providing a set of challenge materials comprised of a simple text file and an
opaque binary. However, the text file turns out to contain an architecture
specification and tells you the next step: implement a virtual machine capable
of running the included binary.

For the first step of this challenge, the objective was to create a virtual
machine based on a provided architecture specification capable of running an
included binary.

After implementing just a subset of the operations it is possible to run the
binary and obtain some output, however the entire architecture specification
has to be implemented properly in order to pass a self check phase which
immediately runs after the initial printout.

After several rounds of failing the self check and refining my virtual machine,
I was finally able to clear it and reveal the true content of the binary: a
text-based role playing game.

## Dependencies

Outside of the standard libraries, just Data.BitVector (package bv-0.4.1)

## Instructions

Only tested in `ghci` so far.

Run `ghci -isrc ./src/vm.hs` then `:main challenge.bin`.

## Implementation Notes
