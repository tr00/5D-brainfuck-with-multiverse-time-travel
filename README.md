# 5D-brainfuck-with-multiverse-time-travel

This is an interpreter for the language '[5D brainfuck with multiverse time travel](https://esolangs.org/wiki/5D_Brainfuck_With_Multiverse_Time_Travel)' by [RocketRace](https://esolangs.org/wiki/User:RocketRace). It is written in [Julia](https://julialang.org/) and uses no external libraries.

## Quick Guide
Check out the [esolangs wiki page](https://esolangs.org/wiki/5D_Brainfuck_With_Multiverse_Time_Travel) for more information.


  `>`&nbsp;&nbsp;&nbsp;Move all memory pointers in this timeline 1 cell to the right.
  
  `<`&nbsp;&nbsp;&nbsp;Move all memory pointers in this timeline 1 cell to the left.
  
  `+`&nbsp;&nbsp;&nbsp;Increment all cells pointed to in this timeline.
  
  `-`&nbsp;&nbsp;&nbsp;Decrement all cells pointed to in this timeline.
  
  `.`&nbsp;&nbsp;&nbsp;Output a character for all cells pointed to in this timeline.
  
  `,`&nbsp;&nbsp;&nbsp;Input a character and store it in all cells pointed to in this timeline.
  
  `[`&nbsp;&nbsp;&nbsp;Move this instruction pointer to the matching `]` if all cells pointed to in this timeline are 0.
  
  `]`&nbsp;&nbsp;&nbsp;Move this instruction pointer back to the matching `[` if any cells pointed to in this timeline are nonzero.
  
  `~`&nbsp;&nbsp;&nbsp;Rewind the current tape back in time by 1 step.
  
  `(`&nbsp;&nbsp;&nbsp;Spawn a parallel timeline "below", with a copy of the tape and all pointers in it. This instruction pointer jumps to the matching `)`.
  
  `)`&nbsp;&nbsp;&nbsp;If this is executed outside of the main timeline, kill this timeline and all the memory/instruction pointers currently in it.
  
  `v`&nbsp;&nbsp;&nbsp;Move all memory pointers in this timeline to the same location in the next ("lower") parallel universe.
  
  `^`&nbsp;&nbsp;&nbsp;Move all memory pointers in this timeline to the same location in the previous ("higher") parallel universe.
  
  `@`&nbsp;&nbsp;&nbsp;Wait until the "lower" parallel universe has 0 memory pointers or doesnt exist anymore.
  
  
## Usage
The interpreter is written Julia 1.5.2 but may also be compatible with some earlier versions of Julia. To run a file, type the following command into your terminal:
`julia interpreter.jl src.5dbfwmtt`
    
    
## Rust Version
There is another version written in Rust. Check it out [here](https://github.com/RocketRace/fivedbf)!
    
## License
  This software is licensed under the MIT License. 2020
