struct EOF <: Exception end

function error(msg)
    printstyled("error: ", msg, '\n', color=:red)
    exit(-1)
end

"""
    Throws an error if the input is invalid. Otherwise it returns a char array of the input
"""
function assert()
    length(ARGS) < 1 && error("missing source file.\nusage: julia interpreter.jl src.5dbfwmtt")
    !isfile(ARGS[1]) && error("given source file is not a valid file")
    if !endswith(ARGS[1], ".5dbfwmtt")
        endswith(ARGS[1], ".bf") || printstyled("warning: source file is not a .5dbfwmtt file", color=:red)
    end
    src = open(f -> [read(f, String)...], ARGS[1], "r")
    brackets = 0
    parenthesis = 0
    pointer = 0
    while pointer < length(src) 
        # I could scan for this during runtime but since ([)] is a thing
        # I cant be bothered to backtrack loops skipped when jumping to ')'
        # Might cause bugs & errors but we'll see (half an hour later...)
        # Yes it does, assume the following code:
        # +[(]) ([)]
        # this program thinks the last closing bracket matches the first opening bracket
        # pls remind me to fix that
        brackets += (src[pointer += 1] === '[') - (src[pointer] === ']')
        brackets < 0 && error("mismatching brackets")
        parenthesis += (src[pointer] === '(') - (src[pointer] === ')')
        parenthesis < 0 && error("mismatching parenthesis")
    end
    brackets > 0 && error("missing ']' in source file")
    parenthesis > 0 && error("missing ')' in source file")
    return src
end

"""
    Converts all chars into their respective token.
    This is kinda useless for a language that consists of only 1-char instructions
    But I have it here for proper comments between # and eol's
"""
function lexer(src::Vector{Char})
    # vanilla bf
    d = Dict('>'=>:move,'<'=>:back,'+'=>:incr,'-'=>:decr,'.'=>:putc,','=>:getc,'['=>:loop,']'=>:goto,
    # 5D + time travel extension
    '~'=>:time,'('=>:copy,')'=>:kill,'v'=>:next,'^'=>:prev)
    res = Symbol[]
    k = 1
    while k ≤ length(src)
        p = src[k]
        if p === '#'
            k = something(findnext(isequal('\n'), src, k), length(src))
        elseif p ∈ "><+-.,[]~()v^"
            push!(res, d[p])
        end
        k += 1
    end
    return push!(res, :eof)
end

"""
    Simple datastruct that uses a dictionary as tape with a potentially multiple cell pointers.
    The time travel aspect is implemented by storing the values & pointers of each change in the tape
    in a stack-like vector and computing the prior state when needed.
    Each Timeline also keeps track of its loops as well as its instruction pointer 
"""
mutable struct Timeline
    tape::Dict{Int, UInt8}
    memory_pointers::Vector{Int}
    instruction_pointer::Int
    loops::Vector{Int}
    history::Vector{Dict}
    Timeline(tape::Dict{Int, UInt8} = Dict(1 => 0x0), mems = Int[1], instr = 1, loops = Int[], history = []) = new(tape, mems, instr, loops, history)
end

"""
    Push all values
"""
snapshot(tl::Timeline) = push!(tl.history, Dict([cell => tl.tape[cell] for cell in tl.memory_pointers]))

# using a ton of dot broadcasting in the following code.
# makes some harder to read. my apologies

"""
    Move all memory pointers in this timeline 1 cell to the right.
"""
function move(tl, tl_pointer, timelines)
    # moving all memory_pointers by b (b should be 1)
    tl.memory_pointers .+= 1
    # initializing cells to zero (if they have no value)
    get!.(Ref(tl.tape), tl.memory_pointers, 0x0)
end

"""
    Move all memory pointers in this timeline 1 cell to the left.
    (note that bf interpreters usually start pointing at the leftmost cell)
"""
function back(tl, tl_pointer, timelines)
    tl.memory_pointers .-= 1
    get!.(Ref(tl.tape), tl.memory_pointers, 0x0)
end

"""
    Increment all cells pointed to in this timeline.
"""
function incr(tl, tl_pointer, timelines)
    snapshot(tl)
    (cell -> tl.tape[cell] += 0x1).(tl.memory_pointers)
end

"""
    Decrement all cells pointed to in this timeline.
"""
function decr(tl, tl_pointer, timelines)
    snapshot(tl)
    (cell -> tl.tape[cell] -= 0x1).(tl.memory_pointers)
end

"""
    Output a character for all cells pointed to in this timeline.
    (using ascii encoding)
"""
function putc(tl, tl_pointer, timelines)
    # alt: print(Char(tl.tape[tl.memory_pointers])^b)
    # but idk if that does what its supposed to do
    print.(Char.(get.(Ref(tl.tape), tl.memory_pointers, nothing)))
    # for cell in tl.memory_pointers
    #     print(Char(tl.tape[cell]))
    # end
end

"""
    Input a character and store it in all cells pointed to in this timeline.
"""
function getc(tl, tl_pointer, timelines)
    snapshot(tl)
    # value = read(stdin, UInt8)
    setindex!.(Ref(tl.tape), read(stdin, UInt8), tl.memory_pointers)
end

"""
    Move this instruction pointer to the matching ] if all cells pointed to in this timeline are 0.
"""
function loop(tl, tl_pointer, timelines)
    if all([tl.tape[cell] == 0 for cell in tl.memory_pointers])
        loop_count = 1
        while loop_count ≥ 1
            loop_count += (src[tl.instruction_pointer += 1] === :loop) - (src[tl.instruction_pointer] === :goto)
        end
    else
        push!(tl.loops, tl.instruction_pointer)
    end
end

"""
    Move this instruction pointer back to the matching [ if any cells pointed to in this timeline are nonzero.
"""
function goto(tl, tl_pointer, timelines) 
    if any([tl.tape[cell] ≠ 0 for cell ∈ tl.memory_pointers])
        tl.instruction_pointer = last(tl.loops)
    else
        length(tl.loops) > 0 ? pop!(tl.loops) : nothing
    end
end

"""
    Rewind the current tape back in time by 1 step.
"""
time(tl, tl_pointer, timelines) = merge!(tl.tape, pop!([Dict(), tl.history...]))

"""
    Spawn a parallel timeline below the current timeline, with a copy of the tape and all pointers in it.
    This instruction pointer jumps to the matching ).
    Spawn a new instruction pointer within the newly spawned timeline,
    beginning execution immediately after this instruction.
"""
function copy(tl, tl_pointer, timelines)
    ntl = deepcopy(tl)
    ntl.instruction_pointer += 1
    loop_count = 1
    while loop_count ≥ 1
        loop_count += (src[tl.instruction_pointer += 1] === :copy) - (src[tl.instruction_pointer] === :kill)
    end
    push!(timelines, ntl)
end

"""
    If this is executed outside of the main timeline, 
    kill this timeline and all the memory/instruction pointers currently in it.
    Otherwise, do nothing.
"""
kill(tl, tl_pointer, timelines) = tl_pointer ≠ 1 ? popat!(timelines, tl_pointer) : nothing

"""
    Move all memory pointers in this timeline to the same location in the next ("lower") parallel universe.
"""
function next(tl, tl_pointer, timelines)
    tl_pointer == length(timelines) && return
    append!(timelines[tl_pointer + 1].memory_pointers, tl.memory_pointers)
    tl.memory_pointers = Int[]
end

"""
    Move all memory pointers in this timeline to the same location in the previous ("higher") parallel universe.
"""
function prev(tl, tl_pointer, timelines)
    tl_pointer == 1 && return
    append!(timelines[tl_pointer - 1].memory_pointers, tl.memory_pointers)
    tl.memory_pointers = Int[]
end

"""
    Do nothing
"""
noop(tl, tl_pointer, timelines) = nothing

"""
    Throw an EOF exception and thus end this program if the current timeline is
    the main timeline. Otherwise it terminates the timeline
"""
eof(tl, tl_pointer, timelines) = tl_pointer == 1 ? throw(EOF()) : popat!(timelines, tl_pointer)

function interpret(_src::Vector{Symbol})
    timelines = Timeline[Timeline()]
    # its just one global and this is one is typeconst
    global src = _src::Vector{Symbol}
    while true # program ends when the main timeline throws EOF()
        global tl_pointer = 1
        while tl_pointer ≤ length(timelines)
            tl = timelines[tl_pointer]
            try
                # Symbols are the type of variable- & functionnames in julia 
                # here we evaluate the symbol and call the prior defined method
                # which corresponds to the symbol
                eval(src[tl.instruction_pointer])(tl, tl_pointer, timelines)
            catch e
                if e isa EOF
                    println()
                    return 
                else
                    rethrow(e)
                end
            end
            tl_pointer += 1
            tl.instruction_pointer += 1
        end
    end
end

(interpret ∘ lexer ∘ assert)()
