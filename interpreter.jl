struct EOF <: Exception end

const _one = one(UInt32)

"""
    Simple datastruct that uses a dictionary as tape with a potentially multiple cell pointers.
    The time travel aspect is implemented by storing the values & pointers of each change in the tape
    in a stack-like vector and computing the prior state when needed.
    Each Timeline also keeps track of its instruction pointer 
"""
mutable struct Timeline
    tape::Dict{Int, UInt8}
    memory_pointers::Vector{UInt32}
    instruction_pointer::Int
    history::Vector{Dict{Int, UInt8}}
    Timeline(tape::Dict{Int, UInt8} = Dict(1 => 0x0), mems = UInt32[_one], instr = 1, history = []) = new(tape, mems, instr, history)
end

function error(msg::AbstractString)
    printstyled("error: ", msg, '\n', color=:red)
    exit(-1)
end

"""
    Throws an error if the input is invalid. Otherwise it returns a char array of the input
"""
function assert()
    length(ARGS) < 1 && error("missing source file.\nusage: julia interpreter.jl src.5dbfwmtt")
    !isfile(ARGS[1]) && error("given source file is not a valid file")
    if !endswith(ARGS[1], ".5dbfwmtt") && !endswith(ARGS[1], ".bf")
        printstyled("warning: source file is not a .5dbfwmtt file", color=:red)
    end
    open(ARGS[1], "r") do f
        res = Char[]
        while !eof(f)
            c = read(f, Char)::Char
            c ∈ "><+-.,[]()~@^v" && push!(res, c)
        end
        res
    end  # O( l )
end

"""
    Create a minimal snapshot of all the cells currently pointed to
"""
snapshot(tl::Timeline) = push!(tl.history, Dict([cell => tl.tape[cell] for cell in tl.memory_pointers]))

"""
    Move all memory pointers in this timeline 1 cell to the right.
"""
function move(tl::Timeline, ::Int, ::Vector{Timeline})
    tl.memory_pointers .+= _one
    # initializing cells to zero (if they have no value)
    get!.(Ref(tl.tape), tl.memory_pointers, 0x0)
end

"""
    Move all memory pointers in this timeline 1 cell to the left.
    (note that bf interpreters usually start pointing at the leftmost cell)
"""
function back(tl::Timeline, ::Int, ::Vector{Timeline})
    any([m === _one for m in tl.memory_pointers]) && error("out of bounds")
    tl.memory_pointers .-= _one
    get!.(Ref(tl.tape), tl.memory_pointers, 0x0)
end

"""
    Increment all cells pointed to in this timeline.
"""
function incr(tl::Timeline, ::Int, ::Vector{Timeline})
    snapshot(tl)
    (cell -> tl.tape[cell] += 0x1).(tl.memory_pointers)
end

"""
    Decrement all cells pointed to in this timeline.
"""
function decr(tl::Timeline, ::Int, ::Vector{Timeline})
    snapshot(tl)
    (cell -> tl.tape[cell] -= 0x1).(tl.memory_pointers)
end

"""
    Output a character for all cells pointed to in this timeline.
    (using ascii encoding)
"""
function putc(tl::Timeline, ::Int, ::Vector{Timeline})
    print.(Char.(getindex.(Ref(tl.tape), tl.memory_pointers)))
end

"""
    Input a character and store it in all cells pointed to in this timeline.
"""
function getc(tl::Timeline, ::Int, ::Vector{Timeline})
    snapshot(tl)
    setindex!.(Ref(tl.tape), read(stdin, UInt8), tl.memory_pointers)
end

"""
    Move this instruction pointer to the matching ] if all cells pointed to in this timeline are 0.
"""
function loop(tl::Timeline, val::UInt32)
    if all([tl.tape[cell] == 0 for cell in tl.memory_pointers])
        tl.instruction_pointer = val # massive speed gain right here
    end
end

"""
    Move this instruction pointer back to the matching [ if any cells pointed to in this timeline are nonzero.
"""
function goto(tl::Timeline, val::UInt32) 
    if any([tl.tape[cell] ≠ 0 for cell ∈ tl.memory_pointers])
        tl.instruction_pointer = val
    end
end

"""
    Rewind the current tape back in time by 1 step.
"""
function time(tl::Timeline, ::Int, ::Vector{Timeline})
    length(tl.history) > 0 && merge!(tl.tape, pop!(tl.history))
end

"""
    Spawn a parallel timeline below the current timeline, with a copy of the tape and all pointers in it.
    This instruction pointer jumps to the matching ).
    Spawn a new instruction pointer within the newly spawned timeline,
    beginning execution immediately after this instruction.
"""
function copy(tl::Timeline, tl_pointer::Int, timelines::Vector{Timeline}, val::UInt32)
    ntl = deepcopy(tl)
    ntl.instruction_pointer += 1
    tl.instruction_pointer = val # jump to the matching )
    insert!(timelines, tl_pointer + 1, ntl)
end

"""
    If this is executed outside of the main timeline, 
    kill this timeline and all the memory/instruction pointers currently in it.
    Otherwise, do nothing.
"""
function kill(::Timeline, tl_pointer::Int, timelines::Vector{Timeline})
    if tl_pointer ≠ 1
        popat!(timelines, tl_pointer)
    end
end

"""
    Move all memory pointers in this timeline to the same location in the next ("lower") parallel universe.
"""
function next(tl::Timeline, tl_pointer::Int, timelines::Vector{Timeline})
    if tl_pointer ≠ length(timelines)
        append!(timelines[tl_pointer + 1].memory_pointers, tl.memory_pointers)
        get!.(Ref(timelines[tl_pointer + 1].tape), tl.memory_pointers, 0x0)
    end
    tl.memory_pointers = UInt32[]
end

"""
    Move all memory pointers in this timeline to the same location in the previous ("higher") parallel universe.
"""
function prev(tl::Timeline, tl_pointer::Int, timelines::Vector{Timeline})
    if tl_pointer ≠ 1
        append!(timelines[tl_pointer - 1].memory_pointers, tl.memory_pointers)
        # defaulting new cells to 0
        get!.(Ref(timelines[tl_pointer - 1].tape), tl.memory_pointers, 0x0)
    end
    tl.memory_pointers = UInt32[]
end

"""
    If the lower timeline has a nonzero amount of memory pointers,
    freeze this program pointer for the duration of this step.
    Otherwise, continue forward.
"""
function wait(tl::Timeline, tl_pointer::Int, timelines::Vector{Timeline})
    # has a lower tl & that tl has mem pointers
    if length(timelines) > tl_pointer && length(timelines[tl_pointer + 1].memory_pointers) > 0
        tl.instruction_pointer -= 1
    end
end

"""
    Ends this program if the current timeline is
    the main timeline. Otherwise it terminates the timeline
"""
done(::Timeline, tl_pointer::Int, timelines::Vector{Timeline}) = tl_pointer == 1 ? exit(0) : popat!(timelines, tl_pointer)

"""
    Converts all chars into their respective token.
    This is kinda useless for a language that consists of only 1-char instructions
    But I have it here for proper comments between # and eol's
"""
function lexer(src::Vector{Char})
    # vanilla bf
    d = Dict('>'=>move,'<'=>back,'+'=>incr,'-'=>decr,'.'=>putc,','=>getc,'['=>loop,']'=>goto,
    # 5D + time travel extension
    '~'=>time,'('=>copy,')'=>kill,'v'=>next,'^'=>prev,'@'=>wait)
    res = Function[]
    k = _one
    loops = UInt32[]
    while k ≤ length(src)
        p = src[k]
        if p ∈ "><+-.,~v^@)"
            push!(res, d[p])
        elseif p === '['
            n = 1
            f = _one
            while n > 0
                f + k > length(src) && error("expected closing bracket")
                n += (src[f + k] === '[') - (src[f + k] === ']')
                f += _one
            end
            push!(loops, k)
            v = f + k - _one
            push!(res, (tl::Timeline, ::Int, ::Vector{Timeline}) -> loop(tl, v))
        elseif p === ']'
            isempty(loops) && error("unexpected closing bracket")
            v = pop!(loops)
            push!(res, (tl::Timeline, ::Int, ::Vector{Timeline}) -> goto(tl, v))
        elseif p === '('
            n = 1
            f = _one
            while n > 0
                f + k > length(src) && error("expected closing parenthesis")
                n += (src[f + k] === '(') - (src[f + k] === ')')
                f += _one
            end
            v = f + k - _one
            push!(res, (tl::Timeline, tl_pointer::Int, timelines::Vector{Timeline}) -> copy(tl, tl_pointer, timelines, v))
        end
        k += _one
    end
    return push!(res, done)
end

function interpret(src::Vector{Function}) # worst: O( i d m ) average: O( i d ) best: O( i )
    timelines = Timeline[Timeline()]
    # O( i ) ; i = total instructions
    while true # program ends when the main timeline calls done()
        tl_pointer = 1
        # O( d ) ; d = depths of timelines
        while tl_pointer ≤ length(timelines)
            tl = timelines[tl_pointer]
            # all of these functions have either constant time O( 1 )
            # or vectorized & efficient O( m ) ; m = amount of memory pointers 
            src[tl.instruction_pointer](tl, tl_pointer, timelines)
            tl_pointer += 1
            tl.instruction_pointer += 1
        end
    end
end

(interpret ∘ lexer ∘ assert)()
