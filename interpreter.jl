struct EOF <: Exception end

"""
    This isnt really needed for an 1-char per instruction
    language but it allows me to abstract the functionality
    of each instruction into its own function.
    Its also super useful to avoid rescanning the source code
    for each and every bracket / parenthesis (including that [(])([)] )
"""
struct Token
    rep::Function
    val::UInt32
    Token(rep::Function, val::UInt32 = zero(UInt32)) = new(rep, val)
end

"""
    Simple datastruct that uses a dictionary as tape with a potentially multiple cell pointers.
    The time travel aspect is implemented by storing the values & pointers of each change in the tape
    in a stack-like vector and computing the prior state when needed.
    Each Timeline also keeps track of its instruction pointer 
"""
mutable struct Timeline
    tape::Dict{Int, UInt8}
    memory_pointers::Vector{Int}
    instruction_pointer::Int
    history::Vector{Dict{Int, UInt8}}
    Timeline(tape::Dict{Int, UInt8} = Dict(1 => 0x0), mems = Int[1], instr = 1, history = []) = new(tape, mems, instr, history)
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
    open(f -> [read(f, String)...], ARGS[1], "r")
end

"""
    Create a snapshot of the current tape
"""
snapshot(tl::Timeline) = push!(tl.history, Dict([cell => tl.tape[cell] for cell in tl.memory_pointers]))

"""
    Move all memory pointers in this timeline 1 cell to the right.
"""
function move(tl, tl_pointer, timelines, token)
    tl.memory_pointers .+= 1
    # initializing cells to zero (if they have no value)
    get!.(Ref(tl.tape), tl.memory_pointers, 0x0)
end

"""
    Move all memory pointers in this timeline 1 cell to the left.
    (note that bf interpreters usually start pointing at the leftmost cell)
"""
function back(tl, tl_pointer, timelines, token)
    tl.memory_pointers .-= 1
    get!.(Ref(tl.tape), tl.memory_pointers, 0x0)
end

"""
    Increment all cells pointed to in this timeline.
"""
function incr(tl, tl_pointer, timelines, token)
    for cell in tl.memory_pointers
        snapshot(tl)
        tl.tape[cell] += 0x1
    end
end

"""
    Decrement all cells pointed to in this timeline.
"""
function decr(tl, tl_pointer, timelines, token)
    for cell in tl.memory_pointers
        snapshot(tl)
        tl.tape[cell] -= 0x1
    end
end

"""
    Output a character for all cells pointed to in this timeline.
    (using ascii encoding)
"""
function putc(tl, tl_pointer, timelines, token)
    print.(Char.(getindex.(Ref(tl.tape), tl.memory_pointers)::Vector{UInt8}))
end

"""
    Input a character and store it in all cells pointed to in this timeline.
"""
function getc(tl, tl_pointer, timelines, token)
    snapshot(tl)
    setindex!.(Ref(tl.tape), read(stdin, UInt8), tl.memory_pointers)
end

"""
    Move this instruction pointer to the matching ] if all cells pointed to in this timeline are 0.
"""
function loop(tl, tl_pointer, timelines, token)
    if all([tl.tape[cell] == 0 for cell in tl.memory_pointers])
        tl.instruction_pointer = token.val # massive speed gain right here
    end
end

"""
    Move this instruction pointer back to the matching [ if any cells pointed to in this timeline are nonzero.
"""
function goto(tl, tl_pointer, timelines, token) 
    if any([tl.tape[cell] ≠ 0 for cell ∈ tl.memory_pointers])
        tl.instruction_pointer = token.val
    end
end

"""
    Rewind the current tape back in time by 1 step.
"""
function time(tl, tl_pointer, timelines, token)
    length(tl.history) > 0 && merge!(tl.tape, pop!(tl.history))
end

"""
    Spawn a parallel timeline below the current timeline, with a copy of the tape and all pointers in it.
    This instruction pointer jumps to the matching ).
    Spawn a new instruction pointer within the newly spawned timeline,
    beginning execution immediately after this instruction.
"""
function copy(tl, tl_pointer, timelines, token)
    ntl = deepcopy(tl)
    ntl.instruction_pointer += 1
    tl.instruction_pointer = token.val # jump to the matching )
    insert!(timelines, tl_pointer + 1, ntl)
end

"""
    If this is executed outside of the main timeline, 
    kill this timeline and all the memory/instruction pointers currently in it.
    Otherwise, do nothing.
"""
function kill(tl, tl_pointer, timelines, token)
    if tl_pointer ≠ 1
        popat!(timelines, tl_pointer)
    end
end

"""
    Move all memory pointers in this timeline to the same location in the next ("lower") parallel universe.
"""
function next(tl, tl_pointer, timelines, token)
    tl_pointer ≠ length(timelines) && append!(timelines[tl_pointer + 1].memory_pointers, tl.memory_pointers)
    get!.(Ref(timelines[tl_pointer + 1].tape), tl.memory_pointers, 0x0)
    tl.memory_pointers = Int[]
end

"""
    Move all memory pointers in this timeline to the same location in the previous ("higher") parallel universe.
"""
function prev(tl, tl_pointer, timelines, token)
    tl_pointer ≠ 1 && append!(timelines[tl_pointer - 1].memory_pointers, tl.memory_pointers)
    # defaulting new cells to 0
    get!.(Ref(timelines[tl_pointer - 1].tape), tl.memory_pointers, 0x0)
    tl.memory_pointers = Int[]
end

"""
    If the lower timeline has a nonzero amount of memory pointers,
    freeze this program pointer for the duration of this step.
    Otherwise, continue forward.
"""
function wait(tl, tl_pointer, timelines, token)
    # has a lower tl & that tl has mem pointers
    if length(timelines) > tl_pointer && length(timelines[tl_pointer + 1].memory_pointers) > 0
        tl.instruction_pointer -= 1
    end
end

"""
    Do nothing
"""
noop(_...) = nothing # one of the coolest functions ive ever written

"""
    Throw an EOF exception and thus end this program if the current timeline is
    the main timeline. Otherwise it terminates the timeline
"""
eof(tl, tl_pointer, timelines, token) = tl_pointer == 1 ? throw(EOF()) : popat!(timelines, tl_pointer)

"""
    Converts all chars into their respective token.
    This is kinda useless for a language that consists of only 1-char instructions
    But I have it here for proper comments between # and eol's
"""
function lexer(src::Vector{Char})
    # vanilla bf
    d = Dict('>'=>move,'<'=>back,'+'=>incr,'-'=>decr,'.'=>putc,','=>getc,'['=>loop,']'=>goto,
    # 5D + time travel extension
    '~'=>time,'('=>copy,')'=>kill,'v'=>next,'^'=>prev,
    # my personal addition
    '@'=>wait)
    R1 = Char[]
    k = one(UInt32)
    begin # pre lexing TODO: maybe i dont need this
        while k ≤ length(src)
            p = src[k]
            if p === '#'
                k = UInt32(something(findnext(isequal('\n'), src, k), length(src)))
            elseif p ∈ "><+-.,~v^@()[]"
                push!(R1, p)
            end
            k += one(UInt32)
        end
    end
    res = Token[]
    k = one(UInt32)
    loops = UInt32[]
    while k ≤ length(R1)
        p = R1[k]
        if p ∈ "><+-.,~v^@)"
            push!(res, Token(d[p]))
        elseif p === '['
            n = 1
            f = zero(UInt32)
            while n > 0
                f + k ≥ length(R1) && error("expected closing bracket")
                n += (R1[(f+=one(UInt32)) + k] === '[') - (R1[f + k] === ']')
            end
            push!(loops, k)
            push!(res, Token(loop, f+k))
        elseif p === ']'
            isempty(loops) && error("unexpected closing bracket")
            push!(res, Token(goto, pop!(loops)))
        elseif p === '(' # TODO: add mismatching check for parenthesis
            n = 1
            f = zero(UInt32)
            while n > 0
                f + k ≥ length(R1) && error("expected closing parenthesis")
                n += (R1[(f+=one(UInt32)) + k] === '(') - (R1[f + k] === ')')
            end
            push!(res, Token(copy, f+k))
        end
        k += one(UInt32)
    end
    return push!(res, Token(eof))
end

function interpret(_src::Vector{Token})
    timelines = Timeline[Timeline()]
    # its just one global and this is one is typeconst
    global src = _src::Vector{Token}
    while true # program ends when the main timeline throws EOF()
        tl_pointer = 1
        while tl_pointer ≤ length(timelines)
            tl = timelines[tl_pointer]
            token = src[tl.instruction_pointer]
            try
                (token.rep)(tl, tl_pointer, timelines, token)
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

# O( i d m + 2l ) # ik 2l is irrelevant in big O but i care about it
# i = number of instructions
# d = depths of concurrent timelines
# m = number of memory pointers (these get vectorized so its not that bad)
# l = length of the input file
(interpret ∘ lexer ∘ assert)()
