# Exploring Interpretation

What happens when a program is running?

```
          Runtime
        |         |
Code -> |   ???   |
Data -> |   ???   | -> Output
        |         |
```

A runtime takes code and data, does something with them, and we get some
output. What happens in the middle?

Last year I made a language that compiled to BEAM bytecode (see my talk).

```
          Compiler
        |             |
        |  Parse AST  |
Code -> |  Codegen    | -> Bytecode
        |             |
```

Compilers:

- Take in source code
- Parse code into an AST
- Transform into an output format (here BEAM bytecode)

But how does the output format run?

```
              Runtime
            |         |
Bytecode -> |   ???   |
Data     -> |   ???   | -> Output
            |         |
```

A runtime takes the compiled output and data, does something, and we get
output.

This is the same as the first diagram. I'm not any closer to understanding
what's happening inside a running program!

I need to build an interpreter.

```
            Interpreter
        |               |
Code -> |  Parse AST    |
Data -> |  Execute AST  | -> Output
        |               |
```

Interpreters:
- Parse source code into AST
- Execute that AST

How might execution work? Let's think about how code reduces.

```elixir
(1 + 2) + (3 + 4)

# reduces to
3 + (3 + 4)

# reduces to
3 + (3 + 4)

# reduces to
3 + 7

# reduces to
3 + 7
```

Evaluation of code can be thought of happening in steps. Each step the code
reduces by the smallest possible amount.

How could we model this in Elixir?

```elixir
defmodule Soup.AST.Number do
  defstruct [:value]

  def new(n), do: %__MODULE__{value: n}
end

defmodule Soup.AST.Add do
  defstruct [:lhs, :rhs]

  def new(lhs, rhs), :do %__MODULE__{lhs: lhs, rhs: rhs}
end
```

Define each elemental part of the language as an Elixir struct with a
constructor function.
