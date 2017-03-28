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
defmodule AST.Number do
  defstruct [:value]

  def new(n), do: %__MODULE__{value: n}
end

defmodule AST.Add do
  defstruct [:lhs, :rhs]

  def new(lhs, rhs), :do %__MODULE__{lhs: lhs, rhs: rhs}
end
```

Define each elemental part of the language as an Elixir struct with a
constructor function.


```elixir
1 + 2

# Can be represented as

Add.new(Number.new(1),
        Number.new(2))
```



```elixir
1 + 2

# reduces to
3
```
```elixir
Add.new(Number.new(1),
        Number.new(2))

# reduces to
Number.new(3)
```

How do we implement reduction behaviour?

With a protocol. It takes an AST node and returns a reduced
one, if it can be reduced.

```elixir
defprotocol AST do
  @spec reduce(AST.t) :: {:ok, AST.t} | :noop
  def reduce(data)
end
```

What happens if you try and reduce a number?

```elixir
defimpl AST, for: AST.Number do
  def reduce(_) do
    :noop
  end
end
```
```elixir
ast = Number.new(2)

:noop = AST.reduce(ast)
```

A number cannot be reduced, it evaluates to itself, so we
return noop.

Addition can be reduced by summing the numbers on each side.

```elixir
defimpl AST, for: AST.Add do
  def reduce(add) do
    number = Number.new(add.lhs.value + add.rhs.value)
    {:ok, number}
  end
end
```

```elixir
ast = Add.new(Number.new(1),
              Number.new(2))

{:ok, new_ast} = AST.reduce(ast)

assert ast == Number.new(3)
```

```elixir
# (1 + 2) + 3
ast = Add.new(Add.new(Number.new(1), Number.new(2))
              Number.new(3))

AST.reduce(ast) # Crash! Left hand side isn't a Number
```

```elixir
defimpl AST, for: AST.Add do
  def reduce(%{lhs: lhs, rhs: rhs}) do
    with {:lhs, :noop} <- {:lhs, AST.reduce(lhs, env)},
         {:rhs, :noop} <- {:rhs, AST.reduce(rhs, env)} do
      new_num = Number.new(lhs.value + rhs.value)
      {:ok, new_num}
    else
      {:lhs, {:ok, new_lhs, new_env}} ->
        {:ok, Add.new(new_lhs, rhs), new_env}

      {:rhs, {:ok, new_rhs, new_env}} ->
        {:ok, Add.new(lhs, new_rhs), new_env}
    end
  end
end
```
