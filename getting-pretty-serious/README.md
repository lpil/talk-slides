# Getting Pretty Serious

I wanna talk about about code style.


Which do you prefer?

### Lists

```elixir
defp deps do
  [{:mix_test_watch, "~> 0.4"},
   {:dialyxir, "~> 0.5"}]
end
```
```elixir
defp deps do
  [
    {:mix_test_watch, "~> 0.4"},
    {:dialyxir, "~> 0.5"},
  ]
end
```


### Maps

```elixir
%{key: value}
```
```elixir
%{ key: value }
```


### Infix operators

```elixir
person.title <> " " <> person.first_name <> " " person.first_name <> " " <> person.last_name <> ": " <> person.job_title
```
```elixir
person.title <> " " <> person.first_name <> " " <> person.first_name <>
  " " <> person.last_name <> ": " <> person.job_title
```


### Function calls

```elixir
do_a_thing(argument_one,
           argument_two,
           argument_three,
           argument_four)
```
```elixir
do_a_thing(
  argument_one,
  argument_two,
  argument_three,
  argument_four
)
```






# TODO

- Consistent code style has benefits.
- Maintaining a consistent style can be expensive.
- Linters can help.
- Introduce formatter.
- Source, parse to AST, convert to Algebra, print Algebra.
- Introduce each different type of Algebra in a practical sense using
  examples.
