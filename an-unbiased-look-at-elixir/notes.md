An Unbiased Look At Elixir
==========================

Today I'd like to take a look at my favourite language, Elixir, and ask what
could it offer us here at Red Badger. To do this I'll make a tiny Elixir
project and look at some of the parts of it.

I believe that in order for a language to be useful or interesting it needs to
have a clear goal, and that goal will shape the language and how it is used.

Elixir's creator, Jose Valid, had three main goals.

1. To facilitate short term productivity.
2. To facilitate long term productivity.
3. To make writing multi-core concurrent applications easy.

To achieve long time productivity and easy concurrency Elixir targets the
Erlang virtual machine and integrates into the Erlang ecosystem.

To achieve short term productivity it takes the best of the Ruby, Clojure and
F# ecosystems and applies these traits to the Erlang world.

Let's start by looking at an Elixir project.

Elixir is very much a batteries included language. It ships with a project
management tool called mix, which I can use to create a new project.


```
mix new badger
cd badger
tree
```

The config directory is for configuring different environments of the app.

The lib directory contains application code.

And the test directory contains test code. The tests can be run with mix.

There's a package manager for Elixir and Erlang called Hex, and through mix we
can interact with the hex system. I'll add a library now.

```elixir
# Open mix file
defp deps do
  [
    {:mix_test_watch, ">= 0.0.0", only: :dev}
  ]
end
```

Here's the first bit of Elixir code we see. It looks a lot like Ruby, but it's
actually a functional language. It has no objects, no classes, and no
mutation.

I'll add the dep to the deps section.

```sh
mix deps.get
```

Mix can now fetch the deps. Unlike NPM mix and hex lock the deps to a
specific version, so we can be sure that all the developers and servers have
the same versions.

The library I've added is a test runner which can be used to explore the code.

```sh
mix test.watch
```
```elixir
# test/badger_test.exs
defmodule BadgerTest do
  use ExUnit.Case
  doctest Badger

  test "the truth" do
    assert 1 + 1 == 2
  end
end
```

Here is ExUnit, which is the testing library that comes with Elixir.

```elixir
# test/badger_test.exs
defmodule BadgerTest do
  use ExUnit.Case
  doctest Badger

  test "the truth" do
    assert "Hello" == "hello"
  end
end
```

If I make the test fail and save the file the tests will be re-run, and I'm
alerted to the problem.

Also in the output the value is diffed with the expected value so we can
easily see what is wrong. The Elixir ecosystem wants to make this as easy as
possible.

```elixir
# test/badger_test.exs
defmodule BadgerTest do
  use ExUnit.Case
  doctest Badger

  test "the truth" do
    assert "Hello" == "hello"
  end

  test "in operator" do
    assert 1 in [2, 3]
  end
end
```

I'll add a new failing test here.

Notice how that this test is not an equality test, and we still get helpful
debugging information specific to what we're trying to do here.

Also both times we just used the `assert` macro, not an `assert_equal` or an
`assert_includes` function. We can just write normal code, not a special
testing mini-language.

ExUnit isn't special in Elixir, it doesn't have any special compiler magic to
detect what the assertion code, instead it uses the macro system. It would be
trivial to write your own assertion macros that worked the same way.

Elixir's macro system is the same sort of Lisp macro system that can be found
in languages such as Clojure. The idea is that code is just data, and we can
inspect and re-write the code as compile time. It's kind of like Babel, easier
and built into the language itself.

Good test tooling is important for short and long term productivity. Let's
look at application code and documentation.

Elixir documentation is generally fantastic.

https://hexdocs.pm/plug/readme.html

Here are the docs for Plug, which is a web server abstraction in the style of
Rack or the core of Express.

This documentation is generated from Plug's source code using Elixir's
first-class documentation features. Rather than using comments Elixir code has
documentation attributes baked into the language, making accessing
documentation easy.

```elixir
defmodule Badger do
  @moduledoc """
  Red Badger **Rulez!**
  """
end
```elixir
# iex -S mix
h Badger
```

If I add some documentation attributes to the code I can then access them from
the shell using the `h` macro. Markdown is supported in documentation.

```elixir
h Enum
```

The documentation for library and standard library code can also be accessed
like this, so you don't need to break focus and switch to a browser to look
things up.

```elixir
defmodule Badger do
  @moduledoc """
  Red Badger **Rulez!**
  """

  @doc """
  Reverse a string

  # Examples

      iex> Badger.reverse("abc")
      "cba"
  """
  def reverse(string) do
    string
  end
end
```
```elixir
h Badger.reverse
```

It also works for functions. Function documentation often includes examples,
like so. These documentation code snippets are converted into unit tests, so
your documentation code will always be correct.

```sh
mix test.watch
```
```elixir
  def reverse(string) do
    string
    |> String.graphemes
    |> Enum.reverse
    |> Enum.join("")
  end
```

So as you can see, there is a real focus on good tooling and documentation.
Coupled with a Ruby style batteries included standard library, immutable
functional programming, and a friendly community it is easy to get up to speed
and be productive with Elixir.

```elixir
defmodule Badger do
end
```

Elixir is a a backend language, so let's make a microservice.

To do this I'll grab the Cowboy webserver, and Plug the server middleware
abstraction.

```elixir
# Mixfile
  defp deps do
    [
      {:mix_test_watch, ">= 0.0.0"},
      {:plug, ">= 0.0.0"},
      {:cowboy, ">= 0.0.0"},
    ]
  end
```
```sh
mix deps.get
mix compile
```

As you can see compilation is much faster than compiling Javascript, and
unlike Ruby it boots quickly.

```elixir
defmodule Badger do
  use Plug.Router

  plug :match
  plug Plug.Logger
  plug :dispatch

  get "/hello/:name" do
    conn
    |> send_resp(200, "Hello, #{name}\n")
  end

  match _ do
    conn
    |> send_resp(404, "Not found!\n")
  end
end
```

Here is a tiny server. It uses a middleware for matching a request, middleware
for logging, and another for dispatching a response.

Lastly it has an endpoint that says hello, and a 404 catch all.

```elixir
# iex -S mix
{:ok, server} = Plug.Adapters.Cowboy.http Badger, [port: 4000]
```

I can start the server and hit it a few times.

```sh
curl localhost:4000/hello/Badgers
```

This isn't very exciting, we've all seen servers before. One thing to note is
that this server is *fast*. Even running in the slower dev mode it's
responding in ~30 micro seconds. Granted we're only returning a string here,
but you certainly can't get that fast with Ruby or Javascript.

What's more, Elixir garbage collection is run on a per-thread basis, and your
program is likely to have thousands if not hundreds of thousands of threads
running concurrently, so the latency is predictable. Your 99th slowest
percentile will still be fast as no one gets caught by a system wide garbage
collection.

```elixir
  require EEx

  template = """
  <!DOCTYPE html>
  <title>Elixir!</title>
  <h1>
    Hello, <%= name %>!
  </h1>
  """
  EEx.function_from_string :defp, :view, template, [:name]

  get "/hello/:name" do
    conn
    |> send_resp(200, view(name))
  end
```

We can add templates too.

The built in templating library is called EEx, and like much of the rest of
Elixir it is built using Lisp style macros. Templates run at compile time, so
all the work of parsing and optimising the template is done then instead of at
runtime so it's blazingly fast.

I went into how the EEx compiler works in my Elixir conference talk if you'd
like to see how it works.
