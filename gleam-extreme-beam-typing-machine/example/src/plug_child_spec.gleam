import example
import plug

pub external type ChildSpec;

enum Scheme =
  | Http
  | Https

enum Option =
  | Port(Int)

enum Arg(plug_arg) =
  | Scheme(Scheme)
  | Options(List(Option))
  | Plug(module {
      fn init(plug_arg) -> plug_arg
      fn call(plug:NewConn, plug_arg) -> plug:SentConn
    })

external fn erl_child_spec(List(Arg(a))) -> ChildSpec
  = "Elixir.Plug.Cowboy" "child_spec"

pub fn child_spec() {
  erl_child_spec([
    Scheme(Http),
    Plug(example),
    Options([Port(8080)])
  ])
}
