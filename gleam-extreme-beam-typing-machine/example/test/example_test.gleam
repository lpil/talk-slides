import example
import expect

pub fn hello_world_test() {
  example:init(1)
  |> expect:equal(_, 1)
}
