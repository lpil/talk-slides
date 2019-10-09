import gleam/string

pub struct Header {
  name: String
  value: String
}

pub struct Response {
  status: Int
  headers: List(Header)
  body: String
}

pub external type Request

pub external fn path(Request) -> List(String)
  = "elli_request" "path"

pub fn handle(req, _arg) -> Response {
  case path(req) {
  | [] ->
      Response(status: 200, headers: [], body: "Welcome home")

  | ["hello", name] ->
      Response(status: 200, headers: [], body: string.append("Hello ", name))

  | _ ->
      Response(status: 404, headers: [], body: "There's nothing here...")
  }
}

pub enum Unit =
| Ok

pub fn handle_event(_event, _data, _arg) -> Unit {
  Ok
}
