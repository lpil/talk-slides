pub external type SentConn;
pub external type NewConn;

pub enum Method =
  | Get
  | Post
  | Patch
  | Delete
  | Options

pub external fn method(NewConn) -> Method
  = "plug_native" "method"

pub external fn send_resp(NewConn, Int, String) -> SentConn
  = "Elixir.Plug.Conn" "send_resp"

pub external fn path_info(NewConn) -> List(String)
  = "plug_native" "path_info"
