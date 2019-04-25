import plug
import str

pub fn init(arg) {
  arg
}

pub fn call(conn, _arg) {
  case {plug:method(conn), plug:path_info(conn)} {
  | {plug:Get, []} ->
      plug:send_resp(conn, 200, "Home page")

  | {plug:Get, ["profile"]} ->
      plug:send_resp(conn, 200, "Welcome to your profile!")

  | {plug:Get, ["profile", name]} ->
      plug:send_resp(conn, 200, str:append("Welcome to the profile of ", name))

  | _ ->
      plug:send_resp(conn, 404, "Nothing here!")
  }
}
