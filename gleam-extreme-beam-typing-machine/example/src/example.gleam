import plug

pub fn init(arg) {
  arg
}

pub fn call(conn, _arg) {
  plug:send_resp(conn, 200, "Hello, world!")
}
