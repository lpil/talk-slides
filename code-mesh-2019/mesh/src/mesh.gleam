fn append(a: String, b: String) {
  a
}

struct User {
  name: String
}

enum Nullable(value) {
  Just(value)
  Null
}

fn get_user_name(user) {
  let User(name: name) = user
  case name {
    "" -> Null
    _ -> Just(name)
  }
}

pub fn page_title(user) {
  append("Hello, ", get_user_name(user))
}
