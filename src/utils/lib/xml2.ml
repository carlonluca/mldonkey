let buffer_escape b text =
  let l = String.length text in
  for p = 0 to l-1 do 
    match text.[p] with
    | '>' -> Buffer.add_string b "&gt;"
    | '<' -> Buffer.add_string b "&lt;"
    | '&' -> Buffer.add_string b "&amp;"
    | '\'' -> Buffer.add_string b "&apos;"
    | '"' -> Buffer.add_string b "&quot;"
    | '\x0A' -> Buffer.add_string b "&#x0A;"
    | '\x0D' -> Buffer.add_string b "&#x0D;"
    | c -> Buffer.add_char b c
  done

let escape s =
  let b = Buffer.create (String.length s) in
  buffer_escape b s;
  Buffer.contents b