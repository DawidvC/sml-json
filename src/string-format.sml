structure StringFormat =
  let
    val escape =
      String.translate let in
        fn #"\"" => "\\\""
         | #"\\" => "\\\\"
         | #"\b" => "\\b"
         | #"\f" => "\\f"
         | #"\n" => "\\n"
         | #"\r" => "\\r"
         | #"\t" => "\\t"
         | char => String.str char
      end
  in
    struct
      fun format s = "\"" ^ escape s ^ "\""
    end
  end
