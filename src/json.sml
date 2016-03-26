structure JSON : JSON =
  struct
    datatype t = datatype Value.t

    type number = Lexer.NumberLexer.repr
    type string = Lexer.StringLexer.repr

    fun decodeString text =
      let
        val bytes = Word8VectorSlice.full (Byte.stringToBytes text)
        val lex = Lexer.lex Encoding.UTF8 Word8VectorSlice.getItem
        val parse = Parser.parse lex
      in
        case parse bytes of
          NONE => NONE
        | SOME (json, bytes) =>
          case lex bytes of
            NONE => SOME json
          | SOME _ => raise Fail "syntax error"
      end

    fun encodeString { spaces } json =
      let
        val (nl, colon, comma) =
          if spaces = "" then ("", ":", ",") else ("\n", ": ", ",\n")

        fun pad isMember n =
          if isMember then "" else String.repeat n spaces

        fun compound { level, isMember } elems start stop mapper =
          let
            val state = { level = level + 1, isMember = false }
          in
            String.concat [
              pad isMember level,
              start,
              nl,
              String.concatWith comma (List.map (mapper state) elems),
              nl,
              pad false level,
              stop
            ]
          end

        fun encodePair { level, isMember } (name, value) =
          String.concat [
            pad isMember level,
            StringFormat.format name,
            colon,
            loop { level = level, isMember = true } value
          ]

        and loop (state as { level, isMember }) json =
          let
            val indent = pad isMember level
          in
            case json of
              OBJECT pairs => compound state pairs "{" "}" encodePair
            | ARRAY elems => compound state elems "[" "]" loop
            | NUMBER n => indent ^ RealFormat.format n
            | STRING s => indent ^ StringFormat.format s
            | FALSE => indent ^ "false"
            | TRUE => indent ^ "true"
            | NULL => indent ^ "null"
          end
      in
        loop { level = 0, isMember = false } json
      end

    fun decodeFile path =
      let
        val file = BinIO.openIn path
        fun cleanup () = BinIO.closeIn file
        val lex = Lexer.lex Encoding.UTF8 BinIO.StreamIO.input1
        val parse = Parser.parse lex
      in
        let in
          case parse (BinIO.getInstream file) of
            NONE => NONE
          | SOME (json, bytes) =>
            case lex bytes of
              NONE => SOME json
            | SOME _ => raise Fail "syntax error"
        end
          handle e => (cleanup () ; raise e)
          before cleanup ()
      end
  end
