structure JSON : JSON =
  struct
    open Fn.Syntax.|> infix |>

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
