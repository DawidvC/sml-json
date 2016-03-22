functor StringLexer(String : STRING) : AUX_LEXER =
  struct
    type repr = String.string

    fun lex decode stream = raise Fail "not implemented"
  end
