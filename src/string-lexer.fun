functor StringLexer(String : STRING) : PARALEXER =
  struct
    type repr = String.string

    fun lex decode stream = raise Fail "not implemented"
  end
