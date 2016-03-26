structure JSONFormat :> JSON_FORMAT =
  struct
    type outer = { indentLevel : int, isMember : bool }

    type 'inner stream = 'inner * outer

    datatype indent_char = SPACE | TAB

    fun stream inner =
      (inner, { indentLevel = 0, isMember = false })

    fun indent (inner, { indentLevel, isMember }) =
      (inner, { indentLevel = indentLevel + 1, isMember = isMember })

    fun dedent (inner, { indentLevel, isMember }) =
      (inner, { indentLevel = indentLevel - 1, isMember = isMember })

    fun setMember (inner, { indentLevel, isMember }) =
      (inner, { indentLevel = indentLevel, isMember = true })

    fun unsetMember (inner, { indentLevel, isMember })  =
      (inner, { indentLevel = indentLevel, isMember = false })

    fun format { numberFormat, stringFormat, indentWidth, indentChar } token stream =
      let
        val indentChar = case indentChar of SPACE => " " | TAB => "\t"
        val newLine = if indentWidth = 0 then "" else "\n"
        val colon = if indentWidth = 0 then ":" else ": "

        fun repeat n = String.repeat (indentWidth * n) indentChar

        fun format (token, stream as (inner, { indentLevel, isMember })) =
          let
            open Token
            fun pad n = if isMember then "" else repeat n
            val padding = pad indentLevel
          in
            case token of
              NUMBER n => SOME (padding ^ numberFormat n, unsetMember stream)
            | STRING s => SOME (padding ^ stringFormat s, unsetMember stream)
            | NULL     => SOME (padding ^ "null", unsetMember stream)
            | TRUE     => SOME (padding ^ "true", unsetMember stream)
            | FALSE    => SOME (padding ^ "false", unsetMember stream)
            | COLON    => SOME (colon, setMember stream)
            | COMMA    => SOME ("," ^ newLine, unsetMember stream)
            | LSQUARE  => SOME (padding ^ "[" ^ newLine, unsetMember (indent stream))
            | LCURLY   => SOME (padding ^ "{" ^ newLine, unsetMember (indent stream))
            | RSQUARE  => SOME ("" ^ newLine ^ pad (indentLevel - 1) ^ "]", unsetMember (dedent stream))
            | RCURLY   => SOME ("" ^ newLine ^ pad (indentLevel - 1) ^ "}", unsetMember (dedent stream))
          end
      in
        Option.mapPartial format (token stream)
      end
  end
