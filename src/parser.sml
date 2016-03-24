structure Parser :> PARSER =
  struct
    open Token

    fun parse token stream =
      let
        fun array stream =
          let
            fun valueNext stream result =
              case value stream of
                SOME (value, s) => loop s (value :: result)
              | _ => NONE

            and loop stream result =
              case token stream of
                SOME (RSQUARE, stream) => SOME (JSON.ARRAY (rev result), stream)
              | SOME (COMMA, stream) => valueNext stream result
              | _ => NONE
          in
            case token stream of
              NONE => NONE
            | SOME (RSQUARE, stream) => SOME (JSON.ARRAY [], stream)
            | SOME _ => valueNext stream []
          end

        and object stream =
          let
            fun valueNext stream result key =
              case value stream of
                SOME (value, s) => loop s ((key, value) :: result)
              | _ => NONE

            and colonNext stream result key =
              case token stream of
                SOME (COLON, stream) => valueNext stream result key
              | _ => NONE

            and stringNext stream result =
              case token stream of
                SOME (STRING key, stream) => colonNext stream result key
              | _ => NONE

            and loop stream result =
              case token stream of
                SOME (RCURLY, stream) => SOME (JSON.OBJECT result, stream)
              | SOME (COMMA, stream) => stringNext stream result
              | _ => NONE
          in
            case token stream of
              SOME (RCURLY, stream) => SOME (JSON.OBJECT [], stream)
            | SOME (STRING key, stream) => colonNext stream [] key
            | _ => NONE
          end

        and value stream =
          case token stream of
            SOME (LCURLY, stream) => object stream
          | SOME (LSQUARE, stream) => array stream
          | SOME (NUMBER n, stream) => SOME (JSON.NUMBER n, stream)
          | SOME (STRING s, stream) => SOME (JSON.STRING s, stream)
          | SOME (FALSE, stream) => SOME (JSON.FALSE, stream)
          | SOME (TRUE, stream) => SOME (JSON.TRUE, stream)
          | SOME (NULL, stream) => SOME (JSON.NULL, stream)
          | _ => NONE
      in
        value stream
      end
  end
