structure Token =
  struct
    datatype ('n, 's) t =
      NUMBER of 'n
    | STRING of 's
    | NULL
    | TRUE
    | FALSE
    | COLON
    | COMMA
    | LSQUARE
    | RSQUARE
    | LCURLY
    | RCURLY
  end
