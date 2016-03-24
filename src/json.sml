structure JSON =
  struct
    datatype ('n, 's) t =
      OBJECT of ('s * ('n, 's) t) list
    | ARRAY of ('n, 's) t list
    | NUMBER of 'n
    | STRING of 's
    | FALSE
    | TRUE
    | NULL
  end
