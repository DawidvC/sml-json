signature ENCODING =
  sig
    datatype t =
      UTF8
    | UTF16BE
    | UTF16LE
    | UTF32BE
    | UTF32LE
    | UNKNOWN

    val guess : (Word8.word, 's) StringCvt.reader -> 's -> t
  end
