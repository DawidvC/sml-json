signature ENCODING =
  sig
    datatype t =
      UTF8
    | UTF16BE
    | UTF16LE
    | UTF32BE
    | UTF32LE

    val guess : (Word8.word, 's) StringCvt.reader -> 's -> t option
  end
