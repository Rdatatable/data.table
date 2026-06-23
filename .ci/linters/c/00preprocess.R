.preprocess = function (f) list(
  path = f, lines = readLines(f),
  preprocessed = system2(
    "gcc", shQuote(c("-fpreprocessed", "-E", f)),
    stdout = TRUE, stderr = FALSE
  )
)
