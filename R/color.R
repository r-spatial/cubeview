## hex color conversion
col2Hex <- function(col, alpha = FALSE) {

  mat <- grDevices::col2rgb(col, alpha = TRUE)
  if (alpha) {
    if (all(grepl("^#[0-9a-fA-F]+", col)) & nchar(col[1] == 9)) {
      return(col)
    } else {
      return(grDevices::rgb(mat[1, ]/255, mat[2, ]/255,
                            mat[3, ]/255, mat[4, ]/255))
    }
  } else {
    if (all(grepl("^#[0-9a-fA-F]+", col)) & nchar(col[1] == 7)) {
      return(col)
    } else {
      return(grDevices::rgb(mat[1, ]/255, mat[2, ]/255, mat[3, ]/255))
    }
  }
}


createId = function(ndigits = 6) {
  paste(sample(c(letters[1:6], 0:9), ndigits), collapse = "")
}
