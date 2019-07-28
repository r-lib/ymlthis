read_rmd <- function(path, output = c("frontmatter", "body")) {
  output <- match.arg(output)
  input_lines <- readLines(path)
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_lines)
  if (!validate_front_matter(delimiters, input_lines)) {
    return(yml_blank())
  }

  return_lines <- (delimiters[1]):(delimiters[2])
  if (output == "body") return_lines <- -return_lines

  input_lines[return_lines]
}

is_blank <- function(x) {
  purrr::is_empty(x) || all(grepl("^\\s*$", x))
}

validate_front_matter <- function(delimiters, input_lines) {
  #  a few conditions
  more_than_one <- length(delimiters) >= 2
  two_after_one <- (delimiters[2] - delimiters[1] > 1)
  all_spaces <- grepl("^---\\s*$", input_lines[delimiters[1]])

  if (more_than_one && two_after_one && all_spaces) {
   valid <- ifelse(
      # if first line, valid
      delimiters[1] == 1,
      TRUE,
      # if not, check that what's before is blank
      is_blank(input_lines[1:delimiters[1] - 1])
    )
   return(valid)
  }

  # if none of these, YAML is not validated
  FALSE
}
