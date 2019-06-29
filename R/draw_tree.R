#' Draw an tree of YAML hierarchy
#'
#' @param .yml
#' @param indent
#'
#' @return
#' @export
#'
#' @examples
draw_yml_tree <- function(.yml, indent = "") {
  nested <- purrr::map_lgl(.yml, is.list)
  for (i in seq_along(.yml)) {
    if (i == length(.yml)) {
      if (nested[i]) {
        if (!rlang::is_named(.yml[i])) {
          print_vector_leaves(.yml[[i]], indent)
          next
        }

        if (is_long_vector(.yml[[i]])) {
          print_vector_leaves(.yml[[i]], indent)
          next
        }

        leaf <- .yml[i] %>%
          color_yml() %>%
          split_pluck() %>%
          purrr::pluck(1)

        cat(paste0(indent, end_tab(), leaf, "\n"))
        draw_yml_tree(.yml[[i]], paste0(indent, "    "))

      } else {
        leaf <- color_yml(.yml[i])
        cat(paste0(indent, end_tab(), leaf))
      }
    } else {
      if (nested[i]) {
        if (!rlang::is_named(.yml[i])) {
          print_vector_leaves(.yml[[i]], indent)
          next
        }

        if (is_long_vector(.yml[[i]])) {
          marker <- ifelse(i != length(.yml), pipe(), " ")
          print_vector_leaves(.yml[[i]], paste0(indent, marker, "   "))
          next
        }

        leaf <- .yml[i] %>%
          color_yml() %>%
          split_pluck() %>%
          purrr::pluck(1)

        cat(paste0(indent, tab(), leaf, "\n"))
        draw_yml_tree(.yml[[i]], paste0(indent, pipe(), "   "))
      } else {
        if (!rlang::is_named(.yml[i])) {
          print_vector_leaves(.yml[[i]], indent)
          next
        }

        if (is_long_vector(.yml[[i]])) {
          marker <- ifelse(i != length(.yml), pipe(), "")
          print_vector_leaves(.yml[i], paste0(indent, marker, "   "))
          next
        }
        leaf <- color_yml(.yml[i])
        leaf_indent <- paste0(indent, tab())
        cat(paste0(leaf_indent, leaf))
      }
    }
  }

  invisible(.yml)
}

is_long_vector <- function(x) {
  is.atomic(x) && length(x) > 1
}

print_vector_leaves <- function(x, indent) {
  if (is.atomic(x)) {
    leaf <- color_yml(x) %>%
      split_pluck()
  } else {
    leaf <- vector("character", length(x))
    for (i in seq_along(x)) {
      leaf[i] <- color_yml(x[i]) %>%
        split_pluck() %>%
        purrr::pluck(1)
    }

  }

  for (i in seq_along(x)) {
    if (i == length(x)) {
      cat(paste0(indent, end_tab(), leaf[i], "\n"))
    } else {
      cat(paste0(indent, tab(), leaf[i], "\n"))
    }
    if (is_long_vector(x[[i]])) {
      marker <- ifelse(i != length(x), pipe(), " ")
      print_vector_leaves(x[[i]], paste0(indent, marker, "   "))
    }
  }
}

pipe <- function() "│"
tab <- function() "├── "
end_tab <- function() "└── "
