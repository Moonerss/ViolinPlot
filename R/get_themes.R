# Themes ------------------------------------------------------------------


#' @importFrom ggplot2 theme_bw theme_classic theme_dark theme_gray theme_grey 
#'  theme_light theme_linedraw theme_minimal theme_void
default_themes <- function() {
  ggplot2 <- c("bw", "classic", "dark", "gray",
               "light", "linedraw", "minimal",
               "void")
  ggplot2 <- setNames(as.list(paste0("theme_", ggplot2)), ggplot2)
  
  themes <- list(
    ggplot2 = ggplot2
  )
  if (requireNamespace("ggthemes", quietly = TRUE)) {
    ggthemes <- c(
      "base", "calc", "economist", "economist_white",
      "excel", "few", "fivethirtyeight", "foundation",
      "gdocs", "hc", "igray", "map", "pander",
      "par", "solarized", "solarized_2", "solid",
      "stata", "tufte", "wsj"
    )
    ggthemes <- setNames(as.list(paste0("ggthemes::theme_", ggthemes)), ggthemes)
    themes$ggthemes <- ggthemes
  }
  
  # if (requireNamespace("hrbrthemes", quietly = TRUE)) {
  #   hrbrthemes <- c(
  #     "ft_rc", "ipsum", "ipsum_ps", "ipsum_rc", "ipsum_tw", "modern_rc"
  #   )
  #   hrbrthemes <- setNames(as.list(paste0("hrbrthemes::theme_", hrbrthemes)), hrbrthemes)
  #   themes$hrbrthemes <- hrbrthemes
  # }
  
  return(themes)
}

check_theme_exist <- function(x, package = "ggplot2") {
  vapply(X = x, FUN = function(fun) {
    if (grepl(pattern = "::", x = fun)) {
      x <- strsplit(x = fun, split = "::")[[1]]
      fun <- x[2]
      package <- x[1]
      exists(fun, where = asNamespace(package), mode = "function")
    } else {
      if (!startsWith(fun, "theme_"))
        fun <- paste0("theme_", fun)
      exists(fun, where = asNamespace(package), mode = "function")
    }
  }, FUN.VALUE = logical(1), USE.NAMES = FALSE)
}


# Get list of themes
get_themes <- function() {
  themes <- getOption("esquisse.themes")
  if (is.null(themes))
    themes <- default_themes()
  if (is.function(themes))
    themes <- themes()
  if (!is.list(themes)) {
    stop("Option 'esquisse.themes' must be a list", call. = FALSE)
  }
  themes <- rapply(
    object = themes, 
    f = function(x) {
      if (all(check_theme_exist(x))) {
        x
      } else {
        warning(paste("Theme", x, "not found!"), call. = FALSE)
        NULL
      }
    }, how = "list"
  )
  dropNullsOrEmptyRecursive(themes)
}


# utilities borrowed from shiny
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}
nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0
}
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE = logical(1))]
}
dropNullsOrEmptyRecursive <- function(x) {
  dropNullsOrEmpty(lapply(
    X = x,
    FUN = function(x) {
      if (is.list(x)) {
        dropNullsOrEmpty(x)
      } else {
        x
      }
    }
  ))
}
