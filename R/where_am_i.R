#' Where am I?
#'
#' Visualize your current working directory.
#'
#' @export
where_am_i <- function(wd = getwd(), start = NULL, tilde = TRUE) {
  if (!fs::dir_exists(wd)) stop("Directory does not exist")

  wd <- fs::path_abs(wd)

  # Determine starting point
  if (is.null(start)) {
    start <- get_start(path = wd)
  } else {
    start <- fs::path_abs(start)
    # Need some error handling for case in which start is not upstream of wd
  }

  if (basename(start) != "") {
    wd <- stringr::str_replace(wd, start, basename(start))
  }
  if (tilde) {
    home_regex <- glue::glue("^{home}", home = fs::path_expand("~"))
    wd <- stringr::str_replace(wd, home_regex, "~")
  }

  nodes <- fs::path_split(wd)[[1]]
  edges <- c(as.list(nodes[-1]), list(character(0)))
  dir_str <- data.frame(nodes, I(edges), stringsAsFactors = FALSE)
  dir_str$label <- dir_str$nodes
  dir_str$label[nrow(dir_str)] <- crayon::underline(dir_str$label[nrow(dir_str)])
  print(cli::tree(dir_str))
  # cat(crayon::red("working directory: "))
  return(invisible(wd))
}

get_start <- function(path = ".") {
  path <- fs::path_real(path)

  # Try to find a project root
  proj_root <- get_proj_root(path = path)
  if (!is.na(proj_root)) {
    return(fs::path_real(proj_root))
  }

  # Try to find OS root
  os_root <- fs::path_split(path)[[1]][1]
  len <- nchar(os_root)
  if (stringr::str_sub(path, end = len) == os_root) {
    return(os_root)
  }

  stop("Unable to find a suitable starting point for the following path. ",
       "Try manually specifying a starting path with the argument \"start\". ",
       "Input: ", path)
}


# Use rprojroot to find a project root if it exists. Otherwise return
# NA_character_.
get_proj_root <- function(path = ".") {
  root <- try(silent = TRUE, rprojroot::find_root(path = path,
      is_rstudio_project |
      is_r_package |
      is_git_root |
      is_svn_root |
      is_vcs_root |
      is_remake_project |
      is_projectile_project |
      is_testthat
  ))
  if (class(root) == "try-error") {
    return(NA_character_)
  }
  return(root)
}
