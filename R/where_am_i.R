#' Where am I?
#'
#' Visualize your current working directory.
#'
#' @export
where_am_i <- function(wd = getwd(), start = c("home", "project", "root")) {
  wd <- fs::path_abs(wd)
  nodes <- fs::path_split(wd)[[1]]
  edges <- c(as.list(nodes[-1]), list(character(0)))
  dir_str <- data.frame(nodes, I(edges), stringsAsFactors = FALSE)
  dir_str$label <- dir_str$nodes
  dir_str$label[nrow(dir_str)] <- crayon::underline(dir_str$label[nrow(dir_str)])
  start_list <- list()
  start_list$home <- basename(fs::path_expand("~"))
  start_list$project <- get_proj_root()
  start_list$project <- basename(start_list$project)
  start_list$root <- nodes[1]
  print(cli::tree(dir_str, root = start_list[[start[1]]]))
  # cat(crayon::red("working directory: "))
  return(invisible(wd))
}

# Use rprojroot to find a project root if it exists. Otherwise return
# NA_character_.
get_proj_root <- function() {
  root <- try(silent = TRUE, rprojroot::find_root(
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
