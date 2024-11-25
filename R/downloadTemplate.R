#' Download the clickStories Template
#'
#' This function allows users to access or download the clickStories template Excel file.
#'
#' @param destfile A character string specifying the destination file path.
#'                 If NULL, the function will return the file path within the package.
#' @param overwrite Logical. If TRUE, overwrite an existing file at `destfile`.
#' @return Returns the path to the file. If `destfile` is provided, the file is copied to the specified location.
#' @examples
#' # Copy the template to your working directory
#' download_clickStories_template("clickStories_template.xlsx")
#'
#' # Get the file path within the package
#' download_clickStories_template()
download_clickStories_template <- function(destfile = NULL, overwrite = FALSE) {
  # Locate the file in the package
  template_path <- system.file("extdata", "clickStories_template.xlsx", package = "clickStories")

  if (template_path == "") {
    stop("Template file not found in the package. Please ensure the file is in 'inst/extdata'.")
  }

  # If a destination file path is provided, copy the file
  if (!is.null(destfile)) {
    if (file.exists(destfile) && !overwrite) {
      stop("File already exists at the destination. Use `overwrite = TRUE` to overwrite.")
    }
    file.copy(template_path, destfile, overwrite = overwrite)
    message("Template copied to ", destfile)
    return(destfile)
  }

  # If no destination is provided, return the internal path
  return(template_path)
}
