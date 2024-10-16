#'
#' This function allows the user to generate a list of panels from a CSV or XLSX file.
#' If the column names in the data file do not match the required fields, and the user hasn't provided `column_mappings`,
#' the function will prompt the user to input the correct column names using `readline()`.
#' The function also supports various visualization types, transforming Figma embeds to the correct prototype format.
#'
#' @param file The path to the CSV or XLSX file that contains the panel data.
#' @param column_mappings A named list specifying the mapping from required field names to the column names in your data file.
#'        If `NULL` and the dataset's columns do not match the expected columns, the user will be prompted interactively.
#'        The list should have the following keys: 'name', 'takeaway', 'text', 'vizType', 'viz', and 'alt'.
#'        If the 'text' column is missing, you can set it to `NA_character_` to handle it as optional.
#'
#' @return A list of panels, where each panel is a list containing:
#' \describe{
#'   \item{name}{The panel's name.}
#'   \item{takeaway}{The main takeaway sentence for the panel.}
#'   \item{text}{Additional context or explanations for the panel. If empty or missing, it will be set to an empty string.}
#'   \item{vizType}{The type of visualization to include ("embed", "image-link", "image").}
#'   \item{viz}{The content of the visualization (embed code, external image link, or local image path). Figma design links
#'              are transformed to prototype links.}
#'   \item{alt}{Alt text for the visualization, for accessibility purposes.}
#' }
#'
#' @importFrom tools file_ext
#' @importFrom readxl read_xlsx
#' @importFrom stats setNames
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' # Example 1: When the column names in the data match the required fields
#' # Assuming the CSV file has columns named 'name', 'takeaway', 'text', 'vizType', 'viz', 'alt'
#' panels <- create_panels_list("path/to/your/file.csv")
#'
#' # Example with a standard CSV file and no manual input (with automatic column mappings):
#' panels <- create_panels_list('panels.csv', column_mappings = list(
#'   name = 'panel_name',
#'   takeaway = 'main_takeaway',
#'   text = NA_character_,  # Optional column
#'   vizType = 'viz_type',
#'   viz = 'visualization',
#'   alt = 'alt_text'
#' ))
#'
#' # Example with an XLSX file and interactive prompt
#' (requires user input if columns are not standard):
#' panels <- create_panels_list('panels.xlsx')
#' }
#'
#' @export

create_panels_list <- function(file, column_mappings = NULL) {

  expected_columns <- c("name", "takeaway", "text", "vizType", "viz", "alt")

  # Extract the file extension
  file_extension <- tools::file_ext(file)

  # Read the data based on the file extension
  if (tolower(file_extension) == "csv") {
    panel_data <- read.csv(file)
  } else if (tolower(file_extension) == "xlsx") {
    panel_data <- readxl::read_xlsx(file)
  } else {
    stop("Unsupported file type. Please provide a CSV or XLSX file.")
  }

  # Check if column_mappings are provided or not
  if (is.null(column_mappings)) {
    # Check if the dataset's columns match the expected columns
    dataset_columns <- colnames(panel_data)
    missing_columns <- setdiff(expected_columns, dataset_columns)

    # If columns don't match, prompt the user
    if (length(missing_columns) > 0) {
      column_mappings <- list(
        name = readline("Please provide the column name for 'name': "),
        takeaway = readline("Please provide the column name for 'takeaway': "),
        vizType = readline("Please provide the column name for 'vizType': "),
        viz = readline("Please provide the column name for 'viz': "),
        alt = readline("Please provide the column name for 'alt': "),
        text = readline("Please provide the column name for 'text': ")
      )
    } else {
      # If columns match, use the dataset column names directly
      column_mappings <- as.list(setNames(dataset_columns, expected_columns))
    }
  }

  # Handle missing 'text' column by checking its existence in the column_mappings
  if (!"text" %in% names(column_mappings) || is.na(column_mappings$text)) {
    column_mappings$text <- NULL
  }

  # Create the panels list based on the detected or provided column mappings
  panels <- lapply(1:nrow(panel_data), function(i) {
    list(
      name = panel_data[[column_mappings$name]][i],
      takeaway = panel_data[[column_mappings$takeaway]][i],
      text = if (!is.null(column_mappings$text)) panel_data[[column_mappings$text]][i] else NULL,
      vizType = panel_data[[column_mappings$vizType]][i],
      viz = panel_data[[column_mappings$viz]][i],
      alt = panel_data[[column_mappings$alt]][i]
    )
  })

  return(panels)
}




#' Transform Figma Embed URL to Prototype Format
#'
#' This internal helper function transforms Figma iframe embed URLs into the
#' standardized prototype format. If the Figma URL is a design link, it is converted
#' to a prototype link, and parameters such as `scaling`, `content-scaling`, `hide-ui`,
#' and `show-proto-sidebar` are adjusted or added as necessary.
#'
#' @param embed_code The iframe embed code containing a Figma embed URL.
#' @return A modified iframe embed code with the required parameters for the Figma prototype.
#' If the embed is not a Figma embed, the original embed code is returned unaltered.
#' @keywords internal
#' @examples
#' \dontrun{
#' figma_embed <- '<iframe style="border: 1px solid rgba(0, 0, 0, 0.1);"
#' width="800" height="450" src=
#' "https://embed.figma.com/design/3BByo3UqianyYN2GaDmePf/Profile-Data-Stories---
#' Brand-Colors?node-id=38-1173&embed-host=share"
#' allowfullscreen></iframe>'
#' transformed_embed <- transform_figma_embed(figma_embed)
#' }
#'
transform_figma_embed <- function(embed_code) {
  # Check if the embed_code contains a Figma iframe link
  if (grepl("embed.figma.com", embed_code)) {
    # Extract the src URL from the original iframe
    src_url <- sub('.*src="([^"]*)".*', '\\1', embed_code)

    # Ensure that the Figma URL is a "proto" URL and not a "design" URL
    if (grepl("/design/", src_url)) {
      src_url <- sub("/design/", "/proto/", src_url)
    }

    # Modify the necessary parameters in the URL
    src_url <- sub("scaling=min-zoom", "scaling=fit-width", src_url)
    src_url <- sub("content-scaling=fixed", "content-scaling=proportional", src_url)
    src_url <- sub("show-proto-sidebar=1", "show-proto-sidebar=0", src_url)

    # Add missing parameters if they are not present in the URL
    if (!grepl("hide-ui=1", src_url)) {
      src_url <- paste0(src_url, "&hide-ui=1")
    }
    if (!grepl("embed-host=share", src_url)) {
      src_url <- paste0(src_url, "&embed-host=share")
    }

    # Construct the new Figma embed with updated parameters
    new_iframe <- paste0(
      '<iframe width="100%" height="550vh" src="', src_url,
      '" allowfullscreen></iframe>'
    )

    return(new_iframe)
  }

  # If not a Figma embed, return the original embed code
  return(embed_code)
}





