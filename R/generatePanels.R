#' Generate Panels from CSV/XLSX File with User Prompts for Column Mapping
#'
#' This function reads data from a CSV or XLSX file and returns a list of panels
#' to be used in creating a Quarto Reveal.js presentation.
#' If the required column names don't match, it prompts the user to provide the correct column names.
#'
#' @param data_file The path to the CSV or XLSX file that contains the panel data.
#' @param col_mapping A named list or vector specifying the mapping from required field names
#'   to the column names in your data file. Defaults to the required field names.
#'
#' @return A list of panels, where each panel is a list containing:
#'   \describe{
#'     \item{name}{The panel's name.}
#'     \item{takeaway}{The main takeaway sentence for the panel.}
#'     \item{text}{Additional context or explanations for the panel. If empty or missing, it will be set to an empty string.}
#'     \item{vizType}{The type of visualization to include ("embed", "image-link", or "image").}
#'     \item{viz}{The content of the visualization (embed code, external image link, or local image path).}
#'     \item{alt}{Alt text for the visualization, for accessibility purposes.}
#'     }
#'
#' @import readr
#' @import readxl
#' @export
#'
#' @description This function allows the user to generate a list of panels from a CSV or XLSX file.
#' If the column names in the data file do not match the required fields, the function will
#' prompt the user to input the correct column names.
#'
#' @examples
#' \dontrun{
#' # Example 1: When the column names in the data match the required fields
#' # Assuming the CSV file has columns named 'name', 'takeaway', 'text', 'vizType', 'viz', 'alt'
#' panels <- create_panels_list("path/to/your/file.csv")
#'
#' # Now you can use 'panels' in the create_story function
#' clickStories::create_story(
#'   story_title = "Your Story Title",
#'   output_dir = "path/to/output/dir",
#'   name = "story_name",
#'   logo = "path/to/logo.png",
#'   style = "path/to/style.scss",
#'   panels = panels
#' )
#'
#' # Example 2: When the column names don't match and you are prompted to provide the correct ones
#' # Assuming the CSV file has columns: 'panel_id', 'main_point', 'description',
#'  'vis_type', 'vis_content', 'alt_description'
#' # The function will prompt for the correct mappings
#' panels <- create_panels_list("path/to/your/file.csv")
#' # You will be prompted for each field (e.g., "Please provide the column name for 'name': panel_id")
#'
#' # Example 3: Providing a column mapping directly without prompts
#' col_mapping <- c(
#'   name = "panel_id",
#'   takeaway = "main_point",
#'   text = "description",
#'   vizType = "vis_type",
#'   viz = "vis_content",
#'   alt = "alt_description"
#' )
#'
#' panels <- create_panels_list("path/to/your/file.csv", col_mapping = col_mapping)
#'
#' # You can now pass 'panels' to the create_story function as usual
#' clickStories::create_story(
#'   story_title = "Your Story Title",
#'   output_dir = "path/to/output/dir",
#'   name = "story_name",
#'   logo = "path/to/logo.png",
#'   style = "path/to/style.scss",
#'   panels = panels
#' )
#' }

create_panels_list <- function(data_file, col_mapping = NULL) {

  # Required fields for panels
  required_fields <- c("name", "takeaway", "text", "vizType", "viz", "alt")

  # Automatically detect the file type based on the file extension
  file_extension <- tools::file_ext(data_file)

  # Read the data based on the file extension
  if (tolower(file_extension) == "csv") {
    panel_data <- read_csv(data_file)
  } else if (tolower(file_extension) == "xlsx") {
    panel_data <- read_xlsx(data_file)
  } else {
    stop("Unsupported file type. Please provide a CSV or XLSX file.")
  }

  # Extract the column names from the data
  data_columns <- colnames(panel_data)

  # Set default column mapping if not provided
  if (is.null(col_mapping)) {
    col_mapping <- required_fields
    names(col_mapping) <- required_fields
  }

  # Prompt the user for missing or incorrect mappings
  for (field in required_fields) {
    if (!col_mapping[field] %in% data_columns) {
      message(glue::glue("The column for '{field}' was not found in the file."))
      col_mapping[field] <- readline(prompt = glue::glue("Please provide the column name for '{field}': "))
    }
  }

  # Ensure that the provided columns exist in the data file
  missing_columns <- setdiff(col_mapping, colnames(panel_data))
  if (length(missing_columns) > 0) {
    stop(glue::glue("The following columns are missing from the data file: {paste(missing_columns, collapse = ', ')}"))
  }

  # Create panels from the data file, handling empty text fields
  panels <- lapply(seq_len(nrow(panel_data)), function(i) {
    panel <- list(
      name = panel_data[[ col_mapping["name"] ]][i],
      takeaway = panel_data[[ col_mapping["takeaway"] ]][i],
      text = ifelse(is.na(panel_data[[ col_mapping["text"] ]][i]) || panel_data[[ col_mapping["text"] ]][i] == "", "", panel_data[[ col_mapping["text"] ]][i]),  # Handle blank text
      vizType = panel_data[[ col_mapping["vizType"] ]][i],
      viz = panel_data[[ col_mapping["viz"] ]][i],
      alt = panel_data[[ col_mapping["alt"] ]][i]
    )

    # Validate that the panel contains all the required fields
    if (!all(c("takeaway", "text", "vizType", "viz") %in% names(panel))) {
      stop(glue::glue("Panel {panel$name} is missing one of the required fields: 'takeaway', 'text', 'vizType', 'viz'."))
    }

    return(panel)
  })

  # Return the panels list
  return(panels)
}



