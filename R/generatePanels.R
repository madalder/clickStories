#' Generate Panels from CSV/XLSX File with User Prompts for Column Mapping and Embed Support
#'
#' This function reads data from a CSV or XLSX file and returns a list of panels
#' to be used in creating a Quarto Reveal.js presentation. If the required column names
#' don't match, it prompts the user to provide the correct column names. The function supports
#' multiple visualization types, including Figma embeds, image links, and other iframe-based embeds.
#' Figma design links are automatically converted to prototype links with the appropriate parameters.
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
#'     \item{vizType}{The type of visualization to include ("embed", "image-link", "image").}
#'     \item{viz}{The content of the visualization (embed code, external image link, or local image path). Figma design links are transformed to prototype links.}
#'     \item{alt}{Alt text for the visualization, for accessibility purposes.}
#'   }
#'
#' @import readr
#' @import readxl
#' @export
#'
#' @description This function allows the user to generate a list of panels from a CSV or XLSX file.
#' If the column names in the data file do not match the required fields, the function will prompt the user
#' to input the correct column names. Additionally, the function supports various visualization types, transforming Figma embeds
#' to the correct prototype format and handling other iframe-based embeds (e.g., YouTube, Google Maps) without altering them.
#'
#' @examples
#' \dontrun{
#' # Example 1: When the column names in the data match the required fields
#' # Assuming the CSV file has columns named 'name', 'takeaway', 'text', 'vizType', 'viz', 'alt'
#' panels <- create_panels_list("path/to/your/file.csv")
#'
#' # Example 2: Handling Figma embeds and other viz types
#' # The function transforms Figma design links to prototype format, while other embeds (YouTube, etc.) remain unchanged.
#' panels <- create_panels_list("path/to/your/file.csv")
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
      viz = if (panel_data[[col_mapping["vizType"]]][i] == "embed") {
        # Apply Figma transformation if the viz content is an embed
        transform_figma_embed(panel_data[[col_mapping["viz"]]][i])
      } else {
        # For other viz types, keep the original content
        panel_data[[col_mapping["viz"]]][i]
      },
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
#' figma_embed <- '<iframe style="border: 1px solid rgba(0, 0, 0, 0.1);" width="800" height="450" src="https://embed.figma.com/design/3BByo3UqianyYN2GaDmePf/Profile-Data-Stories---Brand-Colors?node-id=38-1173&embed-host=share" allowfullscreen></iframe>'
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





