#' Transform Embed Code for Figma and Handle Non-Figma Embeds
#'
#' This function processes Figma embed codes, transforming them to include the necessary parameters and injecting appropriate CSS. Non-Figma embed codes are returned unaltered.
#'
#' @param embed_code Character. The embed code to process (Figma or non-Figma).
#' @return A string containing the transformed HTML for Figma embeds, or the original embed code for non-Figma embeds.
#'
#' @examples
#' \dontrun{
#' # Example of a Figma embed transformation
#' embed_code <- '<iframe src="https://embed.figma.com?file=example" style="width:800px;height:600px;"></iframe>'
#' transformed_code <- generate_figma_embed_value(embed_code)
#' cat(transformed_code)
#'
#' # Example with non-Figma embed
#' non_figma_code <- '<iframe src="https://example.com/embed"></iframe>'
#' result <- generate_figma_embed_value(non_figma_code)
#' cat(result)
#' }
#'
#' @export
generate_figma_embed_value <- function(embed_code) {
  # Validate input
  if (is.null(embed_code) || embed_code == "") {
    return(embed_code)  # Return as-is for NULL or empty input
  }

  # Check if the embed code is a Figma iframe
  if (grepl("<iframe", embed_code) && grepl("embed.figma.com", embed_code)) {
    # Extract the `src` URL from the iframe
    src_url <- sub('.*src="([^"]+)".*', '\\1', embed_code)

    # Modify the necessary parameters in the URL
    if (!grepl("scaling=fit-width", src_url)) {
      src_url <- sub("scaling=[^&]*", "scaling=fit-width", src_url) # Replace existing scaling
    }
    if (!grepl("content-scaling=proportional", src_url)) {
      src_url <- sub("content-scaling=[^&]*", "content-scaling=proportional", src_url) # Replace existing scaling
    }
    if (!grepl("hide-ui", src_url)) {
      src_url <- paste0(src_url, "&hide-ui=1") # Add hide-ui if not present
    }

    # Construct the formatted HTML string
    formatted_html <- paste0(
      '<iframe id="myIframe" style="border: none; width: 100%; height: 500px;" ',
      'data-src="', src_url, '" allowfullscreen></iframe>'
    )

    return(formatted_html)
  }

  # If not a Figma embed, return the embed code unchanged
  return(embed_code)
}



#' Generate Panels List from a Data File
#'
#' This function allows the user to generate a list of panels from a CSV or XLSX file.
#' If the column names in the data file do not match the required fields, and the user hasn't provided `column_mappings`,
#' the function will prompt the user to input the correct column names interactively.
#' The function also supports various visualization types, transforming Figma embeds to the correct prototype format.
#'
#' @param file Character. The path to the CSV or XLSX file that contains the panel data.
#' @param column_mappings Named list (optional). Specifies the mapping from required field names to the column names in your data file.
#'   If `NULL` and the dataset's columns do not match the expected columns, the user will be prompted interactively.
#'   The list should have the following keys: 'name', 'takeaway', 'text', 'vizType', 'viz', and 'alt'.
#'   If the 'text' column is missing, you can set it to `NA_character_` to handle it as optional.
#' @param default_width Character (optional). Default width for embeds. Default is `"100%"`.
#' @param default_height_vertical Character (optional). Default height for vertical visualizations. Default is `"850px"`.
#' @param default_height_similar Character (optional). Default height for similar visualizations. Default is `"650px"`.
#' @param default_height_horizontal Character (optional). Default height for horizontal visualizations. Default is `"600px"`.
#'
#' @return A list of panels, where each panel is a list containing:
#' \describe{
#'   \item{name}{Character. The panel's name.}
#'   \item{takeaway}{Character. The main takeaway sentence for the panel.}
#'   \item{text}{Character (optional). Additional context or explanations for the panel. If empty or missing, it will be set to an empty string.}
#'   \item{vizType}{Character. The type of visualization to include ("embed", "image-link", "image").}
#'   \item{viz}{Character. The content of the visualization (embed code, external image link, or local image path). Figma design links
#'              are transformed to prototype links.}
#'   \item{alt}{Character (optional). Alt text for the visualization, for accessibility purposes.}
#'   \item{vizSpace}{Character (optional). Indicates the layout orientation of the visualization (`"horizontal"`, `"vertical"`, `"similar"`).}
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
#' # Example 2: Using custom column mappings
#' panels <- create_panels_list('panels.csv', column_mappings = list(
#'   name = 'panel_name',
#'   takeaway = 'main_takeaway',
#'   text = NA_character_,  # Optional column
#'   vizType = 'viz_type',
#'   viz = 'visualization',
#'   alt = 'alt_text'
#' ))
#'
#' # Example 3: Using an XLSX file with interactive prompts
#' panels <- create_panels_list('panels.xlsx')
#' }
#'
#' @export
create_panels_list <- function(file, column_mappings = NULL,
                               default_width = "100%",
                               default_height_vertical = "850px",
                               default_height_similar = "650px",
                               default_height_horizontal = "600px") {
  # Expected columns for the panels data
  expected_columns <- c("name", "takeaway", "text", "vizType", "viz", "alt", "vizSpace")

  # Read the data based on the file extension
  file_extension <- tools::file_ext(file)
  panel_data <- switch(tolower(file_extension),
                       "csv" = read.csv(file, stringsAsFactors = FALSE),
                       "xlsx" = readxl::read_xlsx(file),
                       stop("Unsupported file type. Please provide a CSV or XLSX file.")
  )

  # Normalize column names for case-insensitive matching
  colnames(panel_data) <- tolower(colnames(panel_data))
  expected_columns_lower <- tolower(expected_columns)

  # Detect or prompt for column mappings
  if (is.null(column_mappings)) {
    column_mappings <- list()
    for (col in expected_columns) {
      col_lower <- tolower(col)
      column_mappings[[col]] <- ifelse(col_lower %in% colnames(panel_data),
                                       colnames(panel_data)[col_lower == colnames(panel_data)],
                                       NA)
    }
    # Prompt for genuinely missing columns
    for (col in names(column_mappings)) {
      if (is.na(column_mappings[[col]]) || column_mappings[[col]] == "") {
        column_mappings[[col]] <- readline(glue::glue(
          "Please provide the column name for '{col}' (or press Enter to skip): "
        ))
      }
    }
  }

  # Add a default 'vizSpace' column if missing
  if (is.na(column_mappings$vizSpace) || column_mappings$vizSpace == "") {
    panel_data$vizSpace <- NA
    column_mappings$vizSpace <- "vizSpace"
  }

  # Transform the 'viz' column with Figma embed handling
  panel_data[[column_mappings$viz]] <- sapply(1:nrow(panel_data), function(i) {
    viz_entry <- panel_data[[column_mappings$viz]][i]
    viz_type <- tolower(panel_data[[column_mappings$vizType]][i])
    viz_space <- if (!is.null(column_mappings$vizSpace)) panel_data[[column_mappings$vizSpace]][i] else NA

    # Handle Highcharts
    if (viz_type %in% c("highchart", "high chart")) {
      choice <- readline(prompt = "For 'highchart', do you want to (1) paste code or (2) link an R script? Enter 1 or 2: ")
      if (choice == "1") {
        cat("Paste your multi-line R code for the Highchart below (type 'done' on a new line to finish):\n")
        lines <- character()
        repeat {
          line <- readline()
          if (tolower(trimws(line)) == "done") break
          lines <- c(lines, line)
        }
        viz_entry <- paste(lines, collapse = "\n")
      } else if (choice == "2") {
        viz_entry <- readline(prompt = "Provide the path to the R script file: ")
        if (!file.exists(viz_entry)) stop("The provided file path does not exist.")
        viz_entry <- paste0("source('", viz_entry, "')")
      } else {
        stop("Invalid choice. Please enter 1 or 2.")
      }
      panel_data[[column_mappings$viz]][i] <- viz_entry
    }

    # Prompt for missing vizSpace
    if (is.na(viz_space) || viz_space == "") {
      cat(glue::glue(
        "\nFor the panel '{panel_data[[column_mappings$name]][i]}', how does the visualization fit best?\n",
        "  1. Horizontally wide\n",
        "  2. Vertically tall\n",
        "  3. Both dimensions are similar\n"
      ))

      repeat {
        viz_space_input <- readline("Enter 1, 2, or 3: ")
        if (viz_space_input %in% c("1", "2", "3")) {
          viz_space <- switch(viz_space_input, "1" = "horizontal", "2" = "vertical", "3" = "similar")
          panel_data[[column_mappings$vizSpace]][i] <- viz_space
          break
        } else {
          cat("Invalid input. Please enter 1, 2, or 3.\n")
        }
      }
    }


    # Handle Figma embeds
    if (grepl("<iframe", viz_entry) && grepl("embed.figma.com", viz_entry)) {
      embed_height <- switch(viz_space,
                             "vertical" = default_height_vertical,
                             "horizontal" = default_height_horizontal,
                             default_height_similar)
      viz_entry <- generate_figma_embed_value(viz_entry)

      # Adjust iframe style
      if (grepl('style="', viz_entry)) {
        viz_entry <- sub('height: [^;]*;', paste0('height: ', embed_height, ';'), viz_entry)
      } else {
        viz_entry <- sub('<iframe', paste0('<iframe style="height: ', embed_height, '; width: ', default_width, ';"'), viz_entry)
      }

      viz_entry <- sub('width="[^"]*"', paste0('width="', default_width, '"'), viz_entry)
    }

    return(viz_entry)
  }, USE.NAMES = FALSE)

  # Create the panels list based on column mappings
  panels <- lapply(1:nrow(panel_data), function(i) {
    list(
      name = panel_data[[column_mappings$name]][i],
      takeaway = panel_data[[column_mappings$takeaway]][i],
      text = if (!is.null(column_mappings$text)) panel_data[[column_mappings$text]][i] else "",
      vizType = panel_data[[column_mappings$vizType]][i],
      viz = if (!is.null(column_mappings$viz)) panel_data[[column_mappings$viz]][i] else "",
      alt = panel_data[[column_mappings$alt]][i],
      vizSpace = panel_data[[column_mappings$vizSpace]][i]
    )
  })

  # Print the final panels list to verify structure
  print("Final panels list:")
  print(panels)

  return(panels)
}
