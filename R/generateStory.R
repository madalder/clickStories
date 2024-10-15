#' Generate Quarto Reveal.js Click-Through Data Stories
#'
#' This function generates a Quarto Reveal.js presentation with custom slides, including images, text, and visualizations.
#' Each panel can have two columns: one for text and the other for a visualization, which can be an embedded HTML object, an external image, or a local image.
#' You can include custom SCSS themes to style the presentation further. You can also opt to create a self-contained HTML file version of the story.
#'
#' @name create_story
#' @param story_title The title of the presentation.
#' @param output_dir The file path where you want the directory folder and other relevant files to be placed.
#' @param logo The path to the logo image. If provided, it will be copied to the "images" directory.
#' @param name The file name you want to use for the generated directory folder and Quarto `.qmd` document. Default is "story".
#' @param style The path to a custom SCSS file for styling the presentation. If provided, it will be copied to the same directory as the `.qmd` file.
#' @param render_html Logical, if `TRUE`, the function will attempt to render the Quarto `.qmd` file into HTML after creation.
#' @param panels An optional named argument to pass panel definitions as a list. If `panels` is `NULL`, panels are collected from `...`. Each panel is defined as a list containing:
#'   \describe{
#'     \item{name}{A keyword or two (hyphenated) to use as the panel's URL when published.}
#'     \item{takeaway}{The main takeaway sentence that appears first for each panel.}
#'     \item{text}{Additional text to provide context or explanations for the panel.}
#'     \item{vizType}{The type of visualization to include in the second column. Possible values: "embed" (for embedding HTML content), "image-link" (for an external image), "image" (for a local image). If `"image"`, the local image will be copied to the "images" directory within the generated folder.}
#'     \item{viz}{The content of the visualization. It can be an HTML embed code, a link to an external image, or the path to a local image. For local images, the function will automatically copy the file to the "images" folder within the output directory.}
#'     \item{alt}{The alt text for the image or image-link, used for accessibility and image descriptions.}
#'   }
#' @param ... Additional panel definitions, if `panels` is not used. The structure for each panel is the same as described under the `panels` argument.
#'
#' @return A list containing:
#'   \describe{
#'     \item{qmd_file}{The file path to the generated Quarto `.qmd` file.}
#'     \item{html_file}{The file path to the generated HTML file (if `render_html = TRUE`), or `NULL` otherwise.}
#'   }
#'
#' @examples
#' \dontrun{
#' create_story(
#'   story_title = "Data Story Title",
#'   logo = NULL,
#'   output_dir = ".",
#'   name = "story",
#'   style = NULL,
#'   render_html = FALSE,
#'   panels = list(
#'     list(
#'       name = "keyword1",
#'       takeaway = "This is the Main Takeaway of the panel.",
#'       text = "This is additional text that gives the reader additional context.",
#'       vizType = "embed",
#'       viz = '<div class="flourish-embed flourish-chart"
#'       data-src="visualisation/11597006"><script src=
#'       "https://public.flourish.studio/resources/embed.js"></script></div>'
#'     ),
#'     list(
#'       name = "keyword2",
#'       takeaway = "New panel, new main takeaway, summary sentence, etc.",
#'       text = "This is more text that gives the reader additional context.",
#'       vizType = "image-link",
#'       viz = "https://www.naccho.org/uploads/body-images/public-health-101-infographic-no-logo.jpg",
#'       alt = "This is the alt text description for the shared image link"
#'     ),
#'     list(
#'       name = "keyword3",
#'       takeaway = "New panel, new main takeaway, summary sentence, etc.",
#'       text = "This is more text that gives the reader additional context.",
#'       vizType = "image",
#'       viz = "images/test.png",
#'       alt = "The alt text for the associated image."
#'     )
#'   )
#' )
#' }
#' @import glue
#' @import fs
#' @import quarto
#' @export


create_story <- function(story_title, output_dir = NULL, name = "story", logo = NULL, style = NULL, render_html = FALSE, panels = NULL, ...) {


  # If panels are not provided as a named argument, collect from ... ---------
  if (is.null(panels)) {
    panels <- list(...)
  }

  # Debugging: Print out the panels passed to the function---------
  # message("panels:")
  # print(panels)
  #
  # message("panels length:")
  # print(length(panels))

  # Ensure that each panel is a list and contains required fields ---------
  if (!all(sapply(panels, function(panel) is.list(panel) &&
                  all(c("name", "takeaway", "text", "vizType", "viz") %in% names(panel))))) {
    stop("All panels must be lists with 'name', 'takeaway', 'text', 'vizType', and 'viz'.")
  }

  # Ensure panel names are unique ----------
  panel_names <- sapply(panels, function(x) x$name)
  if (length(unique(panel_names)) != length(panel_names)) {
    stop("Each panel name must be unique.")
  }

  # Define folder name --------
  folder_name <- name

  # Automatically append the ".qmd" extension to the filename ---------
  filename <- paste0(name, ".qmd")

  # Set the output directory, default to current working directory if NULL ---------
  if (is.null(output_dir)) {
    output_dir <- "."
  }

  # Combine the output_dir and folder_name to create the target directory ---------
  target_dir <- file.path(output_dir, folder_name)

  # Create the target directory if it doesn't exist ---------
  dir_create(target_dir)

  # Create an "images" subdirectory inside the target directory ---------
  images_dir <- file.path(target_dir, "images")
  dir_create(images_dir)

  # Debugging: Print out the logo passed ---------
  # message("logo:")
  # print(logo)

  # If logo is provided, copy it to the images directory ---------
  if (!is.null(logo) && length(logo) == 1 && logo != "" && file_exists(as.character(logo))) {
    file_copy(logo, file.path(images_dir, "logo.png"), overwrite = TRUE)
    logo <- file.path("images", "logo.png")
  } else {
    logo <- ""  # No logo if not provided or invalid
  }

  # Debugging: Print the length of the logo to verify handling ---------
  # message("logo length:")
  # print(length(logo))

  # If style is provided, copy it to the target directory ---------
  if (!is.null(style) && length(style) == 1 && file_exists(as.character(style))) {
    file_copy(style, file.path(target_dir, "styles.scss"), overwrite = TRUE)
    style <- "styles.scss"
  } else {
    style <- NULL
  }

  # Set self-contained option based on render_html ---------
  if (length(render_html) != 1 || !is.logical(render_html)) {
    stop("render_html must be a single logical value (TRUE or FALSE)")
  }

  self_contained <- if (render_html) "true" else "false"

  # Build the YAML header for the Quarto Reveal.js presentation ---------
  yaml_header <- if (!is.null(style)) {

    glue("
---
title: '{story_title}'
logo: '{logo}'
engine: knitr
format:
  revealjs:
    menu: false
    theme: [default, '{style}']
    code-block-height: 750px
    reference-location: document
    hash-type: number
    controls: true
    auto-animate-easing: ease-in-out
    auto-animate-unmatched: false
    auto-animate-duration: 0.8
    width: 1260
    height: 750
    scrollable: false
    self-contained: {self_contained}
---

", story_title = story_title, logo = logo, style = style, self_contained = self_contained)

  } else {

    glue("
---
title: '{story_title}'
logo: '{logo}'
engine: knitr
format:
  revealjs:
    menu: false
    theme: default
    code-block-height: 750px
    reference-location: document
    hash-type: number
    controls: true
    auto-animate-easing: ease-in-out
    auto-animate-unmatched: false
    auto-animate-duration: 0.8
    width: 1260
    height: 750
    scrollable: false
    self-contained: {self_contained}
---

", story_title = story_title, logo = logo, self_contained = self_contained)

  }

  # Initialize the content with the YAML header ---------
  content <- as.character(yaml_header)

  # Debugging: Check if panels are processed correctly ---------
  if (length(panels) == 0) {
    message("No panels provided.")
  } else {
    for (i in seq_along(panels)) {
      panel <- panels[[i]]
      message(glue("Processing panel: {panel$name}"))

      name <- panel$name  # keyword or two (must be hyphenated) to use as the panel's label when published
      takeaway <- panel$takeaway
      text <- ifelse("text" %in% names(panel), panel$text, "")  # Default empty string for missing text
      vizType <- panel$vizType
      viz <- panel$viz
      alt <- ifelse(!is.null(panel$alt), panel$alt, "")  # Default empty string for alt text

      # Handle different visualization types
      viz_content <- ""
      if (vizType == "embed") {
        # Embedding HTML content directly
        viz_content <- glue("{viz}")

      } else if (vizType == "image-link") {
        # External image link
        viz_content <- glue('<img src="{viz}" alt="{alt}" />')

      } else if (vizType == "image" && !is.null(viz) && length(viz) == 1 && viz != "" && file_exists(as.character(viz))) {
        local_image_path <- file.path(images_dir, basename(viz))
        file_copy(viz, local_image_path, overwrite = TRUE)
        viz_content <- glue('![]({file.path("images", basename(viz))}){{fig-alt="{alt}"}}')
      }

      # Generate slide content ---------
      slide_content <- glue("
##  {{#{name} data-menu-title='{takeaway}'}}

<br>

:::columns

:::{{.column width=45%}}
### {takeaway}

::: fragment
{text}
:::
:::

:::{{.column .fragment width=55%}}
{viz_content}
:::

:::

")

      # Debugging: Check generated slide content ---------
      # message("Generated slide content:")
      # print(slide_content)

      # Append the generated slide content to the overall content ---------
      content <- paste0(content, slide_content)
    }
  }

  # Write the content to the Quarto .qmd file in the target directory ---------
  output_file <- file.path(target_dir, filename)
  writeLines(content, con = output_file)
  message(glue("Story created: {output_file}"))

  # If render_html is TRUE, render the .qmd file to HTML using CLI or cleanly ---------
  if (render_html) {
    result <- try(system(glue::glue("quarto render {output_file} --to html --self-contained"), intern = TRUE))
    if (inherits(result, "try-error")) {
      stop("Failed to render the Quarto file to HTML.")
    } else {
      message(glue("HTML file created at: {target_dir}"))
    }
  }

  # Return the paths of the created files ---------
  return(list(
    qmd_file = output_file,
    html_file = if (render_html) file.path(target_dir, paste0(name, ".html")) else NULL
  ))
}
