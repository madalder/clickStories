#' Create a Quarto Reveal.js Click-Through Story
#'
#' This function generates a Quarto Reveal.js click through with dynamic layouts based on content length.
#' It supports various visualization types and handles special cases like embed-only slides.
#'
#' @param story_title Character. The title of the click through.
#' @param subtitle Character (optional). Subtitle of the click through.
#' @param output_dir Character (optional). Directory where the generated files (e.g., `.qmd`, images) will be stored.
#'   Defaults to the current working directory if NULL.
#' @param name Character. The base name for the generated `.qmd` file and output folder. Default is `"story"`.
#' @param logo Character (optional). Path to a logo image to be included in the click through. The logo will be copied to an `images` directory.
#' @param style Character (optional). Path to a custom SCSS style file. If provided, it will be copied into the project directory and included in the click through.
#' @param render_html Logical. If `TRUE`, renders the `.qmd` file into an HTML click through. Default is `FALSE`.
#' @param panels List. A list of panels to include in the click through. Each panel should be a list containing the following fields:
#'   \itemize{
#'     \item \code{name}: Character. A unique identifier for the panel (used as a slide ID).
#'     \item \code{takeaway}: Character. The main message or headline for the panel.
#'     \item \code{text}: Character (optional). Additional context or supporting text.
#'     \item \code{vizType}: Character. The type of visualization. Supported values: `"embed"`, `"image-link"`, `"image"`, `"highchart"`.
#'     \item \code{viz}: Character. The visualization content. This can be HTML embed code, an external image URL, a local image path, or R code for Highcharts.
#'     \item \code{vizSpace}: Character. The layout orientation of the visualization (`"horizontal"`, `"vertical"`, `"similar"`).
#'     \item \code{alt}: Character (optional). Alt text for images, for accessibility.
#'   }
#' @param ... Additional panels passed directly as arguments instead of through the `panels` parameter.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{qmd_file}: Character. Path to the generated `.qmd` file.
#'   \item \code{html_file}: Character (optional). Path to the rendered HTML file if \code{render_html = TRUE}, or \code{NULL} otherwise.
#' }
#'
#' @import glue
#' @import fs
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' create_story(
#'   story_title = "Sample click through story",
#'   subtitle = "An Example of Dynamic Layouts",
#'   output_dir = "output",
#'   name = "example_story",
#'   panels = list(
#'     list(
#'       name = "slide1",
#'       takeaway = "Key Insight 1",
#'       text = "Supporting details for insight 1.",
#'       vizType = "image",
#'       viz = "path/to/image.png",
#'       vizSpace = "horizontal",
#'       alt = "Description of the image"
#'     ),
#'     list(
#'       name = "slide2",
#'       takeaway = "Key Insight 2",
#'       text = "Additional context for insight 2.",
#'       vizType = "embed",
#'       viz = "<iframe src='https://example.com' />",
#'       vizSpace = "vertical"
#'     )
#'   ),
#'   render_html = TRUE
#' )
#' }



create_story <- function(story_title, subtitle = NULL, output_dir = NULL, name = "story", logo = NULL, style = NULL, render_html = FALSE, panels = NULL, ...) {

  # Helper to detect empty or invalid values
  is_empty <- function(value) {
    is.null(value) ||
      value == "" ||
      tolower(trimws(value)) %in% c("n/a", "na", "null", "undefined", "", " ")
  }

  # Helper to determine layout type based on content and visualization space
  get_layout_type <- function(takeaway, text, viz_space) {
    char_count <- function(value) {
      if (is_empty(value)) {
        return(0)
      }
      nchar(value)
    }
    total_chars <- char_count(takeaway) + char_count(text)

    # Determine layout based on text length and viz_space
    if (is_empty(takeaway) && is_empty(text)) {
      return("embed-only")
    } else if (total_chars < 300) {
      return(paste0("short-", viz_space))
    } else if (total_chars < 500) {
      return(paste0("medium-", viz_space))
    } else {
      return(paste0("long-", viz_space))
    }
  }

  # Validate and process panels
  if (is.null(panels)) {
    panels <- list(...)
  }

  required_fields <- c("name", "takeaway", "vizType", "viz", "vizSpace")

  if (!all(sapply(panels, function(panel) is.list(panel) && all(required_fields %in% names(panel))))) {
    stop("All panels must be lists with 'name', 'takeaway', 'vizType', 'viz', and 'vizSpace'. 'text' is optional.")
  }

  # Ensure unique panel names
  panel_names <- sapply(panels, function(x) x$name)
  if (length(unique(panel_names)) != length(panel_names)) {
    stop("Each panel name must be unique.")
  }

  # Set up directories
  output_dir <- output_dir %||% "."
  target_dir <- file.path(output_dir, name)
  fs::dir_create(target_dir)
  images_dir <- file.path(target_dir, "images")
  fs::dir_create(images_dir)

  # Handle logo and custom style
  if (!is_empty(logo) && fs::file_exists(logo)) {
    fs::file_copy(logo, file.path(images_dir, "logo.png"), overwrite = TRUE)
    logo <- file.path("images", "logo.png")
  } else {
    logo <- ""
  }
  if (!is_empty(style) && fs::file_exists(style)) {
    fs::file_copy(style, file.path(target_dir, "styles.scss"), overwrite = TRUE)
    style <- "styles.scss"
  } else {
    style <- NULL
  }

  # YAML header
  self_contained <- if (render_html) "true" else "false"
  yaml_header <- glue::glue("
---
title: '{story_title}'
subtitle: '{subtitle}'
logo: '{logo}'
engine: knitr
format:
  revealjs:
    menu: false
    theme: [{if (!is_empty(style)) style else 'default'}]
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
    preload-iframes: true
    view-distance: 3
    self-contained: {self_contained}
---
")

  # Initialize content
  content <- as.character(yaml_header)

  # Process each panel
  for (panel in panels) {
    panel_name <- panel$name
    takeaway <- if (!is_empty(panel$takeaway)) panel$takeaway else NULL
    text <- if (!is.null(panel$text) && !is.na(panel$text) && !is_empty(panel$text)) panel$text else ""
    vizType <- if (!is_empty(panel$vizType)) tolower(panel$vizType) else NULL
    viz <- if (!is.null(panel$viz) && !is.na(panel$viz) && !is_empty(panel$viz)) panel$viz else ""
    alt <- if (!is_empty(panel$alt)) panel$alt else ""
    viz_space <- if (!is.null(panel$vizSpace) && !is.na(panel$vizSpace) && !is_empty(panel$vizSpace)) {
      tolower(panel$vizSpace)
    } else {
      "horizontal"
    }

    # Determine layout type using the refactored function
    layout_type <- get_layout_type(takeaway, text, viz_space)

    # Generate visualization content
    viz_content <- ""
    if (!is_empty(vizType)) {
      if (vizType == "embed") {
        viz_content <- glue::glue("{viz}")
      } else if (vizType == "image-link") {
        viz_content <- glue::glue('<img data-src="{viz}" alt="{alt}" />')
      } else if (vizType == "image" && fs::file_exists(viz)) {
        local_image_path <- file.path(images_dir, basename(viz))
        fs::file_copy(viz, local_image_path, overwrite = TRUE)
        viz_content <- glue::glue('![]({file.path("images", basename(viz))}){{fig-alt="{alt}"}}')
      } else if (tolower(vizType) %in% c("highchart", "high chart")) {
        viz_content <- glue::glue('\n```{{r}}\n{viz}\n hc \n```\n')
      } else {
        viz_content <- ""
      }
    }

    # Generate slide content based on layout type
    slide_content <- switch(layout_type,
                            "embed-only" = {
                              embed_url <- sub('.*data-src="([^"]+)".*', '\\1', viz)
                              if (!grepl("content-scaling=fixed", embed_url)) {
                                embed_url <- sub("content-scaling=[^&]*", "content-scaling=fixed", embed_url)
                              }
                              glue::glue("
##   {{ #{panel_name} background-iframe=\"{embed_url}\" background-interactive=true }}
")
                            },
                            "short-horizontal" = glue::glue("
## {{ #{panel_name} }}

:::{{.grid}}

:::{{.g-col-12}}
::: {{.rounded-box}}
### {takeaway}
{text}
:::
:::

:::{{.g-col-12}}
{viz_content}
:::

:::


"),
                            "short-similar" = glue::glue("
## {{ #{panel_name} }}

:::columns
:::{{.column width=20%}}
::: {{ .rounded-box }}
### {takeaway}\n
{text}\n
:::
:::

:::{{.column width=80%}}
{viz_content}
:::

:::


"),
                            "short-vertical" = glue::glue("
## {{ #{panel_name} }}

:::columns

:::{{.column width=45%}}
::: {{ .rounded-box }}
### {takeaway}\n
{text}\n
:::
:::

:::{{.column width=55%}}
{viz_content}
:::

:::
"),
                            "medium-horizontal" = glue::glue("
## {{ #{panel_name} }}

:::{{.grid}}
:::{{.g-col-12}}
::: {{.rounded-box}}
### {takeaway}
{text}
:::
:::

:::{{.g-col-12}}
{viz_content}
:::
:::

"),
                            "medium-similar" = glue::glue("
## {{ #{panel_name} }}

:::columns
### {takeaway}\n
:::{{.column width=30%}}
::: {{ .rounded-box }}
{text}\n
:::
:::

:::{{.column width=70%}}
{viz_content}
:::

:::

"),
                            "medium-vertical" = glue::glue("
## {{ #{panel_name} }}

:::columns

:::{{.column width=45%}}
::: {{ .rounded-box }}
### {takeaway}\n
{text}\n
:::
:::

:::{{.column width=55%}}
{viz_content}
:::

:::
"),
                            "long-horizontal" = glue::glue("
## {{ #{panel_name} }}

:::{{.grid}}
:::{{.g-col-12}}
::: {{.rounded-box}}
#### {takeaway}
{text}
:::
:::

:::{{.g-col-12}}
{viz_content}
:::
:::

"),
                            "long-similar" = glue::glue("
## {{ #{panel_name} }}


:::columns
:::{{.column width=30%}}

<br>


::: {{ .rounded-box .smaller}}
### {takeaway}\n
{text}\n
:::
:::

:::{{.column width=70%}}
{viz_content}
:::

:::

"),
                            "long-vertical" = glue::glue("
## {{ #{panel_name} }}

:::columns

:::{{.column width=50%}}
::: {{ .rounded-box }}
#### {takeaway}\n
{text}\n
:::
:::

:::{{.column width=50%}}
{viz_content}
:::

:::
"),
                            stop("Unexpected layout type")
    )

    # Append slide content to overall content
    content <- paste0(content, "\n\n", slide_content, "\n")
  }

  # Write .qmd file
  output_file <- file.path(target_dir, paste0(name, ".qmd"))
  writeLines(content, con = output_file)
  message(glue::glue("Story created: {output_file}"))

  # Render to HTML if requested
  if (render_html) {
    result <- try(system(glue::glue("quarto render {output_file} --to html --self-contained"), intern = TRUE))
    if (inherits(result, "try-error")) {
      stop("Failed to render the Quarto file to HTML.")
    } else {
      message(glue::glue("HTML file created at: {target_dir}"))
    }
  }

  return(list(
    qmd_file = output_file,
    html_file = if (render_html) file.path(target_dir, paste0(name, ".html")) else NULL
  ))
}
