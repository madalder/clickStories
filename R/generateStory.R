#' Create a click through data story using Quarto revealjs
#'
#' This function generates a Quarto Reveal.js presentation with custom slides, including images, text, and visualizations.
#' Each slide can have two columns, one for text and the other for a visualization, which could be an embedded HTML object, an external image, or a local image.
#'
#' @param story_title The title of the presentation.
#' @param logo The path to the logo image. If provided, it will be copied to the "images" directory.
#' @param filename The filename for the generated Quarto `.qmd` document. Default is "story.qmd".
#' @param style The path to a custom SCSS file for styling the presentation. If provided, it will be copied to the same directory as the `.qmd` file.
#' @param ... Panel definitions. Each panel is defined as a list containing:
#'   \item{name}{A keyword or two (hyphenated) to use as the panel's url when published.}
#'   \item{takeaway}{The larger takeaway sentence that appears first for each panel.}
#'   \item{text}{The additional text to provide context or explanations for the panel.}
#'   \item{vizType}{The type of visualization to include in the second column. Possible values: "embed" (for embedding HTML content), "image-link" (for an external image), "image" (for a local image).}
#'   \item{viz}{The content of the visualization. It can be an HTML embed code, a link to an external image, or the path to a local image.}
#'   \item{alt}{The alt text for the image or image-link, used for accessibility and image descriptions.}
#'
#' @return Creates a Quarto `.qmd` file that can be used to render a Reveal.js presentation.
#' @examples
#' create_story(
#'   story_title = "Data Story Title",
#'   logo = NULL,
#'   filename = "story.qmd",
#'   style = NULL,
#'   list(name = "keyword1",
#'     takeaway = "This is the Main Takeaway of the panel.",
#'     text = "This is additional text that gives the reader additional context.",
#'     vizType = "embed",
#'     viz = '<div class="flourish-embed flourish-chart" data-src="visualisation/11597006"><script src="https://public.flourish.studio/resources/embed.js"></script></div>'),
#'   list(name = "keyword2",
#'     takeaway = "New panel, new main takeaway, summary sentence, etc.",
#'     text = "This is more text that gives the reader additional context.",
#'     vizType = "image-link",
#'     viz = "https://www.naccho.org/uploads/body-images/public-health-101-infographic-no-logo.jpg",
#'     alt = "This is the alt text description for the shared image link"),
#'   list(name = "keyword3",
#'     takeaway = "New panel, new main takeaway, summary sentence, etc.",
#'     text = "This is more text that gives the reader additional context.",
#'     vizType = "image",
#'     viz = "images/test.png",
#'     alt = "The alt text for the associated image.")
#' )
#' @import glue
#' @import fs
#' @import quarto
#' @export



library(glue)
library(fs)

create_story <- function(story_title, logo = NULL, filename = "story.qmd", style = NULL, ...) {
  
  # Collect the pages (panels) as list arguments-----
  panels <- list(...)
  
  
  # Ensure that each panel is a list------
  if (!all(sapply(panels, is.list))) {
    stop("All panels must be lists. Each panel should be defined as a list with 'takeaway', 'text', 'vizType', and 'viz'.")
  }
  
  # Create the "images" directory if it doesn't exist------
  dir_create("images")
  
  # If logo is provided, copy it to the images directory-------
  if (!is.null(logo) && file_exists(logo)) {
    file_copy(logo, "images/logo.png", overwrite = TRUE)
    logo <- "images/logo.png"
  } else {
    logo <- ""  # No logo if not provided
  }
  
  # If style is provided, copy it to the same directory as the .qmd file -------
  if (!is.null(style) && file_exists(style)) {
    file_copy(style, "styles.scss", overwrite = TRUE)
    style <- "styles.scss"
  } else {
    style <- NULL
  }
  
  
  # Build the YAML header for the Quarto Reveal.js presentation
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
---

", story_title = story_title, logo = logo, style = style )
    
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
---

", story_title = story_title, logo = logo)
    
  }
  
  # Initialize the content with the YAML header--------
  content <- as.character(yaml_header)
  
  # message("Print content before panels made")
  # print(content)
  
  # Loop over to create each panel--------
  
  for (i in seq_along(panels)) {
    panel <- panels[[i]]
    
    name <- panel$name #keyword or two (must be hyphenated) to use as the panel's label when published
    takeaway <- panel$takeaway
    text <- panel$text
    vizType <- panel$vizType
    viz <- panel$viz
    alt <- ifelse(!is.null(panel$alt), panel$alt, "")  # Add an alt text with default empty string
    
    # Handle different visualization types ----------
    viz_content <- ""
    if (vizType == "embed") {
      # Embedding HTML content directly
      viz_content <- glue("{viz}")
      
    } else if (vizType == "image-link") {
      # External image link
      viz_content <- glue('<img src="{viz}" alt="{alt}" />')
      
    } else if (vizType == "image") {
      # Local image
      viz_content <- glue('![]({viz}){{fig-alt="{alt}"}}')
    }
    
    
    
    # Generate slide content--------------
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


", name = name, takeaway = takeaway)
    
    # Append the generated slide content to the overall content
    content <- paste0(content, slide_content)
  }
  
  
  # Write the content to the Quarto file
  writeLines(content, con = filename)
  
  message(glue("Story created: {filename}"))
}

# Example usage--------

create_story(
  
  story_title = "Data Story Title",
  logo = NULL,
  filename = "story.qmd",
  style = NULL,
  
  list(name = "keyword1",
       takeaway = "This is the Main Takeaway of the panel.",
       text = "This is additional text that gives the reader additional context.",
       vizType = "embed",
       viz = '<div class="flourish-embed flourish-chart" data-src="visualisation/11597006"><script src="https://public.flourish.studio/resources/embed.js"></script></div>'), #be sure to put this in ' ' and not " "
  
  list(name = "keyword2",
       takeaway = "New panel, new main takeaway, summary sentence, etc.",
       text = "This is more text that gives the reader additional context.",
       vizType = "image-link",
       viz = "https://www.naccho.org/uploads/body-images/public-health-101-infographic-no-logo.jpg",
       alt = "This is the alt text description for the shared image link"),
  
  list(name = "keyword3",
       takeaway = "New panel, new main takeaway, summary sentence, etc.",
       text = "This is more text that gives the reader additional context.",
       vizType = "image",
       viz = "images/test.png",
       alt = "The is the alt text for the associated image.")
)
