
# clickStories

<!-- badges: start -->
<!-- badges: end -->

The goal of clickStories is to ...

## Installation

You can install the development version of clickStories from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("madalder/clickStories")
```

## Code of Conduct
  
  Please note that the clickStories project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

## Example

This is a basic example:

``` r
create_story(
   story_title = "Data Story Title",
   logo = NULL,
   output_dir = ".",
   name = "story",
   style = NULL,
   render_html = FALSE,
   panels = list(
     list(
       name = "keyword1",
       takeaway = "This is the Main Takeaway of the panel.",
       text = "This is additional text that gives the reader additional context.",
       vizType = "embed",
       viz = '<div class="flourish-embed flourish-chart" data-src="visualisation/11597006"><script src="https://public.flourish.studio/resources/embed.js"></script></div>'
     ),
     list(
       name = "keyword2",
       takeaway = "New panel, new main takeaway, summary sentence, etc.",
       text = "This is more text that gives the reader additional context.",
       vizType = "image-link",
       viz = "https://www.naccho.org/uploads/body-images/public-health-101-infographic-no-logo.jpg",
       alt = "This is the alt text description for the shared image link"
     ),
     list(
       name = "keyword3",
       takeaway = "New panel, new main takeaway, summary sentence, etc.",
       text = "This is more text that gives the reader additional context.",
       vizType = "image",
       viz = "images/test.png",
       alt = "The alt text for the associated image."
     )
   )
 )
 
```

