test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("create_story generates the expected .qmd file", {


  temp_dir <- "~/Documents"
  output_dir <- file.path(temp_dir, "test_story")

  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)  # Cleanup after the test

  create_story(
    story_title = "Test Story",
    output_dir = temp_dir,
    name = "test_story",
    render_html = FALSE,
    panels = list(
      list(name = "test-panel",
      takeaway = "Test takeaway",
      text = "Test text for the panel.",
      vizType = "image",
      viz = "~/Documents/test.png",
      alt = ""
    )
    )
  )

  qmd_file <- file.path(output_dir, "test_story.qmd")
  expect_true(file.exists(qmd_file))

  # Read the file content
  content <- readLines(qmd_file)

  # Check the YAML header contains the correct title
  expect_true(any(grepl("title: 'Test Story'", content)))

  # Check that the takeaway and visualization are correctly written
  expect_true(any(grepl("Test takeaway", content)))  # Checks for correct heading
  expect_true(any(grepl("Test text for the panel.", content)))  # Checks for panel text
  expect_true(any(grepl("images/test.png", content)))  # Checks for embedded viz
})



test_that("create_story handles logo and style files correctly", {
  temp_dir <- tempdir()
  output_dir <- file.path(temp_dir, "test_story_logo_style")

  logo_path <- file.path(temp_dir, "test_logo.png")
  style_path <- file.path(temp_dir, "test_style.scss")

  file.create(logo_path)
  file.create(style_path)

  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)  # Cleanup after the test

  create_story(
    story_title = "Story with Logo and Style",
    output_dir = temp_dir,
    name = "test_story_logo_style",
    logo = logo_path,
    style = style_path,
    render_html = FALSE,
    panels = list(
    list(
      name = "panel1",
      takeaway = "Takeaway 1",
      text = "Text 1",
      vizType = "image",
      viz = logo_path,
      alt = "Logo image alt text"
    )
    )
  )

  qmd_file <- file.path(output_dir, "test_story_logo_style.qmd")
  expect_true(file.exists(qmd_file))

  content <- readLines(qmd_file)


  # Check that the logo and style paths are correctly added to the YAML header
  expect_true(any(grepl("logo: 'images/logo.png'", content)))  # Matches logo
  expect_true(any(grepl("[default, 'styles.scss']", content)))  # Matches style
})


test_that("create_story handles missing panels gracefully", {
  temp_dir <- tempdir()
  output_dir <- file.path(temp_dir, "test_story_empty")

  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)  # Cleanup after the test

  # Run function without panels (empty ...)
  create_story(
    story_title = "Empty Story",
    output_dir = temp_dir,
    name = "test_story_empty",
    render_html = FALSE
  )

  # Check that .qmd file exists even if no panels are provided
  qmd_file <- file.path(output_dir, "test_story_empty.qmd")
  expect_true(file.exists(qmd_file))

  # Read the file content
  content <- readLines(qmd_file)

  # Ensure that no slide content was generated
  expect_false(any(grepl("##", content)))
})

test_that("create_story handles invalid panel input", {
  temp_dir <- tempdir()

  expect_error(create_story(
    story_title = "Invalid Panel Test",
    output_dir = temp_dir,
    render_html = FALSE,
    list(name = "panel1", takeaway = "Takeaway 1", text = "Text 1"),  # Missing vizType and viz
    "All panels must be lists with 'name', 'takeaway', 'text', 'vizType', and 'viz'"
  ))
})


test_that("create_story handles local image copying", {
  temp_dir <- "~/Documents"
  output_dir <- file.path(temp_dir, "test_story_images")

  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)  # Cleanup after the test

  create_story(
    story_title = "Story with Local Image",
    output_dir = temp_dir,
    name = "test_story_images",
    render_html = FALSE,
    panels = list(
    list(
      name = "panel1",
      takeaway = "Takeaway 1",
      text = "Text 1",
      vizType = "image",
      viz = "~/Documents/test.png",
      alt = "Local image alt text"
    )
    )
  )

  # Check if the image was copied to the images directory
  expect_true(file.exists(file.path(output_dir, "images", "test.png")))

  # Read the .qmd file
  qmd_file <- file.path(output_dir, "test_story_images.qmd")
  content <- readLines(qmd_file)

  # Ensure that the .qmd file contains the correct image reference
  expect_true(any(grepl("(images/test.png)", content)))
})

# Clean up at the end of tests
if (dir.exists("story")) {
  unlink("story", recursive = TRUE)
}
