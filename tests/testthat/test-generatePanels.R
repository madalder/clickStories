test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("create_panels_list generates panels correctly from CSV", {
  # Create a temporary CSV file with test data
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    name = c("Panel 1", "Panel 2"),
    takeaway = c("Takeaway 1", "Takeaway 2"),
    text = c("Text 1", "Text 2"),
    vizType = c("image", "embed"),
    viz = c("image1.png", "<iframe>...</iframe>"),
    alt = c("Alt text 1", "Alt text 2")
  ), temp_file, row.names = FALSE)

  # Call the function
  panels <- create_panels_list(temp_file)

  # Check the panels structure
  expect_type(panels, "list")
  expect_length(panels, 2)

  # Check the contents of the first panel
  expect_equal(panels[[1]]$name, "Panel 1")
  expect_equal(panels[[1]]$takeaway, "Takeaway 1")
  expect_equal(panels[[1]]$text, "Text 1")  # Make sure 'text' is not NULL
  expect_equal(panels[[1]]$vizType, "image")
  expect_equal(panels[[1]]$viz, "image1.png")
})


test_that("create_panels_list processes column mappings without manual input", {
  # Create a CSV with non-standard column names
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    panel_name = c("Panel 1"),
    main_takeaway = c("Takeaway 1"),
    vizualization_type = c("image"),
    visualization = c("image1.png"),
    alt_text = c("Alt text 1")
  ), temp_file, row.names = FALSE)

  # Directly provide column mappings to bypass readline during testing
  panels <- create_panels_list(temp_file, column_mappings = list(
    name = "panel_name",
    takeaway = "main_takeaway",
    vizType = "vizualization_type",
    viz = "visualization",
    alt = "alt_text"  # Simulating missing 'text' input
  ))

  # Check the panel structure
  expect_type(panels, "list")
  expect_length(panels, 1)
  expect_equal(panels[[1]]$name, "Panel 1")
  expect_equal(panels[[1]]$takeaway, "Takeaway 1")
  expect_equal(panels[[1]]$vizType, "image")
  expect_equal(panels[[1]]$viz, "image1.png")
})


