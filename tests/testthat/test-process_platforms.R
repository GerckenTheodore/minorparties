setup_test <- function() {
  # Create test environment
  test <- new.env(parent = emptyenv())

  # Mock functions that depend on an active python environment
  local_mocked_bindings(
    spacy_initialize = function(...) TRUE,
    spacy_finalize = function(...) TRUE,
    spacy_tokenize = function(text, ...) {
      sentences <- strsplit(text, "(?<=[.!?])\\s+", perl = TRUE)
      names(sentences) <- paste0("text", seq_along(sentences))
      sentences
    },
    .package = "spacyr",
    .env = parent.frame()
  )
  iscores_environment$model <- function(...) {
    numbers <- sample(1:10, 4, replace = TRUE)
    numbers <- numbers / sum(numbers)
    labels <- c("01 – Economy: Positive", "01 – Economy: Negative", "02 – Health: Positive", "02 – Health: Negative")
    list(lapply(seq_along(numbers), function(i) list(label = labels[i], score = numbers[[i]])))
  }

  test
}

test_that("the process platform pipeline creates a tibble with the correct structure", {
  test <- setup_test()

  tibble <- process_platform_emphasis(minorparties::sample_data) |>
    process_platform_position() |>
    calculate_iscores()

  expect_true(nrow(validation(tibble, "finished")) == 0)
})

test_that("process_platform_emphasis correctly errors if python is not available", {
  test <- setup_test()
  local_mocked_bindings(
    spacy_initialize = function(...) rlang::abort("Spacy_initialize() error"),
    .package = "spacyr"
  )

  expect_error(process_platform_emphasis(minorparties::sample_data), "Python environment is not properly configured. Please run `install_python\\(\\)` to set it up.")
})

test_that("the validation procedure correctly errors if the tibble is incorrectly structured", {
  test <- setup_test()

  tryCatch(
    {
      process_platform_emphasis("test")
    },
    error = function(e) {
      expect_equal(e$message, "The tibble is incorrectly structured. See the returned tibble for details.")
      expect_true(nrow(e$tibble) == 3)
      expect_true(all(c("The `tibble` input must be a dataframe", "The `text` column must be a character column", "No entries in the `text` column can be missing") %in% e$tibble$error))
    }
  )
})

test_that("the argument validation procedure correctly errors if an argument is not the correct type", {
  test <- setup_test()

  expect_error(process_platform_emphasis(minorparties::sample_data, cleaning = "TRUE"), "The cleaning input must be a boolean.")
})
