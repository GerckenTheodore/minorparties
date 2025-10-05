setup_test <- function() {
  # Create test environment
  test <- new.env(parent = emptyenv())

  # Create tibble to run process_platform_emphasis on
  test$tibble <- tibble::tibble(
    party = c("GOP 2024", "GOP 2016", "LP 2020"),
    text = c(
      "Our Nation's History is filled with the stories of brave men and women who gave everything they had to build America into the Greatest Nation in the History of the World. Generations of American Patriots have summoned the American Spirit of Strength, Determination, and Love of Country to overcome seemingly insurmountable challenges. The American People have proven time and again that we can overcome any obstacle and any force pitted against us. In the early days of our Republic, the Founding Generation defeated what was then the most powerful Empire the World had ever seen. In the 20th Century, America vanquished Nazism and Fascism, and then triumphed over Soviet Communism after forty-four years of the Cold War. But now we are a Nation in SERIOUS DECLINE. Our future, our identity, and our very way of life are under threat like never before. Today we must once again call upon the same American Spirit that led us to prevail through every challenge of the past if we are going to lead our Nation to a brighter future.",
      "We believe in American exceptionalism. We believe the United States of America is unlike any other nation on earth. We believe America is exceptional because of our historic role — first as refuge, then as defender, and now as exemplar of liberty for the world to see",
      "As Libertarians, we seek a world of liberty: a world in which all individuals are sovereign over their own lives and are not forced to sacrifice their values for the benefit of others. We believe that respect for individual rights is the essential precondition for a free and prosperous world, that force and fraud must be banished from human relationships, and that only through freedom can peace and prosperity be realized. Consequently, we defend each person’s right to engage in any activity that is peaceful and honest, and welcome the diversity that freedom brings. The world we seek to build is one where individuals are free to follow their own dreams in their own ways, without interference from government or any authoritarian power. In the following pages we set forth our basic principles and enumerate various policy stands derived from those principles."
    ),
    minor_party = c(FALSE, FALSE, TRUE),
    major_party_platforms = list(NA, NA, list(list(before = "GOP 2016", after = "GOP 2024", weight = 1)))
  )

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

  tibble <- process_platform_emphasis(test$tibble) |>
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

  expect_error(process_platform_emphasis(test$tibble), "Python environment is not properly configured. Please run `configure_python\\(\\)` to set it up.")
})

test_that("the validation procedure correctly errors if the tibble is incorrectly structured", {
  test <- setup_test()
  test$tibble <- "test"

  tryCatch(
    {
      process_platform_emphasis(test$tibble)
    },
    error = function(e) {
      expect_equal(e$message, "The tibble is incorrectly structured.")
      expect_true(nrow(e$tibble) == 3)
      expect_true(all(c("The `tibble` input must be a dataframe", "The `text` column must be a character column", "No entries in the `text` column can be missing") %in% e$tibble$error))
    }
  )
})

test_that("the argument validation procuedure correctly errors if an argument is not the correct type", {
  test <- setup_test()

  expect_error(process_platform_emphasis(test$tibble, cleaning = "TRUE"), "The cleaning input must be a boolean.")
})
