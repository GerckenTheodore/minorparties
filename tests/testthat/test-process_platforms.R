setup_test <- function() {
  # Create test environment
  test <- new.env(parent = emptyenv())

  # Defer cleanup
  withr::defer(
    {
      rm(list = ls(envir = iscores_environment, all.names = TRUE), envir = iscores_environment)
      rm(list = ls(envir = test, all.names = TRUE), envir = test)
    },
    envir = testthat::teardown_env()
  )

  # Create tibble to run process_platform_emphasis on
  test$tibble <- tibble::tibble(
    party = c("Platform A", "Platform B"),
    text = c(
      "Our Nation's History is filled with the stories of brave men and women who gave everything they had to build America into the Greatest Nation in the History of the World. Generations of American Patriots have summoned the American Spirit of Strength, Determination, and Love of Country to overcome seemingly insurmountable challenges. The American People have proven time and again that we can overcome any obstacle and any force pitted against us. In the early days of our Republic, the Founding Generation defeated what was then the most powerful Empire the World had ever seen. In the 20th Century, America vanquished Nazism and Fascism, and then triumphed over Soviet Communism after forty-four years of the Cold War. But now we are a Nation in SERIOUS DECLINE. Our future, our identity, and our very way of life are under threat like never before. Today we must once again call upon the same American Spirit that led us to prevail through every challenge of the past if we are going to lead our Nation to a brighter future.",
      "Our nation is at an inflection point. What kind of America will we be? A land of more freedom, or less freedom? More rights or fewer? An economy rigged for the rich and powerful, or where everyone has a fair shot at getting ahead? Will we lower the temperature in our politics and come together, or treat each other as enemies instead? The stakes in this election are enormously high. President Biden and Vice President Harris took office during a time of global pandemic, record job loss, and record crime. These past four years, Democrats proved once again that democracy can deliver, and made tremendous progress turning the country around. President Biden and Vice President Harris turned a setback into a great American comeback for working families."
    )
  )

  # Mock functions that depend on an active python environment
  local_mocked_bindings(
    spacy_initialize = function(...) TRUE,
    spacy_finalize = function(...) TRUE,
    spacy_tokenize = function(text, ...) {
      sentences <- strsplit(text, "(?<=[.!?])\\s+", perl = TRUE)
      names(sentences) <- "text1"
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

test_that("the process_platform_emphasis and process_platform_position pipeline creates a tibble with the correct structure", {
  test <- setup_test()

  # Process the tibble and check that its return is correctly structured
  result <- process_platform_emphasis(test$tibble)
  result <- process_platform_position(result)
  expect_true(tibble::is_tibble(result))
  expect_setequal(result$party, c("Platform A", "Platform B"))
  expect_setequal(colnames(result), c("party", "text", "sentence_emphasis_scores", "overall_emphasis_scores", "position_scores"))
  expect_true(nzchar(result$text[[1]]))

  # Check that the sentence emphasis scores were calculated and formatted properly
  sentence_emphasis_scores <- result$sentence_emphasis_scores[[1]]
  expect_true(is.list(sentence_emphasis_scores))
  expect_true(length(sentence_emphasis_scores) > 2)
  test_case <- sentence_emphasis_scores[[1]]
  expect_true(is.character(test_case$sentence))
  expect_true(nchar(test_case$sentence) > 0)
  scores <- test_case$scores[[1]]
  expect_setequal(scores$issue, c("Economy", "Health"))
  expect_equal(sum(scores$score), 1)

  # Check that the overall emphasis scores were calculated and formatted properly
  overall_emphasis_scores <- result$overall_emphasis_scores[[2]]
  expect_setequal(overall_emphasis_scores$issue, c("Economy", "Health"))
  expect_equal(sum(overall_emphasis_scores$score), 1)

  # Check that the position scores were calculated and formatted properly
  position_scores <- result$position_scores[[1]]
  expect_setequal(colnames(position_scores), c("issue", "score", "se", "convergence"))
  expect_setequal(position_scores$issue, c("Economy", "Health"))
})

test_that("process_platform_emphasis stops if python is not available", {
  test <- setup_test()
  iscores_environment$model <- function(...) FALSE
  expect_error(process_platform_emphasis(test$tibble), "Python environment is not properly configured. Please run `configure_python\\(\\)` to set it up.")
})

test_that("process_platform_emphasis handles empty platform correctly", {
  test <- setup_test()
  test <- setup_test()
  test$tibble <- dplyr::mutate(test$tibble, text = NA_character_)

  result <- process_platform_emphasis(test$tibble)
  expect_equal(result$sentence_emphasis_scores[[1]], list())
  expect_equal(result$overall_emphasis_scores[[1]], list())
})

test_that("process_platform_position stops if issue areas are not consistent", {
  test <- setup_test()
  test$tibble <- process_platform_emphasis(test$tibble)
  test$tibble$sentence_emphasis_scores[[1]][[1]]$scores[[1]]$issue <- c("Economy", "National Security")

  expect_error(process_platform_position(test$tibble), "Every sentence in every platform must have the same issue-areas.")
})

test_that("process_platform_position stops if parties are not unique", {
  test <- setup_test()
  test$tibble <- process_platform_emphasis(test$tibble)
  test$tibble$party <- c("Platform A", "Platform A")

  expect_error(process_platform_position(test$tibble), "The 'party' column must be unique for each platform.")
})
