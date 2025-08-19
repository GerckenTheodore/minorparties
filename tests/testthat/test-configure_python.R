setup_test <- function() {
  # Create test environment
  test <- new.env(parent = emptyenv())

  # Defer cleanup
  withr::defer(
    {
      environment <- IScorePackage:::iscores_environment
      rm(list = ls(envir = environment, all.names = TRUE), envir = environment)
      rm(list = ls(envir = test, all.names = TRUE), envir = test)
    },
    envir = testthat::teardown_env()
  )

  # Create test flags
  test$created_env <- NULL
  test$installed_packages <- NULL

  # Mock functions that depend on an active python environment
  local_mocked_bindings(
    spacy_install = function(...) TRUE,
    spacy_finalize = function(...) TRUE,
    spacy_initialize = function(...) TRUE,
    .package = "spacyr",
    .env = parent.frame()
  )
  local_mocked_bindings(
    py_config = function(...) list(python = "/Users/User/.virtualenvs/iscores/bin/python"),
    virtualenv_list = function(...) c(""),
    virtualenv_create = function(envname, ...) test$created_env <- envname,
    virtualenv_install = function(env_name, packages, ...) test$installed_packages <- packages,
    .package = "reticulate",
    .env = parent.frame()
  )
  local_mocked_bindings(
    hf_python_depends = function(...) TRUE,
    hf_load_pipeline = function(...) "mock_pipeline",
    .package = "huggingfaceR",
    .env = parent.frame()
  )

  test
}

test_that("configure_python correctly steps through setup process", {
  test <- setup_test()

  expect_invisible(configure_python())
  expect_equal(test$created_env, "iscores")
  expect_setequal(test$installed_packages, c("transformers", "torch", "sentencepiece"))
  expect_identical(iscores_environment$model, "mock_pipeline")
  expect_identical(iscores_environment$modelId, "manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-2024-1-1")
})

test_that("configure_python handles already-bound environment correctly", {
  test <- setup_test()

  expect_error(configure_python(env_name = "iscores_two"), regexp = "different, currently active python environment")
})
