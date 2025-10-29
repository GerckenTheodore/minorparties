setup_test <- function() {
  # Create test environment
  test <- new.env(parent = emptyenv())
  test$created_env <- NULL
  test$installed_packages <- NULL

  # Mock functions that depend on an active python environment
  local_mocked_bindings(
    py_available = function(...) FALSE,
    virtualenv_list = function(...) c(""),
    virtualenv_create = function(envname, ...) test$created_env <- envname,
    use_virtualenv = function(...) TRUE,
    virtualenv_install = function(env_name, packages, ...) test$installed_packages <- packages,
    py_config = function(...) list(virtualenv = "/Users/User/.virtualenvs/iscores"),
    .package = "reticulate",
    .env = parent.frame()
  )
  local_mocked_bindings(
    spacy_download_langmodel = function(...) TRUE,
    .package = "spacyr",
    .env = parent.frame()
  )
  local_mocked_bindings(
    is_installed = function(...) TRUE,
    .package = "rlang",
    .env = parent.frame()
  )
  local_mocked_bindings(
    getExportedValue = function(package, name) {
      if (name == "hf_python_depends") {
        function(...) TRUE
      } else if (name == "hf_load_pipeline") {
        function(pipeline_name, ...) pipeline_name
      }
    },
    .package = "base",
    .env = parent.frame()
  )

  test
}

test_that("install_python correctly steps through setup process", {
  test <- setup_test()

  expect_invisible(install_python())
  expect_equal(test$created_env, "iscores")
  expect_setequal(test$installed_packages, c("spacy", "transformers", "torch", "sentencepiece"))
  expect_identical(iscores_environment$model, "manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-2024-1-1")
})

test_that("install_python correctly errors if huggingfaceR is not installed", {
  test <- setup_test()
  local_mocked_bindings(
    is_installed = function(...) FALSE,
    .package = "rlang"
  )

  expect_error(install_python(), "This function requires the huggingfaceR package. Install it from https://github.com/farach/huggingfaceR")
})

test_that("install_python correctly errors if Python is already bound to another environment", {
  test <- setup_test()
  local_mocked_bindings(
    py_available = function(...) TRUE,
    py_config = function(...) list(virtualenv = "/Users/User/.virtualenvs/not_iscores"),
    .package = "reticulate"
  )

  expect_error(install_python(), "Python is already bound to the 'not_iscores' environment. Please restart R and try again.")
})

test_that("install_python correctly errors if ManifestoBERTA model cannot be loaded", {
  test <- setup_test()
  local_mocked_bindings(
    getExportedValue = function(package, name) {
      if (name == "hf_python_depends") {
        function(...) TRUE
      } else if (name == "hf_load_pipeline") {
        function(pipeline_name, ...) stop("Could not load model")
      }
    },
    .package = "base"
  )

  expect_error(install_python(), "Could not load the specified ManifestoBERTA model. Ensure the model ID exists and Hugging Face is accessible.")
})
