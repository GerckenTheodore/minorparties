setup_test <- function() {
  # Create test environment
  test <- new.env(parent = emptyenv())

  # Create test flags
  test$miniconda_installed <- FALSE
  test$created_env <- NULL
  test$installed_packages <- NULL

  # Mock reticulate and huggingfaceR functions that depend on an active python environment
  local_mocked_bindings(
    conda_binary = function(...) "",
    install_miniconda = function(...) test$miniconda_installed <- TRUE,
    py_available = function(...) FALSE,
    py_discover_config = function(...) list(python_env_name = ""),
    conda_list = function(...) data.frame(name = character()),
    conda_create = function(env_name, ...) test$created_env <- env_name,
    use_condaenv = function(...) TRUE,
    py_list_packages = function(...) data.frame(package = character()),
    conda_install = function(env_name, packages, ...) test$installed_packages <- packages,
    .package = "reticulate",
    .env = parent.frame()
  )
  local_mocked_bindings(
    `hf_python_depends` = function(...) TRUE,
    `hf_load_pipeline` = function(...) "mock_pipeline",
    .package = "huggingfaceR",
    .env = parent.frame()
  )

  test
}

test_that("configure_python creates environments correctly", {
  # Ensure package environment is cleaned after test
  withr::defer({
    rm(list = ls(envir = iscores_environment), envir = iscores_environment)
  })
  test <- setup_test()

  # Test that the function runs without error and fully sets up the environment
  expect_invisible(configure_python())
  expect_true(test$miniconda_installed)
  expect_equal(test$created_env, "iscores")
  expect_setequal(test$installed_packages, c("transformers", "torch", "sentencepiece", "spacy"))
  expect_identical(iscores_environment$model, "mock_pipeline")
})

test_that("configure_python handles needed but unauthorized installations correctly", {
  withr::defer({
    rm(list = ls(envir = iscores_environment), envir = iscores_environment)
  })
  test <- setup_test()

  # Test that the function throws the correct error when the user provides an install = FALSE flag and check that it does not install anything
  expect_error(configure_python(install = FALSE), "Miniconda is not installed. Please run `configure_python\\(install = TRUE\\)` first.")
  expect_false(test$miniconda_installed)
  expect_null(test$created_env)
  expect_null(test$installed_packages)
})
