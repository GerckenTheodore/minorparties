# Create environment at load to store ManifestoBERTA in
iscores_environment <- new.env(parent = emptyenv())

#' Helper function to setup python tools required for processing platforms
#'
#' @param env_name Name of the conda environment to setup ("iscores" by default)
#' @param install Whether to install missing packages (TRUE by default)
#' @param python_version Version of Python to use if a new environment must be created ("latest" by default)
#' @param manifestoberta_model_id Version of the ManifestoBERTA model to use ("2024-1-1" by default)
#' @return Returns invisible(TRUE) when setup is successful, otherwise it provides a descriptive error
#' @export

configure_python <- function(env_name = "iscores", install = TRUE, python_version = "latest", manifestoberta_model_id = "2024-1-1") {
  # Setup Reticulate
  conda_path <- tryCatch(reticulate::conda_binary("auto"), error = function(e) "") # If conda is not installed (it has no path), install it
  if (!nzchar(conda_path)) {
    if (isFALSE(install)) stop("Miniconda is not installed. Please run `configure_python(install = TRUE)` first.")
    reticulate::install_miniconda()
  }
  if (reticulate::py_available(initialize = FALSE)) { # If the conda environment is already bound, check to make sure it is the correct one
    if (!identical(reticulate::py_discover_config()$python_env_name, env_name)) stop("The conda environment is already bound. Please restart R and run `configure_python()` again.")
  }

  if (!(env_name %in% reticulate::conda_list()$name)) { # If there is no conda environment with the correct name, create one
    if (isFALSE(install)) stop(paste("Conda environment", env_name, "does not exist. Please run `configure_python(install = TRUE)` first."))
    python_version <- if (identical(python_version, "latest")) "python" else paste0("python=", python_version)
    reticulate::conda_create(env_name, packages = python_version, channel = "conda-forge")
  }
  reticulate::use_condaenv(env_name, required = TRUE) # Bind the conda environment

  missing_packages <- setdiff(c("transformers", "torch", "sentencepiece", "spacy"), tolower(reticulate::py_list_packages(env_name)$package)) # Install the missing required packages into the environment
  if (length(missing_packages)) {
    if (isFALSE(install)) stop(paste("The python packages: ", paste(missing_packages, collapse = ", "), " are not installed. Please run `configure_python(install = TRUE)` first."))
    reticulate::conda_install(env_name, missing_packages, pip = TRUE)
  }

  # Setup ManifestoBERTA
  huggingfaceR::hf_python_depends()
  model_id <- paste0("manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-", manifestoberta_model_id)
  if (identical(iscores_environment$modelId, model_id)) { # Skip if the ManifestoBERTA model is already loaded
    return(invisible(TRUE))
  }
  iscores_environment$model <- huggingfaceR::hf_load_pipeline(model_id, task = "text-classification", tokenizer = "xlm-roberta-large", truncation = TRUE, max_length = 512L, trust_remote_code = TRUE, top_k = NULL) # Load the ManifestoBERTA model
  iscores_environment$modelId <- model_id

  invisible(TRUE)
}
