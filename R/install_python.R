#' Set up the python tools required for calculating I-Scores
#'
#' install_python() uses reticulate to create or activate a Python virtual environment and install the required packages for running spacyr (sentence tokenization) and huggingfaceR (issue-area classification via ManifestoBERTA).
#'
#' @param env_name Character. Name of the Python virtual environment to set up. Defaults to "iscores".
#' @param manifestoberta_model_id Character. Version of the ManifestoBERTA model to load (must match a valid model). Defaults to "2024-1-1".
#' @return Invisibly returns TRUE.
#' @export

install_python <- function(env_name = "iscores", manifestoberta_model_id = "2024-1-1") {
  # Ensures huggingfaceR is installed
  if (!rlang::is_installed("huggingfaceR")) rlang::abort("This function requires the huggingfaceR package. Install it from https://github.com/farach/huggingfaceR")

  # Ensures python is not bound to another environment and, if it is not, sets up the environment
  Sys.setenv(RETICULATE_AUTOCONFIGURE = "FALSE")
  if (reticulate::py_available(initialize = FALSE)) {
    current_env <- basename(reticulate::py_config()$virtualenv) # If python is already initialized, it is now safe to drop the initialize = FALSE argument
    if (current_env != env_name) rlang::abort(paste0("Python is already bound to the '", current_env, "' environment. Please restart R and try again."))
  }
  if (!env_name %in% reticulate::virtualenv_list()) reticulate::virtualenv_create(envname = env_name)
  reticulate::use_virtualenv(env_name, required = TRUE)
  reticulate::virtualenv_install(envname = env_name, packages = c("spacy", "transformers", "torch", "sentencepiece"))

  # Sets up spacyr
  path <- reticulate::py_config()$virtualenv
  Sys.setenv(SPACY_PYTHON = path)
  options(spacyr.python_executable = path)
  spacyr::spacy_download_langmodel("en_core_web_sm")

  # Sets up huggingfaceR and loads ManifestoBERTA
  hf_python_depends <- getExportedValue("huggingfaceR", "hf_python_depends")
  hf_load_pipeline <- getExportedValue("huggingfaceR", "hf_load_pipeline")

  hf_python_depends()
  tryCatch(
    {
      iscores_environment[["model"]] <- hf_load_pipeline(paste0("manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-", manifestoberta_model_id), task = "text-classification", tokenizer = "xlm-roberta-large", truncation = TRUE, max_length = 512L, trust_remote_code = TRUE, top_k = NULL)
    },
    error = function(e) rlang::abort("Could not load the specified ManifestoBERTA model. Ensure the model ID exists and Hugging Face is accessible.")
  )

  invisible(TRUE)
}
