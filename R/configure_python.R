#' Helper function to setup python tools required for processing platforms
#'
#' @param env_name Name of the python virtual environment to setup ("iscores" by default)
#' @param manifestoberta_model_id Version of the ManifestoBERTA model to use ("2024-1-1" by default)
#' @return Returns invisible(TRUE) when setup is successful, otherwise it provides a descriptive error
#' @export

configure_python <- function(env_name = "iscores", manifestoberta_model_id = "2024-1-1") {
  # Ensure Python Is Not Bound To Another Environment
  Sys.setenv(RETICULATE_AUTOCONFIGURE = "FALSE")
  if (reticulate::py_available(initialize = FALSE)) {
    current_env <- basename(reticulate::py_config()$virtualenv)
    if (current_env != env_name) rlang::abort(paste0("Python is already bound to the '", current_env, "' environment. Please restart R and try again."))
  }
  if (!env_name %in% reticulate::virtualenv_list()) reticulate::virtualenv_create(envname = env_name)
  reticulate::use_virtualenv(env_name, required = TRUE)

  # Setup Python Tools
  reticulate::virtualenv_install(envname = env_name, packages = c("spacy", "transformers", "torch", "sentencepiece"))

  path <- reticulate::py_config()$virtualenv
  Sys.setenv(SPACY_PYTHON = path)
  options(spacyr.python_executable = path)
  spacyr::spacy_download_langmodel("en_core_web_sm")

  huggingfaceR::hf_python_depends()
  tryCatch(
    {
      iscores_environment[["model"]] <- huggingfaceR::hf_load_pipeline(paste0("manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-", manifestoberta_model_id), task = "text-classification", tokenizer = "xlm-roberta-large", truncation = TRUE, max_length = 512L, trust_remote_code = TRUE, top_k = NULL)
    },
    error = function(e) rlang::abort("Could not load the specified ManifestoBERTA model. Ensure the model ID exists and Hugging Face is accessible.")
  )

  invisible(TRUE)
}
