iscores_environment <- new.env(parent = emptyenv())

#' Helper function to setup python tools required for processing platforms
#'
#' @param env_name Name of the python virtual environment to setup ("iscores" by default)
#' @param manifestoberta_model_id Version of the ManifestoBERTA model to use ("2024-1-1" by default)
#' @return Returns invisible(TRUE) when setup is successful, otherwise it provides a descriptive error
#' @export

configure_python <- function(env_name = "iscores", manifestoberta_model_id = "2024-1-1") {
  # Setup Spacy
  environment <- basename(dirname(dirname(reticulate::py_config()$python)))
  if (environment != env_name) {
    stop("There is a different, currently active python environment. Please restart R and then run `configure_python()` again.")
  }

  Sys.setenv(SPACY_PYTHON = env_name)
  if (!(env_name %in% reticulate::virtualenv_list())) {
    reticulate::virtualenv_create(envname = env_name, python = Sys.which("python3"))
  }
  spacyr::spacy_install()
  try(spacyr::spacy_finalize(), silent = TRUE)
  spacyr::spacy_initialize()

  # Setup ManifestoBERTA
  reticulate::virtualenv_install(envname = Sys.getenv("SPACY_PYTHON"), packages = c("transformers", "torch", "sentencepiece"), ignore_installed = TRUE)
  huggingfaceR::hf_python_depends()
  model_id <- paste0("manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-", manifestoberta_model_id)
  if (identical(iscores_environment$modelId, model_id)) {
    return(invisible(TRUE))
  }

  iscores_environment$model <- huggingfaceR::hf_load_pipeline(model_id, task = "text-classification", tokenizer = "xlm-roberta-large", truncation = TRUE, max_length = 512L, trust_remote_code = TRUE, top_k = NULL)
  iscores_environment$modelId <- model_id

  invisible(TRUE)
}
