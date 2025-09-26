#' Helper function to setup python tools required for processing platforms
#'
#' @param env_name Name of the python virtual environment to setup ("iscores" by default)
#' @param manifestoberta_model_id Version of the ManifestoBERTA model to use ("2024-1-1" by default)
#' @return Returns invisible(TRUE) when setup is successful, otherwise it provides a descriptive error
#' @export

configure_python <- function(env_name = "iscores", manifestoberta_model_id = "2024-1-1") {
  # Setup Spacy
  if (!(env_name %in% reticulate::virtualenv_list())) {
    reticulate::virtualenv_create(envname = env_name, python = Sys.which("python3"))
  }
  reticulate::use_virtualenv(env_name, required = TRUE)

  python_binary <- reticulate::virtualenv_python(env_name)
  suppressWarnings(spacyr::spacy_install(conda = FALSE, envname = env_name, python_executable = python_binary))
  try(spacyr::spacy_finalize(), silent = TRUE)
  suppressMessages(spacyr::spacy_initialize(python_executable = python_binary))

  # Setup ManifestoBERTA
  reticulate::virtualenv_install(envname = env_name, packages = c("transformers", "torch", "sentencepiece"), ignore_installed = TRUE)
  huggingfaceR::hf_python_depends()

  model_id <- paste0("manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-", manifestoberta_model_id)
  iscores_environment$model <- huggingfaceR::hf_load_pipeline(model_id, task = "text-classification", tokenizer = "xlm-roberta-large", truncation = TRUE, max_length = 512L, trust_remote_code = TRUE, top_k = NULL)

  invisible(TRUE)
}
