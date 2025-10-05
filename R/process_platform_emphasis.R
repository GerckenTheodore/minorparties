#' Calculate platforms' issue-area emphasis scores
#'
#' `process_platform_emphasis()` takes a tibble of platforms, splits each platform into sentences, and calculates issue-area emphasis scores for each sentence and for the platform as a whole using the ManifestoBERTA model. These issue-area emphasis scores, respectively, represent the probability that each sentence is discussing each issue-area and the proportion of the platform that is devoted to each issue-area.
#'
#' @param tibble Tibble. One row per platform, containing, at minimum:
#'   - `text`: Character column. The full text of each platform.
#' @param cleaning Logical. Whether to apply basic text cleaning before processing each platform. Defaults to TRUE.
#' @return Tibble. The input tibble with two additional list columns (if a platform cannot be processed due to a lack of text, the function will return an empty list for that platform):
#'   - `sentence_emphasis_scores`: List column. A list per sentence in the platform (in order), containing:
#'     - `sentence`: Character. The sentence.
#'     - `scores`: Tibble. The sentence's emphasis score on each issue-area, containing:
#'       - `issue`: Character column. The issue-area name.
#'       - `score`: Numeric column. The sentence's score for that issue-area (summing to 1).
#'   - `overall_emphasis_scores`: List column. A tibble with the platform's overall emphasis scores, containing:
#'     - `issue`: Character column. The issue-area name.
#'     - `score`: Numeric column. The platform's score for that issue-area (summing to 1).
#' @export

process_platform_emphasis <- function(tibble, cleaning = TRUE) {
  # Check that the inputs are correctly structured
  validator_tibble <- validation(tibble, "emphasis")
  if (nrow(validator_tibble) > 0) {
    rlang::abort("The tibble is incorrectly structured.", tibble = validator_tibble)
  }
  if (!is.logical(cleaning)) rlang::abort("The cleaning input must be a boolean.")
  tibble <- tibble::as_tibble(tibble)

  # Ensure python tools work
  try(spacyr::spacy_finalize(), silent = TRUE)
  spacyr_test <- tryCatch(
    {
      spacyr::spacy_initialize()
      TRUE
    },
    error = function(e) FALSE
  )
  manifestoBERTA_test <- tryCatch(
    {
      result <- iscores_environment$model(list(list(text = "These principles are under threat.", text_pair = paste("Human rights and international humanitarian law are fundamental pillars of a secure global system. These principles are under threat. Some of the world's most powerful states choose to sell arms to human-rights abusing states."))))
      is.list(result) && length(result) > 0
    },
    error = function(e) FALSE
  )
  if (!spacyr_test || !manifestoBERTA_test) stop("Python environment is not properly configured. Please run `configure_python()` to set it up.")

  # Clean platforms with basic cleaning operations if requested
  if (cleaning) {
    tibble <- tibble |>
      dplyr::mutate(
        text = text |>
          stringr::str_replace_all("-\\s*\\n", "") |>
          stringr::str_replace_all("\\n+", " ") |>
          stringr::str_replace_all(c("\u201C" = "\"", "\u201D" = "\"", "\u2018" = "'", "\u2019" = "'", "\u2014" = "-", "\u2013" = "-", "\u2026" = "...")) |>
          stringi::stri_trans_general("Latin-ASCII") |>
          stringr::str_replace_all("[^A-Za-z0-9.,;:!?()\\[\\]{}\"'\\-\\s]", "") |>
          stringr::str_squish()
      )
  }

  # Split each platform into scored sentences
  tibble <- tibble |>
    dplyr::mutate(sentence_emphasis_scores = purrr::map(text, function(text_v) {
      if (is.na(text_v) || !nzchar(text_v)) {
        return(list())
      }

      sentences <- spacyr::spacy_tokenize(text_v, what = "sentence", simplify = TRUE)
      sentences <- sentences[nchar(sentences) > 0][[1]]
      if (!length(sentences)) {
        return(list())
      }

      purrr::map(seq_along(sentences), function(i) {
        current_sentence <- sentences[[i]]
        previous_sentence <- if (i > 1) sentences[[i - 1]] else ""
        next_sentence <- if (i < length(sentences)) sentences[[i + 1]] else ""
        context <- stringr::str_squish(paste(previous_sentence, current_sentence, next_sentence, sep = " "))
        scores <- iscores_environment$model(list(list(text = current_sentence, text_pair = context)))[[1]]

        scores <- tibble::tibble(issue = purrr::map_chr(scores, "label"), score = purrr::map_dbl(scores, "score")) |>
          dplyr::mutate(
            issue = issue |>
              stringr::str_remove("^[0-9]+\\s+[\\u2013-]\\s+") |>
              stringr::str_remove(":\\s*(Positive|Negative)$")
          ) |>
          dplyr::group_by(issue) |>
          dplyr::summarize(score = sum(score), .groups = "drop") |>
          dplyr::arrange(dplyr::desc(score))

        tibble::tibble(
          sentence = current_sentence,
          scores = list(scores)
        )
      })
    }, .progress = list(
      name = "Splitting each platform into scored sentences",
      clear = TRUE,
      type = "iterator"
    )))

  # Calculate overall emphasis scores for each platform
  tibble <- tibble |>
    dplyr::mutate(overall_emphasis_scores = purrr::map(sentence_emphasis_scores, function(emphasis_scores) {
      if (!length(emphasis_scores)) {
        return(list())
      }
      dplyr::bind_rows(emphasis_scores) |>
        tidyr::unnest(scores) |>
        dplyr::group_by(issue) |>
        dplyr::summarize(score = sum(score), .groups = "drop") |>
        dplyr::mutate(score = score / sum(score)) |>
        dplyr::arrange(dplyr::desc(score))
    }))

  # Wrap up spacy process
  spacyr::spacy_finalize()

  return(tibble)
}
