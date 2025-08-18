# Declare global variables to stop erroneous notes
globalVariables(c("text", "issue", "score", "scores", "sentence_emphasis_scores"))

#' Function that takes platforms, splits them into sentences, and calculates issue-area emphasis scores
#'
#' @param tibble Tibble with one row per platform, containing, at minimum:
#'  - `text`: A character column with the full text of each platform
#' @param cleaning Whether to apply basic text-cleaning before processing platforms (TRUE by default)
#' @return The same tibble with two additional list columns (if a platform cannot be processed due to a lack of text, the function will return an empty list for that platform):
#'  - `sentence_emphasis_scores`: A list of tibbles, one per sentence in the platform (in order). Each tibble has:
#'     - `sentence`: the sentence (character)
#'     - `scores`: a tibble with the sentence's emphasis scores, containing:
#'         - `issue`: the issue's name (character)
#'         - `score`: the sentence's score for that issue (numeric, summing to 100)
#'  - `overall_emphasis_scores`: A tibble with the platform's overall emphasis scores, containing:
#'     - `issue`: the issue's name (character)
#'     - `score`: the platform's score for that issue (numeric, summing to 100)
#' @export

process_platform_emphasis <- function(tibble, cleaning = TRUE) {
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
  if (isTRUE(cleaning)) {
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
    dplyr::mutate(sentence_emphasis_scores = purrr::map(text, function(text) { # Go through platforms, one-by-one
      if (is.na(text) || !nzchar(text)) { # If no text survives cleaning, return an empty list
        return(list())
      }

      sentences <- spacyr::spacy_tokenize(text, what = "sentence", simplify = TRUE) # Split the text into tokenized sentences
      sentences <- sentences[nchar(sentences) > 0]
      corpus <- as.list(unname(sentences))
      if (!length(corpus)) { # If no sentences remain, return an empty list
        return(list())
      }

      purrr::map(seq_along(corpus), function(i) { # Go through sentences, one-by-one
        current_sentence <- corpus[[i]] # Use ManifestoBERTA to score the sentence in its context
        previous_sentence <- if (i > 1) corpus[[i - 1]] else ""
        next_sentence <- if (i < length(corpus)) corpus[[i + 1]] else ""
        context <- stringr::str_squish(paste(previous_sentence, current_sentence, next_sentence, sep = " "))
        scores <- iscores_environment$model(list(list(text = current_sentence, text_pair = context)))[[1]]

        scores <- tibble::tibble(issue = purrr::map_chr(scores, "label"), score = purrr::map_dbl(scores, "score")) |> # Combine dichotomous issue-areas and create a tibble with the sentence's score on each issue-area
          dplyr::mutate(
            issue = issue |>
              stringr::str_remove("^[0-9]+\\s+[\\u2013-]\\s+") |>
              stringr::str_remove(":\\s*(Positive|Negative)$")
          ) |>
          dplyr::group_by(issue) |>
          dplyr::summarize(score = sum(score), .groups = "drop") |>
          dplyr::arrange(dplyr::desc(score))

        tibble::tibble( # Assemble the final sentence tibble
          sentence = current_sentence,
          scores = list(scores)
        )
      })
    }))

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
        dplyr::mutate(score = score / sum(score) * 100) |>
        dplyr::arrange(dplyr::desc(score))
    }))

  # Wrap up spacy process
  spacyr::spacy_finalize()

  return(tibble)
}
