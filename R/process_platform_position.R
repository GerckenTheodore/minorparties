#' Function that takes platforms, and scores their relative positions on every issue-area
#'
#' @param tibble Tibble with one row per platform, containing, at minimum (this function is designed to work with the output of `process_platform_emphasis()`):
#'  - `party`: The party's name (character) (this column must be unique for each platform)
#'  - `sentence_emphasis_scores`: A list of tibbles, one per sentence in the platform (in order). Each tibble has:
#'     - `sentence`: The sentence (character)
#'     - `scores`: A tibble with the sentence's emphasis scores, containing:
#'         - `issue`: The issue's name (character) (every sentence in every platform must have the same issue-areas)
#'         - `score`: The sentence's score for that issue (numeric, summing to 1)
#' @param inclusion_threshold The minimum score a sentence must have for an issue-area to be included in the overall emphasis scores (0.2 by default)
#' @return The same tibble with an addition list column `position_scores`, which contains a tibble for each platform with its position-score (and standard error) for each issue-area (flagged if the Wordfish model did not converge)
#' @export

process_platform_position <- function(tibble, inclusion_threshold = 0.2) {
  # Check that the inputs are correctly structured
  validator_tibble <- validation(tibble, "position")
  if (nrow(validator_tibble) > 0) {
    rlang::abort("The tibble is incorrectly structured.", tibble = validator_tibble)
  }
  if (!is.numeric(inclusion_threshold) || inclusion_threshold < 0 || inclusion_threshold > 1) rlang::abort("The inclusion_threshold must be a number between 0 and 1.")
  tibble <- tibble::as_tibble(tibble)

  # Pull the sentences that correspond to each issue
  issues <- tibble$sentence_emphasis_scores[[1]][[1]]$scores[[1]]$issue
  labeled_sentences <- tibble |>
    tidyr::unnest(sentence_emphasis_scores) |>
    tidyr::unnest(sentence_emphasis_scores) |>
    tidyr::unnest(scores) |>
    dplyr::filter(score > inclusion_threshold) |>
    dplyr::select(party, sentence, issue)

  # Calculate the position scores for each issue-area
  position_table <- purrr::map_dfr(issues, function(issue_v) {
    selected_sentences <- labeled_sentences |>
      dplyr::filter(issue == issue_v) |>
      dplyr::select(party, sentence)

    # Return if there are not enough sentences for a Wordfish analysis
    if (nrow(selected_sentences) < 3) {
      return(tibble::tibble(
        issue = issue_v,
        position_table = list(tibble::tibble(
          party = tibble$party,
          score = NA_real_,
          se = NA_real_,
          convergence = FALSE
        ))
      ))
    }

    # Create the dfm
    dfm_input <- selected_sentences |>
      dplyr::group_by(party) |>
      dplyr::summarise(full_text = paste(sentence, collapse = " "), .groups = "drop")
    dfm <- quanteda::corpus(dfm_input, text_field = "full_text", docid_field = "party") |>
      quanteda::tokens(remove_punct = TRUE, remove_numbers = TRUE) |>
      quanteda::tokens_tolower() |>
      quanteda::tokens_remove(quanteda::stopwords("en")) |>
      quanteda::tokens_keep("^[a-z]{3,}$", valuetype = "regex") |>
      quanteda::tokens_wordstem() |>
      quanteda::dfm() |>
      quanteda::dfm_trim(min_termfreq = 5)

    # Return if, after creating the dfm, there are not enough documents or terms
    if (length(quanteda::docnames(dfm)) < 2 || sum(dfm) == 0) {
      return(tibble::tibble(
        issue = issue_v,
        position_table = list(tibble::tibble(
          party = tibble$party,
          score = NA_real_,
          se = NA_real_,
          convergence = FALSE
        ))
      ))
    }

    # Run Wordfish
    warn_message <- ""
    wordfish <- withCallingHandlers(
      quanteda.textmodels::textmodel_wordfish(dfm),
      warning = function(warn) {
        warn_message <<- conditionMessage(warn)
        invokeRestart("muffleWarning")
      }
    )
    convergence <- !grepl("converge", warn_message)

    if (any(is.na(wordfish$theta))) {
      return(tibble::tibble(
        issue = issue_v,
        position_table = list(tibble::tibble(
          party = tibble$party,
          score = NA_real_,
          se = NA_real_,
          convergence = FALSE
        ))
      ))
    }
    tibble::tibble(
      issue = issue_v,
      position_table = list(
        dplyr::bind_rows(
          tibble::tibble(party = wordfish$docs, score = wordfish$theta, se = wordfish$se.theta, convergence),
          tibble::tibble(party = setdiff(tibble$party, wordfish$docs), score = NA_real_, se = NA_real_, convergence = FALSE)
        )
      )
    )
  })

  # Reformat results back into original tibble
  tibble |>
    dplyr::mutate(position_scores = purrr::map(tibble$party, function(party_v) {
      purrr::map_dfr(issues, function(issue_v) {
        position_table |>
          dplyr::filter(issue == issue_v) |>
          dplyr::pull(position_table) |>
          purrr::pluck(1) |>
          dplyr::filter(party == party_v) |>
          dplyr::mutate(issue = issue_v) |>
          dplyr::select(issue, score, se, convergence)
      })
    }))
}
