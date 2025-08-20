#' Function that takes platforms, and scores their relative positions on every issue-area
#'
#' @param tibble Tibble with one row per platform, containing, at minimum (this function is designed to work with the output of `process_platform_emphasis()`):
#'  - `party`: The party's name (character) (this column must be unique for each platform)
#'  - `sentence_emphasis_scores`: A list of tibbles, one per sentence in the platform (in order). Each tibble has:
#'     - `sentence`: The sentence (character)
#'     - `scores`: A tibble with the sentence's emphasis scores, containing:
#'         - `issue`: The issue's name (character) (every sentence in every platform must have the same issue-areas)
#'         - `score`: The sentence's score for that issue (numeric, summing to 100)
#' @param inclusion_threshold The minimum score a sentence must have for an issue-area to be included in the overall emphasis scores (0.2 by default)
#' @return The same tibble with an addition list column `position_scores`, which contains a tibble for each platform with its position-score (and standard error) for each issue-area (flagged if the Wordfish model did not converge)

process_platform_position <- function(tibble, inclusion_threshold = 0.2) {
  # Check that issue areas are consistent across all sentences
  issues <- tibble$sentence_emphasis_scores[[1]][[1]]$scores[[1]]$issue
  if (!all(purrr::map_lgl(tibble$sentence_emphasis_scores, function(platform) {
    all(purrr::map_lgl(platform, function(sentence) {
      issues_here <- sentence$scores[[1]]$issue
      setequal(issues_here, issues)
    }))
  }))) {
    stop("Every sentence in every platform must have the same issue-areas.")
  }

  # Check that parties are unique
  if (length(unique(tibble$party)) != nrow(tibble)) {
    stop("The 'party' column must be unique for each platform.")
  }

  # Pull the sentences that correspond to each issue
  labeled_sentences <- tibble |>
    tidyr::unnest(sentence_emphasis_scores) |>
    tidyr::unnest(sentence_emphasis_scores) |>
    tidyr::unnest(scores) |>
    dplyr::filter(score > inclusion_threshold) |>
    dplyr::select(party, sentence, issue)

  # Calculate the position scores for each issue-area
  position_table <- purrr::map_dfr(issues, function(issue) {
    selected_sentences <- labeled_sentences |>
      dplyr::filter(issue == !!issue) |>
      dplyr::select(party, sentence)

    # Return if there are not enough sentences for a Wordfish analysis
    if (nrow(selected_sentences) < 3) {
      return(tibble::tibble(
        issue,
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
        issue,
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
        issue,
        position_table = list(tibble::tibble(
          party = tibble$party,
          score = NA_real_,
          se = NA_real_,
          convergence = FALSE
        ))
      ))
    }
    tibble::tibble(
      issue,
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
    dplyr::mutate(position_scores = purrr::map(tibble$party, function(part) {
      purrr::map_dfr(issues, function(issu) {
        position_table |>
          dplyr::filter(issue == issu) |>
          dplyr::pull(position_table) |>
          purrr::pluck(1) |>
          dplyr::filter(party == part) |>
          dplyr::mutate(issue = issu) |>
          dplyr::select(issue, score, se, convergence)
      })
    }))
}
