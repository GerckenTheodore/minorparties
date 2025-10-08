#' Calculate platforms' issue-area position scores
#'
#' process_platform_position() takes a tibble of platforms that have already been processed with process_platform_emphasis() and calculates issue-area position scores for each platform using the Wordfish model. These position scores represent each platform's relative position on each issue-area compared to the others in the tibble (the positive end of the scale is arbitrarily defined).
#'
#' @param tibble Tibble. One row per platform, containing, at minimum (this function is designed to work with the output of process_platform_emphasis()):
#' \itemize{
#'   \item{party}{: Character column. The party's name (this column must be unique for each platform).}
#'   \item{sentence_emphasis_scores}{: List column. A list per sentence in the platform (in order), containing:}
#'   \itemize{
#'     \item{sentence}{: Character. The sentence.}
#'     \item{scores}{: Tibble. The sentence's emphasis score on each issue-area, containing:}
#'     \itemize{
#'       \item{issue}{: Character column. The issue-area name.}
#'       \item{score}{: Numeric column. The sentence's score for that issue-area (summing to 1).}
#'     }
#'   }
#' }
#' @param inclusion_threshold Numeric. The minimum probability a sentence must have of discussing an issue-area to be included in that issue-area's Wordfish model. Defaults to 0.2.
#' @return Tibble. The input tibble with an additional list column:
#' \itemize{
#'   \item{position_scores}{: List column. A tibble, containing:}
#'   \itemize{
#'     \item{issue}{: Character column. The issue-area name.}
#'     \item{position_score}{: Numeric column. The platform's position score on the issue-area (NA if the platform did not have enough material about the issue-area to generate a score).}
#'     \item{se}{: Numeric column. The standard error of the position score.}
#'     \item{convergence}{: Logical column. Whether the Wordfish model converged (if the estimation algorithm reached a stable set of position scores without divergence).}
#'   }
#' }
#' @export

process_platform_position <- function(tibble, inclusion_threshold = 0.2) {
  # Checks that the inputs are correctly structured
  validator_tibble <- validation(tibble, "position")
  if (nrow(validator_tibble) > 0) {
    rlang::abort("The tibble is incorrectly structured.", tibble = validator_tibble)
  }
  if (!is.numeric(inclusion_threshold) || inclusion_threshold < 0 || inclusion_threshold > 1) rlang::abort("The inclusion_threshold must be a number between 0 and 1.")
  tibble <- tibble::as_tibble(tibble)

  # Ensures python tools work
  manifestoBERTA_test <- tryCatch(
    {
      result <- iscores_environment$model(list(list(text = "These principles are under threat.", text_pair = paste("Human rights and international humanitarian law are fundamental pillars of a secure global system. These principles are under threat. Some of the world's most powerful states choose to sell arms to human-rights abusing states."))))
      is.list(result) && length(result) > 0
    },
    error = function(e) FALSE
  )
  if (!manifestoBERTA_test) stop("Python environment is not properly configured. Please run `configure_python()` to set it up.")

  # Pulls the sentences that correspond to each issue (Wordfish needs every sentence in an issue-area, regardless of origin, to run most accuratly)
  issues <- tibble$sentence_emphasis_scores[[1]][[1]]$scores[[1]]$issue
  labeled_sentences <- tibble |>
    tidyr::unnest(sentence_emphasis_scores) |>
    tidyr::unnest(sentence_emphasis_scores) |>
    tidyr::unnest(scores) |>
    dplyr::filter(score > inclusion_threshold) |>
    dplyr::select(party, sentence, issue)

  # Calculates the position scores for each issue-area
  position_table <- purrr::map_dfr(issues, function(issue_v) {
    selected_sentences <- labeled_sentences |>
      dplyr::filter(issue == issue_v) |>
      dplyr::select(party, sentence)

    # Returns if there are not enough sentences for a Wordfish analysis (will propagate NAs through to final output, excluding the issue-area from any further analysis)
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

    # Creates the dfm
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

    # Returns if, after creating the dfm, there are not enough documents or terms (as above, this will propagate NAs through to final output)
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

    # Runs Wordfish
    warn_message <- ""
    wordfish <- withCallingHandlers(
      quanteda.textmodels::textmodel_wordfish(dfm),
      warning = function(warn) {
        warn_message <<- conditionMessage(warn)
        invokeRestart("muffleWarning")
      }
    )
    convergence <- !grepl("converge", warn_message) # If the model did not converge, this will be FALSE, allowing users to filter out non-converged models in calculate_i_scores() if they wish

    if (any(is.na(wordfish$theta))) { # If Wordfish fails to produce results, propagates NAs through to final output
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
    tibble::tibble( # Formats the relative positions of every platform into a tibble
      issue = issue_v,
      position_table = list(
        dplyr::bind_rows(
          tibble::tibble(party = wordfish$docs, score = wordfish$theta, se = wordfish$se.theta, convergence),
          tibble::tibble(party = setdiff(tibble$party, wordfish$docs), score = NA_real_, se = NA_real_, convergence = FALSE)
        )
      )
    )
  }, .progress = list(
    name = "Calculating the position scores for each issue-area",
    clear = TRUE,
    type = "iterator"
  ))

  # Reformats results back into original tibble (each platform gets its position scores on every issue-ara)
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
