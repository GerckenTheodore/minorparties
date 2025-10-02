#' Function that calculates minor parties' I-Scores
#'
#' @param tibble Tibble with one row per platform, containing, at minimum (this function is designed to work with the output of `process_platform_position()`):
#'  - `party`: The party's name (character) (this column must be unique for each platform)
#'  - `sentence_emphasis_scores`: A list of tibbles, one per sentence in the platform (in order). Each tibble has:
#'     - `sentence`: The sentence (character)
#'     - `scores`: A tibble with the sentence's emphasis scores, containing:
#'         - `issue`: The issue's name (character) (every sentence in every platform must have the same issue-areas)
#'         - `score`: The sentence's score for that issue (numeric, summing to 1)
#'  - `overall_emphasis_scores`: A tibble with the platform's overall emphasis scores, containing:
#'       - `issue`: The issue's name (character)
#'       - `score`: The platform's score for that issue (numeric, summing to 1)
#'  - `position_scores`: A tibble with the platform's position-score (and standard error) for each issue-area (flagged if the Wordfish model did not converge)
#'  - `minor_party`: Whether the party is a minor party (boolean)
#'  - `major_party_platforms`: Only needed for minor parties. A list of lists with "before", "after", and "weight" entries, containing the name of a major party's platform before or after the minor party and the weight that should be given to the party's changes in IScore calculations.
#' @param p_threshold The maximum p-value for a relationship to be considered significant (0.05 by default).
#' @param core_threshold The minimum score a minor party must have for an issue-area for it to be considered a core issue (0.05 by default).
#' @param exclude_nonconvergence Whether to treat issues where the Wordfish model did not converge as NA when calculating Ip Scores (TRUE by default).
#' @param calculation_tables Whether to return the tables used to calculate I-scores.
#' @return A tibble, containing the minor parties, with the list-column `scores` containing `ie_score`, `ie_score_interpreted`, and `ip_score`. If `calculation_tables` is TRUE, `scores` will also include `ie_score_table` and `ip_score_table`
#' @export

calculate_iscores <- function(tibble, p_threshold = 0.05, core_threshold = 0.05, exclude_nonconvergence = TRUE, calculation_tables = FALSE) {
  # Check that the inputs are correctly structured
  validator_tibble <- validation(tibble, "iscores")
  if (nrow(validator_tibble) > 0) {
    print(validator_tibble)
    rlang::abort("The tibble is incorrectly structured.", tibble = validator_tibble)
  }
  if (!is.numeric(p_threshold) || p_threshold < 0 || p_threshold > 1) rlang::abort("The p_threshold must be a number between 0 and 1.")
  if (!is.numeric(core_threshold) || core_threshold < 0 || core_threshold > 1) rlang::abort("The core_threshold must be a number between 0 and 1.")
  if (!is.logical(exclude_nonconvergence)) rlang::abort("The exclude_nonconvergence input must be a boolean.")
  if (!is.logical(calculation_tables)) rlang::abort("The calculation_tables input must be a boolean.")
  tibble <- tibble::as_tibble(tibble)

  # Pull the major party data relevant for each minor party
  lookup_table <- tibble |>
    dplyr::select(party, sentence_emphasis_scores, overall_emphasis_scores, position_scores) |>
    split(tibble$party)
  minor_parties <- tibble |>
    dplyr::filter(minor_party) |>
    dplyr::mutate(major_party_info = purrr::map(major_party_platforms, function(platforms) {
      purrr::map(platforms, function(major) {
        list(before = lookup_table[[major$before]], after = lookup_table[[major$after]], weight = major$weight)
      })
    }))

  # Calculate I Scores for each minor party
  minor_parties |>
    dplyr::mutate(scores = purrr::map(minor_parties$party, function(party_v) {
      party_row <- dplyr::filter(minor_parties, party == party_v)
      major_info <- party_row$major_party_info[[1]]

      top_issues <- party_row |>
        purrr::pluck("overall_emphasis_scores", 1) |>
        dplyr::filter(score > core_threshold) |>
        dplyr::arrange(issue) |>
        dplyr::pull(issue)
      top_issues <- party_row |>
        purrr::pluck("position_scores", 1) |>
        dplyr::filter(issue %in% top_issues & !is.na(score)) |>
        dplyr::arrange(issue) |>
        dplyr::pull(issue)

      sort_scores <- function(scores, to_pull = "score") {
        scores[[1]] |>
          dplyr::filter(issue %in% top_issues) |>
          dplyr::arrange(factor(issue, levels = top_issues)) |>
          dplyr::pull(to_pull)
      }

      # Ie Scores
      pull_sentence_scores <- function(sentence_emphasis_scores, issue_v) {
        purrr::map_dbl(sentence_emphasis_scores[[1]], function(sentence) {
          matching_row <- dplyr::filter(sentence$scores[[1]], issue == issue_v)
          matching_row$score
        })
      }

      ie_score_tibble <- purrr::imap_dfr(major_info, function(major, i) {
        before_scores <- sort_scores(major$before$overall_emphasis_scores)
        after_scores <- sort_scores(major$after$overall_emphasis_scores)
        change <- after_scores - before_scores

        statistical_significance <- purrr::map_dbl(top_issues, function(issue) {
          before <- pull_sentence_scores(major$before$sentence_emphasis_scores, issue)
          after <- pull_sentence_scores(major$after$sentence_emphasis_scores, issue)
          wilcox.test(before, after, alternative = "two.sided", exact = FALSE)$p.value
        })
        weight <- major$weight

        return_tibble <- rbind(before_scores, after_scores, change, statistical_significance)
        colnames(return_tibble) <- top_issues
        return_tibble <- tibble::as_tibble(return_tibble)
        return_tibble |>
          dplyr::mutate(party_number = i, name = c("before", "after", "change", "significance"), party = c(major$before$party, major$after$party, NA, NA), weight = weight) |>
          dplyr::select(party_number, name, weight, dplyr::everything())
      })

      ie_score <- ie_score_sum(ie_score_tibble, party_row, top_issues, p_threshold)

      # Ip Scores
      minor_position_scores <- party_row |>
        purrr::pluck("position_scores", 1) |>
        dplyr::filter(issue %in% top_issues) |>
        dplyr::arrange(factor(issue, levels = top_issues)) |>
        dplyr::pull(score)

      ip_score_tibble <- purrr::imap_dfr(major_info, function(major, i) {
        before_scores <- sort_scores(major$before$position_scores)
        before_se <- sort_scores(major$before$position_scores, "se")
        after_scores <- sort_scores(major$after$position_scores)
        after_se <- sort_scores(major$after$position_scores, "se")
        before_distance <- abs(minor_position_scores - before_scores)
        after_distance <- abs(minor_position_scores - after_scores)
        change <- before_distance - after_distance
        weight <- major$weight

        if (exclude_nonconvergence) {
          convergence <- sort_scores(major$before$position_scores, "convergence") & sort_scores(major$after$position_scores, "convergence")
          before_scores[!convergence] <- NA
          before_se[!convergence] <- NA
        }

        statistical_significance <- rep(NA_real_, length(before_scores))
        not_NA <- !is.na(before_scores) & !is.na(before_se) & !is.na(after_scores) & !is.na(after_se)
        z <- (before_scores[not_NA] - after_scores[not_NA]) / sqrt(before_se[not_NA]^2 + after_se[not_NA]^2)
        statistical_significance[not_NA] <- 2 * pnorm(-abs(z))

        return_tibble <- rbind(before_scores, after_scores, change, statistical_significance)
        colnames(return_tibble) <- top_issues
        return_tibble <- tibble::as_tibble(return_tibble)
        return_tibble |>
          dplyr::mutate(party_number = i, name = c("before", "after", "change", "significance"), party = c(major$before$party, major$after$party, NA, NA), weight = weight) |>
          dplyr::select(party_number, name, weight, dplyr::everything())
      })

      ip_score <- ip_score_sum(ip_score_tibble, party_row, top_issues, p_threshold)

      # Return Scores
      return_list <- list(ie_score = ie_score$ie_score, ie_score_interpreted = ie_score$ie_score_interpreted, ip_score = ip_score)
      if (calculation_tables) {
        return_list$ie_score_tibble <- ie_score_tibble
        return_list$ip_score_tibble <- ip_score_tibble
      }
      return_list
    })) |>
    dplyr::select(party, scores)
}
