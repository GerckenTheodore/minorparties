#' Function that calculates minor parties' I-Scores with confidence intervals
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
#' @param n The number of bootstrap samples to take for each minor party.
#' @param p_threshold The maximum p-value for a relationship to be considered significant (0.05 by default).
#' @param core_threshold The minimum score a minor party must have for an issue-area for it to be considered a core issue (0.05 by default).
#' @param exclude_nonconvergence Whether to treat issues where the Wordfish model did not converge as NA when calculating Ip Scores (TRUE by default).
#' @return A tibble with three rows per minor party (`upper`, `lower`, and `estimate`), containing the upper and lower bounds of the 95% confidence interval for the party's `ie_score`, `ie_score_interpreted`, and `ip_score` along with the point estimate.
#' @export

confidence_intervals <- function(tibble, n = 1000, p_threshold = 0.05, core_threshold = 0.05, exclude_nonconvergence = TRUE) {
  # Check that the inputs are correctly structured
  validator_tibble <- validation(tibble, "iscores")
  if (nrow(validator_tibble) > 0) {
    print(validator_tibble)
    rlang::abort("The tibble is incorrectly structured.", tibble = validator_tibble)
  }
  if (!is.numeric(n) || n <= 1) rlang::abort("The n must be a whole number greater than 1.")
  if (!is.numeric(p_threshold) || p_threshold < 0 || p_threshold > 1) rlang::abort("The p_threshold must be a number between 0 and 1.")
  if (!is.numeric(core_threshold) || core_threshold < 0 || core_threshold > 1) rlang::abort("The core_threshold must be a number between 0 and 1.")
  if (!is.logical(exclude_nonconvergence)) rlang::abort("The exclude_nonconvergence input must be a boolean.")
  tibble <- tibble::as_tibble(tibble)
  n <- round(n)

  # Bootstrap I-Scores for each minor party
  iscores_results <- calculate_iscores(tibble, p_threshold, core_threshold, exclude_nonconvergence, calculation_tables = TRUE)
  purrr::map2_dfr(iscores_results$party, iscores_results$scores, function(party_v, scores_v) {
    ie_score_tibble <- scores_v$ie_score_tibble
    ip_score_tibble <- scores_v$ip_score_tibble
    party_row <- dplyr::filter(tibble, party == party_v)
    top_issues <- names(dplyr::select(ie_score_tibble, -c(party_number, name, weight, party)))
    top_issue_tibble <- party_row |>
      purrr::pluck("overall_emphasis_scores", 1) |>
      dplyr::filter(issue %in% top_issues)

    scores <- purrr::map_dfr(1:n, function(i) {
      sampled_issues <- sample(top_issues, size = length(top_issues), replace = TRUE)
      sampled_top_issues <- tibble::tibble(issue = sampled_issues) |>
        dplyr::count(issue, name = "frequency") |>
        dplyr::right_join(top_issue_tibble, by = "issue") |>
        dplyr::mutate(frequency = tidyr::replace_na(frequency, 0)) |>
        dplyr::mutate(weighted = score * frequency) |>
        dplyr::mutate(weighted_score = (weighted / sum(weighted)) * sum(score)) |>
        dplyr::mutate(score = weighted_score) |>
        dplyr::select(issue, score) |>
        dplyr::arrange(factor(issue, levels = top_issues))
      sampled_party_row <- party_row
      sampled_party_row$overall_emphasis_scores[[1]] <- sampled_top_issues

      ie_scores <- ie_score_sum(ie_score_tibble, party_row = sampled_party_row, top_issues, p_threshold)
      ip_scores <- ip_score_sum(ip_score_tibble, party_row = sampled_party_row, top_issues, p_threshold)

      tibble::tibble(
        ie_score = ie_scores$ie_score,
        ie_score_interpreted = ie_scores$ie_score_interpreted,
        ip_score = ip_scores
      )
    })

    # Calculate Confidence Intervals
    tibble::tibble(
      party = party_v,
      side = c("lower", "upper", "estimate"),
      ie_score = c(quantile(scores$ie_score, probs = c(0.025, 0.975)), scores_v$ie_score),
      ie_score_interpreted = c(quantile(scores$ie_score_interpreted, probs = c(0.025, 0.975)), scores_v$ie_score_interpreted),
      ip_score = c(quantile(scores$ip_score, probs = c(0.025, 0.975)), scores_v$ip_score)
    )
  })
}
