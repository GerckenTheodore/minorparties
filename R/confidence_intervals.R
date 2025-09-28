#' Function that calculates I-Score confidence intervals
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
#' @return A tibble with two rows per minor party (`upper` and `lower`), containing the upper and lower bounds of the 95% confidence interval for the party's `ie_score`, `ie_score_interpreted`, and `ip_score`.
#' @export

confidence_intervals <- function(tibble, n = 1000, p_threshold = 0.05, core_threshold = 0.05, exclude_nonconvergence = TRUE) {
  # Check that the inputs are correctly structured
  validator_tibble <- validation(tibble, "iscores")
  if (nrow(validator_tibble) > 0) {
    print(validator_tibble)
    rlang::abort("The tibble is incorrectly structured.", tibble = validator_tibble)
  }
  if (!is.integer(n) || p_threshold < 2) rlang::abort("The n must be a whole number greater than 1.")
  if (!is.numeric(p_threshold) || p_threshold < 0 || p_threshold > 1) rlang::abort("The p_threshold must be a number between 0 and 1.")
  if (!is.numeric(core_threshold) || core_threshold < 0 || core_threshold > 1) rlang::abort("The core_threshold must be a number between 0 and 1.")
  if (!is.logical(exclude_nonconvergence)) rlang::abort("The exclude_nonconvergence input must be a boolean.")
  tibble <- tibble::as_tibble(tibble)

  # Retrieve calculation tables
  tibble <- calculate_iscores(tibble, p_threshold, core_threshold, exclude_nonconvergence, collapse = FALSE, calculation_tables = TRUE)

  # Bootstrap I-Scores for each minor party
  pmap_dfr(tibble, function(party, tables, scores) {
    ie_score_table <- tables[[1]]
    ip_score_table <- tables[[2]]
    issues <- names(dplyr::select(tables[[1]], -name))

    scores <- map_dfr(1:n, function(i) {
      sampled_issues <- sample(issues, size = length(issues), replace = TRUE)
      column_names <- paste0("issue", seq_along(sampled_issues))
      ie_score_table_sampled <- ie_score_table[, c("name", sampled_issues)]
      colnames(ie_score_table_sampled) <- c("name", column_names)
      ip_score_table_sampled <- ip_score_table[, c("name", sampled_issues)]
      colnames(ip_score_table_sampled) <- c("name", column_names)

      ie_scores <- ie_score_sum(ie_score_table_sampled)
      ip_scores <- ip_score_sum(ip_score_table_sampled)

      tibble(
        ie_scores = ie_scores$ie_score,
        ie_score_interpreted = ie_scores$ie_score_interpreted,
        ip_score = ip_scores,
      )
    })

    # Calculate Confidence Intervals
    tibble(
      party = party,
      side = c("lower", "upper", "estimate"),
      ie_score = c(quantile(scores$ie_scores, probs = c(0.025, 0.975)), scores$ie_score),
      ie_score_interpreted = c(quantile(scores$ie_score_interpreted, probs = c(0.025, 0.975)), scores$ie_score_interpreted),
      ip_score = c(quantile(scores$ip_score, probs = c(0.025, 0.975)), scores$ip_score)
    )
  })
}
