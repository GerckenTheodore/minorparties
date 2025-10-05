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
#' @param adjust_p_values Whether to adjust p-values to account for the large number of comparisons (TRUE by default).
#' @param confidence_intervals Whether to calculate confidence intervals for the I-Scores (FALSE by default).
#' @param confidence_n The number of bootstrap samples to take for each minor party if `confidence_intervals` is TRUE (1000 by default).
#' @param calculation_tables Whether to return the tables used to calculate I-scores.
#' @return A tibble, containing the minor parties, with the list-column `scores` containing `ie_score`, `ie_score_interpreted`, and `ip_score`. If `calculation_tables` is TRUE, there will be a `calculation_tables` list-column including `ie_score_table` and `ip_score_table`. If `confidence_intervals` is TRUE, there will be a `confidence_intervals` column of tibbles containing the upper and lower bounds of the 95% confidence interval for the party's IScores.
#' @export

calculate_iscores <- function(tibble, p_threshold = 0.05, core_threshold = 0.05, exclude_nonconvergence = TRUE, adjust_p_values = TRUE, confidence_intervals = FALSE, confidence_n = 1000, calculation_tables = FALSE) {
  # Check that the inputs are correctly structured
  validator_tibble <- validation(tibble, "iscores")
  if (nrow(validator_tibble) > 0) {
    print(validator_tibble)
    rlang::abort("The tibble is incorrectly structured.", tibble = validator_tibble)
  }
  if (!is.numeric(p_threshold) || p_threshold < 0 || p_threshold > 1) rlang::abort("The p_threshold must be a number between 0 and 1.")
  if (!is.numeric(core_threshold) || core_threshold < 0 || core_threshold > 1) rlang::abort("The core_threshold must be a number between 0 and 1.")
  if (!is.logical(exclude_nonconvergence)) rlang::abort("The exclude_nonconvergence input must be a boolean.")
  if (!is.logical(adjust_p_values)) rlang::abort("The adjust_p_values input must be a boolean.")
  if (!is.logical(confidence_intervals)) rlang::abort("The confidence_intervals input must be a boolean.")
  if (!is.numeric(confidence_n) || confidence_n <= 1) rlang::abort("The confidence_n input must be a whole number greater than 1.")
  if (!is.logical(calculation_tables)) rlang::abort("The calculation_tables input must be a boolean.")
  tibble <- tibble::as_tibble(tibble)
  confidence_n <- round(confidence_n)

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

  # Construct calculation tibbles for each minor party
  minor_parties <- minor_parties |>
    dplyr::mutate(calculation_tables = purrr::map(minor_parties$party, function(party_v) {
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

      list(ie_score_tibble = ie_score_tibble, ip_score_tibble = ip_score_tibble)
    }, .progress = list(
      name = "Constructing calculation tibbles for each minor party",
      clear = TRUE,
      type = "iterator"
    )))

  # Adjust calculation tibbles to rebalance IScores
  if (adjust_p_values) {
    p_values <- minor_parties |>
      dplyr::select(party, calculation_tables) |>
      tidyr::unnest_longer(calculation_tables, indices_to = "type") |>
      tidyr::unnest(calculation_tables, names_sep = "_") |>
      tidyr::pivot_longer(cols = -c(party, calculation_tables_party, calculation_tables_party_number, calculation_tables_name, calculation_tables_weight, type), names_to = "issue", values_to = "p_value") |>
      dplyr::filter(!is.na(p_value) & calculation_tables_name == "significance") |>
      dplyr::mutate(adjusted_p_value = stats::p.adjust(p_value, method = "BH")) |>
      dplyr::select(party, type, calculation_tables_party_number, issue, adjusted_p_value)

    minor_parties <- minor_parties |>
      dplyr::mutate(calculation_tables = purrr::map2(calculation_tables, party, function(tables, party_v) {
        purrr::imap(tables, function(table, name) {
          new_p_values <- dplyr::filter(p_values, party == party_v & type == name) |>
            dplyr::mutate(issue = stringr::str_replace(issue, "^calculation_tables_", ""), party_number = calculation_tables_party_number) |>
            dplyr::select(-party, -type, -calculation_tables_party_number) |>
            tidyr::pivot_wider(names_from = issue, values_from = adjusted_p_value, values_fill = NA_real_) |>
            dplyr::mutate(name = "significance", party = NA_character_)

          table |>
            dplyr::filter(name != "significance") |>
            dplyr::bind_rows(new_p_values) |>
            dplyr::group_by(party_number) |>
            dplyr::mutate(weight = ifelse(is.na(weight), dplyr::first(na.omit(weight)), weight)) |>
            dplyr::ungroup()
        })
      }))
  }

  # Calculate IScores
  minor_parties <- minor_parties |>
    dplyr::mutate(scores = purrr::map2(party, calculation_tables, function(party_v, tables) {
      party_row <- dplyr::filter(minor_parties, party == party_v)
      top_issues <- tables$ie_score_tibble |>
        dplyr::select(-party_number, -name, -weight, -party) |>
        colnames()

      ie_scores <- ie_score_sum(tables$ie_score_tibble, party_row, top_issues, p_threshold)
      ip_score <- ip_score_sum(tables$ip_score_tibble, party_row, top_issues, p_threshold)

      list(ie_score = ie_scores$ie_score, ie_score_interpreted = ie_scores$ie_score_interpreted, ip_score = ip_score)
    }))

  # Create Confidence Intervals
  if (confidence_intervals) {
    minor_parties <- minor_parties |>
      dplyr::mutate(confidence_intervals = purrr::map2(party, calculation_tables, function(party_v, tables) {
        party_row <- dplyr::filter(tibble, party == party_v)
        top_issues <- tables$ie_score_tibble |>
          dplyr::select(-party_number, -name, -weight, -party) |>
          colnames()
        top_issue_tibble <- party_row |>
          purrr::pluck("overall_emphasis_scores", 1) |>
          dplyr::filter(issue %in% top_issues)

        scores <- purrr::map_dfr(1:confidence_n, function(i) {
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

          ie_scores <- ie_score_sum(tables$ie_score_tibble, party_row = sampled_party_row, top_issues, p_threshold)
          ip_scores <- ip_score_sum(tables$ip_score_tibble, party_row = sampled_party_row, top_issues, p_threshold)

          tibble::tibble(
            ie_score = ie_scores$ie_score,
            ie_score_interpreted = ie_scores$ie_score_interpreted,
            ip_score = ip_scores
          )
        })

        tibble::tibble(
          side = c("lower", "upper"),
          ie_score = c(quantile(scores$ie_score, probs = c(0.025, 0.975))),
          ie_score_interpreted = c(quantile(scores$ie_score_interpreted, probs = c(0.025, 0.975))),
          ip_score = c(quantile(scores$ip_score, probs = c(0.025, 0.975)))
        )
      }, .progress = list(
        name = "Creating Confidence Intervals",
        clear = TRUE,
        type = "iterator"
      )))
  }

  # Return Tibble
  if (!calculation_tables) {
    minor_parties <- dplyr::select(minor_parties, -calculation_tables)
  }
  minor_parties |>
    dplyr::select(party, scores, dplyr::any_of(c("calculation_tables", "confidence_intervals")))
}
