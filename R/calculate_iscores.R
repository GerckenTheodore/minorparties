#' Calculate minor parties' I-Scores
#'
#' calculate_iscores() takes a tibble of platforms that have already been processed with process_platform_emphasis() and process_platform_position() and calculates I-Scores for each minor party in the tibble. I-Scores represent the extent to which a minor party influenced the major parties in its political environment on its top issues. Ie-Scores reflect changes in the emphasis major parties place on a minor party’s core issues (Ie-Scores measure the change in percentage points; interpreted Ie-Scores measure the change in percent), while Ip-Scores track shifts in the position of major parties on those issues.
#'
#' @param tibble Tibble. One row per platform, containing, (this function is designed to work with the output of process_platform_position()):
#' * party: Character column. The party's name (this column must be unique for each platform).
#' * sentence_emphasis_scores: List column. A list per sentence in the platform (in order), containing:
#'   * sentence: Character. The sentence.
#'   * scores: Tibble. The sentence's emphasis score on each issue-area, containing:
#'     * issue: Character column. The issue-area name.
#'     * score: Numeric column. The sentence's score for that issue-area (summing to 1).
#'   * overall_emphasis_scores: List column. A tibble with the platform's overall emphasis scores, containing:
#'     * issue: Character column. The issue-area name.
#'     * score: Numeric column. The platform's score for that issue-area
#'   * position_scores: List column. A tibble, containing:
#'     * issue: Character column. The issue-area name.
#'     * position_score: Numeric column. The platform's position score on the issue-area (NA if the platform did not have enough material about the issue-area to generate a score).
#'     * se: Numeric column. The standard error of the position score.
#'     * convergence: Logical column. Whether the Wordfish model converged (if the estimation algorithm reached a stable set of position scores without divergence).
#'   * minor_party: Logical column. Whether the party is a minor party.
#'   * major_party_platforms: List column. Only required for minor parties. A list containing a list for each major party, each of which contains:
#'     * before: Character. The name (as listed in this tibble's party column) of the major party's platform that precedes the minor party.
#'     * after: Character. The name (as listed in this tibble's party column) of the major party's platform that follows the minor party.
#'     * weight: Numeric. The weight assigned to the major party.
#' @param p_threshold Numeric. The maximum p-value at which a relationship is considered significant. Defaults to 0.05.
#' @param core_threshold Numeric. The minimum emphasis score a minor party must have for an issue-area to be considered a core issue. Defaults to 0.05.
#' @param exclude_nonconvergence Logical. Whether to treat issue-areas where the Wordfish model did not converge as if no score had been found when calculating Ip-Scores. Defaults to TRUE.
#' @param adjust_p_values Logical. Whether to adjust p-values to account for the large number of comparisons. Defaults to TRUE.
#' @param confidence_intervals Logical. Whether to calculate confidence intervals for the I-Scores. Defaults to FALSE.
#' @param confidence_n Numeric. If confidence_intervals is TRUE, the number of bootstrap samples to take in calculating confidence intervals. Defaults to 1000.
#' @param calculation_tables Logical. Whether to include the tables used to calculate I-Scores in the output. Defaults to FALSE.
#' @return Tibble. The input tibble, filtered to only include minor parties, with the following columns:
#' * party: Character column. The party's name.
#' * scores: List column. A list, containing:
#'   * ie_score: Numeric. The party's Ie-Score.
#'   * ie_score_interpreted: Numeric. The party's interpreted Ie-Score
#'   * ip_score: Numeric. The party's Ip-Score.
#' * calculation_tables: List column (present if calculation_tables argument is TRUE). A list, containing:
#'   * ie_score_table: Tibble. The table used to calculate the party's Ie-Score.
#'   * ip_score_table: Tibble. The table used to calculate the party's Ip-Score.
#' * confidence_intervals: List column (present if confidence_intervals argument is TRUE). A tibble, containing:
#'   * side: Character column. "lower" or "upper", indicating the bounds of the confidence interval.
#'   * ie_score: Numeric column. The bound of the confidence interval for the party's Ie-Score.
#'   * ie_score_interpreted: Numeric column. The bound of the confidence interval for the party's interpreted Ie-Score.
#'   * ip_score: Numeric column. The bound of the confidence interval for the party's Ip-Score.
#' @examplesIf interactive()
#' tibble <- minorparties::sample_data |>
#'   minorparties::process_platform_emphasis() |>
#'   minorparties::process_platform_position()
#' processed_tibble <- calculate_iscores(tibble)
#' @export

calculate_iscores <- function(tibble, p_threshold = 0.05, core_threshold = 0.05, exclude_nonconvergence = TRUE, adjust_p_values = TRUE, confidence_intervals = FALSE, confidence_n = 1000, calculation_tables = FALSE) {
  # Checks that the inputs are correctly structured
  validator_tibble <- validation(tibble, "iscores")
  if (nrow(validator_tibble) > 0) rlang::abort("The tibble is incorrectly structured. See the returned tibble for details.", tibble = validator_tibble)
  if (!is.numeric(p_threshold) || p_threshold < 0 || p_threshold > 1) rlang::abort("The p_threshold must be a number between 0 and 1.")
  if (!is.numeric(core_threshold) || core_threshold < 0 || core_threshold > 1) rlang::abort("The core_threshold must be a number between 0 and 1.")
  if (!is.logical(exclude_nonconvergence)) rlang::abort("The exclude_nonconvergence input must be a boolean.")
  if (!is.logical(adjust_p_values)) rlang::abort("The adjust_p_values input must be a boolean.")
  if (!is.logical(confidence_intervals)) rlang::abort("The confidence_intervals input must be a boolean.")
  if (!is.numeric(confidence_n) || confidence_n <= 1) rlang::abort("The confidence_n input must be a whole number greater than 1.")
  if (!is.logical(calculation_tables)) rlang::abort("The calculation_tables input must be a boolean.")
  tibble <- tibble::as_tibble(tibble)
  confidence_n <- round(confidence_n)

  # Gives each minor party platform access to the position and emphasis data of the major party platforms it is tagged with
  lookup_table <- tibble |> # Arranges the data by party
    dplyr::select(party, sentence_emphasis_scores, overall_emphasis_scores, position_scores) |>
    split(tibble$party)
  minor_parties <- tibble |> # Gives each minor party the data of the major parties it is tagged with
    dplyr::filter(minor_party) |>
    dplyr::mutate(major_party_info = purrr::map(major_party_platforms, function(platforms) {
      purrr::map(platforms, function(major) {
        list(before = lookup_table[[major$before]], after = lookup_table[[major$after]], weight = major$weight)
      })
    }))

  # Constructs calculation tibbles for each minor party
  minor_parties <- minor_parties |>
    dplyr::mutate(calculation_tables = purrr::map(minor_parties$party, function(party_v) {
      party_row <- dplyr::filter(minor_parties, party == party_v)
      major_info <- party_row$major_party_info[[1]]

      top_issues <- party_row |> # The issue-areas the minor party dedicates above the core_threshold of their platform to are the core issue-areas its performance is evaluated on
        purrr::pluck("overall_emphasis_scores", 1) |>
        dplyr::filter(score > core_threshold) |>
        dplyr::arrange(issue) |>
        dplyr::pull(issue)
      top_issues <- party_row |>
        purrr::pluck("position_scores", 1) |>
        dplyr::filter(issue %in% top_issues & !is.na(score)) |> # A core issue cannot have a position score of NA, as that would render an Ip-Score impossible to calculate
        dplyr::arrange(issue) |>
        dplyr::pull(issue)

      sort_scores <- function(scores, to_pull = "score") { # Helper function to pull scores in the order of top_issues. THIS IS VERY VERY IMPORTANT, VECTORS MUST BE ALLIGNED IN THE SAME WAY!!!
        scores[[1]] |>
          dplyr::filter(issue %in% top_issues) |>
          dplyr::arrange(factor(issue, levels = top_issues)) |>
          dplyr::pull(to_pull)
      }

      # Creates the Ie-Score calculation tibble (each party's emphasis scores before and after the minor party, the change, and the statistical significance of that change)
      pull_sentence_scores <- function(sentence_emphasis_scores, issue_v) { # Helper function to find the distribution of scores for a given issue across all of the sentences of a platform
        purrr::map_dbl(sentence_emphasis_scores[[1]], function(sentence) {
          matching_row <- dplyr::filter(sentence$scores[[1]], issue == issue_v)
          matching_row$score
        })
      }

      ie_score_tibble <- purrr::imap_dfr(major_info, function(major, i) { # Creates a stacked tibble with each major party's scores and meta-columns to distinguish between layers (party name, weight, party number)
        before_scores <- sort_scores(major$before$overall_emphasis_scores)
        after_scores <- sort_scores(major$after$overall_emphasis_scores)
        change <- after_scores - before_scores # Because minor parties wish to increase the salience of their core issues, an increase in emphasis beyond the minor party's own emphasis continues to be considered a positive change (this is in contrast to the Ip-Score logic where distance from the minor party's position is what matters)

        statistical_significance <- purrr::map_dbl(top_issues, function(issue) {
          before <- pull_sentence_scores(major$before$sentence_emphasis_scores, issue)
          after <- pull_sentence_scores(major$after$sentence_emphasis_scores, issue)
          stats::wilcox.test(before, after, alternative = "two.sided", exact = FALSE)$p.value
        })
        weight <- major$weight

        return_tibble <- rbind(before_scores, after_scores, change, statistical_significance)
        colnames(return_tibble) <- top_issues
        return_tibble <- tibble::as_tibble(return_tibble)
        return_tibble |>
          dplyr::mutate(party_number = i, name = c("before", "after", "change", "significance"), party = c(major$before$party, major$after$party, NA, NA), weight = weight) |>
          dplyr::select(party_number, name, weight, dplyr::everything())
      })

      # Creates the Ip-Score calculation (structured in the same way as the Ie-Score tibble)
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
        change <- before_distance - after_distance # Here, unlike in Ie-Scores, it is the distance from the minor party's position that matters, so moving past a minor party's position would start to decrease the score (imagine a minor party in the center of the major parties; a major party moving from their one extreme to the other, even if technically in the direction of the minor party, is not a positive change in the eyes of the minor)
        weight <- major$weight

        if (exclude_nonconvergence) { # If the user wants to exclude non-converged models, treats those issue-areas as if no score had been found. This should be an edge case.
          convergence <- sort_scores(major$before$position_scores, "convergence") & sort_scores(major$after$position_scores, "convergence")
          before_scores[!convergence] <- NA
          before_se[!convergence] <- NA
        }

        statistical_significance <- rep(NA_real_, length(before_scores))
        not_NA <- !is.na(before_scores) & !is.na(before_se) & !is.na(after_scores) & !is.na(after_se)
        z <- (before_scores[not_NA] - after_scores[not_NA]) / sqrt(before_se[not_NA]^2 + after_se[not_NA]^2) # Z-test
        statistical_significance[not_NA] <- 2 * stats::pnorm(-abs(z))

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

  # Adjusts calculation tibbles to rebalance IScores. This is necessary because the large number of comparisons (each minor party performs two statistical significance tests (Ie and IP Scores) for each issue-area for each major party) increases the likelihood of false positives.
  if (adjust_p_values) {
    p_values <- minor_parties |> # Collapse all p-values into one table (with identifying information on their origin) then re-balance them
      dplyr::select(party, calculation_tables) |>
      tidyr::unnest_longer(calculation_tables, indices_to = "type") |>
      tidyr::unnest(calculation_tables, names_sep = "_") |>
      tidyr::pivot_longer(cols = -c(party, calculation_tables_party, calculation_tables_party_number, calculation_tables_name, calculation_tables_weight, type), names_to = "issue", values_to = "p_value") |>
      dplyr::filter(!is.na(p_value) & calculation_tables_name == "significance") |>
      dplyr::mutate(adjusted_p_value = stats::p.adjust(p_value, method = "BH")) |>
      dplyr::select(party, type, calculation_tables_party_number, issue, adjusted_p_value)

    minor_parties <- minor_parties |> # Goes through each calculation table and replace the p-values with the matching adjusted p-values
      dplyr::mutate(calculation_tables = purrr::map2(calculation_tables, party, function(tables, party_v) {
        purrr::imap(tables, function(table, name) {
          new_p_values <- dplyr::filter(p_values, party == party_v & type == name) |>
            dplyr::mutate(issue = stringr::str_replace(issue, "^calculation_tables_", ""), party_number = calculation_tables_party_number) |>
            dplyr::select(-party, -type, -calculation_tables_party_number) |>
            tidyr::pivot_wider(names_from = issue, values_from = adjusted_p_value, values_fill = NA_real_) |>
            dplyr::mutate(name = "significance", party = NA_character_)

          table |>
            dplyr::filter(name != "significance") |>
            dplyr::bind_rows(new_p_values) |> # bind_rows() matches up columns, so the new p-values will go in the right place
            dplyr::group_by(party_number) |>
            dplyr::mutate(weight = ifelse(is.na(weight), dplyr::first(stats::na.omit(weight)), weight)) |>
            dplyr::ungroup()
        })
      }))
  }

  # Calculates IScores
  minor_parties <- minor_parties |>
    dplyr::mutate(scores = purrr::map2(party, calculation_tables, function(party_v, tables) {
      # Gets the necessary inputs for the ix_scores() functions
      party_row <- dplyr::filter(minor_parties, party == party_v)
      top_issues <- tables$ie_score_tibble |>
        dplyr::select(-party_number, -name, -weight, -party) |>
        colnames()

      ie_scores <- ie_score_sum(tables$ie_score_tibble, party_row, top_issues, p_threshold)
      ip_score <- ip_score_sum(tables$ip_score_tibble, party_row, top_issues, p_threshold)

      list(ie_score = ie_scores$ie_score, ie_score_interpreted = ie_scores$ie_score_interpreted, ip_score = ip_score)
    }))

  # Create confidence intervals
  if (confidence_intervals) {
    minor_parties <- minor_parties |>
      dplyr::mutate(confidence_intervals = purrr::map2(party, calculation_tables, function(party_v, tables) {
        # Gets the necessary inputs for the ix_scores() functions
        party_row <- dplyr::filter(tibble, party == party_v)
        top_issues <- tables$ie_score_tibble |>
          dplyr::select(-party_number, -name, -weight, -party) |>
          colnames()
        top_issue_tibble <- party_row |>
          purrr::pluck("overall_emphasis_scores", 1) |>
          dplyr::filter(issue %in% top_issues)

        scores <- purrr::map_dfr(1:confidence_n, function(i) {
          # Bootstraps by sampling issue-areas with replacement
          sampled_issues <- sample(top_issues, size = length(top_issues), replace = TRUE)
          sampled_top_issues <- tibble::tibble(issue = sampled_issues) |> # Applies the sampling procedure by reweighing the issue-area emphasis scores
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

          # Recalculates the I-Scores based on the reweighed issue-areas
          ie_scores <- ie_score_sum(tables$ie_score_tibble, party_row = sampled_party_row, top_issues, p_threshold)
          ip_scores <- ip_score_sum(tables$ip_score_tibble, party_row = sampled_party_row, top_issues, p_threshold)

          tibble::tibble(
            ie_score = ie_scores$ie_score,
            ie_score_interpreted = ie_scores$ie_score_interpreted,
            ip_score = ip_scores
          ) # Stacks all the IScores generated from bootstrap samples together
        })

        # Calculates 95% confidence intervals from the bootstrap samples' IScores
        tibble::tibble(
          side = c("lower", "upper"),
          ie_score = c(stats::quantile(scores$ie_score, probs = c(0.025, 0.975))),
          ie_score_interpreted = c(stats::quantile(scores$ie_score_interpreted, probs = c(0.025, 0.975))),
          ip_score = c(stats::quantile(scores$ip_score, probs = c(0.025, 0.975)))
        )
      }, .progress = list(
        name = "Creating Confidence Intervals",
        clear = TRUE,
        type = "iterator"
      )))
  }

  # Returns cleaned tibble. If the supurfluous columns in the tibble aren't dropped, the tibble will take a long time to render (this is also true of the process_platform_position/emphasis() outputs).
  if (!calculation_tables) {
    minor_parties <- dplyr::select(minor_parties, -calculation_tables)
  }
  minor_parties |>
    dplyr::select(party, scores, dplyr::any_of(c("calculation_tables", "confidence_intervals")))
}
