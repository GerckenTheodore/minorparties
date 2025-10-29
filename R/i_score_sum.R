#' Weight I-Score calculations
#'
#' weighted_party_scores() weights the changes of major parties by their party's set weight and then weights those using a geometric sequence (to recognize that persuading one major party is likely to incentivize others to move away from their positions while still rewarding a minor party capable of influencing multiple major parties simultaneously).
#'
#' @keywords internal
#' @noRd

weighted_party_scores <- function(scores, weights) {
  avg_weight <- mean(weights)
  adj_weights <- weights / avg_weight
  weighted_scores <- (scores * adj_weights) |>
    sort(decreasing = TRUE)
  raw_geom_weights <- (1 / 3)^(seq_along(weighted_scores) - 1)
  geom_weights <- raw_geom_weights / sum(raw_geom_weights)
  sum(weighted_scores * geom_weights)
}

#' Calculate Ie-Scores
#'
#' ie_score_sum() properly weights each element of an Ie-Score calculation tibble to produce the final Ie-Score and interpreted Ie-Score for a minor party platform.
#'
#' @param ie_score_tibble Tibble. The calculation tibble (created during calculate_i_scores())
#' @param party_row Tibble. The minor party platform's row of the main tibble (the tibble input to calculate_i_scores()).
#' @param top_issues Character vector. The minor party platform's top issues.
#' @return Named list containing the scores.
#'
#' @keywords internal
#' @noRd

ie_score_sum <- function(ie_score_tibble, party_row, top_issues, p_threshold) {
  calculation_tibble <- party_row |>
    purrr::pluck("overall_emphasis_scores", 1) |>
    dplyr::filter(issue %in% top_issues) |> # Gets the weight of each top issue (as determined by the minor party platform's emphasis score of that issue)
    dplyr::mutate(change_score = purrr::map(top_issues, function(issue) {
      party_scores <- purrr::map_dfr(unique(ie_score_tibble$party_number), function(number) { # For each major party, pull its relevant scores (from the ie_score_tibble) for the given issue
        pull_number <- function(type, to_pull) { # Helper function to pull a number from the ie_score_tibble for a given party and type of number (change, before, significance)
          dplyr::filter(ie_score_tibble, party_number == number & name == type) |>
            purrr::pluck(to_pull, 1)
        }

        tibble::tibble(
          party_number = number,
          change = ifelse(pull_number("significance", issue) <= p_threshold, pull_number("change", issue), 0),
          before = pull_number("before", issue),
          weight = pull_number("before", "weight")
        )
      })

      # Finds the weighted change and weighted before scores for the issue
      weighted_change <- weighted_party_scores(party_scores$change, party_scores$weight)
      weighted_before <- sum(party_scores$before * party_scores$weight) / sum(party_scores$weight)

      list(weighted_change = weighted_change, weighted_before = weighted_before)
    })) |>
    tidyr::unnest_wider(change_score)

  ie_score <- sum(calculation_tibble$score * calculation_tibble$weighted_change) / sum(calculation_tibble$score) # Calculates a weighted Ie-Score from the weighted changes of each issue and the weight of each issue
  ie_score_interpreted <- ie_score / (sum(calculation_tibble$score * calculation_tibble$weighted_before) / sum(calculation_tibble$score)) # Adjusts the Ie-Score to be a percentage change (instead of a change measured in percentage points)

  return(list(ie_score = ie_score, ie_score_interpreted = ie_score_interpreted))
}

#' Calculate Ip-Scores
#'
#' ip_score_sum() properly weights each element of an Ip-Score calculation tibble to produce the final Ip-Score for a minor party platform.
#'
#' @param ip_score_tibble Tibble. The calculation tibble (created during calculate_i_scores())
#' @param party_row Tibble. The minor party platform's row of the main tibble (the tibble input to calculate_i_scores()).
#' @param top_issues Character vector. The minor party platform's top issues.
#' @return Named list containing the score.
#'
#' @keywords internal
#' @noRd

ip_score_sum <- function(ip_score_tibble, party_row, top_issues, p_threshold) {
  calculation_tibble <- party_row |>
    purrr::pluck("overall_emphasis_scores", 1) |>
    dplyr::filter(issue %in% top_issues) |>
    dplyr::arrange(factor(issue, levels = top_issues)) |>
    dplyr::mutate(change_score = purrr::map_dbl(top_issues, function(issue) {
      party_scores <- purrr::map_dfr(unique(ip_score_tibble$party_number), function(number) {
        pull_number <- function(type, to_pull) {
          dplyr::filter(ip_score_tibble, party_number == number & name == type) |>
            purrr::pluck(to_pull, 1)
        }

        significance <- pull_number("significance", issue)
        change <- pull_number("change", issue)

        tibble::tibble(
          party_number = number,
          change = ifelse(!is.na(significance) & significance <= p_threshold & !is.na(change), change, 0),
          weight = pull_number("before", "weight")
        )
      })

      weighted_party_scores(party_scores$change, party_scores$weight)
    }))

  sum(calculation_tibble$score * calculation_tibble$change_score) / sum(calculation_tibble$score)
}
