weighted_party_scores <- function(scores, weights) {
  avg_weight <- mean(weights)
  adj_weights <- weights / avg_weight
  weighted_scores <- (scores * adj_weights) |>
    sort(decreasing = TRUE)
  raw_geom_weights <- (1 / 3)^(seq_along(weighted_scores) - 1)
  geom_weights <- raw_geom_weights / sum(raw_geom_weights)
  sum(weighted_scores * geom_weights)
}

ie_score_sum <- function(ie_score_tibble, party_row, top_issues, p_threshold) {
  calculation_tibble <- party_row |>
    purrr::pluck("overall_emphasis_scores", 1) |>
    dplyr::filter(issue %in% top_issues) |>
    dplyr::mutate(change_score = purrr::map(top_issues, function(issue) {
      party_scores <- purrr::map_dfr(unique(ie_score_tibble$party_number), function(number) {
        pull_number <- function(type, to_pull) {
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

      weighted_change <- weighted_party_scores(party_scores$change, party_scores$weight)
      weighted_before <- sum(party_scores$before * party_scores$weight) / sum(party_scores$weight)

      list(weighted_change = weighted_change, weighted_before = weighted_before)
    })) |>
    tidyr::unnest_wider(change_score)

  ie_score <- sum(calculation_tibble$score * calculation_tibble$weighted_change) / sum(calculation_tibble$score)
  ie_score_interpreted <- ie_score / (sum(calculation_tibble$score * calculation_tibble$weighted_before) / sum(calculation_tibble$score))

  return(list(ie_score = ie_score, ie_score_interpreted = ie_score_interpreted))
}

ip_score_sum <- function(ip_score_tibble, party_row, top_issues, p_threshold) {
  calculation_tibble <- party_row |>
    purrr::pluck("position_scores", 1) |>
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
