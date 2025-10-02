validation <- function(tibble, set) {
  error_wrapper <- function(logic, columns = c()) {
    tryCatch(
      {
        if (!all(columns %in% colnames(tibble))) {
          return(FALSE)
        }
        logic
      },
      error = function(e) FALSE
    )
  }

  error_wrapper(issues <- tibble$sentence_emphasis_scores[[1]][[1]]$scores[[1]]$issue, columns = c("sentence_emphasis_scores"))

  tibble::tibble(
    error = c(
      "The `tibble` input must be a dataframe",
      "The `text` column must be a character column",
      "The `party` column must be a character column",
      "Every entry in the 'party' column must be unique",
      "The `sentence_emphasis_scores` column must be a list column",
      "The `overall_emphasis_scores` column must be a list column",
      "The `position_scores` column must be a list column",
      "The `minor_party` column must be a boolean column",
      "The `major_party_platforms` column must be a list column",
      "No entries in the `text` column can be missing",
      "No entries in the `party` column can be missing",
      "No entries in the `sentence_emphasis_scores` column can be missing",
      "No entries in the `overall_emphasis_scores` column can be missing",
      "No entries in the `position_scores` column can be missing",
      "Every sentence list-item in a platform's `sentence_emphasis_scores` must contain a `sentence` character vector and a `scores` data frame",
      "Every `scores` tibble in each sentence list-item in each platform must contain an `issue` column, with the same issue-areas as every other scores data frame, and a `score` column, which sums to 1",
      "Every platform's `overall_emphasis_scores` column must contain an `issue` column, with the same issue-areas as the `sentence_emphasis_scores` data frames, and a `score` column, which sums to 1",
      "Every platform's `position_scores` column must contain a tibble with `issue`, `score`, `se`, and `convergence` columns. The issue column must contain the same issue-areas as the `sentence_emphasis_scores` data frames. The `score` and `se` columns must be numeric, and the `convergence` column must be boolean",
      "Every minor party's `major_party_platforms` column must contain a list for each major party with `before`, `after`, and `weight` entries. The `before` and `after` entries must be character vectors with the name of a major party's platform, contained in the tibble, and the `weight` entry must be numeric and positive",
      "Every party's `scores` column must contain `ie_score`, `ie_score_interpreted`, and `ip_score` numeric columns."
    ),
    passing = list(
      error_wrapper(is.data.frame(tibble)),
      error_wrapper(is.character(tibble$text), columns = ("text")),
      error_wrapper(is.character(tibble$party), columns = ("party")),
      error_wrapper(length(unique(tibble$party)) == nrow(tibble), columns = ("party")),
      error_wrapper(is.list(tibble$sentence_emphasis_scores), columns = ("sentence_emphasis_scores")),
      error_wrapper(is.list(tibble$overall_emphasis_scores), columns = ("overall_emphasis_scores")),
      error_wrapper(is.list(tibble$position_scores), columns = ("position_scores")),
      error_wrapper(is.logical(tibble$minor_party), columns = ("minor_party")),
      error_wrapper(is.list(tibble$major_party_platforms), columns = ("major_party_platforms")),
      error_wrapper(!is.na(tibble$text), columns = ("text")),
      error_wrapper(!is.na(tibble$party), columns = ("party")),
      error_wrapper(!is.na(tibble$sentence_emphasis_scores), columns = ("sentence_emphasis_scores")),
      error_wrapper(!is.na(tibble$overall_emphasis_scores), columns = ("overall_emphasis_scores")),
      error_wrapper(!is.na(tibble$position_scores), columns = ("position_scores")),
      error_wrapper(purrr::map_lgl(tibble$sentence_emphasis_scores, function(platform) {
        length(platform) > 0 && all(purrr::map_lgl(platform, function(sentence) {
          is.character(sentence$sentence) && length(sentence$sentence) == 1 && is.data.frame(sentence$scores[[1]])
        }))
      }), columns = ("sentence_emphasis_scores")),
      error_wrapper(purrr::map_lgl(tibble$sentence_emphasis_scores, function(platform) {
        all(purrr::map_lgl(platform, function(sentence) {
          scores <- sentence$scores[[1]]
          setequal(scores$issue, issues) && dplyr::near(sum(scores$score), 1, tol = 1e-6)
        }))
      }), columns = ("sentence_emphasis_scores")),
      error_wrapper(purrr::map_lgl(tibble$overall_emphasis_scores, function(scores) {
        setequal(scores$issue, issues) && dplyr::near(sum(scores$score), 1, tol = 1e-6)
      }), columns = ("overall_emphasis_scores")),
      error_wrapper(purrr::map_lgl(tibble$position_scores, function(scores) {
        setequal(scores$issue, issues) && is.numeric(scores$score) && is.numeric(scores$se) && is.logical(scores$convergence)
      }), columns = ("position_scores")),
      error_wrapper(purrr::map2_lgl(tibble$major_party_platforms, tibble$minor_party, function(platforms, minor_party) {
        !minor_party || (length(platforms) > 0 & all(purrr::map_lgl(platforms, function(party) {
          is.numeric(party$weight) && party$weight > 0 && all(c(party$before, party$after) %in% tibble$party)
        })))
      }), columns = c("major_party_platforms", "minor_party")),
      error_wrapper(purrr::map_lgl(tibble$scores, function(scores) {
        is.numeric(scores$ie_score) && is.numeric(scores$ie_score_interpreted) && is.numeric(scores$ip_score)
      }), columns = ("scores"))
    ),
    rowwise = c(
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    ),
    sets = list(
      c("position", "emphasis", "iscores", "finished"),
      c("emphasis"),
      c("position", "iscores", "finished"),
      c("position", "iscores", "finished"),
      c("position", "iscores"),
      c("iscores"),
      c("iscores"),
      c("iscores"),
      c("iscores"),
      c("emphasis"),
      c("position", "iscores"),
      c("position", "iscores"),
      c("iscores"),
      c("iscores"),
      c("position", "iscores"),
      c("position", "iscores"),
      c("iscores"),
      c("iscores"),
      c("iscores"),
      c("finished")
    )
  ) |>
    dplyr::mutate(
      passed = purrr::map_lgl(passing, function(check) all(check)),
      failing_rows = purrr::map2(passing, rowwise, function(check, rowwise) {
        if (!rowwise || length(passing) == 1) {
          "tibble-wide issue"
        } else {
          which(!check)
        }
      })
    ) |>
    dplyr::filter(
      !passed,
      purrr::map_lgl(sets, function(row_sets) set %in% row_sets)
    ) |>
    dplyr::select(-c("rowwise", "passing", "passed", "sets"))
}
