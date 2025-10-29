#' Sample Data Results
#'
#' A sample tibble demonstrating the expected result structure of the I-Score pipeline. It includes the IScores (along with confidence intervals) of minor parties that won more than 0.5% of the presidential vote in a given election between 1948-2024. These scores were calculated using the sample_data tibble. Note: Minor parties that won more than 0.5% of the vote in consecutive elections are represented by a single entry spanning those elections.
#'
#' @format A tibble with 13 rows and 3 columns:
#' \describe{
#'   \item{party}{Character column. The party's name (this column must be unique for each platform, e.g., "Democratic Party 1992").}
#'   \item{scores}{List column. A list, containing: ie_score (Numeric. The party's Ie-Score.), ie_score_interpreted (Numeric. The party's interpreted Ie-Score), and ip_score (Numeric. The party's Ip-Score).}
#'   \item{confidence_intervals}{List column. A tibble, containing: side (Character column. "lower" or "upper", indicating the bounds of the confidence interval), ie_score (Numeric column. The bound of the confidence interval for the party's Ie-Score), ie_score_interpreted (Numeric column. The bound of the confidence interval for the party's interpreted Ie-Score), and ip_score (Numeric column. The bound of the confidence interval for the party's Ip-Score).}
#' }
"sample_data_results"
