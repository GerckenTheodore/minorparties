
<!-- README.md is generated from README.Rmd. Please edit that file -->

# minorparties

<!-- badges: start -->

<!-- badges: end -->

`minorparties` is an R package designed to allow researchers to easily
calculate minor political parties’ I-Scores, a quantitative measure of
the extent to which a minor party influences the major parties in its
political environment.

I-Scores are composed of two components, which capture different
dimensions of minor-party influence:

- **Ie-Score (issue-emphasis score)**: Measures the change in the
  emphasis major parties place on a minor party’s core issue areas.
  Ie-Scores are measured as the increase or decrease (in points) in the
  percentage of the average major-party platform devoted to the minor
  party’s average core issue area. The interpreted Ie-Score rescales
  this measure into a percentage change.

- **Ip-Score (issue-position score)**: Measures the change in the
  position major parties take on a minor party’s core issue areas.
  Ip-Scores are measured as the number of standard deviations shifted
  toward the minor party by the average major party on the minor party’s
  average core issue area.

Note: In both measures, the term “average major party on the minor
party’s average core issue area” refers to a weighted average that
incorporates three layers of weighting: the importance a minor party
assigns to each issue area, the weight the researcher assigns to each
major party, and a geometric weighting that emphasizes the best change
(to recognize that persuading one major party is likely to incentivize
others to move away from their positions while still rewarding a minor
party capable of influencing multiple major parties simultaneously).

These measures were developed by Theodore Gercken in *Beyond the Ballot
Box: Toward a Comprehensive Measure of Minor Party Success*.

## Installation

You can install `minorparties` from CRAN with:

``` r
install.packages("minorparties")
```

or you can install the development version of `minorparties` from
[GitHub](https://github.com/GerckenTheodore/minorparties) with:

``` r
# install.packages("devtools")
devtools::install_github("https://github.com/GerckenTheodore/minorparties")
```

To use `install_python()`, which is required for some functions, you
must install the huggingfaceR package from
[Github](https://github.com/farach/huggingfaceR) with:

``` r
# install.packages("devtools")
devtools::install_github("https://github.com/farach/huggingfaceR")
```

## Useage

### Platforms

To use the full `minorparties` pipeline, you must first obtain the party
platforms of both the minor party you wish to test and the major parties
in its political environment before and after the minor party’s
campaign. All platforms should be cleaned in accordance with the
Manifesto Project’s Coding Instructions (stripped of “headings,
statistics, tables of contents, introductory remarks by party leaders,”
etc.). For example, if you were analyzing the influence of the American
Libertarian Party over its 2012 and 2016 campaigns, you would use the
Libertarian Party’s 2012 platform as well as the Republican and
Democratic Party platforms from 2012 and 2020 (published before and
after the 2012–2016 movement).

For a larger study that involves multiple minor parties, you should
include all minor and major parties in the same dataset. However, all
documents must be in the same language because the Wordfish algorithm
assumes a single distribution of words across all documents.

You may use platforms published after 1945 in any of the languages
supported by the [ManifestoBERTA
model](https://huggingface.co/manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-2024-1-1).

### Tibble Construction

Once you have compiled the platforms you will use, create a tibble with
one row per platform and the following columns:

- `party`: Character column. The party’s name (this column must be
  unique for each platform, e.g., “Democratic Party 1992”).

- `text`: Character column. The full text of each platform.

- `minor_party`: Logical column. Whether the party is a minor party.

- `major_party_platforms`: List column. Only required for minor parties.
  A list containing a list for each major party, each of which contains:

  - `before`: Character. The name (as listed in this tibble’s `party`
    column) of the major party’s platform that precedes the minor party.

  - `after`: Character. The name (as listed in this tibble’s `party`
    column) of the major party’s platform that follows the minor party.

  - `weight`: Numeric. The weight assigned to the major party.

*Sample Tibble*

| party | text | minor_party | major_party_platforms |
|:---|:---|:---|:---|
| Democratic Party 1948 | The Democratic Party adopts this platform in the conviction that the destiny of the United States… | FALSE | NULL |
| Democratic Party 1952 | Our nation has entered into an age in which Divine Providence has permitted the genius of man to … | FALSE | NULL |
| Republican Party 1948 | To establish and maintain peace, to build a country in which every citizen can earn a good living… | FALSE | NULL |
| Republican Party 1952 | We maintain that man was not born to be ruled, but that he consented to be governed; and that the… | FALSE | NULL |
| Thurmond 1948 | We believe that the Constitution of the United States is the greatest charter of human liberty ev… | TRUE | \<list\> |

*Sample Major Party Platforms List*

`[[1]]`

`[[1]]`

| before                | after                 | weight |
|:----------------------|:----------------------|-------:|
| Republican Party 1948 | Republican Party 1952 |      1 |

`[[2]]`

| before                | after                 | weight |
|:----------------------|:----------------------|-------:|
| Democratic Party 1948 | Democratic Party 1952 |      1 |

### Processing

Once you have built your tibble of platforms, you can feed it into the
`minorparties` analysis pipeline, which consists of:

1.  `install_python()`: Sets up a Python virtual environment and
    downloads the Python tools required for this package’s analysis.
    This function only needs to be run once per R session and is
    required only for `process_platform_emphasis()`.

2.  `process_platform_emphasis()`: Calculates each platform’s emphasis
    score for each issue area using the ManifestoBERTA model.

3.  `process_platform_position()`: Calculates each platform’s position
    score for each issue area using the Wordfish algorithm.

4.  `calculate_iscores()`: Uses the calculated emphasis and position
    scores, as well as the provided information about which major-party
    platforms are relevant to which minor parties, to calculate each
    minor party’s I-Scores. If you set `confidence_intervals = TRUE`,
    the function will also calculate bootstrapped confidence intervals
    for each minor party’s I-Scores.

The arguments that can be used to customize each function are described
in the function documentation (`?function_name`).

You should avoid using a GUI to open the result of
`process_platform_emphasis()` or `process_platform_position()` because
these objects can be quite large and take significant time to load. This
will not be true of the result of `calculate_iscores()`, though, as the
rows and columns needed for the pipeline (which comprise the majority of
the size of the intermediate objects) are removed.

The result of the pipeline is a tibble with one row per minor party,
containing a scores list column with the minor party’s `ie_score`,
`ie_score_interpreted`, and `ip_score.`

``` r
library(minorparties)

install_python()
results <- process_platform_emphasis(data_tibble) |> # process_platform_emphasis() may take a while
  process_platform_position() |>
  calculate_iscores(confidence_intervals = TRUE)

results
```

*Sample Output*

| party            | scores   | confidence_intervals |
|:-----------------|:---------|:---------------------|
| Anderson 1980    | \<list\> | \<tibble\>           |
| Green 2000       | \<list\> | \<tibble\>           |
| Green 2016       | \<list\> | \<tibble\>           |
| Independent 1968 | \<list\> | \<tibble\>           |
| Libertarian 1980 | \<list\> | \<tibble\>           |
| Libertarian 1996 | \<list\> | \<tibble\>           |
| Libertarian 2016 | \<list\> | \<tibble\>           |
| McCarthy 1976    | \<list\> | \<tibble\>           |
| McMullin 2016    | \<list\> | \<tibble\>           |
| Nader 2008       | \<list\> | \<tibble\>           |
| Perot 1992       | \<list\> | \<tibble\>           |
| Thurmond 1948    | \<list\> | \<tibble\>           |
| Wallace 1948     | \<list\> | \<tibble\>           |

*Sample Scores List*

| ie_score | ie_score_interpreted | ip_score |
|---------:|---------------------:|---------:|
|    0.004 |               0.0575 |  -0.0596 |

*Sample Confidence Intervals Tibble*

| side  | ie_score | ie_score_interpreted | ip_score |
|:------|---------:|---------------------:|---------:|
| lower |  -0.0072 |              -0.1240 |  -0.2861 |
| upper |   0.0241 |               0.3395 |   0.0814 |
