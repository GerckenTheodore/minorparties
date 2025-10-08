
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IScorePackage

<!-- badges: start -->

<!-- badges: end -->

`IScorePackage` is an R package designed to allow researchers to easily
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

You can install the development version of `IScorePackage` from
[GitHub](https://github.com/GerckenTheodore/IScorePackage) with:

``` r
# install.packages("devtools")
devtools::install_github("https://github.com/GerckenTheodore/IScorePackage")
```

## Useage

### Platforms

To use the full `IScorePackage` pipeline, you must first obtain the
party platforms of both the minor party you wish to test and the major
parties in its political environment before and after the minor party’s
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
| Democratic Party 1956 | In the brief space of three and one-haft years, the people of the United States have come to real… | FALSE | NULL |
| Democratic Party 1960 | In 1796, in America’s first contested national election, our Party, under the leadership of Thoma… | FALSE | NULL |
| Democratic Party 1964 | America is One Nation, One People. The welfare, progress, security and survival of each of us res… | FALSE | NULL |
| Democratic Party 1968 | America belongs to the people who inhabit it, The source of the nation’s strength is the people’s… | FALSE | NULL |
| Democratic Party 1972 | Skepticism and cynicism are widespread in America. The people are skeptical of platforms filled w… | FALSE | NULL |
| Democratic Party 1976 | We meet to adopt a Democratic platform, and to nominate Democratic candidates for President and V… | FALSE | NULL |
| Democratic Party 1980 | In its third century, America faces great challenges and an uncertain future. The decade that Ame… | FALSE | NULL |
| Democratic Party 1984 | A fundamental choice awaits America-, a choice between two futures. It is a choice between solvin… | FALSE | NULL |
| Democratic Party 1988 | In order to initiate the changes necessary to keep America strong and make America better, in ord… | FALSE | NULL |
| Democratic Party 1992 | Two hundred summers ago, this Democratic Party was founded by the man whose burning pen fired the… | FALSE | NULL |
| Democratic Party 1996 | In 1996, America will choose the President who will lead us from the millennium which saw the bir… | FALSE | NULL |
| Democratic Party 2000 | Today, America finds itself in the midst of prosperity, progress, and peace. We have arrived at t… | FALSE | NULL |
| Democratic Party 2004 | As we come together to declare our vision as Democrats, we are mindful that the challenges of our… | FALSE | NULL |
| Democratic Party 2008 | We come together at a defining moment in the history of our nation - the nation that led the 20th… | FALSE | NULL |
| Democratic Party 2012 | Four years ago, Democrats, independents, and many Republicans came together as Americans to move … | FALSE | NULL |
| Democratic Party 2016 | In 2016, Democrats meet in Philadelphia with the same basic belief that animated the Continental … | FALSE | NULL |
| Democratic Party 2020 | America is an idea-one that has endured and evolved through war and depression, prevailed over fa… | FALSE | NULL |
| Democratic Party 2024 | Our nation is at an inflection point. What kind of America will we be? A land of more freedom, or… | FALSE | NULL |
| Republican Party 1948 | To establish and maintain peace, to build a country in which every citizen can earn a good living… | FALSE | NULL |
| Republican Party 1952 | We maintain that man was not born to be ruled, but that he consented to be governed; and that the… | FALSE | NULL |
| Republican Party 1956 | America’s trust is in the merciful providence of God, in whose image every man is created … the… | FALSE | NULL |
| Republican Party 1960 | The United States is living in an age of profoundest revolution. The lives of men and of nations … | FALSE | NULL |
| Republican Party 1964 | Humanity is tormented once again by an age-old issue-is man to live in dignity and freedom under … | FALSE | NULL |
| Republican Party 1968 | Twice before, our Party gave the people of America leadership at a time of crisis-leadership whic… | FALSE | NULL |
| Republican Party 1972 | This year our Republican Party has greater reason than ever before for pride in its stewardship. … | FALSE | NULL |
| Republican Party 1976 | To you, an American citizen: You are about to read the 1976 Republican Platform. We hope you will… | FALSE | NULL |
| Republican Party 1980 | The Republican Party convenes, presents this platform, and selects its nominees at a time of cris… | FALSE | NULL |
| Republican Party 1984 | This year, the American people will choose between two diametrically opposed visions of what Amer… | FALSE | NULL |
| Republican Party 1988 | An election is about the future, about change. But it is also about the values we will carry with… | FALSE | NULL |
| Republican Party 1992 | Abraham Lincoln, our first Republican President, expressed the philosophy that inspires Republica… | FALSE | NULL |
| Republican Party 1996 | We meet to nominate a candidate and pass a platform at a moment of measureless national opportuni… | FALSE | NULL |
| Republican Party 2000 | We meet at a remarkable time in the life of our country. Our powerful economy gives America a uni… | FALSE | NULL |
| Republican Party 2004 | One hundred and fifty years ago, Americans who had gathered to protest the expansion of slavery g… | FALSE | NULL |
| Republican Party 2008 | This is a platform of enduring principle, not passing convenience - the product of the most open … | FALSE | NULL |
| Republican Party 2012 | The 2012 Republican Platform is a statement of who we are and what we believe as a Party and our … | FALSE | NULL |
| Republican Party 2016 | We believe in American exceptionalism. We believe the United States of America is unlike any othe… | FALSE | NULL |
| Republican Party 2020 | We believe in American exceptionalism. We believe the United States of America is unlike any othe… | FALSE | NULL |
| Republican Party 2024 | Our Nation’s History is filled with the stories of brave men and women who gave everything they h… | FALSE | NULL |
| Anderson 1980 | As the nineteen eighties begin, where does America stand? The United States finds itself knee-dee… | TRUE | \<list\> |
| Green 2000 | As the new century dawns, we look back with somber reflection at how we have been as a people and… | TRUE | \<list\> |
| Green 2016 | The Green Platform The Green Platform presents an eco-social analysis and vision for our country…. | TRUE | \<list\> |
| Independent 1968 | A sense of destiny pervades the creation and adoption of this first Platform of the American Inde… | TRUE | \<list\> |
| Libertarian 1980 | We, the members of the Libertarian Party, challenge the cult of the omnipotent state and defend t… | TRUE | \<list\> |
| Libertarian 1996 | As Libertarians, we seek a world of liberty; a world in which all individuals are sovereign over … | TRUE | \<list\> |
| Libertarian 2016 | As Libertarians, we seek a world of liberty; a world in which all individuals are sovereign over … | TRUE | \<list\> |
| McCarthy 1976 | Americans look for several basic qualities in presidential candidates. Intelligence, for example,… | TRUE | \<list\> |
| McMullin 2016 | America’s men and women in uniform are the pride of our nation. Their sacrifices and hard work ke… | TRUE | \<list\> |
| Nader 2008 | After more than 300 years of de facto affirmative action to benefit white males, we need affirmat… | TRUE | \<list\> |
| Perot 1992 | In June, 117,000 more Americans were thrown out of work. While we were putting the finishing touc… | TRUE | \<list\> |
| Thurmond 1948 | We believe that the Constitution of the United States is the greatest charter of human liberty ev… | TRUE | \<list\> |
| Wallace 1948 | Three years after the end of the second world war, the drums are beating for a third. Civil liber… | TRUE | \<list\> |

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
IScorePackage analysis pipeline, which consists of:

1.  `configure_python()`: Sets up a Python virtual environment and
    downloads the Python tools required for this package’s analysis.
    This function only needs to be run once per R session and is
    required only for `process_platform_emphasis()` and
    `process_platform_position()`.

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
library(IScorePackage)

configure_python()
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
| upper |   0.0249 |               0.3882 |   0.0814 |
