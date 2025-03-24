
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ECFchinook

<!-- badges: start -->

<!-- badges: end -->

The goal of ECFchinook is to evaluate impacts and catches in hypothetial
emerging commercial gear fisheries.

## Installation

You can install the development version of ECFchinook from
[GitHub](https://github.com/wdfw-fp/ECFchinook) with:

``` r
# install.packages("pak")
pak::pak("wdfw-fp/ECFchinook")
```

## Use

Once the package is installed, a template of the input file can be
downloaded with a call as follows, where the `path` is the filepath to
to the location on your computer where you want to save teh file.

``` r
library(ECFchinook)
```

``` r
download_template(path="ECF_planning",filename="Inputs_template.xlsx")
```

the inputs can be changed to that file to represent the season and the
fishery, and that edited version should be saved.

The impacts can than be evaluated and and a report generated. There are
two option when evaluating the impacts:

1.  Evaluate the impacts of the number of days entered in the *Seasons*
    tab of the inputs. This is accomplished by setting
    `find_quota = FALSE`
2.  Find the maximum fishing effort that fits within the allocation,
    with the effort spread out proportionally to the the days specified
    in the *Seasons* tab of the input file. his is accomplished by
    setting `find_quota = TRUE`

``` r
ECF_mod_report(Input_path = "ECF_planning/Inputs_2025.xlsx", year=2025,
               find_quota = FALSE,
                 output_file= "Best_days_report_25.html",
               output_dir ="ECF_planning")

ECF_mod_report(Input_path = "ECF_planning/Inputs_2025.xlsx", year=2025,
               find_quota = TRUE,
                 output_file= "Best_quota_report_25.html",
               output_dir ="ECF_planning")
```

The above calls will save *.html* file in your specified output
directory with the results. It will also save two *.csv* files of
Chinook and steelhead results.

You can look up the help files for a functions with, for example:

``` r
?ECF_mod_report()
```

If you want to run the code without rendering the report and *.csv*
files, you could make a call like the following:

``` r
my_run<-ECF_mod(path ="ECF_planning/Inputs_2025.xlsx",
                year=2025,
                find_quota = FALSE)
```
