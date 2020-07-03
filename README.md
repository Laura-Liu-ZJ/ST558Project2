Automating
================
Zhijun Liu
7/2/2020

  - [Automating Process](#automating-process)
  - [Link of Each Weekday](#link-of-each-weekday)

# Automating Process

``` r
# prepare for the library
library(rmarkdown)
```

    ## Warning: package 'rmarkdown' was built under R version 3.6.2

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.6.3

    ## -- Attaching packages ------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.1     v purrr   0.3.3
    ## v tibble  3.0.1     v dplyr   1.0.0
    ## v tidyr   1.0.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## Warning: package 'ggplot2' was built under R version 3.6.3

    ## Warning: package 'tibble' was built under R version 3.6.3

    ## Warning: package 'tidyr' was built under R version 3.6.3

    ## Warning: package 'readr' was built under R version 3.6.2

    ## Warning: package 'purrr' was built under R version 3.6.2

    ## Warning: package 'dplyr' was built under R version 3.6.3

    ## Warning: package 'stringr' was built under R version 3.6.2

    ## Warning: package 'forcats' was built under R version 3.6.2

    ## -- Conflicts ---------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# get weekdays variables
weekdays <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
# create a list for each weekday with just the weekday parameter
params <- lapply(weekdays, FUN = function(x){list(weekday = x)})
# create file name
reports <- tibble(paste0(weekdays, ".md"), params)
# automatically output 7 files
apply(reports, MARGIN = 1, FUN = function(x){
  render(input = "project2.Rmd", output_file = x[[1]], params = x[[2]])
})
```

    ## 
    ## 
    ## processing file: project2.Rmd

    ##   |                                                                              |                                                                      |   0%  |                                                                              |...                                                                   |   4%
    ##    inline R code fragments
    ## 
    ##   |                                                                              |......                                                                |   8%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |........                                                              |  12%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...........                                                           |  16%
    ## label: library

    ## Warning: package 'leaps' was built under R version 3.6.3

    ## Warning: package 'caret' was built under R version 3.6.3

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

    ## Warning: package 'knitr' was built under R version 3.6.3

    ## Warning: package 'corrplot' was built under R version 3.6.2

    ## corrplot 0.84 loaded

    ##   |                                                                              |..............                                                        |  20%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................                                                     |  24%
    ## label: readData

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   url = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ##   |                                                                              |....................                                                  |  28%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |......................                                                |  32%
    ## label: varSelect

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in =
    ## force.in, : 1 linear dependencies found

    ## Warning: `tbl_df()` is deprecated as of dplyr 1.0.0.
    ## Please use `tibble::as_tibble()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ##   |                                                                              |.........................                                             |  36%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |............................                                          |  40%
    ## label: corrplot (with options) 
    ## List of 1
    ##  $ out.width: chr "200%"

    ## Warning in text.default(pos.xlabel[, 1], pos.xlabel[, 2], newcolnames, srt =
    ## tl.srt, : "lower.col" is not a graphical parameter

    ## Warning in text.default(pos.ylabel[, 1], pos.ylabel[, 2], newrownames, col =
    ## tl.col, : "lower.col" is not a graphical parameter

    ## Warning in title(title, ...): "lower.col" is not a graphical parameter

    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter
    
    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter
    
    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter

    ##   |                                                                              |...............................                                       |  44%
    ##    inline R code fragments
    ## 
    ##   |                                                                              |..................................                                    |  48%
    ## label: summary
    ##   |                                                                              |....................................                                  |  52%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.......................................                               |  56%
    ## label: orgAccuracy
    ##   |                                                                              |..........................................                            |  60%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.............................................                         |  64%
    ## label: modelLinear
    ##   |                                                                              |................................................                      |  68%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |..................................................                    |  72%
    ## label: modelNonlinear
    ##   |                                                                              |.....................................................                 |  76%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................................              |  80%
    ## label: testlinear
    ##   |                                                                              |...........................................................           |  84%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |..............................................................        |  88%
    ## label: testNonlinear
    ##   |                                                                              |................................................................      |  92%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...................................................................   |  96%
    ## label: modelCom
    ##   |                                                                              |......................................................................| 100%
    ##    inline R code fragments

    ## output file: project2.knit.md

    ## "A:/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS project2.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output monday.md --standalone --table-of-contents --toc-depth 3 --template "A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "A:/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS monday.md --to html4 --from gfm --output monday.html --standalone --self-contained --highlight-style pygments --template "A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none --metadata pagetitle=PREVIEW

    ## 
    ## Preview created: C:\Users\laura\AppData\Local\Temp\RtmpKy5ymn\preview-525c35687b85.html

    ## 
    ## Output created: monday.md

    ## 
    ## 
    ## processing file: project2.Rmd

    ##   |                                                                              |                                                                      |   0%  |                                                                              |...                                                                   |   4%
    ##    inline R code fragments
    ## 
    ##   |                                                                              |......                                                                |   8%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |........                                                              |  12%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...........                                                           |  16%
    ## label: library
    ##   |                                                                              |..............                                                        |  20%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................                                                     |  24%
    ## label: readData

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   url = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ##   |                                                                              |....................                                                  |  28%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |......................                                                |  32%
    ## label: varSelect
    ##   |                                                                              |.........................                                             |  36%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |............................                                          |  40%
    ## label: corrplot (with options) 
    ## List of 1
    ##  $ out.width: chr "200%"

    ## Warning in text.default(pos.xlabel[, 1], pos.xlabel[, 2], newcolnames, srt =
    ## tl.srt, : "lower.col" is not a graphical parameter

    ## Warning in text.default(pos.ylabel[, 1], pos.ylabel[, 2], newrownames, col =
    ## tl.col, : "lower.col" is not a graphical parameter

    ## Warning in title(title, ...): "lower.col" is not a graphical parameter

    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter
    
    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter
    
    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter

    ##   |                                                                              |...............................                                       |  44%
    ##    inline R code fragments
    ## 
    ##   |                                                                              |..................................                                    |  48%
    ## label: summary
    ##   |                                                                              |....................................                                  |  52%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.......................................                               |  56%
    ## label: orgAccuracy
    ##   |                                                                              |..........................................                            |  60%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.............................................                         |  64%
    ## label: modelLinear
    ##   |                                                                              |................................................                      |  68%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |..................................................                    |  72%
    ## label: modelNonlinear
    ##   |                                                                              |.....................................................                 |  76%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................................              |  80%
    ## label: testlinear
    ##   |                                                                              |...........................................................           |  84%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |..............................................................        |  88%
    ## label: testNonlinear
    ##   |                                                                              |................................................................      |  92%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...................................................................   |  96%
    ## label: modelCom
    ##   |                                                                              |......................................................................| 100%
    ##    inline R code fragments

    ## output file: project2.knit.md

    ## "A:/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS project2.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output tuesday.md --standalone --table-of-contents --toc-depth 3 --template "A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "A:/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS tuesday.md --to html4 --from gfm --output tuesday.html --standalone --self-contained --highlight-style pygments --template "A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none --metadata pagetitle=PREVIEW

    ## 
    ## Preview created: C:\Users\laura\AppData\Local\Temp\RtmpKy5ymn\preview-525c76005109.html

    ## 
    ## Output created: tuesday.md

    ## 
    ## 
    ## processing file: project2.Rmd

    ##   |                                                                              |                                                                      |   0%  |                                                                              |...                                                                   |   4%
    ##    inline R code fragments
    ## 
    ##   |                                                                              |......                                                                |   8%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |........                                                              |  12%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...........                                                           |  16%
    ## label: library
    ##   |                                                                              |..............                                                        |  20%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................                                                     |  24%
    ## label: readData

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   url = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ##   |                                                                              |....................                                                  |  28%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |......................                                                |  32%
    ## label: varSelect

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in =
    ## force.in, : 1 linear dependencies found

    ##   |                                                                              |.........................                                             |  36%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |............................                                          |  40%
    ## label: corrplot (with options) 
    ## List of 1
    ##  $ out.width: chr "200%"

    ## Warning in text.default(pos.xlabel[, 1], pos.xlabel[, 2], newcolnames, srt =
    ## tl.srt, : "lower.col" is not a graphical parameter

    ## Warning in text.default(pos.ylabel[, 1], pos.ylabel[, 2], newrownames, col =
    ## tl.col, : "lower.col" is not a graphical parameter

    ## Warning in title(title, ...): "lower.col" is not a graphical parameter

    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter
    
    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter
    
    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter

    ##   |                                                                              |...............................                                       |  44%
    ##    inline R code fragments
    ## 
    ##   |                                                                              |..................................                                    |  48%
    ## label: summary
    ##   |                                                                              |....................................                                  |  52%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.......................................                               |  56%
    ## label: orgAccuracy
    ##   |                                                                              |..........................................                            |  60%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.............................................                         |  64%
    ## label: modelLinear
    ##   |                                                                              |................................................                      |  68%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |..................................................                    |  72%
    ## label: modelNonlinear
    ##   |                                                                              |.....................................................                 |  76%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................................              |  80%
    ## label: testlinear
    ##   |                                                                              |...........................................................           |  84%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |..............................................................        |  88%
    ## label: testNonlinear
    ##   |                                                                              |................................................................      |  92%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...................................................................   |  96%
    ## label: modelCom
    ##   |                                                                              |......................................................................| 100%
    ##    inline R code fragments

    ## output file: project2.knit.md

    ## "A:/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS project2.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output wednesday.md --standalone --table-of-contents --toc-depth 3 --template "A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "A:/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS wednesday.md --to html4 --from gfm --output wednesday.html --standalone --self-contained --highlight-style pygments --template "A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none --metadata pagetitle=PREVIEW

    ## 
    ## Preview created: C:\Users\laura\AppData\Local\Temp\RtmpKy5ymn\preview-525c2a7115e5.html

    ## 
    ## Output created: wednesday.md

    ## 
    ## 
    ## processing file: project2.Rmd

    ##   |                                                                              |                                                                      |   0%  |                                                                              |...                                                                   |   4%
    ##    inline R code fragments
    ## 
    ##   |                                                                              |......                                                                |   8%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |........                                                              |  12%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...........                                                           |  16%
    ## label: library
    ##   |                                                                              |..............                                                        |  20%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................                                                     |  24%
    ## label: readData

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   url = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ##   |                                                                              |....................                                                  |  28%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |......................                                                |  32%
    ## label: varSelect

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in =
    ## force.in, : 1 linear dependencies found

    ##   |                                                                              |.........................                                             |  36%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |............................                                          |  40%
    ## label: corrplot (with options) 
    ## List of 1
    ##  $ out.width: chr "200%"

    ## Warning in text.default(pos.xlabel[, 1], pos.xlabel[, 2], newcolnames, srt =
    ## tl.srt, : "lower.col" is not a graphical parameter

    ## Warning in text.default(pos.ylabel[, 1], pos.ylabel[, 2], newrownames, col =
    ## tl.col, : "lower.col" is not a graphical parameter

    ## Warning in title(title, ...): "lower.col" is not a graphical parameter

    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter
    
    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter
    
    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter

    ##   |                                                                              |...............................                                       |  44%
    ##    inline R code fragments
    ## 
    ##   |                                                                              |..................................                                    |  48%
    ## label: summary
    ##   |                                                                              |....................................                                  |  52%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.......................................                               |  56%
    ## label: orgAccuracy
    ##   |                                                                              |..........................................                            |  60%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.............................................                         |  64%
    ## label: modelLinear
    ##   |                                                                              |................................................                      |  68%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |..................................................                    |  72%
    ## label: modelNonlinear
    ##   |                                                                              |.....................................................                 |  76%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................................              |  80%
    ## label: testlinear
    ##   |                                                                              |...........................................................           |  84%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |..............................................................        |  88%
    ## label: testNonlinear
    ##   |                                                                              |................................................................      |  92%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...................................................................   |  96%
    ## label: modelCom
    ##   |                                                                              |......................................................................| 100%
    ##    inline R code fragments

    ## output file: project2.knit.md

    ## "A:/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS project2.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output thursday.md --standalone --table-of-contents --toc-depth 3 --template "A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "A:/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS thursday.md --to html4 --from gfm --output thursday.html --standalone --self-contained --highlight-style pygments --template "A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none --metadata pagetitle=PREVIEW

    ## 
    ## Preview created: C:\Users\laura\AppData\Local\Temp\RtmpKy5ymn\preview-525c1834f35.html

    ## 
    ## Output created: thursday.md

    ## 
    ## 
    ## processing file: project2.Rmd

    ##   |                                                                              |                                                                      |   0%  |                                                                              |...                                                                   |   4%
    ##    inline R code fragments
    ## 
    ##   |                                                                              |......                                                                |   8%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |........                                                              |  12%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...........                                                           |  16%
    ## label: library
    ##   |                                                                              |..............                                                        |  20%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................                                                     |  24%
    ## label: readData

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   url = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ##   |                                                                              |....................                                                  |  28%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |......................                                                |  32%
    ## label: varSelect

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in =
    ## force.in, : 1 linear dependencies found

    ##   |                                                                              |.........................                                             |  36%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |............................                                          |  40%
    ## label: corrplot (with options) 
    ## List of 1
    ##  $ out.width: chr "200%"

    ## Warning in text.default(pos.xlabel[, 1], pos.xlabel[, 2], newcolnames, srt =
    ## tl.srt, : "lower.col" is not a graphical parameter

    ## Warning in text.default(pos.ylabel[, 1], pos.ylabel[, 2], newrownames, col =
    ## tl.col, : "lower.col" is not a graphical parameter

    ## Warning in title(title, ...): "lower.col" is not a graphical parameter

    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter
    
    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter
    
    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter

    ##   |                                                                              |...............................                                       |  44%
    ##    inline R code fragments
    ## 
    ##   |                                                                              |..................................                                    |  48%
    ## label: summary
    ##   |                                                                              |....................................                                  |  52%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.......................................                               |  56%
    ## label: orgAccuracy
    ##   |                                                                              |..........................................                            |  60%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.............................................                         |  64%
    ## label: modelLinear
    ##   |                                                                              |................................................                      |  68%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |..................................................                    |  72%
    ## label: modelNonlinear
    ##   |                                                                              |.....................................................                 |  76%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................................              |  80%
    ## label: testlinear
    ##   |                                                                              |...........................................................           |  84%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |..............................................................        |  88%
    ## label: testNonlinear
    ##   |                                                                              |................................................................      |  92%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...................................................................   |  96%
    ## label: modelCom
    ##   |                                                                              |......................................................................| 100%
    ##    inline R code fragments

    ## output file: project2.knit.md

    ## "A:/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS project2.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output friday.md --standalone --table-of-contents --toc-depth 3 --template "A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "A:/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS friday.md --to html4 --from gfm --output friday.html --standalone --self-contained --highlight-style pygments --template "A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none --metadata pagetitle=PREVIEW

    ## 
    ## Preview created: C:\Users\laura\AppData\Local\Temp\RtmpKy5ymn\preview-525c6ab829c7.html

    ## 
    ## Output created: friday.md

    ## 
    ## 
    ## processing file: project2.Rmd

    ##   |                                                                              |                                                                      |   0%  |                                                                              |...                                                                   |   4%
    ##    inline R code fragments
    ## 
    ##   |                                                                              |......                                                                |   8%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |........                                                              |  12%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...........                                                           |  16%
    ## label: library
    ##   |                                                                              |..............                                                        |  20%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................                                                     |  24%
    ## label: readData

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   url = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ##   |                                                                              |....................                                                  |  28%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |......................                                                |  32%
    ## label: varSelect

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in =
    ## force.in, : 1 linear dependencies found

    ##   |                                                                              |.........................                                             |  36%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |............................                                          |  40%
    ## label: corrplot (with options) 
    ## List of 1
    ##  $ out.width: chr "200%"

    ## Warning in text.default(pos.xlabel[, 1], pos.xlabel[, 2], newcolnames, srt =
    ## tl.srt, : "lower.col" is not a graphical parameter

    ## Warning in text.default(pos.ylabel[, 1], pos.ylabel[, 2], newrownames, col =
    ## tl.col, : "lower.col" is not a graphical parameter

    ## Warning in title(title, ...): "lower.col" is not a graphical parameter

    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter
    
    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter
    
    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter

    ##   |                                                                              |...............................                                       |  44%
    ##    inline R code fragments
    ## 
    ##   |                                                                              |..................................                                    |  48%
    ## label: summary
    ##   |                                                                              |....................................                                  |  52%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.......................................                               |  56%
    ## label: orgAccuracy
    ##   |                                                                              |..........................................                            |  60%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.............................................                         |  64%
    ## label: modelLinear
    ##   |                                                                              |................................................                      |  68%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |..................................................                    |  72%
    ## label: modelNonlinear
    ##   |                                                                              |.....................................................                 |  76%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................................              |  80%
    ## label: testlinear
    ##   |                                                                              |...........................................................           |  84%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |..............................................................        |  88%
    ## label: testNonlinear
    ##   |                                                                              |................................................................      |  92%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...................................................................   |  96%
    ## label: modelCom
    ##   |                                                                              |......................................................................| 100%
    ##    inline R code fragments

    ## output file: project2.knit.md

    ## "A:/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS project2.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output saturday.md --standalone --table-of-contents --toc-depth 3 --template "A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "A:/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS saturday.md --to html4 --from gfm --output saturday.html --standalone --self-contained --highlight-style pygments --template "A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none --metadata pagetitle=PREVIEW

    ## 
    ## Preview created: C:\Users\laura\AppData\Local\Temp\RtmpKy5ymn\preview-525c631f2c5c.html

    ## 
    ## Output created: saturday.md

    ## 
    ## 
    ## processing file: project2.Rmd

    ##   |                                                                              |                                                                      |   0%  |                                                                              |...                                                                   |   4%
    ##    inline R code fragments
    ## 
    ##   |                                                                              |......                                                                |   8%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |........                                                              |  12%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...........                                                           |  16%
    ## label: library
    ##   |                                                                              |..............                                                        |  20%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................                                                     |  24%
    ## label: readData

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   url = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ##   |                                                                              |....................                                                  |  28%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |......................                                                |  32%
    ## label: varSelect

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in =
    ## force.in, : 1 linear dependencies found

    ##   |                                                                              |.........................                                             |  36%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |............................                                          |  40%
    ## label: corrplot (with options) 
    ## List of 1
    ##  $ out.width: chr "200%"

    ## Warning in text.default(pos.xlabel[, 1], pos.xlabel[, 2], newcolnames, srt =
    ## tl.srt, : "lower.col" is not a graphical parameter

    ## Warning in text.default(pos.ylabel[, 1], pos.ylabel[, 2], newrownames, col =
    ## tl.col, : "lower.col" is not a graphical parameter

    ## Warning in title(title, ...): "lower.col" is not a graphical parameter

    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter
    
    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter
    
    ## Warning in doTryCatch(return(expr), name, parentenv, handler): "lower.col" is
    ## not a graphical parameter

    ##   |                                                                              |...............................                                       |  44%
    ##    inline R code fragments
    ## 
    ##   |                                                                              |..................................                                    |  48%
    ## label: summary
    ##   |                                                                              |....................................                                  |  52%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.......................................                               |  56%
    ## label: orgAccuracy
    ##   |                                                                              |..........................................                            |  60%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.............................................                         |  64%
    ## label: modelLinear
    ##   |                                                                              |................................................                      |  68%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |..................................................                    |  72%
    ## label: modelNonlinear
    ##   |                                                                              |.....................................................                 |  76%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................................              |  80%
    ## label: testlinear
    ##   |                                                                              |...........................................................           |  84%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |..............................................................        |  88%
    ## label: testNonlinear
    ##   |                                                                              |................................................................      |  92%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...................................................................   |  96%
    ## label: modelCom
    ##   |                                                                              |......................................................................| 100%
    ##    inline R code fragments

    ## output file: project2.knit.md

    ## "A:/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS project2.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output sunday.md --standalone --table-of-contents --toc-depth 3 --template "A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "A:/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS sunday.md --to html4 --from gfm --output sunday.html --standalone --self-contained --highlight-style pygments --template "A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:A:\R-3.6.1\library\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none --metadata pagetitle=PREVIEW

    ## 
    ## Preview created: C:\Users\laura\AppData\Local\Temp\RtmpKy5ymn\preview-525c2ee828f5.html

    ## 
    ## Output created: sunday.md

    ## [1] "D:/2020_3rd_semester/ST558/6. Project/Project2/ST558Project2/monday.md"   
    ## [2] "D:/2020_3rd_semester/ST558/6. Project/Project2/ST558Project2/tuesday.md"  
    ## [3] "D:/2020_3rd_semester/ST558/6. Project/Project2/ST558Project2/wednesday.md"
    ## [4] "D:/2020_3rd_semester/ST558/6. Project/Project2/ST558Project2/thursday.md" 
    ## [5] "D:/2020_3rd_semester/ST558/6. Project/Project2/ST558Project2/friday.md"   
    ## [6] "D:/2020_3rd_semester/ST558/6. Project/Project2/ST558Project2/saturday.md" 
    ## [7] "D:/2020_3rd_semester/ST558/6. Project/Project2/ST558Project2/sunday.md"

# Link of Each Weekday

  - The analysis for [Monday is available here](monday.md).
  - The analysis for [Tuesday is available here](tuesday.md).
  - The analysis for [Wednesday is available here](wednesday.md).
  - The analysis for [Thursday is available here](thursday.md).
  - The analysis for [Friday is available here](friday.md).
  - The analysis for [Saturday is available here](saturday.md).
  - The analysis for [Sunday is available here](sunday.md).
