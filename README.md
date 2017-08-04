README
================

[![Build Status](https://travis-ci.org/lorenzwalthert/strcode.svg?branch=master)](https://travis-ci.org/lorenzwalthert/strcode) [![Project Status: WIP ? Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/strcode)](https://cran.r-project.org/package=strcode) [![packageversion](https://img.shields.io/badge/Package%20version-0.2.0-orange.svg?style=flat-square)](commits/master)

The `strcode` (short for structuring code) package contains tools to organize and abstract your code better. It consists of

-   An [RStudio Add-in](https://rstudio.github.io/rstudioaddins/) that lets you quickly add code block separators and titles (possibly with unique identifiers) to divide your work into sections. The titles are recognized as sections by RStudio, which enhances the coding experience further.
-   A function `sum_str` that summarizes the code structure based on the separators and their comments added with the Add-in. For one or more files, it can cat the structure to the console or a file.
-   An [RStudio Add-in](https://rstudio.github.io/rstudioaddins/) that lets you insert a code anchor, that is, a hash sequence which can be used to uniquely identify a line in a large code base.

<!-- You can learn more about structuring code in [Bono Usu](https://github.com/lorenzwalthert/bonousu/blob/devel/docs/commenting-code.html), 
a guide for good practice in R programming. -->
<img src="https://raw.githubusercontent.com/lorenzwalthert/strcode/master/demos/strcode_v0.2.0_video_to_gif2_large.gif" width="650px" />

Installation
============

You can install the package from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("XiaoliangJiang/strcode",ref="semantics")
```

Structuring code
================

For basic use
-------------
We suggest three levels of granularity for code structuring, whereas higher-level blocks can contain lower-level blocks.

-   level 1 sections, which are high-level blocks that can be separated as follows:

``` r
#       ________________________________________________________________________
#       A title                                                             ####
```

-   level 2 sections, which are medium-level blocks that can be separated as follows:


``` r
##      ........................................................................
##      A subtitle                                                          ####
```

-   level 3 sections, which are low-level blocks that can be separated as follows:

``` r
###     .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ...
###     One more                                                            ####
```

You can notice from the above that

-   The number of `#` used in front of the break character (`___`, `...`, `.. ..`) corresponds to the level of granularity that is separated.
-   The breaks characters `___`, `...`, `.. ..` were chosen such that they reflect the level of granularity, namely `___` has a much higher visual density than `.. ..`.
-   Each block has an (optional) short title on what that block is about.
-   Every title ends with `####`. Therefore, the titles are recognized by RStudio as [sections](https://support.rstudio.com/hc/en-us/articles/200484568-Code-Folding-and-Sections). This has the advantages that you can get a quick summary of your code in Rstudio's code pane and you can fold sections as you can fold code or function declarations or if statements. See the pictures below for details.

The separators all have length 80. The value is looked up in the global option `strcode$char_length` and can therefore be changed by the user.

By default, breaks and titles are inserted via a Shiny Gadget, but this default can be overridden by setting the option `strcode$insert_with_shiny` to `FALSE` and hence only inserting the break.

For semantic use
----------------
Firstly, click check box `Add semantics` to show more options for semantic use. We suggest seven levels of granularity for code structuring, whereas higher-level blocks can contain lower-level blocks.

-   level 1 sections, which are highest-level blocks that usually represents user's workflow domain and can be separated as follows:

``` r
#       ________________________________________________________________________
#       YourWorkflow {WorkID provone:Workflow}                              ####
```

-   level 2 sections, which are second-highest-level blocks that are the highest level for detailed entities can be separated as follows:

``` r
##      ........................................................................
##      YourProcess {ProcessID provone:Process}                             ####
```

-   level 3 sections, which are lower-level blocks that can be separated as follows:

``` r
###     .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ...
###     YourProcess02 {ProcessID02 provone:Process}                         ####
```

-   level 4 sections, which are lower-level blocks that can be separated as follows:

``` r
####    ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ....
####    YourData {DataID provone:Data}                                      ####
```

You can notice from the above that

-   The breaks characters `___`, `...`, `.. ..`,`... ...` were chosen such that they reflect the level of granularity, namely `___` means highest level (or domain level), and the number of dots between spaces represents different lower levels, for example, `.. ..` means second level of entities level (third level in total).

-   The first argument which is outside of brace is title. The first argument in the pair of braces is ID and second argument is class. And if you add more manully inputs, they will appear in the pair of braces after class, like:

``` r
####    ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ....
####    YourData {DataID provone:Data manuallyinput=value}                  ####
```

-   Manually input values should follow the structure: property=value. The user interface only accept manually input values in correct structure.
-   A JSON-LD structure style also could be generated by the function with click the check box `JSON-LD` to enable it. With same input above, the result of using JSON-LD style is like:

``` r
####    .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ...
####    {"@id":DataID, "@type":provone:Data, "label":YourData}              ####
```

Anchoring sections
==================

Sometimes it is required to refer to a code section, which can be done by title. A better way, however, is to use a unique hash sequence - let us call it a code anchor - to create an arguably unique reference to that section. A code anchor in `strcode` is enclosed by `#<` and `>#` so all anchors can be found using regular expressions. You can add section breaks that include a hash. That might look like this:

``` r
##  .................. #< 685c967d4e78477623b861d533d0937a ># ..................
##  An anchored section                                                     ####
```

Insert a code anchor
====================

Code anchors might prove helpful in other situations where one want to anchor a single line. That is also possible with `strcode`. An example of a code anchor is the following:

``` r
#< 56f5139874167f4f5635b42c37fd6594 >#
this_is_a_super_important_but_hard_to_describe_line_so_let_me_anchor_it
```

The hash sequences in strcode are produced with the R package [digest](https://github.com/eddelbuettel/digest).

Summarizing code
================

Once code has been structured by adding sections (as above), it can easily be summarized or represented in a compact and abstract form. This is particularly handy when the codebase is large, when a lot of people work on the code or when new people join a project. The function `sum_str` is designed for the purpose of extracting separators and respective comments, in order to provide high level code summaries. It is highly customizable and flexible, with a host of options. Thanks to RStudio's API, you can even create summaries of the file you are working on, simply by typing `sum_str()` in the console. The file presented in the example section below can be summarized as follows:

``` r
sum_str(path_in = "placeholder_code/example.R", 
        file_out = "",
        width = 40,
        granularity = 2,
        lowest_sep = FALSE, 
        header = TRUE)
#> Summarized structure of placeholder_code/example.R
#> 
#> line  level section
#> 2    #   _
#> 3    #   function test
#> 6    ##  -A: pre-processing
#> 57   ##  B: actual function
#> 83   #   ____________________________________
#> 84   #   function test2
#> 87   ##  A: pre-processing
#> 138  ##  B: actual function
#> 169  ##  test
```

-   `path_in` specifies a directory or filenames for looking for content to summarize.
-   `file_out` indicates where to dump the output.
-   `width` gives the width of the output in characters.
-   `granularity = 2`indicates that we want two of three levels of granularity to be contained in the summary and don't include level 3 comments.
-   Similarly, we use `lowest_sep = FALSE` to indicate that we want lowest separators (given `granularity`) to be omitted between the titles of the sections.
-   `header` was set to `TRUE`, so the column names were reported as well. Note that they are slightly off since knitr uses a different tab length. In the R console and more imporantly in the outputed file, they are aliged.

Example of improved legibility
------------------------------

To demonstrate the improvement in legibility, we give an extended example with some placeholder code.

``` r
#   ____________________________________________________________________________
#   function test                                                           ####
test <- function(x) {
##  ............................................................................
##  A: pre-processing                                                       ####
### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
### a: assertive tests                                                      ####
  # x
  if(missing(x) || is.null(x)){ 
    x <- character()
  }
  assert(
    # use check within assert
    check_character(x),
    check_factor(x),
    check_numeric(x)
  )
  
  # levels 
  if(!missing(levels)){
    assert(
      check_character(levels),
      check_integer(levels),
      check_numeric(levels))
    levels <- na.omit(levels)
    
  }
  
  # labels
  if(!missing(labels)){
    assert(
      check_character(labels),
      check_numeric(labels),
      check_factor(labels)
      )
  }
  
### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
### b: coercion / remove missing                                            ####
  x <- as.character(x)
  uniq_x <- unique(na.omit(x), nmax = nmax)
  
### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
### c: warnings                                                             ####
  
  if(length(breaks) == 1) {
    if(breaks > max(x) - min(x) + 1) {
      stop("range too small for the number of breaks specified")
    }
    if(length(x) <= breaks) {
      warning("breaks is a scalar not smaller than the length of x")
    }
  }  
  
##  ............................................................................
##  B: actual function                                                      ####
   variable < -paste("T", period, "nog_", sector, sep = "")
   variable <- paste(variable, "==", 1, sep = "")
        
   arg<-substitute(variable)
   r<-eval(arg, idlist.data[[1]])
   a<<-1
   
   was_factor <- FALSE
   if (is.factor(yes)) {
     yes <- as.character(yes)
     was_factor <- TRUE
   } 
   if (is.factor(no)) {
     no <- as.character(no)
     was_factor <- TRUE
   }
   out <- ifelse(test, yes, no)
   if(was_factor) {
     cfactor(out)
   } else {
     out
   } 
   
##  ............................................................................
}
#   ____________________________________________________________________________
#   function test2                                                          ####
test2 <- function(x) {
##  ............................................................................
##  A: pre-processing                                                       ####
### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
### a: assertive tests                                                      ####
  # x
  if(missing(x) || is.null(x)){ 
    x <- character()
  }
  assert(
    # use check within assert
    check_character(x),
    check_factor(x),
    check_numeric(x)
  )
  
  # levels 
  if(!missing(levels)){
    assert(
      check_character(levels),
      check_integer(levels),
      check_numeric(levels))
    levels <- na.omit(levels)
    
  }
  
  # labels
  if(!missing(labels)){
    assert(
      check_character(labels),
      check_numeric(labels),
      check_factor(labels)
      )
  }
  
### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
### b: coercion / remove missing                                            ####
  x <- as.character(x)
  uniq_x <- unique(na.omit(x), nmax = nmax)
  
### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
### c: warnings                                                             ####
  
  if(length(breaks) == 1) {
    if(breaks > max(x) - min(x) + 1) {
      stop("range too small for the number of breaks specified")
    }
    if(length(x) <= breaks) {
      warning("breaks is a scalar not smaller than the length of x")
    }
  }  
  
##  ............................................................................
##  B: actual function                                                      ####
   variable < -paste("T", period, "nog_", sector, sep = "")
   variable <- paste(variable, "==", 1, sep = "")
        
   arg<-substitute(variable)
   r<-eval(arg, idlist.data[[1]])
   a<<-1
   
   was_factor <- FALSE
   if (is.factor(yes)) {
     yes <- as.character(yes)
     was_factor <- TRUE
   } 
   if (is.factor(no)) {
     no <- as.character(no)
     was_factor <- TRUE
   }
   out <- ifelse(test, yes, no)
   if(was_factor) {
     cfactor(out)
   } else {
     out
   } 
   
##  ............................................................................
}
```
