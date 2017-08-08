README
================

[![Build Status](https://travis-ci.org/lorenzwalthert/strcode.svg?branch=master)](https://travis-ci.org/lorenzwalthert/strcode) [![Project Status: WIP ? Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/strcode)](https://cran.r-project.org/package=strcode) [![packageversion](https://img.shields.io/badge/Package%20version-0.2.0-orange.svg?style=flat-square)](commits/master)

# Table of Contents
1. [Introduction](#intro)
2. [Installation](#install)
3. [Structuring Code](#structuring)
   + Basic Code Structuring (no embedded semantics)
   + [Structuring Code with Embedded Semantics](#structuringSem)
   + Anchoring Sections
   + Inserting a code anchor
4. [Summarizing Code](#summarizing)
   + Summarizing without Embedded Semantics
   + [Summarizing with Embedded Semantics](#summarizingSem)
5. [Improving Legibility through Code Structuring](#legibility)
   + Improve Legibility without Embedded Semantics
   + [Improved Legibility with Embedded Semantics](#legibilitySem)

<a id="intro"></a>
# Introduction

The `strcode` (short for structuring code) package contains tools to organize and abstract your code better. It consists of

- An [RStudio Add-in](https://rstudio.github.io/rstudioaddins/) that lets you quickly add code block separators and titles (possibly with unique identifiers) to divide your work into sections. The titles are recognized as sections by RStudio, which enhances the coding experience further.
- A function `sum_str` that summarizes the code structure based on the separators and their comments added with the Add-in. For one or more files, it can cat the structure to the console or a file. 
  - `sum_str` function can also structure code with embedded semantics, and generate an RDF file and RDF diagram automatically. 
  - Users can define the association library by themselves for `sum_str` which contains rules of automatically generated associations.
- An [RStudio Add-in](https://rstudio.github.io/rstudioaddins/) that lets you insert a code anchor, that is, a hash sequence which can be used to uniquely identify a line in a large code base.

<!-- You can learn more about structuring code in [Bono Usu](https://github.com/lorenzwalthert/bonousu/blob/devel/docs/commenting-code.html), 
a guide for good practice in R programming. -->
<img src="https://raw.githubusercontent.com/lorenzwalthert/strcode/master/demos/strcode_v0.2.0_video_to_gif2_large.gif" width="650px" />

<a id="install"></a>
# Installation

You can install the package from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("XiaoliangJiang/strcode",ref="semantics")
```
<a id="structuring"></a>
# Structuring Code

## Basic Code Structuring (no embedded semantics)
We suggest three levels of granularity for 'basic' code structuring, whereas higher-level blocks can contain lower-level blocks.

- **Level 1** sections: high-level blocks that can be separated as follows:

``` r
#       ________________________________________________________________________
#       A title                                                             ####
```

- **Level 2** sections: medium-level blocks that can be separated as follows:


``` r
##      ........................................................................
##      A subtitle                                                          ####
```

- **Level 3** sections: low-level blocks that can be separated as follows:

``` r
###     .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ...
###     One more                                                            ####
```

Notice from these examples that:

- The number of `#` used in front of the break character (`___`, `...`, `.. ..`) corresponds to the level of granularity that is separated.
- The break characters `___`, `...`, `.. ..` were chosen such that they reflect the level of granularity, namely `___` has a much higher visual density than `.. ..`.
- Each block has an (optional) short title on what that block is about.
- Every title ends with `####`. Therefore, the titles are recognized by RStudio as [sections](https://support.rstudio.com/hc/en-us/articles/200484568-Code-Folding-and-Sections). This has the advantages that you can get a quick summary of your code in Rstudio's code pane and you can fold sections as you can fold code or function declarations or if statements. See the pictures below for details.

*The separators each have a length of 80 characters*. The value is set by the global option `strcode$char_length` and can therefore be changed by the user.

By default, breaks and titles are inserted via a `shiny` Gadget, but this default may  be overridden by setting the option `strcode$insert_with_shiny` to `FALSE` and hence only the break is inserted.

<a id="structuringSem"></a>
## Structuring Code with Embedded Semantics

After invoking the interface, click check box `Add semantics` to show more options for semantic use. For complicated scripts up to seven levels of granularity might be required. Examples semantic annotation at different levels appear below.

- **Level 1** sections: the highest-level blocks that usually represent user's workflow domain and can be separated as follows:

``` r
#       ________________________________________________________________________
#       YourWorkflow {WorkID provone:Workflow}                              ####
```

- **Level 2** sections: second-highest-level blocks that are the highest level for detailed entities can be separated as follows:

``` r
##      ........................................................................
##      YourProcess {ProcessID provone:Process}                             ####
```

- **Level 3** sections: lower-level blocks that can be separated as follows:

``` r
###     .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ...
###     YourProcess02 {ProcessID02 provone:Process}                         ####
```

-   **Level 4** sections: lower-level blocks that can be separated as follows:

``` r
####    ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ....
####    YourData {DataID provone:Data}                                      ####
```
or 
``` r
####    ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ....
####    YourData {VisualizationID provone:Visualization}                    ####
```

Notice from these examples that:

- The break characters `___`, `...`, `.. ..`,`... ...` were chosen such that they reflect the level of granularity, namely `___` means highest level (or domain level), and the number of dots between spaces represents different lower levels, for example, `.. ..` means second level of entities level (third level in total).

- The first argument which is outside of the brace `{` is the *title*. The first argument in the pair of braces is *ID* and second argument is *class*. If you add more manual inputs, they will appear in the pair of braces after class, like:

``` r
####    ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ....
####    YourData {DataID provone:Data manuallyinput=value}                  ####
```

- Manually inputted values must follow the structure: `property=value`. The user interface can only accept manually input values following this structure!
- A structure following JSON-LD syntax also can be generated by the function by selecting `JSON-LD`. With same input above, the result of using JSON-LD style let users understand arguments easier and it is like:

``` r
####    .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ...
####    {"@id":DataID, "@type":provone:Data, "label":YourData}              ####
```

- **Since the summarization function uses code levels to automatically generate associations between entities, for semantics embedding a child entity must be 'under' its parent entity.**
- Notice that only given classes exist in `AssociationLibrary.txt` file (in `semantics` branch, `demos` folder) can serve as associations and automatically generated by the function. After downloading and pasting it into your current working directory --- use `getwd()` to show your working directory in R --- you could added new associations into this library by yourself. This file has five columns: `ParentClass` , `ChildClass`, `Ways`, `Property`, and `ReverseProperty`. You must follow this structure and add five values --- separated by commas `,` --- for your user-defined associations. The first value in `ParentClass` column is the class of parent entity; the second value in `ChildClass` is the class of child entity; the third value in `Ways` column can be only `1` or `2` which indicates this association is one-way or two-way; the fourth value in `Property` column represents the property of the association from parent entity to child entity, which also will be shown in RDF diagram; the last value in the `ReverseProperty` column represent the property in reverse way, which is from child entity to parent entity, and this value could be " " in onw-way associations.

-   If a parent entity has multiple child entities, put these child entities under the parent entity level, and the function will generat correct relationship automatically. For example, if your inputs are as follows:

``` r
#       ________________________________________________________________________
#       YourWorkflow {WorkID provone:Workflow}                              ####

##      ........................................................................
##      YourProcess {ProcessID provone:Process}                             ####

##      ........................................................................
##      YourProcess02 {ProcessID02 provone:Process}                         ####
```
To insert semantics breaks, you can follow this:
<img src="https://github.com/XiaoliangJiang/strcode/blob/semantics/demos/Howtoadd.gif" width="600px" />

A graphical representation of the RDF for this structure based on `igraph` looks like:

<img src="https://github.com/XiaoliangJiang/strcode/blob/semantics/demos/Multiple%20Child%20entities.png" width="300px" />

-   If a child entity has multiple parent entities, please manually input associations in this structure: associations=ID. for example, if your breaks are as follows:

``` r
#       ________________________________________________________________________
#       YourWorkflow {WorkID provone:Workflow provone:hasSubProcess=ProcessID02}####

##      ........................................................................
##      YourProcess {ProcessID provone:Process}                             ####

###     .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ...
###     YourProcess02 {ProcessID02 provone:Process provone:wasDerivedFrom=WorkID}####
```
The RDF graph now looks like :

<img src="https://github.com/XiaoliangJiang/strcode/blob/semantics/demos/Multiple%20Parent%20entities.png" width="300px" />

Notice that if you want to generate two-ways association, you need to manually added associations both in parent and child entities. In addition, only given words which exist in AssociationNames.txt file (in semantics branch, demos folder) can serve as associations between two entities and be manually added in. After download and paste this file into your current working directory, you could add associations by yourself. For more information about how to add associations into the list, please read Summarizing code part.

## Anchoring Sections

Sometimes it is required to refer to a code section, which can be done by title. A better way, however, is to use a unique hash sequence - let us call it a code anchor - to create an arguably unique reference to that section. A code anchor in `strcode` is enclosed by `#<` and `>#` so all anchors can be found using regular expressions. You can add section breaks that include a hash. That might look like this:

``` r
##  .................. #< 685c967d4e78477623b861d533d0937a ># ..................
##  An anchored section                                                     ####
```

## Inserting a code anchor

Code anchors might prove helpful in other situations where one want to anchor a single line. That is also possible with `strcode`. An example of a code anchor is the following:

``` r
#< 56f5139874167f4f5635b42c37fd6594 >#
this_is_a_super_important_but_hard_to_describe_line_so_let_me_anchor_it
```

The hash sequences in strcode are produced with the R package [digest](https://github.com/eddelbuettel/digest).

<a id="summarizing"></a>
# Summarizing Code

## Without Embedded Semantics
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

<a id="summarizingSem"></a>
## Summarizing with Embedded Semantics
For semantic use, some new arguments are needed. 
-   `rdf` specifies a type of output rdf file. Only "ttl" has been added into function right now.
-   `graph` indicates whether to generate a RDF graph or not. The default value is FALSE.
-   `domain` indicates whether to use User-defined working domain. If `domain=TRUE`, the value of baseURI, UserID and prefix values are needed
-   `baseURI` specifies a working URI, the default value is `"http://example.org/base/"`.
-   `UserID` specifies a lower level of working URI, the default value is "UserID". Combine with baseURI the full URI with default value will be `"http://example.org/base/UserID"`.
-   `prefix` specifies an abbreviate name of user domain. The default value is "user". The prefix and full URI will become prefix header of output RDF file as follows: ` @prefix cwf:    <http://cwf.tw.rpi.edu/data#/> .`
-   `UserAL` indicates whether to use User-defined association library, and the default value is FALSE. With default value FALSE, the function will generate a .txt file in your current working directory named DefaultAssociationLibrary.txt. You can rename it as AssociationLibrary.txt to let it serve as your user-defined association library, and add new relations in it.
-   `UserANM` indicates whether to use User-defined association names, and the default value is FALSE. With default value FALSE, the function will generate a .txt file in your current working directory named DefaultAssociationNames.txt. You can rename it as AssociationNames.txt to let it serve as your user-defined association names list, and add new associations in it.
-   `fillAssociation` indicates whether to use default associations `str:has` and `str:belongTo` to supplement vacancy association. The default value is TRUE. Since the function could only use associations which exist in associations, if set this value to FALSE and no association belongs to some given pairs of parent and child entities, the output file and RDF diagram may be incorrect. If you see `str:has` and `str:belongTo` in your output file or RDF diagram, you need to add that associations in AssociationLibrary.txt and regenerate your output file.

You can find an example of how those arguments work in Example of improved legibility part.

<a id="legibility"></a>
# Improving Legibility through Code Structuring
## Improve Legibility without Embedded Semantics

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
<a id="legibilitySem"></a>
## Improved Legibility with Embedded Semantics

By using input breaks as follows:
``` r
#       ________________________________________________________________________
#       YourWorkflow {WorkID provone:Workflow}                              ####

##      ........................................................................
##      YourProcess {ProcessID provone:Process}                             ####

###     .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ...
###     YourProcess02 {ProcessID02 provone:Process}                         ####
```
And `sum_str` function with following arguments:
``` r
sum_str(rdf="ttl")
```
You can get an out put file like this:
```
 @prefix user:    <http://example.org/base/UserID/> .
 @prefix provone: <http://dataone.org/ns/provone#> . 
 
 user:WorkID a provone:Workflow ; 
 	 rdfs:label "YourWorkflow" ; 
  	 str:has user:ProcessID . 
 
 user:ProcessID a provone:Process ; 
 	 rdfs:label "YourProcess" ; 
  	 str:belongTo user:WorkID ; 
 	 provone:hasSubProcess user:ProcessID02 . 
 
 user:ProcessID02 a provone:Process ; 
 	 rdfs:label "YourProcess02" ; 
  	 provone:wasDerivedFrom user:ProcessID . 
```
Notice that `str:has` and `str:belongTo` are in this output file, which means no such association between a `provone:Workflow` and `provone:Process` exist in default associations library. You can rename the auto-created file DefaultAssociationLibrary.txt in your working directory and add a new line `"provone:Workflow","provone:Process",1,"provone:hasSubProcess"," "` in it. After that, save your file and if you run the function again with a new argument `UserAL=TRUE`, the output file will as follows:
```
 @prefix user:    <http://example.org/base/UserID/> .
 @prefix provone: <http://dataone.org/ns/provone#> . 
 
 user:WorkID a provone:Workflow ; 
 	 rdfs:label "YourWorkflow" ; 
  	 provone:hasSubProcess user:ProcessID . 
 
 user:ProcessID a provone:Process ; 
 	 rdfs:label "YourProcess" ; 
  	 provone:hasSubProcess user:ProcessID02 . 
 
 user:ProcessID02 a provone:Process ; 
 	 rdfs:label "YourProcess02" ; 
  	 provone:wasDerivedFrom user:ProcessID . 
```
You may find that default associations are missing in this output file.

You can add another argument `graph=TRUE` into your function. With `sum_str(rdf="ttl", UserAL=TRUE, graph=TRUE)`, you can generate a RDF diagram as follows:

<img src="https://github.com/XiaoliangJiang/strcode/blob/semantics/demos/example%20RDF%20diagram.png" width="300px" />

If you want to add new associations into existing file, you need to check whether it exists in user-defined AssociationNames.txt file. For example, now we want to let `YourProcess02` be a sub-process of `YourWorkflow`. First, you need to check whether `provone:hasSubProcess` is in AssociationNames.txt. You can rename the DefaultAssociationNames.txt file which automatically generated in your current working directory as AssociationNames.txt. We could find this association in this file apparently. Then, you can add a new argument `UserANM=TRUE` into `sum_str` function, and use manually input bar in UI to type in `provone:hasSubProcess=ProcessID02` and regenerate YourWorkflow entity like this:
```r
#       ________________________________________________________________________
#       YourWorkflow {WorkID provone:Workflow provone:hasSubProcess=ProcessID02}####
```
Notice that using ID as value here, but not title name.
With `sum_str(rdf="ttl", UserAL=TRUE, graph=TRUE, UserANM=TRUE)`, you can generate a new output file with `provone:hasSubProcess user:ProcessID02`, and a new RDF diagram as follows:

<img src="https://github.com/XiaoliangJiang/strcode/blob/semantics/demos/new%20RDF%20diagram.png" width="300px" />

If you want to define your domain, you need to add more arguments. For example, with `sum_str(rdf="ttl", UserAL=TRUE, graph=TRUE, UserANM=TRUE,prefix="test",baseURI = "http://testwebsite/",UserID ="testUser")`, you can generate a new output file as follows:
```
 @prefix test:    <http://testwebsite/testUser/> .
 @prefix provone: <http://dataone.org/ns/provone#> . 
 
 test:WorkID a provone:Workflow ;
  	 rdfs:label "YourWorkflow" ; 
 	 provone:hasSubProcess test:ProcessID02 ; 
 	 provone:hasSubProcess test:ProcessID . 
 
 test:ProcessID a provone:Process ; 
 	 rdfs:label "YourProcess" ; 
  	 provone:hasSubProcess test:ProcessID02 . 
 
 test:ProcessID02 a provone:Process ; 
 	 rdfs:label "YourProcess02" ; 
  	 provone:wasDerivedFrom test:ProcessID . 
```
Notice that all prefixes are set to test, and your user-defined domain appears in the first line.

Now we could use a part of a real example to generate a sample output file. Notice that all `%20` below means space which automatically generated by the function if you type spaces in your title or manually input bar. They will return as spaces in output file and RDF diagram.
You can try to use these code as your input breaks:
``` r
#       ________________________________________________________________________
#       CPP_Workflow {workflow_cpp provone:Workflow skos:altLabel="CPP%20Workflow:%20Workflow%20top%20level"}####

##      ........................................................................
##      Recipe%20for%20CPP%20Data {gates_recipe prov:Plan skos:altLabel="Recipe%20document:%20Documentation%20on%20Recipe%20for%20CPP%20Data" rdfs:seeAlso=<http://bit.ly/cwf_recipe_fall2015> rdfs:seeAlso=<http://bit.ly/cpp_workflow_jun2016> cwfo:hasCode="MATLAB:%20TensorExplorationPARAFAC.m,%20YenerTensor.m"}####

##      ........................................................................
##      Data%20Gathering {data_gathering  provone:Program skos:altLabel="Workflow%20Phase%200"}####

###     .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ...
###     Load%20Test%20Dataset {load_test_dataset provone:Program skos:altLabel="ANTHAyenerfinalmine.xlsx" rdfs:comment="Loading%20of%20initial%20matrix%20(output%20of%20YenerTensor.m)"}####

####    ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ....
####    Test%20Dataset%20(TENSOR) {test_dataset_data provone:Data rdfs:comment="Test%20Dataset" cwfo:hasCode="MATLAB:%20YenerTensor.m"}####

##      ........................................................................
##      Data%20Processing {data_processing provone:Program skos:altLabel="Workflow%20Phase%201"}####

###     .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ...
###     Adjust%20APGAR%20Scores {adjust_apgar_scores  provone:Program skos:altLabel="Ensure%20APGAR%20scores%20are%20between%200-10" rdfs:comment="MATLAB%20code:%20YenerTensor.m,%20Section%201" dcterms:conformsTo=<https://www.nlm.nih.gov/medlineplus/ency/article/003402.htm> cwfo:hasInData=test_dataset_data}####

####    ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ....
####    Adjusted%20APGAR%20Data {adjusted_apgar_data provone:Data rdfs:comment="Adjusted%20APGAR%20Data"}####

###     .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ...
###     Choose%20Subjects {choose_subjects provone:Program skos:altLabel="Choose%20only%20subjects%20with%20all%20five%20time%20points" rdfs:comment="MATLAB%20code:%20YenerTensor.m,%20Section%201" dcterms:conformsTo=<https://www.nlm.nih.gov/medlineplus/ency/article/003402.htm> cwfo:hasInData=adjusted_apgar_data cwfo:hasOutData=growth_data}####
```
Then download `AssociationNames.txt` and `AssociationLibrary.txt` in `demos` forder, and paste them into your current working directory. With `sum_str(rdf="ttl",graph=TRUE,UserAL=FALSE,UserANM=FALSE,prefix="cwf",baseURI = "http://cwf.tw.rpi.edu/",UserID ="data#" )`, you could generate a output file as follows:
```
 @prefix test:    <http://testwebsite/testUser/> .
 @prefix provone: <http://dataone.org/ns/provone#> . 
 @prefix skos:    <http://www.w3.org/2004/02/skos/core#> . 
 @prefix prov:    <http://www.w3.org/ns/prov#> . 
 @prefix rdfs:    <http://www.w3.org/2000/01/rdf-schema#> . 
 @prefix cwfo:    <http://cwf.tw.rpi.edu/vocab#> . 
 @prefix dcterms: <http://purl.org/dc/terms/> . 
 
 test:workflow_cpp a provone:Workflow ;
 	 skos:altLabel "CPP Workflow: Workflow top level" ; 
 	 rdfs:label "CPP_Workflow" ; 
 	 prov:hadPlan test:gates_recipe ; 
 	 provone:hasSubProgram test:data_gathering ; 
 	 provone:hasSubProgram test:data_processing . 
 
 test:gates_recipe a prov:Plan ;
 	 skos:altLabel "Recipe document: Documentation on Recipe for CPP Data" ; 
 	 rdfs:seeAlso <http://bit.ly/cwf_recipe_fall2015> ; 
 	 rdfs:seeAlso <http://bit.ly/cpp_workflow_jun2016> ; 
 	 cwfo:hasCode "MATLAB: TensorExplorationPARAFAC.m, YenerTensor.m" ; 
 	 rdfs:label "Recipe for CPP Data" ; 
 
 test:data_gathering a provone:Program ;
 	 skos:altLabel "Workflow Phase 0" ; 
 	 rdfs:label "Data Gathering" ; 
 	 provone:hasSubProgram test:load_test_dataset . 
 
 test:load_test_dataset a provone:Program ;
 	 skos:altLabel "ANTHAyenerfinalmine.xlsx" ; 
 	 rdfs:comment "Loading of initial matrix (output of YenerTensor.m)" ; 
 	 rdfs:label "Load Test Dataset" ; 
 	 cwfo:hasOutData test:test_dataset_data . 
 
 test:test_dataset_data a provone:Data ;
 	 rdfs:comment "Test Dataset" ; 
 	 cwfo:hasCode "MATLAB: YenerTensor.m" ; 
 	 rdfs:label "Test Dataset (TENSOR)" ; 
 
 test:data_processing a provone:Program ;
 	 skos:altLabel "Workflow Phase 1" ; 
 	 rdfs:label "Data Processing" ; 
 	 provone:hasSubProgram test:adjust_apgar_scores ; 
 	 provone:hasSubProgram test:choose_subjects . 
```

The code generates a graph representation of the resulting RDF such as this:

<img src="https://github.com/XiaoliangJiang/strcode/blob/semantics/demos/final%20example.png" width="350px" />

