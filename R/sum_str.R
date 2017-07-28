#' Summarize the code structure
#'
#' Create a summary of one or multiple code files based on the section
#' separators and their titles.
#' @param path_in Either a path to a directory or to a single file. If it is
#'   a directory path, all files in that directory will be summarised. If it
#'   is a single file path, only the resepective file will be summarised.
#'   The default value uses the RStudio API to produce a summary of content from
#'   the source editor. This requires that the file is saved before
#'   \code{sum_str} is called.
#' @param file_in_extension If \code{file_in} is \code{NULL}, all files with the
#'   \code{file_in_extension} are considered, defaults to ".R".
#' @param dir_out The directory to print the output to. "" implies the console.
#' @param file_out A connection or character string naming the file to print to.
#'   If set to \code{NULL}, the name will be \code{paste0("code-summary_", file_in)}.
#'   If \code{dir_out} is set to "", \code{file_out} can be set to
#'   "object" and the output of the function will be returned as an
#'   object instead of just printed to the console with \code{cat}. This is
#'   useful if output should be assigned to an object. If not set to "object",
#'   \code{cat} will be used.
#' @param file_out_extension A file extension for the file to be created.
#' @param rm_rh_hashes Boolean value indicating whether or not to remove
#'   righthand hashes in section titles for the summary
#'   (see section Removing spaces and hashes).
#' @param rm_rh_spaces Boolean value indicating whether or not to remove
#'   righthand spaces in section titles for the summary
#'   (see section Removing spaces and hashes).
#' @param rm_break_anchors Boolean value indicating whether or not the anchors
#'   inserted in code separators should be removed for the summary.
#' @param width The character width of the output. If NULL, it is set to the
#'   length of the longest separator title.
#' @param line_nr A boolean value that indicates whether the line numbers should
#'   be printed along with the structure summary.
#' @param granularity Indicates the lowest level of granularity that should be
#'   included in the summary.
#' @param last_sep A boolean value indicating whether or not the separating
#'   lines of the highest granularity should be printed.
#' @param title A boolean value indicating whether the reported summary should
#'   contain a title or not.
#' @param header A boolean value indicating whether a column header should
#'   indicate the name of the columns (line, level, section).
#' @param ... futher arguments to be passed from and to other methods, in
#'   particular \code{\link{list.files}} for reading in multiple files.
#' @details To create the summary, \code{sum_str} uses regular expressions.
#'   Hence it is crucial that the code separators and the separator titles
#'   match the regular expression pattern. We recommend inserting
#'   separators and their titles using the RStudio Add-in that is contained
#'   in this package. The definition is rather intuitive as can be seen in the
#'   example section below. However, we shall provide a formal definition here
#'   as well.
#'   \itemize{
#'     \item A code separator is defined as a line that starts with n hashes,
#'     followed by 4-n spaces where 0 < n < 4. This sequence is followed by one
#'     or more either \code{.} or \code{_}.
#'     \item A title associated with a code separator is defined as a line
#'     that starts with n hashes, followed by 4-n spaces where 0 < n < 4. This
#'     sequence is \emph{not} followed by \code{.} or \code{_}.
#'   }
#'   Lines that do not satisfy these requirements (e.g. do not start with #s,
#'   do not contain the right number of spaces after the #, indent before any #
#'   ect.) are not considered by \code{sum_str}.
#' @section Removing spaces and hashes:
#'   The add-in contained in this package inserts section titles in a way that
#'     that they are recognised by RStudio as sections (for details, see
#'     \href{https://support.rstudio.com/hc/en-us/articles/200484568-Code-Folding-and-Sections}{RStudio's official website}. One structure that is
#'     recognised by RStudio as section is a line starting with a hash and ending
#'     with four hashes. This structure is implemented with \code{strcode}.
#'     Hence when creating the summary, it might be desired to remove the right
#'     hand hashes and spaces, which can be specified with the respective options
#'     \code{rm_rh_hashes} and \code{rm_rh_spaces}.
#' @seealso insert_l_break
#' @examples
#' # the following separator is an example of a valid
#' # separator and associated title
#'
#' #   __________________________________________________
#' #   this is a level 1 title                     ####
#' ##  . . . . . . . . . . . . . . . . . . . . . . . . .
#' ##  note that the title or the separator character (_, .)
#' ##  always starts at indention 4.
#'
#' \dontrun{
#' # Open a new .R file in RStudio, insert some code breaks
#' # using the Add-in of this package, save the file and run:
#' sum_str() # get a summary of the source editor.
#' }
#' @importFrom rstudioapi getSourceEditorContext
#' @export
#'
#   ____________________________________________________________________________
#   user function                                                           ####
sum_str <- function(path_in = getSourceEditorContext()$path,
                    file_in_extension = ".R",
                    dir_out = "",
                    file_out = NULL,
                    file_out_extension = "",
                    width = NULL,
                    rm_rh_hashes = TRUE,
                    rm_rh_spaces = TRUE,
                    rm_break_anchors = TRUE,
                    line_nr = TRUE,
                    granularity = 3,
                    last_sep = FALSE,
                    title = TRUE,
                    header = TRUE,
                    rdf = FALSE,
                    graph=FALSE,
                    domain=FALSE,
                    baseURI="http://example.org/base/",
                    UserID="UserID",
                    prefix="user",
                    UserAL=FALSE,
                    fillAssociation=FALSE,
                    ...) {

##  ............................................................................
##  assertive tests                                                         ####
assert_number(granularity, lower = 1, upper = 3)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### prepare input to call helper repeated times                             ####
  # check if file can be directory or path
  if (is.na(file.info(path_in)$isdir)) {
    stop("Indicated path (", path_in, ") is neither a directory nor a valid file name")
  }
  # create files if path_in is directory
  else if(file.info(path_in)$isdir) {
    all_files <- list.files(path = path_in,
                            pattern = paste0(file_in_extension, "$"),
                            full.names = FALSE)
    # files contain path name
    all_files <- paste(path_in, all_files, sep = "/")
    if (length(all_files) == 0) {
      warning("there are no files in the directory")
    }

  # in the case path_in is already a file
  } else {
    all_files <- path_in
  }

  # if output is not printed in the console, print a short summary.
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### call helper                                                             ####
  if (dir_out != "") {
    cat("The following files were summarized \n")
  }
  output <- lapply(all_files, function(g) {
    # pass all arguments as is except the path_in
    if (dir_out != "") {
      cat(g, sep = " \n")
    }
    sum_str_helper(path_in = g,
                   dir_out = dir_out,
                   file_out = file_out,
                   file_out_extension = file_out_extension,
                   width = width,
                   rm_rh_hashes = rm_rh_hashes,
                   rm_rh_spaces = rm_rh_spaces,
                   rm_break_anchors = rm_break_anchors,
                   line_nr = line_nr,
                   granularity = granularity,
                   last_sep = last_sep,
                   title = title,
                   header = header,
                   rdf=rdf,
                   graph=graph,
                   domain=domain,
                   baseURI=baseURI,
                   UserID=UserID,
                   prefix=prefix,
                   UserAL=UserAL,
                   fillAssociation=fillAssociation)
  })

  if (dir_out == "" && !is.null(file_out) && file_out == "object") {
    output
  } else {
    invisible() # avoid unnecessary NULL return
  }
}
#' helper function for code summarisation
#   ____________________________________________________________________________
#   helper function: sum_str_helper                                         ####
#' return code summary for one file
#'
#' Function is called by \code{sum_str()} and returns summary of one code file.
#' @inheritParams sum_str
#' @details The core of the function is described best as follows: after a file
#' was read in and stored in a vector *lines* whereas each element describes a
#' line of code, the candidate lines (in the sense that they might be contained
#' in the summary) were evaluated, their indices stored in *cand*. Next,
#' various regex patterns are matched against *lines*. Then,
#' after all tests were executed, the variable *remove* contains all indices
#' that should be removed from *lines* before it is returned as the summary of
#' the code file. Hence, applying \code{setdiff(cand, remove)} contains
#' the subset of *lines* that we finally want to output.
#' @keywords internal
#' @import checkmate
sum_str_helper <- function(path_in,
                           dir_out,
                           file_out,
                           file_out_extension,
                           rm_rh_hashes,
                           rm_rh_spaces,
                           rm_break_anchors,
                           width,
                           line_nr,
                           granularity,
                           last_sep,
                           title,
                           header,
                           rdf,
                           graph,
                           domain,
                           baseURI,
                           UserID,
                           prefix,
                           UserAL,
                           fillAssociation) {

##  ............................................................................
##  argument interaction                                                    ####
### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### get the file_out together
  if (is.null(file_out)) {
    # file_out must be a file name. Grap the last expression after backslash.
    backslash_rm <- gsub("^.*/(.*)", "\\1", path_in, perl = TRUE)
    file_out <- paste0("code_summary-",
                       gsub("^(.*)\\..*$", "\\1", backslash_rm, perl = TRUE),
                       file_out_extension)
  }

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### paths

  # path_out
  ## path_out is "" if dir_out is ""
  if (dir_out == "") {
    path_out <- ""
  # otherwise it is composed of dir_out and file_out, if file_out
  # has a not empty value
  } else {
    path_out <- paste(dir_out, file_out, sep = "/")
  }

##  ............................................................................
##  get pattern

  lines <- readLines(con = path_in)
  sub_pattern <- "^#       [^ ]|^##      [^ ]|^###     [^ ]|^####    [^ ]|^#####   [^ ]|^######  [^ ]|^####### [^ ]"
  cand <- grep(sub_pattern, lines, perl = FALSE) # extract candiates id
  if (length(cand) == 0) {
    return(warning("No line matching the required pattern",
                   call. = FALSE, immediate. = TRUE))
  }

##  .................. #< 3b5746a13447c5269736b631d6a9370d ># ..................
##  replace hashed seps                                                     ####
if (rm_break_anchors) {
  # extract candidates for replacement
  hash_candid <- intersect(grep("(\\s#<\\s[0-9a-z]{1,33}\\s>#\\s)", lines, perl = TRUE),
                           cand)
  # get their level
  lvl <- nchar(gsub("^(#+)\\s.*$", "\\1", lines[hash_candid], perl = TRUE))
  replacement <- vapply(lvl, function(x) help_create_break(start = paste0(rep("#", x), collapse = ""),
                                           break_char = give_breakchar(x),
                                           sep = paste(rep(" ", 8 - x), collapse = ""), anchor_in_sep = FALSE),
                  FUN.VALUE = character(1))
  lines[hash_candid] <- replacement

}
##  ............................................................................
##  modify pattern according to arguments
### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### getting the granularity right
  remove <- c()
  if (granularity < 3) { # if there are any lines to remove
    hashes <- (granularity + 1):3
    spaces <-  8- hashes

    # this variable stores the indices of all lines that should be dropped.
    for (i in 1:length(hashes)) {
      sub_pattern <- paste0("^#{", hashes[i], "}\\s{", spaces[i], "}.*$")
      remove <- append(remove, grep(sub_pattern, lines, perl = TRUE))
    }
  }

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### remove last separator
  if (last_sep == FALSE) {
    hashes <- min(find_gran("down", lines = lines), granularity)
    spaces <- 8 - hashes
    sub_pattern <- paste0("^#{", hashes, "}\\s{", spaces, "}[\\._].*$")
    remove <- append(remove, grep(sub_pattern, lines, perl = TRUE))
  }

##  ............................................................................
##  select elements that "survived all tests"
  tokeep <- setdiff(cand, remove)
  lines <- lines[tokeep]

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### width adjust line_nr, title, output path, header

  # only continue if there is a valid pattern
  if (length(lines) == 0) {
    return(warning("No line matching the required pattern",
                   call. = FALSE, immediate. = TRUE))
  }

  # issue warning if there are no titles
  pattern_titles <- grep("^(#   |##  |### )[^\\._)].*$", lines, value = TRUE)
  if (length(pattern_titles) == 0) { # if there were no titles
    warning("There are no segment titles.",
            call. = FALSE, immediate. = TRUE)
  }

  # remove right hand hashes if desired
  if (rm_rh_hashes) {
    lines <- gsub("####$", "    ", lines, perl = TRUE)
  }

  # remove right hand spaces if desired
  if (rm_rh_spaces) {
    lines <- gsub("\\s*$", "", lines, perl = TRUE)
  }

  # adjust length of pattern.
  if (is.null(width)) {
  # first calculate width. It is the length of the maximal title string
  ## get the title strings
    if (length(pattern_titles) == 0) { # if there were no titles
      width <- options()$strcode$char_length
      warning("width set to options()$strcode.char.length",
              call. = FALSE, immediate. = TRUE)
    } else { # if there were titles
      width <- max(nchar(pattern_titles))
    }
  }

  lines <- substring(lines, 1, width)

  if (line_nr == TRUE) {
    lines <- paste(tokeep, lines, sep = "\t")
  }

  if (header == TRUE) {
    lines <- append(c("line  level section"), lines)
  }
  if (title == TRUE) {
    lines <- append(paste0("Summarized structure of ", path_in, "\n"), lines)
  }

##  ............................................................................
##  output the pattern
 if (rdf=="ttl"|graph){
    datetime <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
    fileformat=".ttl" #".txt"
    outputfile2 <- paste("RDF_output_file_",datetime,fileformat,sep="")
    write(lines,file=outputfile2)
    templines=readLines(outputfile2)
lines_content=templines[4:length(templines)]
lines_split=strsplit(lines_content, " ")

baseURI=baseURI
UserID=UserID
FullURI=paste0(baseURI,UserID,"/")
prefix=prefix

schemalist=list()

schemas=c(rdfs="@prefix rdfs:    <http://www.w3.org/2000/01/rdf-schema#> .",
          xsd="@prefix xsd:     <http://www.w3.org/2001/XMLSchema#> .",
          owl="@prefix owl:     <http://www.w3.org/2002/07/owl#> .",
          dcterms="@prefix dcterms: <http://purl.org/dc/terms/> .",
          prov="@prefix prov:    <http://www.w3.org/ns/prov#> .",
          wfms="@prefix wfms:    <http://www.wfms.org/registry.xsd> .",
          rdf="@prefix rdf:       <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .",
          provone="@prefix provone: <http://dataone.org/ns/provone#> .",
          skos="@prefix skos:    <http://www.w3.org/2004/02/skos/core#> .",
          yw="@prefix yw:   	 <http://yesworkflow.org/ns/yesworkflow#> .",
          cwfo="@prefix cwfo:    <http://cwf.tw.rpi.edu/vocab#> .",
          cwf="@prefix cwf:     <http://cwf.tw.rpi.edu/data#> .")
# Using ":" to find class
for (i in 1:length(lines_split)){
  schemalist[[i]]=grep(":",lines_split[[i]])
}
tempcount0=0
schemalist1=list()
for (i in 1:length(schemalist)){
  if (length(schemalist[[i]])>0){
    tempcount0=tempcount0+1
    schemalist1[[tempcount0]]=schemalist[[i]]
  }
}
schemalist1

tempcount0=0
lines_split1=list()
for (i in 1:length(schemalist)){
  if (length(schemalist[[i]])>0){
    tempcount0=tempcount0+1
    lines_split1[[tempcount0]]=lines_split[[i]]
  }
}
lines_split1

lines_split=lines_split1
schemalist=schemalist1
lines_split1=schemalist
for (i in 1:length(lines_split)){
  tempcount0=0
  for (j in 1:length(lines_split[[i]])){
    if (nchar(lines_split[[i]][j])>0){
      tempcount0=tempcount0+1
      lines_split1[[i]][tempcount0]=lines_split[[i]][j]
    }
  }
}
lines_split1

for (i in 1:length(lines_split1)){
  schemalist[[i]]=grep(":",lines_split1[[i]])
}
schemalist
lines_split=lines_split1

infolist=lines_split
for (i in 1:length(infolist)){
  templevel=strsplit(infolist[[i]][1],"\\t")
  infolist[[i]][1]=nchar(templevel[[1]][2])
  #infodf$level[i]=lines_split[[i]][1]
}
for (i in 1:length(infolist)){
  infolist[[i]][3]=gsub("\\{","",infolist[[i]][3])
  infolist[[i]][3]=gsub("\\#","",infolist[[i]][3])
  #infodf$level[i]=lines_split[[i]][1]
}
for (i in 1:length(infolist)){
  infolist[[i]][length(infolist[[i]])]=gsub("\\}","",infolist[[i]][length(infolist[[i]])])
  infolist[[i]][length(infolist[[i]])]=gsub("\\.","",infolist[[i]][length(infolist[[i]])])
  #infodf$level[i]=lines_split[[i]][1]
}
infolist

infolist1=infolist
for (i in grep("\":",infolist)){
  infolist[[i]][2]=gsub(",","",infolist1[[i]][4])
  infolist[[i]][3]=gsub("#","",gsub(",","",infolist1[[i]][2]))
  infolist[[i]][4]=gsub("\\.","",gsub(",","",infolist1[[i]][3]))
}
infolist

#strsplit(infolist[[4]][2], "\":")[[1]][2]
for (j in 1:length(infolist)){
#for (j in 4:4){
  for (i in 1:length(strsplit(infolist[[j]], "\":"))){
    if (length(strsplit(infolist[[j]], "\":")[[i]])>1){
      infolist1[[j]][i]=strsplit(infolist[[j]], "\":")[[i]][2]
  }
}
}
infolist=infolist1
 
for (i in 1:length(infolist)){
  for (j in 1:length(infolist[[i]])){
  infolist[[i]][j]=return_space(infolist[[i]][j])
  }
}
   
schemahad=0
lines_rdf=""
count0=1
    
schemalist1=list()
for (i in 1:length(infolist)){
  schemalist1[[i]]=grep(":",infolist[[i]])
}
schemalist=schemalist1
# add prefix
for (i in 1:length(schemalist)){
  #print (i)
  for (j in 1:length(schemalist[[i]])){
    #print (j)
    tempstr=infolist[[i]][(schemalist[[i]])[j]]
    #print(tempstr)
    tempschemastr=gsub("\\.","",strsplit(tempstr,'\\:')[[1]][1])
    schemas[tempschemastr]
    if (tempschemastr %in% schemahad) {#print("!")
    }
    else{schemahad[count0]=tempschemastr
    count0=count0+1}
  }
}
# adding headers:
lines_rdf=paste0(" @prefix ",prefix,":    ","<",FullURI,"> .\n")
for (i in 1:length(schemahad)){
  lines_rdf=paste(lines_rdf,schemas[schemahad[i]],"\n")
}

# RDF word list:
ProvONElist=c("provone:Process","provone:InputPort","provone:OutputPort",
              "provone:DataLink","provone:SeqCtrlLink","provone:Workflow",
              "provone:User","provone:ProcessExec","provone:Data",
              "provone:Collection","provone:Visualization","provone:Program")
# Association word list:
Associationlist=c("provone:hasSubProcess","provone:sourcePToCL","provone:CLtoDestP",
                  "provone:hasInPort","provone:hasOutPort","provone:hasDefaultParam",
                  "provone:DLToInPort","provone:outPortToDL","provone:inPortToDL",
                  "provone:DLToOutPort","provone:wasAttributedTo","provone:wasDerivedFrom",
                  "provone:dataOnLink","provone:used","provone:wasGeneratedBy",
                  "provone:wasAssociatedWith","provone:wasInformedBy","provone:isPartOf",
                  "provone:hadMember")
# Association library:
DefaultAL=paste0("ParentClass,","ChildClass,","Ways,","Property,","ReverseProperty\n",
          "\"provone:Process\",","\"provone:Process\",","2,","\"provone:hasSubProcess\",","\"provone:wasDerivedFrom\"\n",
          "\"provone:Process\",","\"provone:Data\",","2,","\"provone:wasDerivedFrom\",","\"provone:hasMember\"\n",
          "\"provone:Process\",","\"provone:Visualization\",","2,","\"provone:wasDerivedFrom\",","\"provone:hasMember\"\n")
write(DefaultAL,file="DefaultAssociationLibrary.txt")

nodesnames=nodesclasses=nodesfrom=nodesto=nodesproperty=parentclass=property=line_rdf_vector=""
templevel=parentlevel=parentindex=0
levelvector=rep(0,7)

for (j in 1:length(infolist)){
  line_rdf=""
  title0=infolist[[j]][2]
  ID=infolist[[j]][3]
  parentlevel=templevel
  templevel=infolist[[j]][1]
  tempclass=infolist[[j]][4]
  if (infolist[[j]][1]==1){
    if (levelvector[1]==0){
      levelvector[1]=j
    }
  }
  if (infolist[[j]][1]==2){
    if (levelvector[2]==0){
      levelvector[2]=j
    }
  }
  if (infolist[[j]][1]==3){
    if (levelvector[3]==0){
      levelvector[3]=j
    }
  }
  if (infolist[[j]][1]==4){
    if (levelvector[4]==0){
      levelvector[4]=j
    }
  }
  if (infolist[[j]][1]==5){
    if (levelvector[5]==0){
      levelvector[5]=j
    }
  }
  if (infolist[[j]][1]==6){
    if (levelvector[6]==0){
      levelvector[6]=j
    }
  }
  if (infolist[[j]][1]==7){
    if (levelvector[7]==0){
      levelvector[7]=j
    }
  }
  if (as.numeric(parentlevel)!=0){
  if (as.numeric(templevel)>as.numeric(parentlevel)){
    parentindex=j-1
    parentclass=infolist[[j-1]][4]
  }
  else if (templevel==parentlevel){
    parentindex=levelvector[as.numeric(templevel)-1]
    parentclass=infolist[[as.numeric(parentindex)]][4]
  }
  else {
    levelvector[templevel]=j
    parentindex=levelvector[as.numeric(templevel)-1]
  }
  }
  # judge association:
  if (UserAL==FALSE){
    AssociationsLib=read.table("DefaultAssociationLibrary.txt",sep=",",header=TRUE)
  }
  else if (UserAL==TRUE) {
    AssociationsLib=read.table("AssociationsLibrary.txt",sep=",",header=TRUE)
  }

  AssociationNUM=which(tempclass==AssociationsLib$ChildClass[which(parentclass==AssociationsLib$ParentClass)])
  print ("fillAssociation")
  print (fillAssociation=TRUE)
  print ("as.numeric")
  print (as.numeric(parentlevel)!=0)
  print ("total")
  print ((fillAssociation=TRUE)&(as.numeric(parentlevel)!=0))
  if (length(AssociationNUM)>0){
    property=as.character(AssociationsLib$Property[AssociationNUM])
    if (AssociationsLib$Ways[AssociationNUM]==2){
      nodesfrom=paste0(nodesfrom,infolist[[as.numeric(parentindex)]][2]," ")
      nodesto=paste0(nodesto,infolist[[j]][2]," ")
      nodesproperty=paste0(nodesproperty,property," ")
      nodesfrom=paste0(nodesfrom,infolist[[j]][2]," ")
      nodesto=paste0(nodesto,infolist[[as.numeric(parentindex)]][2]," ")
      nodesproperty=paste0(nodesproperty,AssociationsLib$ReverseProperty," ")
    }
  }
  else if ((fillAssociation=TRUE)&(as.numeric(parentlevel)!=0)){
    property="str:has"
    nodesfrom=paste0(nodesfrom,infolist[[as.numeric(parentindex)]][2]," ")
    nodesto=paste0(nodesto,infolist[[j]][2]," ")
    nodesproperty=paste0(nodesproperty,property," ")
    nodesfrom=paste0(nodesfrom,infolist[[j]][2]," ")
    nodesto=paste0(nodesto,infolist[[as.numeric(parentindex)]][2]," ")
    nodesproperty=paste0(nodesproperty,"str:belongTo"," ")
  }
    
  #if (parentclass=="provone:Process"&tempclass=="provone:Process"){
  #  property="provone:hasSubProcess"
  #}
  #else if (parentclass=="provone:Process"&(tempclass=="provone:Data"|tempclass=="provone:Visualization")){
  #  property="provone:wasDerivedFrom"
  #}
  #else if (as.numeric(parentlevel)!=0){
  #  property="str:has"
  #}
  
  #if (property=="provone:hasSubProcess"){
  #  nodesfrom=paste0(nodesfrom,infolist[[as.numeric(parentindex)]][2]," ")
  #  nodesto=paste0(nodesto,infolist[[j]][2]," ")
  #  nodesproperty=paste0(nodesproperty,property," ")
  #  nodesfrom=paste0(nodesfrom,infolist[[j]][2]," ")
  #  nodesto=paste0(nodesto,infolist[[as.numeric(parentindex)]][2]," ")
  #  nodesproperty=paste0(nodesproperty,"provone:wasDerivedFrom"," ")
  #}
  #else if(property=="provone:wasDerivedFrom"){
  #  nodesfrom=paste0(nodesfrom,infolist[[as.numeric(parentindex)]][2]," ")
  #  nodesto=paste0(nodesto,infolist[[j]][2]," ")
  #  nodesproperty=paste0(nodesproperty,property," ")
  #  nodesfrom=paste0(nodesfrom,infolist[[j]][2]," ")
  #  nodesto=paste0(nodesto,infolist[[as.numeric(parentindex)]][2]," ")
  #  nodesproperty=paste0(nodesproperty,"provone:hasMember"," ")
  #}
  #else if (as.numeric(parentlevel)!=0){
  #  nodesfrom=paste0(nodesfrom,infolist[[as.numeric(parentindex)]][2]," ")
  #  nodesto=paste0(nodesto,infolist[[j]][2]," ")
  #  nodesproperty=paste0(nodesproperty,property," ")
  #  nodesfrom=paste0(nodesfrom,infolist[[j]][2]," ")
  #  nodesto=paste0(nodesto,infolist[[as.numeric(parentindex)]][2]," ")
  #  nodesproperty=paste0(nodesproperty,"str:belongTo"," ")
  #}  

  for (i in 4:length(infolist[[j]])){
    tempword=""
    tempentity=""
    temp_line=""
    if (i==4){
      tempword=infolist[[j]][4]
      nodesnames=paste0(nodesnames,title0," ")
      nodesclasses=paste0(nodesclasses,tempword," ")
      
      #entityname=paste0(FullURI,ID)  
      #title=paste0("<",entityname,">")
      entityname=paste0(prefix,":",ID)  
      title=paste0(entityname)
      line_rdf=paste("\n",title,"a",tempword)
      
      if (i==length(infolist[[j]])){
        line_rdf=paste(line_rdf,";","\n")
        #line_rdf=paste(line_rdf,"\t","rdfs:label",title0,".","\n")
        title0=paste0("\"",title0,"\"")
        line_rdf=paste(line_rdf,"\t","rdfs:label",title0,";","\n")#,".","\n")
      }
      else{
        line_rdf=paste(line_rdf,";","\n")
      }
      
    }# out of if i==4
    else { # i>4
      tempword=infolist[[j]][i]
      # old association
      if (grepl("=",tempword)){
        tempwordlist=strsplit(tempword,"=")
        #tempentity=paste0("<",tempwordlist[[1]][2],">")
        
        if (tempwordlist[[1]][1] %in% Associationlist)
        { 
          nodesfrom=paste0(nodesfrom,title0," ")
          nodesto=paste0(nodesto,tempwordlist[[1]][2]," ")
          nodesproperty=paste0(nodesproperty,tempwordlist[[1]][1]," ")
        }
        else{
        temp_line=paste(tempwordlist[[1]][1],tempwordlist[[1]][2])
        }
      }
      #end session
      
      if (i==length(infolist[[j]])){
        if (nchar(temp_line)>0){
        temp_line=paste("\t",temp_line,";","\n")
          }
        #temp_line=paste(temp_line,"\t","rdfs:label",title0,".","\n")
        title0=paste0("\"",title0,"\"")
        temp_line=paste(temp_line,"\t","rdfs:label",title0,";","\n")#,".","\n")
      }
      else {
        if (nchar(temp_line)>0){
        temp_line=paste("\t",temp_line,";","\n")
          }
      }
      
    }
    line_rdf=paste(line_rdf,temp_line)
    line_rdf_vector[j]=line_rdf
  }
  #lines_rdf=paste(lines_rdf,line_rdf)
}

library(igraph)
nodesnames2=strsplit(nodesnames," ")
nodesclasses2=strsplit(nodesclasses," ")
nodes <- data.frame(name = nodesnames2[[1]],
                    class = nodesclasses2[[1]])
#print (nodes)    
nodesfrom2=strsplit(nodesfrom," ")
nodesto2=strsplit(nodesto," ")
nodesproperty2=strsplit(nodesproperty," ")
#nodesdf=data.frame(from=nodesfrom2[[1]],to=nodesto2[[1]],property=nodesproperty2[[1]])
exceptnum=except=nodesfrom3=nodesto3=nodesproperty3=nodesnm=0
exceptwords=c("str:has","str:belongTo")
#diagonal matrix
for (i in 1:(length(nodesfrom2[[1]])-1)){
  #print (i)
  for (j in ((i+1):length(nodesfrom2[[1]]))){
    #print (j)
    if (i!=j){
      if((nodesfrom2[[1]][i]==nodesfrom2[[1]][j])&(nodesto2[[1]][i]==nodesto2[[1]][j])){
        exceptnum=exceptnum+1
        if (nodesproperty2[[1]][i] %in% exceptwords){
          except[exceptnum]=i
        }
        else { except[exceptnum]=j}
      }
    }
  }
}
#print (nodesfrom2)
#print (nodesto2)
#print (nodesproperty2)
#print (except) 
for (i in 1:length(nodesfrom2[[1]])){
  if (i %in% except){}
  else {
    nodesnm=nodesnm+1
    nodesfrom3[nodesnm]=nodesfrom2[[1]][i]
    nodesto3[nodesnm]=nodesto2[[1]][i]
    nodesproperty3[nodesnm]=nodesproperty2[[1]][i]
    }
}
    

nesting <- data.frame(from = nodesfrom3,
                      to = nodesto3,
                      property = nodesproperty3)
   
#print (nesting)
g3 <- graph_from_data_frame(nesting, directed=TRUE, vertices=nodes)
E(g3)$label <- E(g3)$property

titles=IDs=0
for (i in 1:length(infolist)){
  titles[i]=infolist[[i]][2]
  IDs[i]=infolist[[i]][3]
}

for (i in 1:length(line_rdf_vector)){
  tempnumber=which(nesting$from==titles[i])
  for (j in 1:length(tempnumber)){
    #entityname2=paste0("<",FullURI,IDs[which(titles==nesting$to[tempnumber[j]])],">")
    entityname2=paste0(prefix,":",IDs[which(titles==nesting$to[tempnumber[j]])])
    if (j==length(tempnumber)){
      line_rdf_vector[i]=paste(line_rdf_vector[i],"\t",nesting$property[tempnumber[j]],entityname2,".","\n")}
    else{line_rdf_vector[i]=paste(line_rdf_vector[i],"\t",nesting$property[tempnumber[j]],entityname2,";","\n")}
    
  }
}

for (i in 1:length(line_rdf_vector)){
  lines_rdf=paste(lines_rdf,line_rdf_vector[i])
}

if (rdf=="ttl"){
  write(lines_rdf,file=outputfile2)
print("Create a RDF file successfully. Please find the output file in:")
print(getwd())
print(paste("Your file name is:",outputfile2))
}
if (graph){
print(g3, e=TRUE, v=TRUE)
plot(g3, edge.arrow.size=.2, edge.curved=.4)
  }
    

  } 
  # original below (delet else):
  else if (dir_out == "" && file_out == "object") {
    lines
  } else {
    cat(lines, file = path_out, sep = "\n")
  }
}

#   ____________________________________________________________________________
#   helper function: find_gran
# find maximal or minimal level of granularity used.
#' Find out granularity of a string vector
#'
#' This helper function takes a string (vector) as an input and searches for the
#' highest or lowest granularity. Granularity is defined in terms of a hash/
#' space sequence (see \code{\link{sum_str}}). The search is implemented using
#' a recursive approach.
#' @param direction either "up" or "down". Down means the algorithm starts with
#' the pattern containing a certain number of hashes (specified in
#' \code{highest}) and searches for a regex match. If it can't find one, it
#' reduces the number of hashes by one and searches again, until it finds a
#' pattern.
#' @param highest The highest level of granularity to search for.
#' @param lowest The lowest level of granularity to search for.
#' @param lines a character vector containing the lines to match against.
#' @examples
#'   strcode:::find_gran("down", highest = 3, lowest = 1, lines = c("##  .", "#   _"))
#' @keywords internal
find_gran <- function(direction = "down", highest = 3, lowest = 1, lines) {
  # direction
  if (direction == "up") {
    current <- lowest
    m <- 1
  } else if (direction == "down") {
    current <- highest
    m <- -1
  }

    helper_find_gran <- function(direction) {
    assert_number(current, lower = lowest, upper = highest)
    pattern <- paste0("^", paste0(rep("#", current), collapse = ""),
                      "\\s{", highest + 1 - current, "}.*$")
    if (any(grepl(pattern, lines, perl = TRUE))) {
      current
    } else {
      current <<- current + m * 1
      helper_find_gran(direction = direction)
    }
  }
  helper_find_gran(direction)
}



