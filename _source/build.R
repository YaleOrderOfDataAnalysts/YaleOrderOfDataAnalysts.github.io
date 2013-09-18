##
## build.R
## 
## by Jay Emerson, Yale University
## September 17, 2013
##

library(knitr)
library(markdown)

mytempenv <- new.env()
Rmdfiles <- dir()[grep("\\.Rmd$", dir())]

for (file in Rmdfiles) {
  x <- scan(file, what="", sep="\n")
  temp <- grep("^---$", x)[1:2]
  x <- x[(temp[1]+1):(temp[2]-1)]
  x <- matrix(unlist(strsplit(x, ":")), ncol=2, byrow=TRUE)
  x <- as.data.frame(x, stringsAsFactors=FALSE)
  names(x) <- c("var", "val")
  x$val <- gsub("^\\s+", "", x$val, perl=TRUE) # leading whitespace
  x$val <- gsub("\\s+$", "", x$val, perl=TRUE) # trailing whitespace

  page <- scan(x$val[x$var=="TEMPLATE"], what="", sep="\n")
  x <- x[x$var!="TEMPLATE",]
  dest <- x$val[x$var=="DESTINATION"]
  x <- x[x$var!="DESTINATION",]
  
  if (grepl("\\.Rmd$", file)) {
    # knit the R Markdown and then process the resulting markdown
    knit(file, envir=mytempenv)
    markdownToHTML(gsub("\\.Rmd$", ".md", file),
                   gsub("\\.Rmd$", ".html", file))
    unlink(gsub("\\.Rmd$", ".md", file))
    htmlfile <- gsub("\\.Rmd$", ".html", file)
  }
  
  # Now we hopefully have HTML content:
  if (grepl("\\.html$", htmlfile)) {
    content <- scan(htmlfile, what="", sep="\n")
    unlink(htmlfile)
    unlink("figure", recursive=TRUE, force=TRUE)
  } else {
    content <- "NO CURRENTLY VALID CONTENT"
  }
  
  # If the content includes <body> and </body> then extract content.
  if ( length(grep('<body>', content, fixed=TRUE))==1 &&
       length(grep('</body>', content, fixed=TRUE))==1) {
    content <- content[(grep('<body>', content)+1):(grep('</body>', content)-1)] 
  }
  
  for (arg in x$var) {
    if (arg=="MENU") {
       if (x$val[x$var==arg]=="none") {
         page <- page[-c(grep("BEGINMENU", page):grep("ENDMENU", page))]
       } else {
         temp <- x$val[x$var==arg]
         temp <- unlist(strsplit(temp, ","))
         z <- NULL
         for (i in 1:length(temp)) {
           item <- unlist(strsplit(temp[i], ";"))
           if (item[2]==".") this <- paste('<li class="thispage">',
                                           '<a href="#">', item[1],
                                           '</a></li>', sep="")
           if (item[2]!=".") this <- paste('<li><a href="', item[2],
                                           '">', item[1], '</a></li>', sep="")
           z <- c(z, this)
         }
         page[grep("MENUITEMSHERE", page)] <- paste(z, collapse=" ")
       }
    } else {
      if (x$val[x$var==arg]=="none") {
        page <- page[-grep(arg, page)]
      } else {
        page <- gsub(arg, x$val[x$var==arg], page)
      }
    }
  }
  
  page <- c(page[1:(grep("CONTENT", page)-1)],
            content,
            page[(grep("CONTENT", page)+1):length(page)])
  
  write(page, dest)
}

