##
## build.R
## 
## by Jay Emerson, Yale University
## September, 2013
##

library(knitr)
library(markdown)

roots <- dir()[grep("\\.root$", dir())]
for (root in roots) {
  x <- read.table(root, as.is=TRUE, sep=":")
  names(x) <- c("var", "val")
  x$val <- gsub("^\\s+", "", x$val, perl=TRUE) # leading whitespace
  x$val <- gsub("\\s+$", "", x$val, perl=TRUE) # trailing whitespace

  page <- scan(x$val[x$var=="page"], what="", sep="\n")
  cfile <- x$val[x$var=="content"]
  dest <- x$val[x$var=="destination"]
  title <- x$val[x$var=="title"]
  
  if (length(grep("\\.Rmd$", cfile)) > 0) {
    # knit the R Markdown and then process the resulting markdown
    knit(cfile, envir=new.env())
    markdownToHTML(gsub("\\.Rmd$", ".md", cfile),
                   gsub("\\.Rmd$", ".html", cfile))
    unlink(gsub("\\.Rmd$", ".md", cfile))
    cfile <- gsub("\\.Rmd$", ".html", cfile)
  }
  
  # Now we hopefully have HTML content:
  if (length(grep("\\.html$", cfile)) > 0) {
    content <- scan(cfile, what="", sep="\n")
    unlink(cfile)
    unlink("figure", recursive=TRUE, force=TRUE)
  } else {
    content <- "NO CURRENTLY VALID CONTENT"
  }
  
  # If the content includes <body> and </body> then extract content.
  if ( length(grep('<body>', content, fixed=TRUE))==1 &&
       length(grep('</body>', content, fixed=TRUE))==1) {
    content <- content[(grep('<body>', content)+1):(grep('</body>', content)-1)] 
  }
  
  page <- gsub("TITLE", title, page)
  page <- c(page[1:(grep("CONTENT", page)-1)],
            content,
            page[(grep("CONTENT", page)+1):length(page)])
  
  write(page, dest)
}

