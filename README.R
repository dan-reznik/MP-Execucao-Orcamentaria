suppressMessages(library(tidyverse))

# for reprex
setwd("C:/Users/dreznik/Dropbox/Data Science/R Projects/read_lines_bug")
# Confirm UTF-8 encoding of included zip file (it contains a larger .txt)
fname <- "test_file.zip"
guess_encoding(fname)

#Read all lines in zip (approx. 31.6k)
all_lines <- read_lines(fname)
length(all_lines)


# Notice all lines have "well-formed" terminations with a carriage-return followed by a newline.
{
  s <- read_file(fname)
  print(s %>% str_count("\\r"))
  print(s %>% str_count("\\n"))
  print(s %>% str_count("\\r\\n"))
  print(s %>% str_count("\\n\\r"))
}

# For this file, the separator is ";" (irrelevant for read_lines()), and there should be 45 per line. However, notice a few lines have a non-standard number of separators (';'), namely: 46 and 48.
all_lines %>%
  str_count(";") %>%
  table

# List which lines have the wrong number of separators, as this will drive us to the bug.
all_lines %>%
  str_count(";") %>%
  {which(.!=45)}

# Use `read_lines()` to read only the first lines with the wrong number of separators, shown above to be line # 14579. So "skip" that number of lines minus one.
line1 <- read_lines(fname,
                    skip=14578,
                    n_max=1)

# Notice its content not only has a different number of separators, it's a different line altogether! (herein lies the bug)
all_lines[14579] %>% str_count(";")
line1 %>% str_count(";")
all_lines[14579] == line1

# Notice the line retrieved by `read_lines(skip=14578,n_max=1)` above actually appears much later in the file, suggesting the reading under this skip+n_max mode lost "sync".
(all_lines==line1)%>%which

# Notes: 
# * this bug is not caused by the .zip (same behavior if starts from an uncompressed file)
# * this is not related to encoding, in fact, I made sure the file inside the zip is UTF-8
# * this issue seems to manifest itself with longer files only. potentially there's a character within this file being interpreted as a carriage return.

# binary search
find_weird_row <- function(n0,n_min,n_max) {
  l_skip <- read_lines(fname,skip=n0,n_max=1)
  l_all <- all_lines[n0+1]
  l_eq <- l_skip==l_all
  print(n0)
  #browser()
  if(n0%in%c(n_min,n_max)) {
    print(sprintf("line %d with l_eq=%s",
                  n0,
                  l_eq%>%as.character()))
    n0
  } else {
    if (!l_eq)
      find_weird_row(as.integer((n0+n_min)/2),n_min,n0)
    else
      find_weird_row(as.integer((n0+n_max)/2),n0,n_max)
  }
}

find_weird_row(15000,1,length(all_lines))