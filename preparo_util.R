# Inclusão de pacotes

library(tidyverse)
library(stringi)
library(fs)

loc_bz <- locale(encoding="ISO-8859-1")

# Remover espaços.
remove_espacos <- function(s) s %>% # 'pipe' do pacote "magrittr"
  str_remove_all("\\s+"," ") %>%
  str_remove_all(" ;",";") %>% # antes do separador 
  str_remove_all("\\s+$","") # antes do fim de uma linha

# troca a extensão de um nome de arquivo
  
repl_ext <- function(s,new_ext) s %>%
  str_replace_all("(?<=\\.).+$",new_ext)

squish_file <- function(fname) {
  lines_all <- read_lines(fname,locale=loc_bz)
  line4 <- lines_all[4]
  fname_out <- add_prior_ext(fname,"squished")
  lines_all %>%
    tail(-5) %>% # skip first 5
    remove_espacos %>%
    write_lines(fname_out)
  fname_out_zip <- repl_ext(fname_out,"zip")
  zip::zip(fname_out_zip,fname_out)
}

# files:
fnames<-dir_ls("data",regexp="despesa\\d{4}\\.csv")  
