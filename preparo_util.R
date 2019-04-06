# Inclusão de pacotes

library(tidyverse)
library(stringi)
library(fs)
library(zip)

loc_bz <- locale(encoding="ISO-8859-1")

# Remover espaços.
remove_espacos <- function(s) s %>% # 'pipe' do pacote "magrittr"
  str_remove_all("\\s+(?=;)") %>% # antes do separador 
  str_remove_all("\\s+$") %>% # antes do fim de uma linha
  str_replace_all("\\s+"," ") # double spaces
# troca a extensão de um nome de arquivo
  
remove_ext <- function(s) s %>%
  str_remove("\\..*$")
get_ext <- function(s) s %>%
  str_extract("(?<=\\.).+$")
repl_ext <- function(s,new_ext) s %>%
  str_replace_all("(?<=\\.).+$",new_ext)


extract_period <- function(s) s %>%
  str_extract_all("\\d{2}\\/\\d{2}\\/\\d{4}") %>%
  first %>%
  str_replace_all("(\\d{2})\\/(\\d{2})\\/(\\d{4})","\\3\\2\\1") %>%
  str_c(collapse="_")

squish_file <- function(fname) {
  print(sprintf("arquivo: %s",fname))
  skip <- 5
  lines_all <- read_lines(fname,locale=loc_bz)
  size <- lines_all %>% tail(-skip) %>% str_length %>% sum
  print(sprintf("   linhas: %d ", length(lines_all)))
  print(sprintf("   bytes : %d",size))
  # currently not used, could add it to columns
  period <- lines_all[skip] %>% extract_period
  fname_out <- remove_ext(fname)%s+%"_squished."%s+%get_ext(fname)
  lines_all_squished <- lines_all %>%
    tail(-skip) %>% # skip first 5
    remove_espacos
  size_compr <- lines_all_squished %>% str_length %>% sum
  print(sprintf("   compr : %d",size_compr))
  print(sprintf("   ratio : %.2f",size_compr/size))
  lines_all_squished %>% write_lines(fname_out)
  fname_out_zip <- repl_ext(fname_out,"zip")
  print(sprintf("   zip   : %s",fname_out_zip))
  zip::zip(fname_out_zip,fname_out)
  print(sprintf("   exclui: %s",fname_out))
  file_delete(fname_out)
}

# to do all files
# fnames <- dir_ls("data",regexp="despesa\\d{4}\\.csv")
# fnames %>% walk(squish_file)
  
