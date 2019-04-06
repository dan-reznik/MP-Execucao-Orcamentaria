Empenho: Preparo Série Histórica
================

Inclusão de pacotes

``` r
library(tidyverse)
library(fs)
source("preparo_util.R")
```

Comprime todos os arquivos .csv (retira espaços extra)

``` r
fnames <- dir_ls("data",regexp="despesa\\d{4}\\.csv")
fnames %>% walk(squish_file)
```

``` r
fnames_squished <- dir_ls("data",regexp="despesa\\d{4}_squished\\.zip") %>%
  as.character
fnames_squished
```

    ## [1] "data/despesa2018_squished.zip"

Extrai ano de cada nome de arqiuvo

``` r
anos <- map_chr(fnames_squished,str_extract,"\\d{4}")
anos
```

    ## [1] "2018"

Lê todos os arquivos (sem descomprimir .zip), adiciona coluna “ano”, e
concatena num só data frame

``` r
df_all <- fnames_squished %>%
  map2_dfr(anos,~{read_delim(.x,delim=";",quote="^")%>%
      mutate(ano=.y)%>%
      select(ano,everything())}) %>%
  mutate_if(is.character,as.factor) # taking long...
nrow(df_all)
```

Escreve num arquivo do r (formato RDS)

``` r
df_all %>% write_rds("data/df_all.rds",compress="bz2")
```

Apaga da memória

``` r
rm(df_all)
```
