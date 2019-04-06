Empenho: Análise dos Dados, 2018
================

Inclusão de pacotes

``` r
library(tidyverse)
library(fs)
```

Checa tamanho dos arquivos

``` r
dir_info("data") %>%
  select(path,size,modification_time)
#> # A tibble: 4 x 3
#>   path                                 size modification_time  
#>   <fs::path>                    <fs::bytes> <dttm>             
#> 1 data/despesa.zip                  125.32M 2019-03-29 07:40:36
#> 2 data/despesa2018.zip               17.98M 2019-03-29 16:40:21
#> 3 data/despesa2018_squished.zip       9.08M 2019-04-06 10:25:50
#> 4 data/df_all.rds                    24.37M 2019-03-30 12:05:39
```

# Leitura de Dados Higienizados

Colocar num data frame (“tibble”). Nota: arquivo agora ’e UTF-8

``` r
fname_2018_squished_zip <- "data/despesa2018_squished.zip"
df_orcamento <- read_delim(fname_2018_squished_zip,delim=";",
                           quote="'") # evita erro com "
#> Parsed with column specification:
#> cols(
#>   .default = col_character(),
#>   Poder = col_double(),
#>   Grupo = col_double(),
#>   `Nome Grupo` = col_logical(),
#>   `Modalidade de Aplicação` = col_double(),
#>   Elemento = col_double(),
#>   `Sub Elemento` = col_double(),
#>   Empenho = col_double(),
#>   `Valor Empenhado` = col_number(),
#>   `Valor Liquidado` = col_number(),
#>   `Valor Pago` = col_number()
#> )
#> See spec(...) for full column specifications.
glimpse(df_orcamento)
#> Observations: 99,362
#> Variables: 32
#> $ Poder                          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
#> $ `Nome Poder`                   <chr> "Executivo", "Executivo", "Exec...
#> $ Grupo                          <dbl> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3...
#> $ `Nome Grupo`                   <lgl> NA, NA, NA, NA, NA, NA, NA, NA,...
#> $ `Modalidade de Aplicação`      <dbl> 90, 90, 90, 90, 90, 90, 90, 90,...
#> $ `Nome Modalidade de Aplicação` <chr> "Aplicações Diretas", "Aplicaçõ...
#> $ Elemento                       <dbl> 339036, 339014, 339039, 339030,...
#> $ `Nome Elemento`                <chr> "Outros Serviços de Terceiros -...
#> $ `Sub Elemento`                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA,...
#> $ `Nome Sub Elemento`            <chr> NA, NA, NA, NA, NA, NA, NA, NA,...
#> $ Órgão                          <chr> "21", "31", "29", "21", "40", "...
#> $ `Nome Órgão`                   <chr> "Secretaria de Estado da Casa C...
#> $ UO                             <chr> "2133", "3131", "2961", "2104",...
#> $ `Nome UO`                      <chr> "Departamento de Trânsito do Es...
#> $ UG                             <chr> "263100", "053100", "296100", "...
#> $ `Nome UG`                      <chr> "DEPARTAMENTO DE TRANSITO DO RI...
#> $ Credor                         <chr> "965.197.977-15", "042.670.427-...
#> $ `Nome Credor`                  <chr> "ALEXANDRE CESAR DE SOUZA", "LU...
#> $ `Fonte de Recursos`            <chr> "32", "12", "22", "00", "00", "...
#> $ `Nome Fonte de Recursos`       <chr> "Taxas pelo Exercício do Poder ...
#> $ Processo                       <chr> "E-12/061/7244/20", "E-12/171/1...
#> $ Função                         <chr> "06", "22", "10", "04", "13", "...
#> $ `Nome Função`                  <chr> "Segurança Pública", "Indústria...
#> $ `Sub Função`                   <chr> "122", "122", "302", "122", "12...
#> $ `Nome Sub Função`              <chr> "Administração Geral", "Adminis...
#> $ Licitação                      <chr> "05", "07", "09", "09", "05", "...
#> $ `Nome Licitação`               <chr> "DISPENSA", "NAO APLICAVEL", "P...
#> $ Empenho                        <dbl> 4390, 1354, 7927, 561, 752, 140...
#> $ Histórico                      <chr> "Cancelamento conforme Decreto ...
#> $ `Valor Empenhado`              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
#> $ `Valor Liquidado`              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
#> $ `Valor Pago`                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
```

``` r
problems(df_orcamento)
#> # tibble [0 x 4]
#> # ... with 4 variables: row <int>, col <int>, expected <chr>, actual <chr>
```

Quantas linhas, colunas, ou dimensões?

``` r
# nrow(df_orcamento)
# ncol(df_orcamento)
dim(df_orcamento)
#> [1] 99362    32
```

# Análise do “Empenho”

Calcula sumarização de colunas que parecem conter valores:

``` r
summary(df_orcamento%>%select(contains("empenh"),
                              contains("valor")))
#>     Empenho      Valor Empenhado     Valor Liquidado    
#>  Min.   :    1   Min.   :0.000e+00   Min.   :0.000e+00  
#>  1st Qu.:  236   1st Qu.:0.000e+00   1st Qu.:0.000e+00  
#>  Median :  659   Median :2.588e+03   Median :1.254e+03  
#>  Mean   : 2475   Mean   :3.747e+07   Mean   :3.768e+07  
#>  3rd Qu.: 2085   3rd Qu.:9.188e+04   3rd Qu.:7.438e+04  
#>  Max.   :27484   Max.   :1.930e+11   Max.   :1.930e+11  
#>    Valor Pago       
#>  Min.   :0.000e+00  
#>  1st Qu.:0.000e+00  
#>  Median :2.750e+02  
#>  Mean   :3.315e+07  
#>  3rd Qu.:4.694e+04  
#>  Max.   :1.930e+11
```

Quantas linhas não preenchidas (com ‘NA’)?

``` r
df_orcamento$`Valor Empenhado` %>% is.na %>% sum
#> [1] 0
```

Plota histograma do Valor Empenhado

``` r
df_orcamento %>%
  transmute(valor_empenhado=(1+`Valor Empenhado`))%>%
  ggplot(aes(valor_empenhado)) +
  geom_histogram(bins=30,fill="blue",color="black") +
  scale_x_log10(breaks=10^(0:11),labels=c(1,10,100,
                                          "1k","10k" ,"100k" ,
                                          "1M","10M","100M",
                                          "1B","10B","100B"))+
  labs(title="Histograma de Valores Empenhados",
       subtitle="Ano 2018",
       x="Valor Empenhado (R$)")
```

![](empenho_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Estudo por Órgãos

Quantos por órgão?

``` r
df_orcamento %>%
  rename(orgao_cod=`Órgão`,
         orgao=`Nome Órgão`) %>%
  count(orgao_cod,orgao,sort=T)
#> # A tibble: 27 x 3
#>    orgao_cod orgao                                             n
#>    <chr>     <chr>                                         <int>
#>  1 18        Secretaria de Estado de Educação              19940
#>  2 40        Secretaria de Estado de Ciência Tecnologia    12816
#>  3 29        Secretaria de Estado de Saúde                 11056
#>  4 21        Secretaria de Estado da Casa Civil e Desenvol 10603
#>  5 26        Secretaria de Estado de Segurança              5542
#>  6 13        Secretaria de Estado de Agricultura Pecuária   4800
#>  7 07        Secretaria de Estado de Obras                  4671
#>  8 20        Secretaria de Estado de Fazenda e Planejament  4192
#>  9 31        Secretaria de Estado de Transportes            3436
#> 10 10        Ministério Público                             2629
#> # ... with 17 more rows
```

Nomes de órgãos são únicos por código? Parece que há um problema no
código 40

``` r
df_orcamento %>%
  rename(orgao_cod=`Órgão`,
         orgao=`Nome Órgão`) %>%
  count(orgao_cod,orgao) %>%
  count(orgao_cod,sort=T)
#> # A tibble: 26 x 2
#>    orgao_cod    nn
#>    <chr>     <int>
#>  1 40            2
#>  2 01            1
#>  3 02            1
#>  4 03            1
#>  5 07            1
#>  6 08            1
#>  7 09            1
#>  8 10            1
#>  9 11            1
#> 10 13            1
#> # ... with 16 more rows
```

Quem é número 40 e em quais formas aparece?

``` r
df_orcamento %>%
  rename(orgao_cod=`Órgão`,
         orgao=`Nome Órgão`) %>%
  count(orgao_cod,orgao) %>%
  filter(orgao_cod==40)
#> # A tibble: 2 x 3
#>   orgao_cod orgao                                            n
#>   <chr>     <chr>                                        <int>
#> 1 40        Secretaria de Estado de Ciência Tecnologia   12816
#> 2 40        Secretaria de Estado de Ciência Tecnologia I  1062
```

Empenho por órgão, harmonizando orgao\_cod=40:

``` r
df_orcamento %>%
  rename(valor_empenhado=`Valor Empenhado`,
         orgao_cod=`Órgão`,
         orgao=`Nome Órgão`) %>%
  mutate(orgao=if_else(orgao_cod==40,"Secretaria de Estado de Ciência Tecnologia",orgao)) %>%
  group_by(orgao) %>%
  summarize(n=n(),
            total=sum(valor_empenhado),
            media=mean(valor_empenhado),
            mediana=median(valor_empenhado),
            desvio_padrao=sd(valor_empenhado),
            mad=mad(valor_empenhado)) %>%
  mutate_at(vars(-orgao,-n),~(./10^6)%>%as.integer) %>%
  arrange(-total)
#> # A tibble: 26 x 7
#>    orgao                         n  total media mediana desvio_padrao   mad
#>    <chr>                     <int>  <int> <int>   <int>         <int> <int>
#>  1 Secretaria de Estado de ~  4192 1.49e6   355       0          4574     0
#>  2 Secretaria de Estado de ~  5542 4.38e5    79       0          1543     0
#>  3 Encargos Gerais do Estado  2082 2.99e5   143       0          1776     0
#>  4 Tribunal de Justiça do E~  2436 2.55e5   104       0          1718     0
#>  5 Secretaria de Estado de ~ 19940 2.50e5    12       0           204     0
#>  6 Secretaria de Estado de ~ 11056 2.29e5    20       0           274     0
#>  7 Secretaria de Estado de ~ 13878 1.78e5    12       0           254     0
#>  8 Secretaria de Estado de ~  1649 1.01e5    61       0           312     0
#>  9 Secretaria de Estado da ~ 10603 9.26e4     8       0           126     0
#> 10 Secretaria de Estado de ~  3436 7.64e4    22       0           706     0
#> # ... with 16 more rows
```

Remove prefixos de nomes de órgãos

``` r
clean_orgao <- function(orgao) orgao %>%
  str_remove("^Secretaria d[aeo] Estado d[aeo] ")
```

Empenho total por órgão: Top 5

``` r
df_orcamento %>%
  rename(valor_empenhado=`Valor Empenhado`,
         orgao_cod=`Órgão`,
         orgao=`Nome Órgão`) %>%
  mutate(orgao=if_else(orgao_cod==40,"Secretaria de Estado de Ciência Tecnologia",orgao),
         orgao=orgao%>%clean_orgao,
         orgao=orgao%>%fct_reorder(-valor_empenhado,sum)) %>%
  filter(as.integer(orgao)<6) %>%
  mutate(orgao=orgao%>%fct_rev) %>%
  group_by(orgao) %>%
  summarize(total=sum(valor_empenhado)/10^9) %>%
  ggplot(aes(orgao,total)) +
  geom_col(aes(fill=orgao)) +
  coord_flip() +
  labs(title="Valor Empenhado Total por Órgão",
       subtitle="Ano 2018, Top 5",
       y="Valor Empenhado Total (R$ bilhões)") +
  theme(legend.position = "none",
        axis.title.y=element_blank())
```

![](empenho_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Distribuição do Empenho por Órgão (boxbplot)

``` r
df_orcamento %>%
  rename(valor_empenhado=`Valor Empenhado`,
         orgao_cod=`Órgão`,
         orgao=`Nome Órgão`) %>%
  mutate(orgao=if_else(orgao_cod==40,"Secretaria de Estado de Ciência Tecnologia",orgao),
         orgao=orgao%>%clean_orgao,
         orgao=orgao%>%fct_reorder(valor_empenhado+1,.fun=median,.desc=T)) %>%
  filter(as.integer(orgao)<6) %>%
  mutate(orgao=orgao%>%fct_rev) %>% # for coord_flip
  ggplot(aes(orgao,valor_empenhado+1)) +
  geom_boxplot(aes(fill=orgao),notch=T) +
  scale_y_log10(breaks=10^c(3,6,9),labels=c("1k","1M","1B"))+
  coord_flip() +
  labs(title="Distribuição dos Valores Empenhados por Órgão",
       subtitle="Ano 2018, Top 5 medianas",
       y="Valor Empenhado (R$)") +
  theme(legend.position = "none",
        axis.title.y=element_blank())
```

![](empenho_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
