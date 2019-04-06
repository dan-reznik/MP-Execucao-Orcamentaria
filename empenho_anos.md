Empenho: Análise dos Dados, 2014-8
================

Inclusão de pacotes

``` r
library(tidyverse)
```

Lê do arquivo “.rds” preparado em `preparo_anos.Rmd`

``` r
df_all <- read_rds("data/df_all.rds")
nrow(df_all)
#> [1] 705527
```

Limpa nome de orgao de maneira mais genérica

``` r
clean_orgao_gen <- function(orgao) orgao %>%
  str_remove_all("[^[:alpha:]\\s]") %>%
  str_remove_all("Sec(ret|retaria)? ?d[aeo] Est(ado)? (d[aeo])?") %>%
  str_remove_all("Sec Est de ")
  #str_remove("^Sec(retaria|ret)?\\.? ?d[aeo] Est.?(ado)? d[aeo] ?")
```

Cria tabela de referência p/ nomes de órgãos (usa o primeiro a aparecer)

``` r
df_orgao_ref <- df_all %>%
  rename(valor_empenhado=`Valor Empenhado`,
         orgao_cod=`Órgão`,
         orgao=`Nome Órgão`) %>%
  mutate(orgao=orgao%>%as.character) %>% # era fatir
  group_by(orgao_cod) %>%
  summarize(n=n(),orgao_list=list(orgao)) %>%
  mutate(orgao_ref=map_chr(orgao_list,first)) %>%
  select(orgao_cod,orgao_ref) %>%
  mutate(orgao_ref=orgao_ref%>%clean_orgao_gen) %>%
  arrange(orgao_cod)
df_orgao_ref
#> # A tibble: 34 x 2
#>    orgao_cod orgao_ref                         
#>    <fct>     <chr>                             
#>  1 01        Assembléia Legislativa            
#>  2 02        Tribunal de Contas do Estado do RJ
#>  3 03        Tribunal de Justiça               
#>  4 07        " Obras"                          
#>  5 08        ViceGovernadoria                  
#>  6 09        Procuradoria Geral do Estado      
#>  7 10        Ministério Público                
#>  8 11        Defensoria Pública Geral do Estado
#>  9 12        " Planejamento e Gestão"          
#> 10 13        " Agricultura e Pecuária"         
#> # ... with 24 more rows
```

Órgãos com maior valor empenhado total (top 5)

``` r
anos <- unique(df_all$ano%>%as.character)%>%as.integer

df_all_top_orgaos_valor_empenhado <- df_all %>%
  rename(valor_empenhado=`Valor Empenhado`,
         orgao_cod=`Órgão`) %>%
  group_by(orgao_cod) %>%
  summarize(n=n(),
            total=sum(valor_empenhado)) %>%
  arrange(desc(total)) %>%
  head(5) %>%
  inner_join(df_orgao_ref,by="orgao_cod") %>%
  select(orgao_cod,orgao_ref,everything()) %>%
  mutate(media_anual=total/length(anos)) %>%
  mutate_at(vars(total,media_anual),~(./10^9)%>%as.integer) %>%
  rename_at(vars(total,media_anual),~str_c(.,"_b")) # billions
df_all_top_orgaos_valor_empenhado
#> # A tibble: 5 x 5
#>   orgao_cod orgao_ref                     n total_b media_anual_b
#>   <fct>     <chr>                     <int>   <int>         <int>
#> 1 37        Encargos Gerais do Estado 19223    4274           854
#> 2 20        " Fazenda"                13140    3940           788
#> 3 12        " Planejamento e Gestão"  12912    3917           783
#> 4 26        " Segurança"              38427    2569           513
#> 5 29        " Saúde"                  77625    1646           329
```

Plota médias anuais

``` r
df_all_top_orgaos_valor_empenhado%>%
  mutate(orgao_ref=orgao_ref%>%fct_inorder()%>%fct_rev)%>%
  ggplot(aes(orgao_ref,media_anual_b)) +
  geom_col(aes(fill=orgao_ref)) +
  coord_flip() +
  labs(title="Média de Valores Empenhados Anuais por Órgão",
       subtitle=sprintf("Anos %s-%s, Top 5", min(anos),max(anos)),
       y="Valor Empenhado Anual Médio (R$ bilhões)") +
  theme(legend.position = "none",
        axis.title.y=element_blank())
```

![](empenho_anos_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Evolução história dos Valores Empenhados destes top órgãos:

``` r
df_hist <- df_all %>%
  rename(valor_empenhado=`Valor Empenhado`,
         orgao_cod=`Órgão`) %>%
  inner_join(df_all_top_orgaos_valor_empenhado%>%
               select(orgao_cod,orgao=orgao_ref),
             by="orgao_cod") %>%
  group_by(ano,orgao) %>%
  summarize(total_b=sum(valor_empenhado)/10^9) %>%
  arrange(desc(total_b)) %>%
  ungroup() %>% # para q proxima linha rode corretamente
  mutate(orgao=orgao%>%fct_inorder) #
```

Grafa o histórico:

``` r
df_hist %>%
  ggplot(aes(ano,total_b,group=orgao,color=orgao)) +
  geom_line(size=I(1)) +
  # scale_y_log10() +
  labs(title="Valor Empenhado Anual por Órgão",
       subtitle=sprintf("Anos %s-%s, Top 5", min(anos),max(anos)),
       y = "Valor Empenhado Total (R$ bilhões)") +
  theme(legend.position = "bottom")
```

![](empenho_anos_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Distribuição de Valor Empenhado por ano (faceteamento)

``` r
df_all %>%
  rename(valor_empenhado=`Valor Empenhado`,
         orgao_cod=`Órgão`) %>%
  inner_join(df_all_top_orgaos_valor_empenhado%>%
               select(orgao_cod,orgao=orgao_ref),
             by="orgao_cod") %>%
  mutate(orgao=orgao%>%fct_reorder(valor_empenhado,sum)) %>%
  ggplot(aes(orgao,valor_empenhado+1)) +
  geom_boxplot(aes(fill=orgao),outlier.shape=NA) +
  scale_y_log10(breaks=10^c(0,3,6,9),labels=c("1","1k","1M","1B")) + #breaks=10^(1:5),labels=10^(1:5)%>%as.integer) +
  coord_flip() + # ylim=c(10,.5*10^5)) +
  facet_wrap(~ano,ncol=2) +
  labs(title="Distribuição do Valor Empenhado",
       y = "Valor Empenhado Anual (R$)") +
  theme(legend.position = "none",
        axis.title.y=element_blank())
```

![](empenho_anos_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
