rm(list = ls())
# Cabecalho ####
# Luiz henrique lima da Silva
# --Graduando em Estatistica
# Análise de dados
library(rjson)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(rlist)
library(glue)
setwd("~/Documentos/Git/analise_tags")

# Declaracao de funcoes ####
novo_nome <- function(nome_antigo) {
  return("tag")
}
padrao <- function(data) {
  data$tag <- toupper(data$tag) %>%
    str_remove_all("[:digit:]|[:punct:]") %>%
    str_extract("[:alpha:]+")
  return(data)
}
unir <- function(vetor) {
  vetor <- vetor[!is.na(vetor)]
  vetor <- unlist(vetor, use.names = F)
  return(glue_collapse(vetor, sep = ";"))
}
tabela_temporal[101,3] - min(tabela_temporal[101,2:3],na.rm = F)
vetor <- tabela_temporal[101,2:3]
sub_min <- function(vetor){
  vetor <- unlist(vetor,use.names = F)
  return(vetor[2] - min(vetor,na.rm = F))
}

# Leitura de dados ####
url <- "https://startragnarok.herokuapp.com/view"
dados <- fromJSON(paste(readLines(url), collapse = ""))
rm(url)
# Tabela temporal ####
tabela_temporal <- dplyr::bind_rows(dados)
tabela_temporal$data <- lubridate::mdy_hms(tabela_temporal$data)
tabela_temporal$contagem <- as.numeric(tabela_temporal$contagem)
page <- as.numeric(tabela_temporal$url %>% str_extract("[0-9]+"))
tabela_temporal$indice <- (tabela_temporal$indice + (page - 1) * 35) + 1

# tabela_temporal$data <- str_c(lubridate::day(tabela_temporal$data),
#   if_else(as.numeric(lubridate::hour(tabela_temporal$data)) == 6, 1,
#     if_else(as.numeric(lubridate::hour(tabela_temporal$data)) == 18, 2, 9)
#   ),
#   sep = "."
# )
# 
# View(
# tabela_temporal %>%
#   dplyr::group_by(tag, data) %>%
#   dplyr::summarise(contagem = min(contagem))
# )
# 
# tabela_temporal <- tabela_temporal %>% dplyr::select(tag, data, contagem)
# tabela_temporal <- tabela_temporal %>%
#   dplyr::group_by(tag, data) %>%
#   dplyr::summarise(contagem = min(contagem)) %>%
#   tidyr::spread(data, contagem) %>%
#   select(-"22.9")
# tabela_temporal <- padrao(tabela_temporal)
# aux <- tabela_temporal
# i <- 3
#   while(i <= ncol(tabela_temporal)){
#     aux[,i] <- apply(tabela_temporal[,(i-1):i], 1, sub_min)
#     i <- i+1
#   }
# med <- aux[,3:ncol(aux)] %>% select(-"24.1") %>% apply(1, mean)
# View(med)
# aux$med <- med
# View(
#   aux
# )
# tabela_temporal[,2:ncol(tabela_temporal)]
rm(page)
# Tabela propocao anos ####

tabela_stackoverflow2014 <-
  readr::read_csv("2014 Stack Overflow Survey Responses.csv")
n <- nrow(tabela_stackoverflow2014)
idade_col <- 4
gender_col <- 5
occupation_col <- 7
# language_col <- c(43,54)
language <- apply(
  tabela_stackoverflow2014[, 43:54],
  1,
  unir
)
stackoverflow_cadastro2014 <- tibble(
  year = rep(2014, n),
  old = unlist(tabela_stackoverflow2014[, idade_col], use.names = F),
  gender = unlist(tabela_stackoverflow2014[, gender_col], use.names = F),
  occupation = unlist(tabela_stackoverflow2014[, occupation_col], use.names = F),
  language = language
)
# while (i <= 66) {
#   names <- factor(names(tabela_stackoverflow2014[, i]))
#   lista2014[[i - 42]] <- tabela_stackoverflow2014[, i] %>%
#     dplyr::group_by_at(names) %>%
#     summarise(qnt2014 = n()) %>%
#     rename_with(novo_nome, starts_with("X") | starts_with("W"))
#   i <- i + 1
# }
# # Dos respondentes x% utilizao a linguagem y
# lista2014 <- dplyr::bind_rows(lista2014) %>% dplyr::filter(!is.na(tag))
#
# prop2014 <- padrao(lista2014) %>%
#   dplyr::group_by(tag) %>%
#   dplyr::summarise(qnt2014 = sum(qnt2014)) %>%
#   dplyr::group_by(tag, qnt2014) %>%
#   dplyr::summarise(prop_respondentes2014 = qnt2014 / sum(lista2014$qnt2014) * 100)

tabela_stackoverflow2015 <-
  readr::read_csv("2015 Stack Overflow Survey Responses.csv", skip = 1)
n <- nrow(tabela_stackoverflow2015)
idade_col <- 2
gender_col <- 3
occupation_col <- 6
# language_col <- c(11,51)
language <- apply(
  tabela_stackoverflow2015[, 11:51],
  1,
  unir
)
stackoverflow_cadastro2015 <- tibble(
  year = rep(2015, n),
  old = unlist(tabela_stackoverflow2015[, idade_col], use.names = F),
  gender = unlist(tabela_stackoverflow2015[, gender_col], use.names = F),
  occupation = unlist(tabela_stackoverflow2015[, occupation_col], use.names = F),
  language = (language)
)

# lista2015 <- list()
# i <- 9
# while (i <= 51) {
#   names <- factor(names(tabela_stackoverflow2015[, i]))
#   lista2015[[i - 8]] <- tabela_stackoverflow2015[, i] %>%
#     dplyr::group_by_at(names) %>%
#     summarise(qnt_2015 = n()) %>%
#     rename_with(novo_nome, starts_with("Curre"))
#   i <- i + 1
# }
# # Dos respondentes x% utilizao a linguagem y
# lista2015 <- dplyr::bind_rows(lista2015) %>% dplyr::filter(!is.na(tag))
# n <- sum(lista2015$qnt_2015)
# prop2015 <- padrao(lista2015) %>%
#   dplyr::group_by(tag) %>%
#   dplyr::summarise(qnt_2015 = sum(qnt_2015)) %>%
#   dplyr::group_by(tag, qnt_2015) %>%
#   dplyr::summarise(prop_respondentes2015 = qnt_2015 / n * 100)


tabela_stackoverflow2016 <-
  readr::read_csv("2016 Stack Overflow Survey Responses.csv")
n <- nrow(tabela_stackoverflow2016)
idade_col <- 2
gender_col <- 3
occupation_col <- 6
# language_col <- c(11,51)
language <- apply(
  tabela_stackoverflow2016[, 11:51],
  1,
  unir
)
stackoverflow_cadastro2016 <- tibble(
  year = rep(2016, n),
  old = unlist(tabela_stackoverflow2016[, idade_col], use.names = F),
  gender = unlist(tabela_stackoverflow2016[, gender_col], use.names = F),
  occupation = unlist(tabela_stackoverflow2016[, occupation_col], use.names = F),
  language = (language)
)
# n <- nrow(tabela_stackoverflow2016)
# idade_col <- 6
# gender_col <- 8
# occupation_col <- 10
# # language_col <- c(11,51)
# language <- tabela_stackoverflow2016[, 17]
# # language <- apply(
# #   tabela_stackoverflow2016[, 11:51],
# #   1,
# #   unir
# # )
# stackoverflow_cadastro2016 <- tibble(
#   year = rep(2016, n),
#   old = unlist(tabela_stackoverflow2016[, idade_col],use.names = F),
#   gender = unlist(tabela_stackoverflow2016[, gender_col], use.names = F),
#   occupation = unlist(tabela_stackoverflow2016[, occupation_col], use.names = F),
#   language = unlist(language, use.names = F)
# )
tabela_stackoverflow2017 <-
  readr::read_csv("2017 Stack Overflow Survey Responses.csv")
n <- nrow(tabela_stackoverflow2017)
# idade_col <- 6
gender_col <- 146
occupation_col <- 2
# language_col <- c(11,51)
language <- tabela_stackoverflow2017[, 89]
# language <- apply(
#   tabela_stackoverflow2017[, 11:51],
#   1,
#   unir
# )
stackoverflow_cadastro2017 <- tibble(
  year = rep(2017, n),
  # old = unlist(tabela_stackoverflow2017[, idade_col],use.names = F),
  gender = unlist(tabela_stackoverflow2017[, gender_col], use.names = F),
  occupation = unlist(tabela_stackoverflow2017[, occupation_col], use.names = F),
  language = unlist(language, use.names = F)
)
#
# i <- 89
# aux <- NULL
# aux <- tabela_stackoverflow2017[, i]$HaveWorkedLanguage %>%
#   str_split(";") %>%
#   rlist::list.ungroup()
#
# aux <- aux[!is.na(aux)]
# n <- length(aux)
# prop2017 <- tibble(tag = aux) %>%
#   padrao() %>%
#   dplyr::group_by(tag) %>%
#   dplyr::summarise(qnt_2017 = n()) %>%
#   dplyr::group_by(tag, qnt_2017) %>%
#   dplyr::summarise(prop_respondentes2017 = qnt_2017 / n * 100)

tabela_stackoverflow2018 <-
  readr::read_csv("2018 Stack Overflow Survey Responses.csv")
n <- nrow(tabela_stackoverflow2018)
idade_col <- 125
gender_col <- 121
occupation_col <- 10
# language_col <- c(11,51) 68
language <- tabela_stackoverflow2018[,66]
# language <- apply(
#   tabela_stackoverflow2018[, 11:51],
#   1,
#   unir
# )
stackoverflow_cadastro2018 <- tibble(
  year = rep(2018, n),
  old = unlist(tabela_stackoverflow2018[, idade_col], use.names = F),
  gender = unlist(tabela_stackoverflow2018[, gender_col], use.names = F),
  occupation = unlist(tabela_stackoverflow2018[, occupation_col], use.names = F),
  language = (language)
)
# i <- 66
# aux <- NULL
#
# aux <- tabela_stackoverflow2018[, i]$LanguageWorkedWith %>%
#   str_split(";") %>%
#   rlist::list.ungroup()
#
# aux <- aux[!is.na(aux)]
# n <- length(aux)
# prop2018 <- tibble(tag = aux) %>%
#   padrao() %>%
#   dplyr::group_by(tag) %>%
#   dplyr::summarise(qnt_2018 = n()) %>%
#   dplyr::group_by(tag, qnt_2018) %>%
#   dplyr::summarise(prop_respondentes2018 = qnt_2018 / n * 100)

tabela_stackoverflow2019 <-
  readr::read_csv("2019 Stack Overflow Survey Responses.csv")
View(tabela_stackoverflow2019)
n <- nrow(tabela_stackoverflow2019)
idade_col <- 78
gender_col <- 79
occupation_col <- 8
# language_col <- c(11,51) 68
language <- tabela_stackoverflow2019[,44]
# language <- apply(
#   tabela_stackoverflow2019[, 11:51],
#   1,
#   unir
# )
stackoverflow_cadastro2019 <- tibble(
  year = rep(2019, n),
  old = unlist(tabela_stackoverflow2019[, idade_col], use.names = F),
  gender = unlist(tabela_stackoverflow2019[, gender_col], use.names = F),
  occupation = unlist(tabela_stackoverflow2019[, occupation_col], use.names = F),
  language = (language)
)

# i <- 44
# aux <- NULL
#
# aux <- tabela_stackoverflow2019[, i]$LanguageWorkedWith %>%
#   str_split(";") %>%
#   rlist::list.ungroup()
#
# aux <- aux[!is.na(aux)]
# n <- length(aux)
# prop2019 <- tibble(tag = aux) %>%
#   padrao() %>%
#   dplyr::group_by(tag) %>%
#   dplyr::summarise(qnt_2019 = n()) %>%
#   dplyr::group_by(tag, qnt_2019) %>%
#   dplyr::summarise(prop_respondentes2019 = qnt_2019 / n * 100)

rm(list = (c("lista2014", "lista2015")))


# Merge de dados ####
stackoverflow_cadastro2014$language <- as.character(cbind(stackoverflow_cadastro2014$language))
stackoverflow_cadastro2015$language <- as.character(cbind(stackoverflow_cadastro2015$language))
stackoverflow_cadastro2016$language <- as.character(cbind(stackoverflow_cadastro2016$language))
stackoverflow_cadastro2017$language <- as.character(cbind(stackoverflow_cadastro2017$language))
stackoverflow_cadastro2018$language <- as.character(cbind(stackoverflow_cadastro2018$language))
stackoverflow_cadastro2019$language <- as.character(cbind(stackoverflow_cadastro2019$language))

aux <- full_join(stackoverflow_cadastro2014, stackoverflow_cadastro2015) %>%
  full_join(stackoverflow_cadastro2016) %>%
  full_join(stackoverflow_cadastro2017) %>%
  full_join(stackoverflow_cadastro2019)
full_join(stackoverflow_cadastro2018)


tabelaaux <- read_csv("export_dataframe.csv")
exemplo <- apply(tabelaaux$title,1,str_detect("c#"))
apply(tabelaaux$title,1,str_detect(tabelaaux$title,"c++"))

c_mais <- if_else(str_detect(tabelaaux$title,"c++"),1,0)
javascript <- if_else(str_detect(tabelaaux$title,"javascript"),1,0)
java <- if_else(str_detect(tabelaaux$title,"java"),1,0)

python <- if_else(str_detect(tabelaaux$title,"Python"),1,0)
python[59]
tabelaaux$title[59]

sum(python)
sum(c_mais)

javascript



save.image("~/Documentos/Git/analise_tags/mini_artigo.RData")

write_csv(tabela_temporal, "tabela_temporal2.csv")
