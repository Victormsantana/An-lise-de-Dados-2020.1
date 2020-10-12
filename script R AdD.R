library(tidyverse)
library(dplyr)
library(haven)

url <- "https://www.dropbox.com/s/xqh3mjf0m64mkr7/EES2019%20Voter%20Study%20early%20release.sav?raw=1"

download.file(url, "EES2019 Voter Study early release.sav", mode = "wb")

dataset <- read_sav("EES2019 Voter Study early release.sav")

# Seleção das Variáveis
banco_selecionado <- dataset %>% select(Q21, Q8, Q5, Q22, Q23, Q11, Q19, Q20, Q18_1,
                                        Q18_2, Q6, hCountry, D3, D4_1, D6, D7, D8, D9)

# Recodificação das variáveis
banco_recodificado <- banco_selecionado %>% 
  mutate(Ex_comunista = case_when(hCountry == 1 ~ 0,
                                  hCountry == 2 ~ 0,
                                  hCountry == 3 ~ 1,
                                  hCountry == 4 ~ 1,
                                  hCountry == 5 ~ 0,
                                  hCountry == 6 ~ 1,
                                  hCountry == 7 ~ 0,
                                  hCountry == 8 ~ 0,
                                  hCountry == 9 ~ 1,
                                  hCountry == 10 ~ 0,
                                  hCountry == 11 ~ 0,
                                  hCountry == 12 ~ 0,
                                  hCountry == 13 ~ 1,
                                  hCountry == 14 ~ 0,
                                  hCountry == 15 ~ 0,
                                  hCountry == 16 ~ 1,
                                  hCountry == 17 ~ 1,
                                  hCountry == 18 ~ 0,
                                  hCountry == 19 ~ 0,
                                  hCountry == 20 ~ 0,
                                  hCountry == 21 ~ 1,
                                  hCountry == 22 ~ 0,
                                  hCountry == 23 ~ 1,
                                  hCountry == 24 ~ 1,
                                  hCountry == 25 ~ 1,
                                  hCountry == 26 ~ 0,
                                  hCountry == 27 ~ 0,
                                  hCountry == 28 ~ 0),
         Sanções = case_when(hCountry == 1 ~ 0,
                             hCountry == 2 ~ 1,
                             hCountry == 3 ~ 0,
                             hCountry == 4 ~ 0,
                             hCountry == 5 ~ 0,
                             hCountry == 6 ~ 0,
                             hCountry == 7 ~ 0,
                             hCountry == 8 ~ 0,
                             hCountry == 9 ~ 0,
                             hCountry == 10 ~ 0,
                             hCountry == 11 ~ 0,
                             hCountry == 12 ~ 0,
                             hCountry == 13 ~ 0,
                             hCountry == 14 ~ 0,
                             hCountry == 15 ~ 0,
                             hCountry == 16 ~ 0,
                             hCountry == 17 ~ 0,
                             hCountry == 18 ~ 1,
                             hCountry == 19 ~ 0,
                             hCountry == 20 ~ 0,
                             hCountry == 21 ~ 0,
                             hCountry == 22 ~ 0,
                             hCountry == 23 ~ 0,
                             hCountry == 24 ~ 0,
                             hCountry == 25 ~ 0,
                             hCountry == 26 ~ 0,
                             hCountry == 27 ~ 0,
                             hCountry == 28 ~ 0),
         Eleições_em_conjunto = case_when(hCountry == 1 ~ 0,
                                          hCountry == 2 ~ 1,
                                          hCountry == 3 ~ 0,
                                          hCountry == 4 ~ 0,
                                          hCountry == 5 ~ 0,
                                          hCountry == 6 ~ 0,
                                          hCountry == 7 ~ 0,
                                          hCountry == 8 ~ 0,
                                          hCountry == 9 ~ 0,
                                          hCountry == 10 ~ 0,
                                          hCountry == 11 ~ 0,
                                          hCountry == 12 ~ 1,
                                          hCountry == 13 ~ 0,
                                          hCountry == 14 ~ 1,
                                          hCountry == 15 ~ 0.5,
                                          hCountry == 16 ~ 0,
                                          hCountry == 17 ~ 1,
                                          hCountry == 18 ~ 0,
                                          hCountry == 19 ~ 1,
                                          hCountry == 20 ~ 0,
                                          hCountry == 21 ~ 0,
                                          hCountry == 22 ~ 0,
                                          hCountry == 23 ~ 1,
                                          hCountry == 24 ~ 0,
                                          hCountry == 25 ~ 0,
                                          hCountry == 26 ~ 0,
                                          hCountry == 27 ~ 0,
                                          hCountry == 28 ~ 0),
         Ciclo_eleitoral = case_when(hCountry == 1 ~ 0.824,
                                     hCountry == 2 ~ 0,
                                     hCountry == 3 ~ 0.540,
                                     hCountry == 4 ~ 0.708,
                                     hCountry == 5 ~ 0.260,
                                     hCountry == 6 ~ 0.396,
                                     hCountry == 7 ~ 0.993,
                                     hCountry == 8 ~ 0.408,
                                     hCountry == 9 ~ 0.057,
                                     hCountry == 10 ~ 0.028,
                                     hCountry == 11 ~ 0.410,
                                     hCountry == 12 ~ 0.969,
                                     hCountry == 13 ~ 0.282,
                                     hCountry == 14 ~ 0.819,
                                     hCountry == 15 ~ 0.245,
                                     hCountry == 16 ~ 0.158,
                                     hCountry == 17 ~ 0.645,
                                     hCountry == 18 ~ 0.122,
                                     hCountry == 19 ~ 0.394,
                                     hCountry == 20 ~ 0.546,
                                     hCountry == 21 ~ 0.779,
                                     hCountry == 22 ~ 0.909,
                                     hCountry == 23 ~ 0.9,
                                     hCountry == 24 ~ 0.244,
                                     hCountry == 25 ~ 0.807,
                                     hCountry == 26 ~ 0.142,
                                     hCountry == 27 ~ 0.177,
                                     hCountry == 28 ~ 0.778)) %>% 
  mutate(Q21 = case_when(Q21 == 1 ~ 4,
                         Q21 == 2 ~ 3,
                         Q21 == 3 ~ 2,
                         Q21 == 4 ~ 1,
                         Q21 == 98 ~ NA_real_,
                         Q21 == 99 ~ NA_real_),
         Q8 = case_when(Q8 == 0 ~ 0,
                        Q8 == 1 ~ 1,
                        Q8 == 2 ~ 2,
                        Q8 == 3 ~ 3,
                        Q8 == 4 ~ 4,
                        Q8 == 5 ~ 5,
                        Q8 == 6 ~ 6,
                        Q8 == 7 ~ 7,
                        Q8 == 8 ~ 8,
                        Q8 == 9 ~ 9,
                        Q8 == 10 ~ 10,
                        Q8 == 98 ~ NA_real_,
                        Q8 == 99 ~ NA_real_),
         Q5 = case_when(Q5 == 1 ~ "Aprova",
                        Q5 == 2 ~ "Desaprova",
                        Q5 == 98 ~ NA_character_,
                        Q5 == 99 ~ NA_character_),
         Q22 = case_when(Q22 == 1 ~ "Algo bom",
                         Q22 == 2 ~ "Algo ruim",
                         Q22 == 3 ~ "Nenhuma das opções",
                         Q22 == 98 ~ NA_character_),
         Q23 = case_when(Q23 == 0 ~ 0,
                         Q23 == 1 ~ 1,
                         Q23 == 2 ~ 2,
                         Q23 == 3 ~ 3,
                         Q23 == 4 ~ 4,
                         Q23 == 5 ~ 5,
                         Q23 == 6 ~ 6,
                         Q23 == 7 ~ 7,
                         Q23 == 8 ~ 8,
                         Q23 == 9 ~ 9,
                         Q23 == 10 ~ 10,
                         Q23 == 97 ~ NA_real_,
                         Q23 == 98 ~ NA_real_),
         Q11 = case_when(Q11 == 0 ~ "Extrema-esquerda",
                         Q11 == 1 ~ "Extrema-esquerda",
                         Q11 == 2 ~ "Esquerda",
                         Q11 == 3 ~ "Esquerda",
                         Q11 == 4 ~ "Centro-esquerda",
                         Q11 == 5 ~ "Centro",
                         Q11 == 6 ~ "Centro-direita",
                         Q11 == 7 ~ "Direita",
                         Q11 == 8 ~ "Direita",
                         Q11 == 9 ~ "Extrema-direita",
                         Q11 == 10 ~ "Extrema-direita",
                         Q11 == 97 ~ NA_character_,
                         Q11 == 98 ~ NA_character_),
         Q19 = case_when(Q19 == 1 ~ 1,
                         Q19 == 2 ~ 2,
                         Q19 == 3 ~ 3,
                         Q19 == 4 ~ 4,
                         Q19 == 5 ~ 5,
                         Q19 == 98 ~ NA_real_),
         Q20 = case_when(Q20 == 1 ~ 1,
                         Q20 == 2 ~ 2,
                         Q20 == 3 ~ 3,
                         Q20 == 4 ~ 4,
                         Q20 == 5 ~ 5,
                         Q20 == 98 ~ NA_real_),
         Q18_1 = case_when(Q18_1 == 1 ~ 1,
                           Q18_1 == 2 ~ 2,
                           Q18_1 == 3 ~ 3,
                           Q18_1 == 4 ~ 4,
                           Q18_1 == 5 ~ 5,
                           Q18_1 == 98 ~ NA_real_,
                           Q18_1 == 99 ~ NA_real_),
         Q18_2 = case_when(Q18_2 == 1 ~ 1,
                           Q18_2 == 2 ~ 2,
                           Q18_2 == 3 ~ 3,
                           Q18_2 == 4 ~ 4,
                           Q18_2 == 5 ~ 5,
                           Q18_2 == 98 ~ NA_real_,
                           Q18_2 == 99 ~ NA_real_),
         Q6 = case_when(Q6 == 1 ~ 1,
                        Q6 == 2 ~ 0,
                        Q6 == 98 ~ NA_real_,
                        Q6 == 99 ~ NA_real_),
         D3 = case_when(D3 == 1 ~ "Masculino",
                        D3 == 2 ~ "Feminino",
                        D3 == 3 ~ NA_character_),
         D4_1 = case_when(D4_1 >= 1994 ~ "16 a 25 anos",
                          D4_1 <= 1993 & D4_1 > 1983 ~ "26 a 35 anos",
                          D4_1 <= 1983 & D4_1 > 1973 ~ "36 a 45 anos",
                          D4_1 <= 1973 & D4_1 > 1963 ~ "46 a 55 anos",
                          D4_1 <= 1963 & D4_1 > 1953 ~ "56 a 65 anos",
                          D4_1 <= 1953 ~ "66 anos ou mais"),
         D6 = case_when(D6 == 1 ~ "Autônomo(a)",
                        D6 == 2 ~ "Empregado(a)",
                        D6 == 3 ~ "Estudante",
                        D6 == 4 ~ "Doméstico(a)",
                        D6 == 5 ~ "Aposentado(a)",
                        D6 == 6 ~ "Desempregado(a)",
                        D6 == 7 ~ "Outro",
                        D6 == 98 ~ NA_character_,
                        D6 == 99 ~ NA_character_),
         D7 = case_when(D7 == 1 ~ "Classe Trabalhadora",
                        D7 == 2 ~ "Classe Média Baixa",
                        D7 == 3 ~ "Classe Média",
                        D7 == 4 ~ "Classe Média Alta",
                        D7 == 5 ~ "Classe Alta",
                        D7 == 6 ~ "Outro",
                        D7 == 7 ~ NA_character_,
                        D7 == 98 ~ NA_character_,
                        D7 == 99 ~ NA_character_),
         D8 = case_when(D8 == 1 ~ "Área rural ou vilarejo",
                        D8 == 2 ~ "Cidade de pequeno ou médio porte",
                        D8 == 3 ~ "Cidade de grande porte"),
         D9 = case_when(D9 == 1 ~ "Católica",
                        D9 == 2 ~ "Ortodoxa",
                        D9 == 3 ~ "Protestante",
                        D9 == 4 ~ "Outro Cristianismo",
                        D9 == 5 ~ "Judeu",
                        D9 == 6 ~ "Muçulmano",
                        D9 == 7 ~ "Ex. Or.",
                        D9 == 8 ~ "Ex. Or.",
                        D9 == 9 ~ "Ex. Or.",
                        D9 == 10 ~ "Ateísta",
                        D9 == 11 ~ "Agnóstico",
                        D9 == 12 ~ "Outra",
                        D9 == 98 ~ NA_character_,
                        D9 == 99 ~ NA_character_),
         hCountry = case_when(hCountry == 1 ~ "Áustria",
                              hCountry == 2 ~ "Bélgica",
                              hCountry == 3 ~ "Bulgária",
                              hCountry == 4 ~ "Croácia",
                              hCountry == 5 ~ "Chipre",
                              hCountry == 6 ~ "República Tcheca",
                              hCountry == 7 ~ "Dinamarca",
                              hCountry == 8 ~ "Alemanha",
                              hCountry == 9 ~ "Estônia",
                              hCountry == 10 ~ "Finlândia",
                              hCountry == 11 ~ "França",
                              hCountry == 12 ~ "Grécia",
                              hCountry == 13 ~ "Hungria",
                              hCountry == 14 ~ "Irlanda",
                              hCountry == 15 ~ "Itália",
                              hCountry == 16 ~ "Letônia",
                              hCountry == 17 ~ "Lituânia",
                              hCountry == 18 ~ "Luxemburgo",
                              hCountry == 19 ~ "Malta",
                              hCountry == 20 ~ "Países Baixos",
                              hCountry == 21 ~ "Polônia",
                              hCountry == 22 ~ "Portugal",
                              hCountry == 23 ~ "Romênia",
                              hCountry == 24 ~ "Eslovênia",
                              hCountry == 25 ~ "Eslováquia",
                              hCountry == 26 ~ "Espanha",
                              hCountry == 27 ~ "Suécia",
                              hCountry == 28 ~ "Reino Unido")) %>%
  rename(Interesse_na_política = Q21,
         Informação_política = Q8,
         Oposição_gov_nac = Q5,
         Rejeição_membresia = Q22,
         Rejeição_aprofundamento = Q23,
         Extremismo_ideológico = Q11,
         Pessimismo_ano_anterior = Q19,
         Pessimismo_ano_futuro = Q20,
         Desconfiança_gov_nac = Q18_1,
         Desconfiança_do_PE = Q18_2,
         Turnout = Q6,
         Gênero = D3,
         Faixa_Etária = D4_1,
         Emprego = D6,
         Classe_Social = D7,
         Urbanização = D8,
         Religião = D9,
         País = hCountry)

#Transformação para fatores

banco_recodificado$Oposição_gov_nac <- as.factor(banco_recodificado$Oposição_gov_nac)
banco_recodificado$Rejeição_membresia <- as.factor(banco_recodificado$Rejeição_membresia)
banco_recodificado$Extremismo_ideológico <- as.factor(banco_recodificado$Extremismo_ideológico)
banco_recodificado$País <- as.factor(banco_recodificado$País)
banco_recodificado$Gênero <- as.factor(banco_recodificado$Gênero)
banco_recodificado$Faixa_Etária <- as.factor(banco_recodificado$Faixa_Etária)
banco_recodificado$Emprego <- as.factor(banco_recodificado$Emprego)
banco_recodificado$Classe_Social <- as.factor(banco_recodificado$Classe_Social)
banco_recodificado$Urbanização <- as.factor(banco_recodificado$Urbanização)
banco_recodificado$Religião <- as.factor(banco_recodificado$Religião)

# Retirando NAs: Banco Final

banco_final <- na.omit(banco_recodificado)

# Atribuindo categorias de referência

banco_final$Extremismo_ideológico <- fct_relevel(banco_final$Extremismo_ideológico,
                                                 "Centro")

banco_final$País <- fct_relevel(banco_final$País,
                                "Alemanha")

banco_final$Faixa_Etária <- fct_relevel(banco_final$Faixa_Etária,
                                        "66 anos ou mais")

banco_final$Emprego <- fct_relevel(banco_final$Emprego,
                                   "Empregado(a)")

banco_final$Classe_Social <- fct_relevel(banco_final$Classe_Social,
                                         "Classe Média")

banco_final$Religião <- fct_relevel(banco_final$Religião,
                                    "Católica")

# Regressão Geral

Regressão_log_geral <- glm(Turnout ~ Oposição_gov_nac + Extremismo_ideológico + 
                             Pessimismo_ano_anterior + Pessimismo_ano_futuro +
                             Rejeição_membresia + Rejeição_aprofundamento + 
                             Desconfiança_do_PE + Desconfiança_gov_nac + 
                             Interesse_na_política + Informação_política + 
                             Interesse_na_política*Informação_política + País + 
                             Gênero + Faixa_Etária + Emprego + Classe_Social + 
                             Urbanização + Religião,
                           data = banco_final, family = "binomial")


# Análise descritiva: Turnout

library(ggplot2)
library(scales)

ggplot(banco_final, aes(Turnout, ..count../sum(..count..))) +
  geom_bar() +
  scale_y_continuous(labels = percent)

summary(banco_final$Turnout)

# Análise descritiva: Turnout desagregado por país, faixa etária e gênero

str(banco_final)

banco_final %>%
  group_by(País) %>%
  summarise(mediana = median(Turnout),
            media = mean(Turnout),
            desvio = sd(Turnout),
            n = n()) %>%
  print(tbl_df(df), n=28)

banco_final %>%
  group_by(Faixa_Etária) %>%
  summarise(mediana = median(Turnout),
            media = mean(Turnout),
            desvio = sd(Turnout),
            n = n())

banco_final %>%
  group_by(Gênero) %>%
  summarise(mediana = median(Turnout),
            media = mean(Turnout),
            desvio = sd(Turnout),
            n = n())

t.test(Turnout ~ Gênero, data = banco_final)

# Análise descritiva: Oposição ao governo nacional

ggplot(banco_final, aes(Oposição_gov_nac, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent)

summary(banco_final$Oposição_gov_nac)

# Análise descritiva: Oposição ao governo nacional desagregado

tabela_oposição_país <- table(banco_final$País, banco_final$Oposição_gov_nac)
tabela_oposição_faixa_etária <- table(banco_final$Faixa_Etária, banco_final$Oposição_gov_nac)
tabela_oposição_gênero <- table(banco_final$Gênero, banco_final$Oposição_gov_nac)

prop.table(tabela_oposição_país, 1)
prop.table(tabela_oposição_faixa_etária, 1)
prop.table(tabela_oposição_gênero, 1)

chisq.test(tabela_oposição_país)
chisq.test(tabela_oposição_faixa_etária)
chisq.test(tabela_oposição_gênero)

# Análise descritiva: extremismo ideológico

ggplot(banco_final, aes(Extremismo_ideológico, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(banco_final$Extremismo_ideológico)

# Análise descritiva: extremismo ideológico desagregado

tabela_extremismo_país <- table(banco_final$País, banco_final$Extremismo_ideológico)
tabela_extremismo_faixa_etária <- table(banco_final$Faixa_Etária, banco_final$Extremismo_ideológico)
tabela_extremismo_gênero <- table(banco_final$Gênero, banco_final$Extremismo_ideológico)

prop.table(tabela_extremismo_país, 1)
prop.table(tabela_extremismo_faixa_etária, 1)
prop.table(tabela_extremismo_gênero, 1)

chisq.test(tabela_extremismo_país)
chisq.test(tabela_extremismo_faixa_etária)
chisq.test(tabela_extremismo_gênero)

# Análise descritiva: Pessimismo para o ano anterior

ggplot(banco_final, aes(Pessimismo_ano_anterior, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent)

ggplot(banco_final, aes(x = "", y = Pessimismo_ano_anterior)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

summary(banco_final$Pessimismo_ano_anterior)

# Análise descritiva: Pessimismo para o ano anterior desagregado

banco_final %>%
  group_by(País) %>%
  summarise(mediana = median(Pessimismo_ano_anterior),
            media = mean(Pessimismo_ano_anterior),
            desvio = sd(Pessimismo_ano_anterior),
            n = n()) %>%
  print(tbl_df(df), n=28)

banco_final %>%
  group_by(Faixa_Etária) %>%
  summarise(mediana = median(Pessimismo_ano_anterior),
            media = mean(Pessimismo_ano_anterior),
            desvio = sd(Pessimismo_ano_anterior),
            n = n())

banco_final %>%
  group_by(Gênero) %>%
  summarise(mediana = median(Pessimismo_ano_anterior),
            media = mean(Pessimismo_ano_anterior),
            desvio = sd(Pessimismo_ano_anterior),
            n = n())

t.test(Pessimismo_ano_anterior ~ Gênero, data = banco_final)

# Análise descritiva: pessimismo para o ano posterior

ggplot(banco_final, aes(Pessimismo_ano_futuro, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent)

ggplot(banco_final, aes(x = "", y = Pessimismo_ano_futuro)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

summary(banco_final$Pessimismo_ano_futuro)

# Análise descritiva: pessimismo para o ano posterior desagregado

banco_final %>%
  group_by(País) %>%
  summarise(mediana = median(Pessimismo_ano_futuro),
            media = mean(Pessimismo_ano_futuro),
            desvio = sd(Pessimismo_ano_futuro),
            n = n()) %>%
  print(tbl_df(df), n=28)

banco_final %>%
  group_by(Faixa_Etária) %>%
  summarise(mediana = median(Pessimismo_ano_futuro),
            media = mean(Pessimismo_ano_futuro),
            desvio = sd(Pessimismo_ano_futuro),
            n = n())

banco_final %>%
  group_by(Gênero) %>%
  summarise(mediana = median(Pessimismo_ano_futuro),
            media = mean(Pessimismo_ano_futuro),
            desvio = sd(Pessimismo_ano_futuro),
            n = n())

t.test(Pessimismo_ano_futuro ~ Gênero, data = banco_final)

cor.test(banco_final$Pessimismo_ano_futuro, banco_final$Pessimismo_ano_anterior)

# Análise descritiva: Rejeição à membresia

ggplot(banco_final, aes(Rejeição_membresia, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent)

summary(banco_final$Rejeição_membresia)

# Análise descritiva: rejeição à membresia desagregado

tabela_membresia_país <- table(banco_final$País, banco_final$Rejeição_membresia)
tabela_membresia_faixa_etária <- table(banco_final$Faixa_Etária, banco_final$Rejeição_membresia)
tabela_membresia_gênero <- table(banco_final$Gênero, banco_final$Rejeição_membresia)

prop.table(tabela_membresia_país, 1)
prop.table(tabela_membresia_faixa_etária, 1)
prop.table(tabela_membresia_gênero, 1)

chisq.test(tabela_membresia_país)
chisq.test(tabela_membresia_faixa_etária)
chisq.test(tabela_membresia_gênero)

# Análise descritiva: rejeição ao aprofundamento da integração

ggplot(banco_final, aes(Rejeição_aprofundamento, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent)

ggplot(banco_final, aes(x = "", y = Rejeição_aprofundamento)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

summary(banco_final$Rejeição_aprofundamento)

# Análise descritiva: rejeição ao aprofundamento da integração desagregado

banco_final %>%
  group_by(País) %>%
  summarise(mediana = median(Rejeição_aprofundamento),
            media = mean(Rejeição_aprofundamento),
            desvio = sd(Rejeição_aprofundamento),
            n = n()) %>%
  print(tbl_df(df), n=28)

banco_final %>%
  group_by(Faixa_Etária) %>%
  summarise(mediana = median(Rejeição_aprofundamento),
            media = mean(Rejeição_aprofundamento),
            desvio = sd(Rejeição_aprofundamento),
            n = n())

banco_final %>%
  group_by(Gênero) %>%
  summarise(mediana = median(Rejeição_aprofundamento),
            media = mean(Rejeição_aprofundamento),
            desvio = sd(Rejeição_aprofundamento),
            n = n())

t.test(Rejeição_aprofundamento ~ Gênero, data = banco_final)

# Análise descritiva: desconfiança do PE

ggplot(banco_final, aes(Desconfiança_do_PE, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent)

ggplot(banco_final, aes(x = "", y = Desconfiança_do_PE)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

summary(banco_final$Desconfiança_do_PE)

# Análise descritva: desconfiança do PE desagregado

banco_final %>%
  group_by(País) %>%
  summarise(mediana = median(Desconfiança_do_PE),
            media = mean(Desconfiança_do_PE),
            desvio = sd(Desconfiança_do_PE),
            n = n()) %>%
  print(tbl_df(df), n=28)

banco_final %>%
  group_by(Faixa_Etária) %>%
  summarise(mediana = median(Desconfiança_do_PE),
            media = mean(Desconfiança_do_PE),
            desvio = sd(Desconfiança_do_PE),
            n = n())

banco_final %>%
  group_by(Gênero) %>%
  summarise(mediana = median(Desconfiança_do_PE),
            media = mean(Desconfiança_do_PE),
            desvio = sd(Desconfiança_do_PE),
            n = n())

t.test(Desconfiança_do_PE ~ Gênero, data = banco_final)

cor.test(banco_final$Desconfiança_do_PE, banco_final$Rejeição_aprofundamento)

# Análise descritiva: desconfiança do governo nacional

ggplot(banco_final, aes(Desconfiança_gov_nac, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent)

ggplot(banco_final, aes(x = "", y = Desconfiança_gov_nac)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

summary(banco_final$Desconfiança_gov_nac)

# Análise descritiva: desconfiança do governo nacional desgregado

banco_final %>%
  group_by(País) %>%
  summarise(mediana = median(Desconfiança_gov_nac),
            media = mean(Desconfiança_gov_nac),
            desvio = sd(Desconfiança_gov_nac),
            n = n()) %>%
  print(tbl_df(df), n=28)

banco_final %>%
  group_by(Faixa_Etária) %>%
  summarise(mediana = median(Desconfiança_gov_nac),
            media = mean(Desconfiança_gov_nac),
            desvio = sd(Desconfiança_gov_nac),
            n = n())

banco_final %>%
  group_by(Gênero) %>%
  summarise(mediana = median(Desconfiança_gov_nac),
            media = mean(Desconfiança_gov_nac),
            desvio = sd(Desconfiança_gov_nac),
            n = n())

t.test(Desconfiança_gov_nac ~ Gênero, data = banco_final)

cor.test(banco_final$Desconfiança_gov_nac, banco_final$Desconfiança_do_PE)

# Análise descritiva: Interesse na política

ggplot(banco_final, aes(Interesse_na_política, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent)

ggplot(banco_final, aes(x = "", y = Interesse_na_política)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

summary(banco_final$Interesse_na_política)

# Análise descritiva: Interesse na política desagregado

banco_final %>%
  group_by(País) %>%
  summarise(mediana = median(Interesse_na_política),
            media = mean(Interesse_na_política),
            desvio = sd(Interesse_na_política),
            n = n()) %>%
  print(tbl_df(df), n=28)

banco_final %>%
  group_by(Faixa_Etária) %>%
  summarise(mediana = median(Interesse_na_política),
            media = mean(Interesse_na_política),
            desvio = sd(Interesse_na_política),
            n = n())

banco_final %>%
  group_by(Gênero) %>%
  summarise(mediana = median(Interesse_na_política),
            media = mean(Interesse_na_política),
            desvio = sd(Interesse_na_política),
            n = n())

t.test(Interesse_na_política ~ Gênero, data = banco_final)

# Análise descritiva: Informação do eleitor

ggplot(banco_final, aes(Informação_política, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent)

ggplot(banco_final, aes(x = "", y = Informação_política)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

summary(banco_final$Informação_política)

# Análise descritiva: informação do eleitor desagregado

banco_final %>%
  group_by(País) %>%
  summarise(mediana = median(Informação_política),
            media = mean(Informação_política),
            desvio = sd(Informação_política),
            n = n()) %>%
  print(tbl_df(df), n=28)

banco_final %>%
  group_by(Faixa_Etária) %>%
  summarise(mediana = median(Informação_política),
            media = mean(Informação_política),
            desvio = sd(Informação_política),
            n = n())

banco_final %>%
  group_by(Gênero) %>%
  summarise(mediana = median(Informação_política),
            media = mean(Informação_política),
            desvio = sd(Informação_política),
            n = n())

t.test(Informação_política ~ Gênero, data = banco_final)

cor.test(banco_final$Informação_política, banco_final$Interesse_na_política)

# Análise descritiva: País

ggplot(banco_final, aes(País, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(banco_final$País)

# Análise descritiva: Gênero

ggplot(banco_final, aes(Gênero, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent)

summary(banco_final$Gênero)

# Análise descritiva: Faixa Etária

ggplot(banco_final, aes(Faixa_Etária, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(banco_final$Faixa_Etária)

ggplot(banco_recodificado, aes(Faixa_Etária, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(banco_recodificado$Faixa_Etária)

# Análise descritiva: emprego

ggplot(banco_final, aes(Emprego, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(banco_final$Emprego)

ggplot(banco_recodificado, aes(Emprego, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(banco_recodificado$Emprego)

# Análise descritiva: classe social

ggplot(banco_final, aes(Classe_Social, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(banco_final$Classe_Social)

ggplot(banco_recodificado, aes(Classe_Social, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(banco_recodificado$Classe_Social)

# Análise descritiva: urbanização

ggplot(banco_final, aes(Urbanização, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(banco_final$Urbanização)

ggplot(banco_recodificado, aes(Urbanização, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(banco_recodificado$Urbanização)

# Análise descritiva: religião

ggplot(banco_final, aes(Religião, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(banco_final$Religião)

ggplot(banco_recodificado, aes(Religião, ..count../sum(..count..))) +
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(banco_recodificado$Religião)

# Pressupostos da regressão: linearidade

plot(Regressão_log_geral, 1)

# Pessupostos da regressão: Homocedasticidade

library(lmtest)

plot(Regressão_log_geral, 3)
plot(Regressão_log_geral, 1)

bptest(Regressão_log_geral)

# Pressupsotos da regressão: ausência de autocorrelação entre os resíduos

library(car)

acf(Regressão_log_geral$residuals)

durbinWatsonTest(Regressão_log_geral)

# Pressupsotos da regressão: normalidade dos resíduos

plot(Regressão_log_geral, 2)

# Pressupsotos da regressão: multicolinearidade

vif(Regressão_log_geral)

# Modelos alternativos

Banco_final_Europa_Oc <- banco_final %>% 
  filter(Ex_comunista == 0)

Banco_final_Ex_Comunista <- banco_final %>%
  filter(Ex_comunista == 1)

Banco_final_Sanções <- banco_final %>%
  filter(Sanções == 1)

Regressão_log_Europa_Oc <- glm(Turnout ~ Oposição_gov_nac + Extremismo_ideológico + 
                                 Pessimismo_ano_anterior + Pessimismo_ano_futuro +
                                 Rejeição_membresia + Rejeição_aprofundamento + 
                                 Desconfiança_do_PE + Desconfiança_gov_nac + 
                                 Interesse_na_política + Informação_política + 
                                 Interesse_na_política*Informação_política + País + 
                                 Gênero + Faixa_Etária + Emprego + Classe_Social + 
                                 Urbanização + Religião,
                               data = Banco_final_Europa_Oc, family = "binomial")

Regressão_log_Ex_Comunista <- glm(Turnout ~ Oposição_gov_nac + Extremismo_ideológico + 
                                    Pessimismo_ano_anterior + Pessimismo_ano_futuro +
                                    Rejeição_membresia + Rejeição_aprofundamento + 
                                    Desconfiança_do_PE + Desconfiança_gov_nac + 
                                    Interesse_na_política + Informação_política + 
                                    Interesse_na_política*Informação_política + País + 
                                    Gênero + Faixa_Etária + Emprego + Classe_Social + 
                                    Urbanização + Religião,
                                  data = Banco_final_Ex_Comunista, family = "binomial")

Regressão_log_Sanções <- glm(Turnout ~ Oposição_gov_nac + Extremismo_ideológico + 
                               Pessimismo_ano_anterior + Pessimismo_ano_futuro +
                               Rejeição_membresia + Rejeição_aprofundamento + 
                               Desconfiança_do_PE + Desconfiança_gov_nac + 
                               Interesse_na_política + Informação_política + 
                               Interesse_na_política*Informação_política + País + 
                               Gênero + Faixa_Etária + Emprego + Classe_Social + 
                               Urbanização + Religião,
                             data = Banco_final_Sanções, family = "binomial")

Banco_final_Europa_Oc$País <- as.factor(as.character(Banco_final_Europa_Oc$País))

Banco_final_Ex_Comunista$País <- as.factor(as.character(Banco_final_Ex_Comunista$País))

Banco_final_Sanções$País <- as.factor(as.character(Banco_final_Sanções$País))

# Testes: modelo geral

summary(Regressão_log_geral)

exp(cbind(coef(Regressão_log_geral), confint(Regressão_log_geral)))

library(sjPlot)

plot_model(Regressão_log_geral, type = "pred", 
           terms = c("Interesse_na_política", "Informação_política"), 
           ci.lvl = 0.9)

library(margins)

summary(margins(Regressão_log_geral))

plot_model(Regressão_log_geral)

# Testes: modelo Europa Ocidental

summary(Regressão_log_Europa_Oc)

exp(cbind(coef(Regressão_log_Europa_Oc), confint(Regressão_log_Europa_Oc)))

plot_model(Regressão_log_Europa_Oc)

# Testes: modelo Europa ex-comunista

options(scipen = 10)
summary(Regressão_log_Ex_Comunista)

exp(cbind(coef(Regressão_log_Ex_Comunista), confint(Regressão_log_Ex_Comunista)))

plot_model(Regressão_log_Ex_Comunista)

# Testes: modelo países com sanções

options(scipen = 10)
summary(Regressão_log_Sanções)

exp(cbind(coef(Regressão_log_Sanções), confint(Regressão_log_Sanções)))

# Ajuste do modelo: pseudo R2

library(pscl)

pR2(Regressão_log_geral)
pR2(Regressão_log_Europa_Oc)
pR2(Regressão_log_Ex_Comunista)
pR2(Regressão_log_Sanções)

# Classificações corretas

## Geral

library(InformationValue)

predicted_prob_geral <- predict(Regressão_log_geral, type = "response")

1 - misClassError(banco_final$Turnout,
                  predicted_prob_geral, threshold = 0.5)

opt_cutoff_geral <- optimalCutoff(banco_final$Turnout, predicted_prob_geral)

1 - misClassError(banco_final$Turnout,
                  predicted_prob_geral, threshold = opt_cutoff_geral)

confusionMatrix(banco_final$Turnout, predicted_prob_geral, threshold = opt_cutoff_geral)

prop.table(confusionMatrix(banco_final$Turnout, 
                           predicted_prob_geral, 
                           threshold = opt_cutoff_geral))

## Europa Ocidental

predicted_prob_Europa_Oc <- predict(Regressão_log_Europa_Oc, type = "response")

1 - misClassError(Banco_final_Europa_Oc$Turnout,
                  predicted_prob_Europa_Oc, threshold = 0.5)

opt_cutoff_Europa_Oc <- optimalCutoff(Banco_final_Europa_Oc$Turnout, predicted_prob_Europa_Oc)

1 - misClassError(Banco_final_Europa_Oc$Turnout,
                  predicted_prob_Europa_Oc, threshold = opt_cutoff_Europa_Oc)

confusionMatrix(Banco_final_Europa_Oc$Turnout, predicted_prob_Europa_Oc, threshold = opt_cutoff_Europa_Oc)

prop.table(confusionMatrix(Banco_final_Europa_Oc$Turnout, 
                           predicted_prob_Europa_Oc, 
                           threshold = opt_cutoff_Europa_Oc))

## Ex-comunistas

predicted_prob_Ex_Comunista <- predict(Regressão_log_Ex_Comunista, type = "response")

1 - misClassError(Banco_final_Ex_Comunista$Turnout,
                  predicted_prob_Ex_Comunista, threshold = 0.5)

opt_cutoff_Ex_Comunista <- optimalCutoff(Banco_final_Ex_Comunista$Turnout, predicted_prob_Ex_Comunista)

1 - misClassError(Banco_final_Ex_Comunista$Turnout,
                  predicted_prob_Ex_Comunista, threshold = opt_cutoff_Ex_Comunista)

confusionMatrix(Banco_final_Ex_Comunista$Turnout, predicted_prob_Ex_Comunista, threshold = opt_cutoff_Ex_Comunista)

prop.table(confusionMatrix(Banco_final_Ex_Comunista$Turnout, 
                           predicted_prob_Ex_Comunista, 
                           threshold = opt_cutoff_Ex_Comunista))

## Sanções

predicted_prob_Sanções <- predict(Regressão_log_Sanções, type = "response")

1 - misClassError(Banco_final_Sanções$Turnout,
                  predicted_prob_Sanções, threshold = 0.5)

opt_cutoff_Sanções <- optimalCutoff(Banco_final_Sanções$Turnout, predicted_prob_Sanções)

1 - misClassError(Banco_final_Sanções$Turnout,
                  predicted_prob_Sanções, threshold = opt_cutoff_Sanções)

confusionMatrix(Banco_final_Sanções$Turnout, predicted_prob_Sanções, threshold = opt_cutoff_Sanções)

prop.table(confusionMatrix(Banco_final_Sanções$Turnout, 
                           predicted_prob_Sanções, 
                           threshold = opt_cutoff_Sanções))