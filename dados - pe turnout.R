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
  mutate(Q21 = case_when(Q21 == 1 ~ 4,
                       Q21 == 2 ~ 3,
                       Q21 == 3 ~ 2,
                       Q21 == 4 ~ 1,
                       Q21 == 98 ~ NA_real_,
                       Q21 == 99 ~ NA_real_),
         Q8 = case_when(Q8 == 0 ~ 0,
                        Q21 == 1 ~ 1,
                        Q21 == 2 ~ 2,
                        Q21 == 3 ~ 3,
                        Q21 == 4 ~ 4,
                        Q21 == 5 ~ 5,
                        Q21 == 6 ~ 6,
                        Q21 == 7 ~ 7,
                        Q21 == 8 ~ 8,
                        Q21 == 9 ~ 9,
                        Q21 == 10 ~ 10,
                        Q21 == 98 ~ NA_real_,
                        Q21 == 99 ~ NA_real_),
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
                                         "Classe média")

# Regressão

Regressão_log_geral <- glm(Turnout ~ Oposição_gov_nac + Extremismo_ideológico + 
                             Pessimismo_ano_anterior + Pessimismo_ano_futuro + 
                             Rejeição_membresia + Rejeição_aprofundamento + 
                             Desconfiança_do_PE + Interesse_na_política + 
                             Informação_política + Interesse_na_política*Informação_política + País + Gênero + Faixa_Etária + 
                             Emprego + Classe_Social + Urbanização + Religião,
                           data = banco_final, family = "binomial")

# Transformação para probabilidades

library(margins)
summary(margins(Regressão_log_geral))

efeito_marginal <- summary(margins(Regressão_log_sem_interação)) %>%
  rename(term = factor,
         estimate = AME,
         std.error = SE,
         statistic = z,
         p.value = p) %>%
  arrange(estimate)

dwplot(efeito_marginal)