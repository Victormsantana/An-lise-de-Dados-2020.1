#
# Suponha que tenhamos o dataframe df abaixo
#
# x     y
# A     5
# A     3
# B     8
# B    12
#
# Complete o código que obtém o seguinte resultado:
#
#        z
#        7
#

df %>%
?(z = mean(y))

x <- c("A", "A", "B", "B")
y <- c(5, 3, 8, 12)

df <- data.frame(x, y)

df %>% summarise(z = mean(y))

#######################################################################

# Suponha que tenhamos o dataframe df abaixo
#
# y1    y2    y3    y4
# 8.04  9.14  7.46  6.58
# 6.95  8.14  6.77  5.76
# 7.58  8.74  12.74 7.71
#
# Complete o código que obtém o seguinte resultado:
#
# y1    
# 8.04  
# 6.95  
# 7.58  

df %>%
?

  y1 <- c(8.04, 6.95, 7.58)
y2 <- c(9.14, 8.14, 8.74)
y3 <- c(7.46, 6.77, 12.74)
y4 <- c(6.58, 5.76, 7.71)

df_02 <- data.frame(y1, y2, y3, y4)

df_02

df_02 %>% select(y1)

  
#######################################################################

# Suponha que tenhamos o dataframe df abaixo
#
#    x  y
#   1  10
#   6  8
#   2  3
#   4  5
#
# Complete o código que obtém o seguinte resultado, fazendo uma operação
# entre x e y
#
#    x  y   z
#   1  10  -9
#   6  8   -2
#   2  3   -1
#   4  5   -1
#

df %>%
?
  
x <- c(1, 6, 2, 4)
y <- c(10, 8, 3, 5)

df_03 <- data.frame(x, y)

df_03

z <- c((x-y))

df_03_adicionado <- df_03 %>% mutate(z)

df_03_adicionado
  
########################################################################

#
# Suponha que tenhamos o dataframe df abaixo
#
#    city sales
# Boston   220
# Boston   125
#    NYC   150
#    NYC   250
#
# Complete o código que obtém o seguinte resultado:
#
# city   avg_sales
# Boston      172
# NYC         200 

df %>%
  ? %>%
  summarise(avg_sales = mean(sales))

city <- c("Boston", "Boston", "NYC", "NYC")
sales <- c(220, 125, 150, 250)

df_04 <- data.frame(city, sales)

df_04

df_04 %>% group_by(city) %>% summarise(avg_sales = mean(sales))
  
########################################################################

# Suponha que tenhamos o dataframe df abaixo
#
#week   min   max
#  3    55    60
#  2    52    56
#  1    60    63
#  4    65    67
#
# Complete o código que obtém o seguinte resultado:
#
#week   min   max
#  1    60    63
#  2    52    56
#  3    55    60
#  4    65    67

df %>%
  ?

week <- c(3, 2, 1, 4)
min <- c(55, 52, 60, 65)
max <- c(60, 56, 63, 67)

df_05 <- data.frame(week, min, max)

df_05

df_05_reorganizado <- df_05 %>% arrange(week)

df_05_reorganizado

########################################################################

# Suponha que tenhamos o dataframe df abaixo
#
# x_b_1  x_b_2  y_c_1  y_c_2
#  A      2      W1     25
#  A      4      W2     21
#  B      6      W1     26
#  B      8      W2     30
#
# Complete o código que obtém o seguinte resultado:
#
# y_c_1  y_c_2
#  W1     25
#  W2     21
#  W1     26
#  W2     30

df %>%
select(?(?))

x_b_1 <- c("A", "A", "B", "B")
x_b_2 <- c(2, 4, 6, 8)
y_c_1 <- c("W1", "W2", "W1", "W2")
y_c_2 <- c(25, 21, 26, 30)

df_06 <- data.frame(x_b_1, x_b_2, y_c_1, y_c_2)

df_06

df_06_selecionado <- df_06 %>% select(contains("c"))

df_06_selecionado

#########################################################################

# Suponha que tenhamos o dataframe df abaixo
#
# Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# 78           6.7         3.0          5.0         1.7 versicolor
# 121          6.9         3.2          5.7         2.3  virginica
# 11           5.4         3.7          1.5         0.2     setosa
# 92           6.1         3.0          4.6         1.4 versicolor
# 146          6.7         3.0          5.2         2.3  virginica
# 62           5.9         3.0          4.2         1.5 versicolor
# 50           5.0         3.3          1.4         0.2     setosa
# 17           5.4         3.9          1.3         0.4     setosa
# 69           6.2         2.2          4.5         1.5 versicolor
# 143          5.8         2.7          5.1         1.9  virginica
#
# Complete o código que obtém o seguinte resultado:
#
#Species      Sepal.Area
#versicolor      20.10
#virginica       22.08
#setosa          19.98
#versicolor      18.30
#virginica       20.10
#versicolor      17.70
#setosa          16.50
#setosa          21.06
#versicolor      13.64
#virginica      15.66


df %>%
  ?(?)

Sepal.Length <- c(6.7, 6.9, 5.4, 6.1, 6.7, 5.9, 5.0, 5.4, 6.2, 5.8)
Sepal.Width <- c(3.0, 3.2, 3.7, 3.0, 3.0, 3.0, 3.3, 3.9, 2.2, 2.7)
Petal.Length <- c(5.0, 5.7, 1.5, 4.6, 5.2, 4.2, 1.4, 1.3, 4.5, 5.1)
Petal.Width <- c(1.7, 2.3, 0.2, 1.4, 2.3, 1.5, 0.2, 0.2, 1.5, 1.9)
Species <- c("versicolor", "virginica", "setosa", "versicolor", "virginica", "versicolor", "setosa", "setosa", "versicolor", "virginica")

df_07 <- data.frame(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, Species)

df_07

df_07_modificada <- df_07 %>% transmute(Species, Sepal.Area = Sepal.Length*Sepal.Width)

df_07_modificada

########################################################################

# Suponha que tenhamos o dataframe df abaixo
#
#name         start       end         party     
#Eisenhower   1953-01-20  1961-01-20  Republican
#Kennedy      1961-01-20  1963-11-22  Democratic
#Johnson      1963-11-22  1969-01-20  Democratic
#Nixon        1969-01-20  1974-08-09  Republican
#Ford         1974-08-09  1977-01-20  Republican
#Carter       1977-01-20  1981-01-20  Democratic
#Reagan       1981-01-20  1989-01-20  Republican
#Bush         1989-01-20  1993-01-20  Republican
#Clinton      1993-01-20  2001-01-20  Democratic
#Bush         2001-01-20  2009-01-20  Republican
#Obama        2009-01-20  2017-01-20  Democratic
#
#Crie um código abaixo para que se altere a variável party
#deixando apenas a primeira letra dos partidos

df %>%
  ?

name <- c("Eisenhower", "Kennedy", "Johnson", "Nixon", "Ford", "Carter", "Reagan", "Bush", "Clinton", "Bush", "Obama")  
start <- c("1953-01-20", "1961-01-20",  "1963-11-22", "1969-01-20", "1974-08-09", "1977-01-20", "1981-01-20", "1989-01-20", "1993-01-20", "2001-01-20", "2009-01-20")
end <- c("1961-01-20", "1963-11-22", "1969-01-20", "1974-08-09", "1977-01-20", "1981-01-20", "1989-01-20", "1993-01-20", "2001-01-20", "2009-01-20", "2017-01-20")
party <- c("Republican", "Democratic", "Democratic", "Republican", "Republican", "Democratic", "Republican", "Republican", "Democratic", "Republican", "Democratic")

df_08 <- data.frame(name, start, end, party)

df_08

df_08_alterada <- df_08 %>% mutate(party = recode(party,
                                                  Republican = "R", 
                                                  Democratic = "D"))

df_08_alterada

###############################################################################

# No pacote poliscidata existe um banco de dados chamado nes, com informações 
# do American National Election Survey. Para os exerícicios a seguir, instale 
# o pacote poliscidata e tidyverse, carregue-os e crie um objeto chamado
# df com os dados do nes. 

install.packages("tidyverse")
install.packages("poliscidata")

library(tideverse)
library(poliscidata)

df_09 <- nes

# Faça uma primeira exploração do banco de dados com todos os comandos
# passados até aqui que possuem esse objetivo

glimpse(nes)
?nes
str(nes)
summary(nes)

# Quantos respondentes possui na pesquisa?

# RESPOSTA: 5916 respondentes

# Caso queiram ter mais informações sobre as variáveis do nes, basta rodar
# o código `?nes`, que no canto inferior direito aparecerá uma descrição.
# Como temos muitas variáveis, deixe apenas as colunas
# ftgr_cons, dem_raceeth, voted2012, science_use, preknow3, obama_vote
# income5, gender.


df_09_selecionado <- df_09 %>% select("ftgr_cons", "dem_raceeth", "voted2012", "science_use", "preknow3", "obama_vote", "income5", "gender")

df_09_selecionado


# Se quisermos ter informações apenas de pessoas que votaram na
# eleição de 2012, podemos usar a variável voted2012. Tire do banco
# os respondentes que não votaram

df_09_filtrado <- df_09_selecionado %>% filter(voted2012 == "Voted")

df_09_filtrado

df_09_filtrado %>% count(obama_vote)

# Quantos respondentes sobraram?

# RESPOSTA: Sobraram 4404 respondentes 



# Crie uma variável chamada white que indica se o respondente é branco
# ou não a partir da variável dem_raceeth, crie a variável ideology a
# partir da variável ftgr_cons (0-33 como liberal, 34 a 66 como centro,
# e 67 a 100 como conservador), ao mesmo tempo em que muda
# a variável obama_vote para trocar o 1 por "Sim" e 2 por "não"

df_09_alterado <- df_09_filtrado %>% mutate(white = recode(dem_raceeth, 
                                                           "1. White non-Hispanic" = "YES", 
                                                           "2. Black non-Hispanic" = "NO", 
                                                           "3. Hispanic" = "NO", 
                                                           "4. Other non-Hispanic" = "NO"), 
                                            ideology = case_when
                                            (ftgr_cons >= 0 & ftgr_cons < 34 ~ "Liberal", 
                                              ftgr_cons >= 34 & ftgr_cons < 67 ~ "Centro",
                                              ftgr_cons >= 67 ~ "Conservador"), 
                                            obama_vote = case_when
                                            (obama_vote == 1 ~ "Sim", 
                                              obama_vote == 0 ~ "Não"))
  
df_09_alterado

df_09_alterado %>% count(obama_vote, sort = T)

# Demonstre como observar a quantidade de pessoas em cada uma das
# categorias de science_use

df_09_alterado %>% count(science_use, sort = T)

# Demonstre como observar a média de conservadorismo (variável 
# ftgr_cons) para cada categoria de science_use

df_09_alterado %>% group_by(science_use) %>% 
  summarise(mean(ftgr_cons, na.rm = TRUE))
  
###############################################################################

# Responder as questões teóricas da aula abaixo

Artigo Selecionado: SCHMITT, H; TOYGÜR, I. European Parliament Elections of May 2014: 
  Driven by National Politics or EU Policy Making? Politics and Governance, 
v. 4, n. 1, p. 167-181, 2016. Disponível em: <https://www.cogitatiopress.com/politicsandgovernance/article/view/464/464>. 
Acesso em: 19/06/2020.

1.1 - Qual é a questão de pesquisa?
      Os autores visam estudar se as eleições de 2014 para o Parlamento Europeu
      foram de segunda-ordem, assim como prevê o modelo teórico; e se políticas
      iniciadas pela UE, mais especificamente a livre circulação de pessoas e a
      moeda única influenciaram no voto nessa eleição em específico. 
  
1.2 - Qual é a teoria?
      O modelo teórico adotado afirma que eleições em geral se dividem em duas
      categorias: eleições de primeira-ordem (de maior destaque na arena nacional,
      atraem mais eleitores e partidos maiores e/ou menos extremistas se saem melhor)
      e eleições de segunda-ordem (de menor destaque, em níveis sub ou supranacionais
      têm maiores taxas de abstenção e partidos menores e/ou de oposição e/ou mais 
      extremistas se saem melhor - em comparação com as eleições de primeira-ordem).
      Além disso, os ciclos eleitorais seriam altamente correlacoionados: eleições
      de segunda-ordem próximas das de primeira-ordem tendem a favorecer partidos
      governistas e eleições em meio de mandato (tendo como parâmetro as de primeira
      ordem) tendem a favorecer a oposição.
      As eleições para o Parlamento Europeu (PE) seriam de segunda-ordem.
      
1.3 - Qual é o desenho de pesquisa?
      O artigo consiste de hipóteses de nível nacional e de nível partidário.
      As hipóteses de nível nacional são testadas de maneira bivariada,
      comparando os resultados das eleições de primeira-ordem precedentes à
      eleição de 2014 para o PE nos respectivios países, em 28 casos - os 28
      países membros. As hipóteses de nível partidário são testadas em um 
      modelo multinível usando o pacote lme4 do R (versão 1.1-7) com 160 casos 
      (partidos representados no PE) no primeiro nível e 28 casos (países 
      membros) no segundo.

1.4 - Como o artigo se sai nos 4 quesitos de avaliação de causalidade?
      Quesito 1 - sim, a teoria foi formulada no início dos anos 80 e
                  já foi amplamente estudada por diversos autores, com
                  eleições diferentes em contextos e países diferentes.
      Quesito 2 - sim, as taxas de abstenções são mais altas em eleições
                  de segunda-ordem porque, por exemplo, os eleitores têm
                  a percepção de que "há menos em jogo", ou seja, há uma
                  menor percepção de improtância ou urgência. A abstenção
                  não pode causar mais ou menos percepção de "urgência" 
                  ou "importância" nos eleitores.
      Quesito 3 - sim, várias pesquisas já comprovaram que a abstenção é
                  maior em eleições desse tipo, assim como a % de votos para
                  partidos de oposição e/ou menores e/ou extemistas.
      Quesito 4 - sim, no artigo em questão, os autores pesquisam se questões
                  como maior imigração ou menor desempenho econômico afetam
                  as taxas de abstenção e o voto em partidos de oposição e/ou
                  extremistas.
                
1.5 - O que ele conclui?
      A tendência de que as eleições para o PE sejam consideradas pela
      população em geral como de segunda-ordem persiste, apesar do cenário
      de crise econômica e política. Não houve correlação entre imigração e
      crise econômica com maior apoio para a extrema direita e para partidos
      eurocéticos, respectivamente. 

1.6 - Como a sua pesquisa dá um passo a mais para o desenvolvimento teórico
      presente neste artigo?
      A minha pesquisa busca não só fazer uma meta-análise dos principais
      trabalhos da área desde o artigo inicial de Schmitt e Teif de 1980,
      como também busca definir os critérios que distinguem eleições de
      primeira e segunda ordens para então mensurá-los em cada eleição
      e comparar sua eleição de 1979 até 2019, buscando verificar se houve
      alguma mudança no caráter de segunda-ordem de tais eleições e se o
      avanço da direita eurocética está correlacionado com essa mudança, caso
      ela exista.

2 - O caráter de segunda-ordem das eleições para o PE mudou nas eleições mais
    recentes? Caso sim, o avanço da direita eurocética estaria correlacionado
    com essa mudança?

3 - Quesito 1 - sim, autores como Brack(2018) afirmam que a UE carecia de um
                espaço institucionalizado para a expessão da oposição via
                representação popular, uma vez que o PE é dominado por partidos
                e parlamentares pró-UE e que os eurocéticos que lá operam
                raramente conseguem destaque na atuação política interna, ou por
                boicote dos pró-UE, ou por não darem importância às instituições
                européias (uma vez que o próprio parlamentar é eurocético).
                Contudo, esse cenário tem gerado insatisfações no eleitorado,
                que teria votado mais recentemente em maior escala em partidos
                eurocéticos de forma a expressar seu descontentamento com as 
                políticas da UE, abrindo margem para uma institucionalização
                da oposição e dando uma maior percepção de urgência e importância
                às eleições UE, o que minimizaria seu caráter de segunda-ordem.
                Outros autores ressaltam a "europeizção" da política na Europa
                e que isso pode ter aumentado a percepção de importância dos
                assuntos políticos europeus para o eleitorado.
                Contudo, a própria teoria de eleições de segunda-ordem prevê que
                partidos de oposição se saiam melhor em determinados contextos
                em eleições do tipo, o que também deve ser levado em consideração.
    Quesito 2 - creio que não, não consigo imaginar os votos em si aumentando
                essa percepção de importância; o que pode ocorrer é, com o
                avanço da direita eurocética as campanhas eleitorais se 
                intensificarem mais, chamando a atenção do eleitor; contudo 
                isso depõe contra a a teoria de eleições de segunda-ordem,
                pois mostraria que tanto eleitores como partidos estariam
                valorizando mais as eleições para o PE.
    Quesito 3 - É o que minha pesquisa visa analisar :D
    Quesito 4 - Como dito, pode ser que, dentro de certos contextos inerentes a
                cada país (a combinação ou o distanciamento entre ciclos eleitorais
                de primeira e de segunda ordem, por exemplo), o melhor resultado
                obtido por partidos de oposição e/ou extremistas esteja de acordo
                com as previsões do modelo de eleições de segunda-ordem. Contudo, 
                tanto em 2014 como em 2019, em certos países, a direita eurocética
                conseguiu um destaque bem maior em comparação com todas as outras
                eleições, o que pode não ser necessariamente explicado pela teoria
                em questão.