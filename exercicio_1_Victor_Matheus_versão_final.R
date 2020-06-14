
# Entre no seguinte link:
# https://pt.wikipedia.org/wiki/EleiÃ§Ã£o_presidencial_no_Brasil_em_2002
# VÃ¡ atÃ© o tÃ³pico RESUMO DAS ELEICOES
# Crie um vetor com o nome dos seis candidatos a presidÃªncia

candidatos <- 

# Crie um vetor com a sigla do partido de cada candidato

partido <- 

# Crie um vetor com o total de votos de cada candidato
  
votos_candidatos <- 

# Crie um objeto calculando a soma do votos dos candidatos no 1o turno
  
total_votos <- 

# Crie um vetor com a porcentagem de votos de cada candidato
# fazendo uma operaÃ§Ã£o aritmÃ©tica entre o objeto votos_candidatos
# e o objeto total_votos

porcentagem_votos <- 

# Crie uma matriz que conste uma coluna com o total de votos de cada candidato
# e outra com a porcentagem dos votos de cada candidato

matriz_votos <- 

# Nomeie as linhas da matriz com o nome dos candidatos


# Nomeie tambÃ©m as colunas


# Crie um dataframe com o nome dos candidatos, o partido,
# a quantidade de votos e o percentual


# Crie um vetor lÃ³gico, indicado TRUE ou FALSE, com a informacao se
# o candidato foi para o segundo turno


# Adicione esta coluna no dataframe


# Calcule a soma da porcentagem dos dois candidatos que obtiveram mais votos


# Exiba as informaÃ§Ãµes do dataframe dos dois candidatos com mais votos

candidatos <- c("Luiz Inácio Lula da Silva" , "José Serra" , "Anthony Garotinho" , "Ciro Gomes" , "José Maria de Almeida" , "Rui Costa Pimenta" ) 

partido <- c("PT" , "PSDB" , "PSB" , "PPS" , "PSTU" , "PCO" )

votos_candidatos <- c(39455233 , 19705445 , 15180097 , 10170882 , 402236 , 38619 )

total_votos <- c(84952512)

porcentagem_votos <- c((votos_candidatos/total_votos)*100)

matriz_votos <- matrix(c(votos_candidatos, porcentagem_votos), byrow = FALSE, nrow = 6)

row.names(matriz_votos) <- candidatos

matriz_votos

colnames(matriz_votos) <- c("votos dos candidatos", "porcentagem dos votos totais")

matriz_votos

segundo_turno <- c(T, T, F, F, F, F)

matriz_votos_coluna_extra <- cbind(matriz_votos, segundo_turno)

matriz_votos_coluna_extra

matriz_votos_coluna_extra[1,2]+matriz_votos_coluna_extra[2,2]

matriz_votos_partido <- matrix (c(votos_candidatos, partido, porcentagem_votos), byrow = FALSE, nrow = 6)

matriz_votos_partido

row.names(matriz_votos_partido) <- candidatos

colnames(matriz_votos_partido) <- c("votos dos candidatos", "partido", "porcentagem dos votos locais")

matriz_votos_partido_coluna_extra <- cbind(matriz_votos_partido, segundo_turno)

matriz_votos_partido_coluna_extra

candidatos_segundo_turno <- data.frame(matriz_votos_partido_coluna_extra[1,], matriz_votos_partido_coluna_extra[2,])

candidatos_segundo_turno

col.names.data.frame(candidatos_segundo_turno) <- c(candidatos [1, 2])

candidatos_segundo_turno

#PROFESSOR, tentei mudar o nome para colocar o nome dos presidentes no dataframe mas não consegui


###############################################################################

# Substitua o sÃ�mbolo de interrogaÃ§Ã£o por um 
# cÃ³digo que retorne o seguinte resultado:
#
# [1] 24 18 31

q <- c(47, 24, 18, 33, 31, 15)
q[?]


q[c(2:3, 5)]


###############################################################################

# Substitua o sÃ�mbolo de interrogaÃ§Ã£o por um 
# cÃ³digo que retorne o seguinte resultado:
#
# Out Nov
#  24   2

x <- c(5, 4, 24, 2)
y <- c("Ago", "Set", "Out", "Nov")
names(x) <- y

x[?]

x[names(x)[3:4]]

###############################################################################

# Substitua o sÃ�mbolo de interrogaÃ§Ã£o por um 
# cÃ³digo que retorne o seguinte resultado:
#
# 'data.frame':	2 obs. of  2 variables:
# $ x: Factor w/ 2 levels "d","e": 1 2
# $ y: num  1 4

df <- data.frame(x = c("d", "e"), y = c(1,4))

str(df)


###############################################################################

# Crie a seguinte matriz
#
#       [,1] [,2] [,3]
# [1,]   19   22   25
# [2,]   20   23   26
# [3,]   21   24   27

vetor_1 <- c(19, 22, 25)
vetor_2 <- c(20, 23, 26)
vetor_3 <- c(21, 24, 27)

matriz_exemplo <- matrix(c(vetor_1, vetor_2, vetor_3), byrow = TRUE, nrow = 3)

matriz_exemplo


###############################################################################

# Se Z Ã© uma matriz 4 por 4, qual Ã© o resultado de Z[1,4] ?

# RESPOSTA:vai selecionar o último elemento da primeira linha da matriz

###############################################################################

# Substitua o sÃ�mbolo de interrogaÃ§Ã£o por um 
# cÃ³digo que retorne o seguinte resultado:
#
#  W3 W4 W1 W2 
#  20 69  5 88 

y <- c(20, 69, 5, 88)
q <- c("W3", "W4", "W1", "W2")
names(?) <- ?
  
names(y) <- q

y
  
  ###############################################################################

# Substitua o sÃ�mbolo de interrogaÃ§Ã£o por um 
# cÃ³digo que retorne o seguinte resultado:
#
#       [,1] [,2]
# [1,]    4    6
# [2,]    3    7
# [3,]    1    8


cbind(c(4, 3, 1), ?)


vetor_a <- c(6, 7, 8)

cbind(c(4, 3, 1), vetor_a)
cbind(c(4, 3, 1), c(6, 7, 8))


###############################################################################

# Substitua o sÃ�mbolo de interrogaÃ§Ã£o por um 
# cÃ³digo que retorne o seguinte resultado:
#       [,1] [,2] [,3] [,4]
# [1,]    1    3   13   15
# [2,]    2    4   14   16

x <- 1:4
y <- 13:26

matrix(?,
       nrow = 2,
       byrow = FALSE)

matriz_exemplo_2 <- matrix(c(x, y[1:4]), nrow = 2, byrow = FALSE)

matriz_exemplo_2


###############################################################################

# Crie o seguinte dataframe df
#
# df
#    x  y    z
# 1 17  A  Sep
# 2 37  B  Jul
# 3 12  C  Jun
# 4 48  D  Feb
# 5 19  E  Mar




# Ainda utilizando o dataframe df,
# qual cÃ³digo produziria o seguinte resultado?
#
#    x  y
# 1 17  A
# 2 37  B
# 3 12  C


x <- c(17, 37, 12, 48, 19)
y <- c("A", "B", "C", "D", "E")
z <- c("Sep", "Jul", "Jun", "Feb", "Mar")

df <- data.frame(x, y, z)

df

###############################################################################

# Responder o exercÃ�cio teÃ³rico abaixo

#Explicação causal teórica: Recentemente, a UE vivenciou um contexto de crises múltiplas, 
#destacando-se a crise do Euro iniciada em 2011 e a crise dos refugiados iniciada quase que 
#concomitantemente, mas cujo auge ocorreu dois a três anos depois. Em outras palavras, ao mesmo
#tempo em que a UE estava lidando com baixo desempenho econômico, desemprego, e em alguns 
#países até mesmo deflação; um contingente considerável e cada vez maior de imigrantes e refugiados
#chegava ao continente, agravando a situação de crise e dividindo opiniões tanto entre gestores e
#líderes políticos como entre a população. Como se o cenário já não fosse instável o suficiente, 
#desde então multiplicaram-se atentados terroristas por todo o continente, parte considerável 
#deles perpretados por radicais muçulmanos ou de forma isolada ou com o apoio de organizações
#terroristas. Todo esse cenário de crise, instabilidade e insegurança na Europa fez com que
#movimentos nacionalistas e xenófobos, assim como partidos populistas de extrema-direita 
#ressurgissem e/ou ganhassem força. No caso destes últimos, parte deles obteve consideráveis
#margens de apoio elitoral, sejam em eleições nacionais ou comunitárias. Pode-se considerar, 
#portanto, que o aumento da imigração na Europa, aliado de um cenário de crise econômica, 
#reforçou a performance eleitoral de partidos populistas de extrema-direita pela Europa, 
#destacando-se as eleições para o Parlamento Europeu.

#Hipóteses: 1. Partidos populistas de extrema-direita tendem a ter melhores resultados eleitorais
#              em países que registram altas ou crescentes taxas de imigração.
#           2. Partidos populistas de extrema-direita tendem a ter melhores resultados eleitorais 
#              em países que registram alto ou crescente índice de desemprego.
#           3. A diferença na votação para partidos populistas de extrema-direita entre países no 
#              caso das eleições para o Parlamento Europeu pode ser explicada pelas variáveis acima.

#Variáveis independentes: 1. Taxas de imigração
#                         2. índice de desemprego

#Variável dependente: 1. Votos obtidos nas eleições para o PE pelos partidos populistas de 
#                        extrema-direita.

#Relação entre as variáveis: Relação positiva entre taxa de imigração e votos
#                            Relação positiva entre desemprego e votos

#Operacionalização dos conceitos:
#     Variável taxa de imigração: proporção de imigrantes / habitante + número de imigrantes 
#                                 que chegaram desde a última eleição
#     Variável desemprego: taxa de desemprego medida por órgãos nacionais em cada país
#     Variável votação: resultados eleitorais de tais partidos disponíveis no site da UE

