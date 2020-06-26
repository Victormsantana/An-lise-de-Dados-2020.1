# Utilizando o banco world do pacote poliscidata, faça um  
# histograma que também indique a média  e um boxplot 
# da variável gini10
# Descreva o que você pode observar a partir deles.

library(tidyverse)
library(poliscidata)

banco_1 <- world

ggplot(banco_1, aes(gini10)) +
  geom_histogram()+
  geom_vline(aes(xintercept = mean(gini10, na.rm = T)))

ggplot(banco_1, aes(y = gini10)) + 
  geom_boxplot()

Do histograma dá para observar que a maior parte
dos casos se concentra entre os ginis 30 e 45, sendo a
média bastante próxima de 40, há também pouquíssimos
casos após 60.

Do boxplot dá para observar que a media fica um pouco abaixo
de 40 e que o terceiro quartil é ligeiramente maior que o
segundo quartil. Além disso, o maior valor discrepante fica
próximo de 75.

# Utilizando as funções de manipulação de dados da aula passada,
# faça uma tabela que sumarize a media (função mean), 
# mediana (funcao median) e o desvio padrão (fundao sd) da 
# renda per capta (variável gdppcap08), agrupada por tipo de regime 
# (variável democ).
# Explique a diferença entre valores das médias e medianas.
# Ilustre com a explicação com gráfico de boxplot.
# Os dados corroboram a hipótese da relação entre democracia
# e desempenho economico?

banco_1 %>% group_by(democ) %>%
  summarise(media = mean(gdppcap08, na.rm = T),
            mediana = median(gdppcap08, na.rm = T),
            "desvio padrão" = sd(gdppcap08, na.rm = T))

A média é a soma de todos os gdp per capita da categoria
dividido pelo número de países dessa categoria. Já a mediana
é o número que se encontra no meio quando todos os gdps per
capita são listados em ordem crescente. A média du mais alta
em ambos os casos provavelmente porque há casos bem
discrepantes, que puxam a média para cima.

ggplot(banco_1, aes(y = gdppcap08)) +
  geom_boxplot()

O boxplot geral mostra que há pelo menos 6 casos discrepantes 
(2 deles de forma bem acentuada), que puxaram a média para
cima. Filtrando os dados e gerando dois boxplots separados
para países democráticos e não democráticos, percebe-se
que há apenas um caso extremamente discrepante para os países
democráticos (que é Luxemburgo), sendo os outros 5 casos
todos não-democracias (Singapura, Qatar, Bahrain, Kuwait e
Guiné Equatorial). No entanto, nota-se que há uma diferença
considerável nos gráficos: tanto o espaço entre o limite
do segundo e o limite do terceiro quartil é maior como o
terceiro quartil é bem maior no grupo das democracias do
que no grupo das não-democracias, o que indica gdps maiores.
Graficamnte falando, a correlação é notável e os dados
corroboram a hipótese. 

banco_1_no_democ <- banco_1 %>% filter(democ == "No")
banco_1_yes_democ <- banco_1 %>% filter(democ == "Yes")

ggplot(banco_1_yes_democ, aes(y = gdppcap08)) +
  geom_boxplot()

ggplot(banco_1_no_democ, aes(y = gdppcap08)) +
  geom_boxplot()

# Carregue o banco states que está no pacote poliscidata 
# Mantenha apenas as variáveis obama2012, conpct_m, hs_or_more,
# prcapinc, blkpct10, south, religiosity3, state

banco_state <- states %>% 
  select(obama2012, conpct_m, hs_or_more, 
         prcapinc, blkpct10, south, religiosity3, state)


# Carregue o banco nes que está no pacote poliscidata
# Mantenha apenas as variáveis obama_vote, ftgr_cons, dem_educ3,
# income5, black, south, relig_imp, sample_state

banco_nes <- nes %>% 
  select(obama_vote, ftgr_cons, dem_educ3,income5, black, 
         south, relig_imp, sample_state)

# As variáveis medem os mesmos conceitos, voto no obama em 2012, 
# conservadorismo, educação, renda, cor, norte-sul, 
# religiosidade e estado. A diferença é que o nes é um banco de
# dados com surveys individuais e o states é um banco de dados
# com informações por estado
#
# Faça um gráfico para cada banco representando o nível de
# conservadorismo. Comente as conclusões que podemos ter
# a partir deles sobre o perfil do eleitorado estadunidense.
# Para ajudar, vocês podem ter mais informações sobre os bancos
# de dados digitando ?states e ?nes, para ter uma descrição das
# variáveis

?states
?nes

glimpse (banco_state)

ggplot(banco_state, aes("", conpct_m)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_beeswarm()

ggplot(banco_nes, aes(ftgr_cons)) + 
  geom_histogram(bins = 10, aes(y=..density..)) + 
  geom_density() +
  geom_vline(aes(xintercept = mean(ftgr_cons, na.rm = T)))

ggplot(banco_nes, aes(ftgr_cons)) + 
  geom_histogram(bins = 10, aes(y=..density..)) + 
  geom_density() +
  geom_vline(aes(xintercept = median(ftgr_cons, na.rm = T)))

Os gráficos mostram um perfil mais moderado no nível de
conservadorismo, com o número de casos reduzindo à medida que
nos distanciamos da mediana. Fiz um gráfico adicional com a média
dos dados do NES, a média se situa próximo de 53, enquanto a
mediana fica extatamente no valor 50. Devido ao grande número
de casos, ficou inviável representar os dados do NES na forma
de bloxplot, por isso optei pelo histograma.

# Qual é o tipo de gráfico apropriado para descrever a variável
# de voto em obama nos dois bancos de dados?
# Justifique e elabore os gráficos

Os dados do States ficam melhor representados na forma de violino +,
pontos, por serem valores númericos. Já os dados do NES ficam melhor 
representados em barra, por serem variáveis binárias com grande 
número de casos.


ggplot(banco_state, aes("", obama2012)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_beeswarm() 

ggplot(banco_nes, aes(obama_vote)) +
  geom_bar()


# Crie dois bancos de dados a partir do banco nes, um apenas com
# respondentes negros e outro com não-negros. A partir disso, faça
# dois gráficos com a proporção de votos no obama.
# O que você pode afirmar a partir dos gráficos?
# Você diria que existe uma relação entre voto em Obama e cor?

banco_nes_negros <- banco_nes %>% filter(black == "Yes")
banco_nes_não_negros <- banco_nes %>% filter(black == "No")

ggplot(banco_nes_não_negros, aes(obama_vote)) +
  geom_bar()

ggplot(banco_nes_negros, aes(obama_vote)) +
  geom_bar()

Os gráficos permitem afirmar uma correlação positiva entre ser
negro e votar no Obama. Enquanto que nos não-negros o número de
votos no Obama e na oposição foi próximo, nos negros houve uma
grande diferença.


# A partir do banco de dados states, faça uma comparação semelhante.
# Faça um gráfico com as porcentagens de votos em Obama para estados
# que estão acima da mediana da porcentagem de população negra nos estados,
# e outro gráfico com as porcentagens de votos em Obama para os estados
# que estão abaixo da mediana de população negra.
# O que você pode afirmar a partir dos gráficos?
# Podemos chegar a mesma conclusão anterior?

banco_state %>% summarise(mediana = median(blkpct10, na.rm = T))

banco_states_negros <- banco_state %>% filter(blkpct10 > 8.25)
banco_states_não_negros <- banco_state %>% filter(blkpct10 <8.25)

ggplot(banco_states_não_negros, aes("", obama2012)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_beeswarm()

ggplot(banco_states_negros, aes("", obama2012)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_beeswarm()

ggplot(banco_states_negros, aes(obama2012)) +
  geom_histogram(bins = 25)

Os gráficos mostram que os estados cujo menos de 8.25% da população
se identifica como afro-descendente votaram em maior porcentagem para
Obama, ao contrário dos achados do NES. Talvez isso tenha ocorrido
porque como a base dos States se divide por estados, e boa parte
dos estados do sul, de maior população afro-descendente, é
tradicionalmente republicana, os dados ficaram enviezados. 


# A partir da varíavel X do banco df abaixo
df <- data.frame(x = cos(seq(-50,50,0.5)))
# Faça os tipos de gráficos que representem esse tipo de variável
# comentando as diferenças entre elas e qual seria a mais adequada

glimpse(df)

df

ggplot(df, aes(x)) +
  geom_histogram()
#Dá para perceber a curva de tendência maior nas pontas e menor no
#centro, mas a informação não é tão clara.

ggplot(df, aes(x)) +
  geom_density()
#Dá para perceber claramente a curva da tendência maior nas pontas
#e menor no centro. Visual simples e limpo.

ggplot(df, aes(y = x)) +
  geom_boxplot()
#Da para perceber a distribuição equidistante da mediana dos valores
# do df, mas não dá para ver seu padrão de distribuição

ggplot(df, aes("", y = x)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
#Dá para perceber tanto a equidistância como o padrão de distribuição
#dos valores. Creio que este é o melhor tipo de gráfico para
#os dados em questão, resume melhor a quantidade de informações de
#forma clara e simples visualmente (como fez o de densidade), mas 
#com o acréscimo da visualização dos segundo e terceito quartis e da
#mediana.

ggplot(df, aes("", y = x)) +
  geom_beeswarm()
#Gráfico com informações demais, torna a compreensão difícil e a
#vizualização poluída. Pouco inteligível. 




# responsa as questões teóricas abaixo

Como não dá para repetir o desenho do livro aqui no R, irei dispor de cada 
parte como um tópico

Pré-operacionalização
Variável independente -> "atributos eleitorais"
Teoria causal -> eleições de primeira e segunda-ordem
Variável dependente -> "caráter de segunda-ordem das eleições do PE"

Pós-operacionalização
Variável independente -> taxa de comparecimento às urnas, posição das eleições
                         do PE com relação à eleição de primeira-ordem mais
                         próxima, diferença dos votos obtidos para partidos
                         governistas, diferença dos votos obtidos para partidos
                         extremistas.
Hipótese -> o caráter de segunda-ordem das eleições do Parlamento Europeu (PE) 
            caiu nas últimas duas últimas eleições
Variável dependente -> coeficiente de segunda-ordem das eleições do PE
                       (valoração das variáveis independentes somadas em cada
                         eleição)



Sobre disponibilidade dos dados: há numerosos artigos que versam sobre essas
eleições, tanto em específico como em conjunto. Contudo, ainda não pesquisei
a fundo se os autores disponibilizaram os dados que utilizaram já tabelados
e codificados de forma aberta na internet (como parte considerável dos trabalhos
é anterior a 2014 acho pouco provável). Com são dados relativos à eleições em
países europeus, boa parte deles é fácil de achar, algumas informações estão
disponíveis no próprio site do PE ou em plataformas online como Parties and
Elections e Europe Politique. Contudo, eu ainda teria que montar o banco de dados
com todos os números e códigos específicos. 



Sobre confiabilidade e validade das variáveis: como os dados sobre eleições são
dados históricos que raramente são alterados de forma retroativa, com relação
a isso, os dados são os mesmos para todas as pesquisas. A forma como irei 
operacionalizá-los para formar esse "coeficiente" é que pode ser questionada 
(se irei considerar cada variável tendo o mesmo peso na composição do coeficiente
  ou não, caso não, como avaliar que uma variável é mais importante que a outra,
  como saber que esgotei todas as variáveis estatisticamente importantes
  para o coeficiente, etc.), mas o coeficiente em si não é irreproduzível. 
Em outras palavras, as variáveis parecem ser confiáveis, mas a validade vai
ser dependente da operacionalização, a qual deve ser a mais fiel possível do
que já foi pré-estabelecido pelo teoria e pelos trabalhos anteriormente produzidos.



Sobre a forma mais ideal e adequada para operacionalização das variáveis: 
no momento, considero a construção desse "coeficiente" como a mais adequada,
mas ainda não defini 100% como vou construí-lo.