
## Faça todos os gráficos utilizando um tema que você ache mais adequado
## e nomeie os eixos x e y da maneira adequada

## Carregue o banco world do pacote poliscidata

library(poliscidata)
library(dplyr)
library(tidyverse)
library(ggbeeswarm)

banco <- world

## Observe o banco de dados com as funções adequadas

summary(banco)
str(banco)
glimpse(banco)


## A variável democ_regime08 indica se um país é democrático.
## Usando as ferramentas de manipulacao de bancos de dados, verifique
## quantos paises sao democraticos ou nao, e apresente esta variável 
## graficamente

banco_selecionado <- banco %>% 
  select(democ_regime08, muslim, gdppcap08, dem_score14) %>% 
  filter(democ_regime08 != "NA") %>%
  filter(dem_score14 != "NA")

glimpse(banco_selecionado)
banco_selecionado %>% count(democ_regime08)

"Dentre os 164 países na amostra, 69 foram classificados como não democracias,
e 95 como democracias"

ggplot(banco_selecionado, aes(democ_regime08)) + geom_bar()


## Teste a relação entre a variável democ_regime08 e a variável
## muslim (que indica se um país é muçulmano ou não). E represente
## visualmente as variáveis para visualizar se esta religião
## aumenta ou diminui a chance de um país ser democrático
## Qual seria sua conclusão com relação a associação destas duas
## variáveis?

banco_selecionado %>% count(muslim)

tabela <- table(banco_selecionado$democ_regime08, banco_selecionado$muslim)

chisq.test(tabela)

mosaicplot(tabela, shade = TRUE)

ggplot(banco_selecionado, aes(democ_regime08, fill = muslim)) +
  geom_bar(position = "fill")

ggplot(banco_selecionado, aes(x = democ_regime08, y = muslim)) +
  geom_count(aes(group = muslim, size = after_stat(prop))) + 
  scale_size_area(max_size = 10)

"Os gráficos mostram que, ao passo que metade dos países não democráticos
são muçulmanos, apenas 10-12% dos países democráticos são muçulmanos. 
Há uma associação positiva emtre as variáveis, e o p-valor foi bem baixo."

## A variável gdppcap08 possui informação sobre o PIB per capta
## dos países. Faça uma representação gráfica desta variável

ggplot(banco, aes("", gdppcap08)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

ggplot(banco, aes("", gdppcap08)) + 
  geom_beeswarm()

ggplot(banco, aes(gdppcap08)) +
  geom_density()


## Faça um sumario com a média, mediana e desvio padrão do pib per capta
## para cada tipo de regime politico, represente a associação destas
## variáveis graficamente, e faça o teste estatístico adequado para
## chegar a uma conclusão. Existe associaçào entre as variáveis?

banco_selecionado %>%
  summarise(média = mean(gdppcap08, na.rm = T),
            mediana = median(gdppcap08, na.rm = T),
            DP = sd(gdppcap08, na.rm = T))

banco_democracias <- banco_selecionado %>% filter(democ_regime08 == "Yes")
banco_autocracias <- banco_selecionado %>% filter(democ_regime08 == "No")

banco_democracias %>%
  summarise(média = mean(gdppcap08, na.rm = T),
            mediana = median(gdppcap08, na.rm = T),
            DP = sd(gdppcap08, na.rm = T))

banco_autocracias %>%
  summarise(média = mean(gdppcap08, na.rm = T),
            mediana = median(gdppcap08, na.rm = T),
            DP = sd(gdppcap08, na.rm = T))

ggplot(banco_selecionado, aes(gdppcap08)) +
  geom_density() +
  geom_vline(aes(xintercept = mean(gdppcap08, na.rm = T))) +
  geom_vline(aes(xintercept = median(gdppcap08, na.rm = T))) + 
  geom_vline(aes(xintercept = sd(gdppcap08, na.rm = T)))

ggplot(banco_democracias, aes(gdppcap08)) +
  geom_density() +
  geom_vline(aes(xintercept = mean(gdppcap08, na.rm = T))) +
  geom_vline(aes(xintercept = median(gdppcap08, na.rm = T))) + 
  geom_vline(aes(xintercept = sd(gdppcap08, na.rm = T)))

ggplot(banco_autocracias, aes(gdppcap08)) +
  geom_density() +
  geom_vline(aes(xintercept = mean(gdppcap08, na.rm = T))) +
  geom_vline(aes(xintercept = median(gdppcap08, na.rm = T))) + 
  geom_vline(aes(xintercept = sd(gdppcap08, na.rm = T)))


t.test(gdppcap08 ~ democ_regime08, data = banco_selecionado)


"Parece existir uma associação positiva entre as variáveis "democracia" e "pib 
per capita". Tanto a média como a mediana dos países democráticos foi mais 
alta do que a dos não-democráticos (especialmente a mediana), e novamente,
o p-valor foi baixo."

## Por fim, ao invés de utilizar uma variável binária de democracia,
## utilize a variável dem_score14 para avaliar se existe uma associação
## entre regime político e desenvolvimento econômico. Represente
## a associação graficamente, faça o teste estatístico e explica sua
## conclusão

ggplot(banco_selecionado, aes(dem_score14, gdppcap08)) +
  geom_jitter() +
  theme_classic()

ggplot(banco_democracias_02, aes(dem_score14, gdppcap08)) +
  geom_jitter() +
  theme_classic()

ggplot(banco_autocracias_02, aes(dem_score14, gdppcap08)) +
  geom_jitter() +
  theme_classic()

banco_democracias_02 <- banco_selecionado %>% 
  filter(dem_score14 > 6)

banco_autocracias_02 <- banco_selecionado %>%
  filter(dem_score14 <= 6)

?world

banco_selecionado %>%
  summarise(média = mean(gdppcap08, na.rm = T),
            mediana = median(gdppcap08, na.rm = T),
            DP = sd(gdppcap08, na.rm = T))

banco_democracias_02 %>%
  summarise(média = mean(gdppcap08, na.rm = T),
            mediana = median(gdppcap08, na.rm = T),
            DP = sd(gdppcap08, na.rm = T))

banco_autocracias_02 %>%
  summarise(média = mean(gdppcap08, na.rm = T),
            mediana = median(gdppcap08, na.rm = T),
            DP = sd(gdppcap08, na.rm = T))

cor.test(banco_selecionado$dem_score14, banco_selecionado$gdppcap08)


"A diferença entre os dois grupos ficou ainda maior. a média do PIB per capita 
das democracias é quase o triplo da média das não democracias; e a mediana é 
quase 6 vezes e meia maior que a mediana das não democracias. Os gráficos 
confirmam a correlação positiva entre qualidade da democracia e PIB per capita,
assim como o tste r de Pearson, cujo p-valor deu extremamente baixo"

## Teste a associação entre renda perca capta e religiao (com a variável
## muslim) e represente graficamente. Qual é sua conclusão? 

ggplot(banco_selecionado, aes(gdppcap08, fill = muslim)) +
  geom_density(alpha = 0.3)

ggplot(banco_selecionado, aes(muslim, gdppcap08)) + 
  geom_boxplot()

t.test(gdppcap08 ~ muslim, data = banco_selecionado)

"Há uma certa correlação entre as variáveis, contudo ela é menos forte em
comparação com as outras mostradas acima, apesar do p-valor ainda poder
ser considerado "estatisticamente significante"."

## Comparando suas conclusões anteriores, é possível afirmar qual
## das duas variáveis possui maior impacto no desenvolvimento economico?
## Por que? 

Pelo que os dados indicam, a variável democracia apresenta uma correlação
mais forte com a variável PIB per capita. Creio que isso acontece porque
o grupo de países mulçumanos além de ser menor em número, também é composto
de países que são considerados ricos ou que têm o PIB per capita mais elevado,
por conta da exploração do petróleo (apesar de boa parte deles não ser
considerada como democrática). A riqueza gerada pelo petróleo, além do critério
escolhido para medir "desenvolvimento econômico" (PIB per capita, variável que
não mede crescimento acumulado ao longo dos anos, nem desigualdade, por exemplo),
podem ter gerado essas distorções nos dados, ressaltando a correlação entre
democracias e "riqueza".

##########################################################################

## Exercício teórico
## Levando em consideração as variáveis de seu trabalho final,
## qual dos 3 testes estatísticos utilizados seria adequado utilizar?

Como ambas as variáveis seriam contínuas, a melhor opção é o teste r de Pearson.
