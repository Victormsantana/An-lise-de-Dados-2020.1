Exercicio 10
================

### Continuaremos com a utilização dos dados do ESEB2018. Carregue o banco da mesma forma que nos exercicios anteriores

``` r
library(tidyverse)
library(haven)

link <- "https://github.com/MartinsRodrigo/Analise-de-dados/blob/master/04622.sav?raw=true"

download.file(link, "04622.sav", mode = "wb")

banco <- read_spss("04622.sav") 

banco <- banco %>%
  mutate(D10 = as_factor(D10)) %>%
  filter(Q18 < 11,
         D9 < 9999998,
         Q1501 < 11,
         Q12P2_B < 3) %>%
  mutate(Q12P2_B = case_when(Q12P2_B == 1 ~ 0,  # Quem votou em Haddad = 0
                             Q12P2_B == 2 ~ 1)) # Quem votou em Bolsonaro = 1
```

### Crie a mesma variável de religião utilizada no exercício anterior

``` r
Outras <- levels(banco$D10)[-c(3,5,13)]

banco <- banco %>%
  mutate(religiao = case_when(D10 %in% Outras ~ "Outras",
                              D10 == "Católica" ~ "Católica",
                              D10 == "Evangélica" ~ "Evangélica",
                              D10 == "Não tem religião" ~ "Não tem religião"))
```

### Faça uma regressão linear utilizando as mesmas variáveis do exercício 9 - idade(D1A\_ID), educação (D3\_ESCOLA), renda (D9), nota atribuída ao PT (Q1501), auto-atribuição ideológica (Q18), sexo (D2\_SEXO) e religião (variável criada no passo anterior) - explicam o voto em Bolsonaro (Q12P2\_B).

``` r
options("scipen" = 10)
regressão_lpm <- lm(Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + D2_SEXO + religiao, data = banco)
summary(regressão_lpm)
```

    ## 
    ## Call:
    ## lm(formula = Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + 
    ##     D2_SEXO + religiao, data = banco)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.05532 -0.19854  0.01565  0.16182  0.96682 
    ## 
    ## Coefficients:
    ##                               Estimate    Std. Error t value Pr(>|t|)    
    ## (Intercept)               0.7066528237  0.0646893413  10.924  < 2e-16 ***
    ## D1A_ID                    0.0011401012  0.0007538896   1.512  0.13074    
    ## D3_ESCOLA                 0.0055466625  0.0052257983   1.061  0.28873    
    ## D9                       -0.0000009837  0.0000031963  -0.308  0.75832    
    ## Q1501                    -0.0772824015  0.0027990853 -27.610  < 2e-16 ***
    ## Q18                       0.0265094568  0.0030932523   8.570  < 2e-16 ***
    ## D2_SEXO                  -0.0528631872  0.0208943372  -2.530  0.01154 *  
    ## religiaoEvangélica        0.0768358985  0.0236336987   3.251  0.00118 ** 
    ## religiaoNão tem religião -0.0027459861  0.0423769173  -0.065  0.94835    
    ## religiaoOutras           -0.0726267771  0.0367795766  -1.975  0.04855 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3489 on 1138 degrees of freedom
    ## Multiple R-squared:  0.5028, Adjusted R-squared:  0.4989 
    ## F-statistic: 127.9 on 9 and 1138 DF,  p-value: < 2.2e-16

### Interprete o resultado dos coeficientes

RESPOSTA: As variáveis idade, escolaridade e renda não deram
significância estatística. O beta para idade deu 0,0011, indicando que
o aumento de um ano na idade do entrevistado aumentaria 0,0011 a chance
dele ter votado em Bolsonaro. O beta para escolaridade também deu
positivo, indicando que um avanço entre uma categoria e outra dentre as
categorias estabelecidas pelo survey aumenta em 0,0055 a probabilidade
do entrevistado ter votado em Bolsonaro. Por sua vez, a variável renda
teve correlação negativa, mostrando que a mudança de um real na renda
diminui em 0,00000098 a chance do entrevistado votar em Bolsonaro. De
qualquer forma, como ressaltado, essas variáveis não deram
significância, o que significa que não podemos desconsiderar a hipótese
nula com segurança.

Por sua vez, as variáveis nota atribuída ao PT, autoatribuição
ideológica e sexo deram singificância estatística, principalmente as
duas primeiras. O beta da nota ao PT deu -0.077, indicando que uma nota
maior ao PT está negativamente correlacionada com o voto em Bolsonaro.
Já a variável autoatribuição ideológica deu correlação positiva em
0.026, ou seja, a mudança de um ponto a mais na escala de ideologia
implica em 0.026 maior chance do entrevistado ter votado em Bolsonaro.
No seu turno, a variável sexo deu correlação negativa, indicando que
mulheres tem -0.052 menos chance de votar em Bolsonaro (é importante
lembrar que ambas as variáveis são binárias).

Por fim, a variável religião mostrou que o padrão de voto entre os que
não têm religião e os católicos não é estatisticamente significante (o
beta também deu um valor bastante baixo) No entanto, as outras
categorias deram significância, destacando-se os evangélicos. O beta
para outras religiões foi -0.07 e para evangélicos doi 0.76, indicando
que pessoas com outras religiões tem -0.07 chance de ter votado em
Bolsonaro em comparação com os católicos; ao passo que os evangélicos
tem 0.76 chance a mais de votar em Bolsonaro do que os católicos.

### O resultado difere dos encontrados anteriormente, quando a variavel dependente era a aprovação de Bolsonaro?

RESPOSTA: O R-quadrado deu consideravelmente alto, comparando com as
regressões dos exercícios anteriores.

Com relação às variáveis, idade e escolaridade perderam signficância,
principalmente escolaridade, cujo p-valor cresceu consideravelmente. Na
variável religião, a significância para as categorias evangélica e
outras religiões aumentou.

No entanto, é importante ressaltar que a regressão do exercício 9 tem
uma variável interativa entre sexo e religião, o que impacta no p-valor.
Além disso, é dificíl de comparar os coeficientes porque as “escalas”
são diferentes. No exercício 9, a escala ia de um a dez se não me
engano. Aqui, a variável é binária, o que na prática faz a “escala” ser
de 0 a 1.

### Faça uma regressão logistica com as mesmas variaveis

``` r
options("scipen" = 10)
regressão_logistica <- glm(Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + D2_SEXO + religiao, 
                           data = banco, family = "binomial")
summary(regressão_logistica)
```

    ## 
    ## Call:
    ## glm(formula = Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + 
    ##     D2_SEXO + religiao, family = "binomial", data = banco)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7529  -0.5625   0.2518   0.4744   2.5830  
    ## 
    ## Coefficients:
    ##                              Estimate   Std. Error z value Pr(>|z|)    
    ## (Intercept)               0.820905327  0.529763447   1.550  0.12124    
    ## D1A_ID                    0.010013686  0.006336810   1.580  0.11405    
    ## D3_ESCOLA                 0.056341787  0.043575753   1.293  0.19602    
    ## D9                       -0.000004635  0.000023959  -0.193  0.84660    
    ## Q1501                    -0.467805560  0.026663935 -17.545  < 2e-16 ***
    ## Q18                       0.224213882  0.027479605   8.159 3.37e-16 ***
    ## D2_SEXO                  -0.449713328  0.173903438  -2.586  0.00971 ** 
    ## religiaoEvangélica        0.621655696  0.198470380   3.132  0.00173 ** 
    ## religiaoNão tem religião -0.021056111  0.347756068  -0.061  0.95172    
    ## religiaoOutras           -0.673554187  0.312177200  -2.158  0.03096 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1557.84  on 1147  degrees of freedom
    ## Residual deviance:  862.45  on 1138  degrees of freedom
    ## AIC: 882.45
    ## 
    ## Number of Fisher Scoring iterations: 5

### Transforme os coeficientes estimados em probabilidade

``` r
library(margins)

margins(regressão_logistica)
```

    ##    D1A_ID D3_ESCOLA            D9    Q1501     Q18 D2_SEXO religiaoEvangélica
    ##  0.001171  0.006589 -0.0000005421 -0.05471 0.02622 -0.0526            0.07346
    ##  religiaoNão tem religião religiaoOutras
    ##                 -0.002521       -0.08172

``` r
summary(margins(regressão_logistica))
```

    ##                    factor     AME     SE        z      p   lower   upper
    ##                    D1A_ID  0.0012 0.0007   1.5849 0.1130 -0.0003  0.0026
    ##                   D2_SEXO -0.0526 0.0202  -2.6078 0.0091 -0.0921 -0.0131
    ##                 D3_ESCOLA  0.0066 0.0051   1.2949 0.1953 -0.0034  0.0166
    ##                        D9 -0.0000 0.0000  -0.1935 0.8466 -0.0000  0.0000
    ##                     Q1501 -0.0547 0.0009 -57.9079 0.0000 -0.0566 -0.0529
    ##                       Q18  0.0262 0.0030   8.8434 0.0000  0.0204  0.0320
    ##        religiaoEvangélica  0.0735 0.0235   3.1280 0.0018  0.0274  0.1195
    ##  religiaoNão tem religião -0.0025 0.0417  -0.0605 0.9517 -0.0842  0.0791
    ##            religiaoOutras -0.0817 0.0379  -2.1574 0.0310 -0.1560 -0.0075

### Quais foram as diferenças no resultado entre usar a regressão linear e a logistica?

RESPOSTA: Os equivalentes aos coeficientes betas caíram (e
consequentemente, os erros padrão), mas ressalta-se que o valor descrito
na tabela é na verdade a média para o total de casos da regressão. Além
disso, os resultados são menos determinísticos, pois os coeficientes
dados são relativos a probabilidades. A maior alteração ocorreu com os
p-valores, O de idade desceu um pouco mas continuou sem significância. O
de escolaridade caiu bastante, e passou a dar alta significância. O
p-valor de renda, por sua vez, subiu bastante, afastando-se ainda mais
do limiar de significância. Nota ao PT e autoatribuição ideológica
continuaram altamente significantes, assim como sexo. Já na variável
religião, o p-valor da categoria não tem religião continuou bastante
alto, indicando a semelhança no padrão de voto entre esse grupo e os
católicos. O p-valor de evangélicos e outras religiões continuou
significante, confirmando a diferença entre essas categorias e os
católicos.

### Verifique a quantidade de classificações corretas da regressao logistica e avalie o resultado

``` r
predicted_prob <- predict(regressão_logistica, type = "response")

library(InformationValue)

1 - misClassError(banco$Q12P2_B,
                  predicted_prob, threshold = 0.5)
```

    ## [1] 0.8301

``` r
opt_cutoff <- optimalCutoff(banco$Q12P2_B, predicted_prob)

1 - misClassError(banco$Q12P2_B,
                  predicted_prob, threshold = 0.5566)
```

    ## [1] 0.8362

RESPOSTA: percebe-se que a porcentagem de predições corretas foi
bastante alta, chegando a 83,62% dos casos, o que demonstra que o modelo
estatístico é consistente e foi bem construído, principalmente
levando-se em consideração o R-quadrado da regressão linear anterior,
que ficou em torno de 0.5.
