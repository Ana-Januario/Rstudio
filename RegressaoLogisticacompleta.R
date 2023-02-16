####################################################################################
#                                                                                  #
#                         INTRODUCAO A REGRESSAO LOGISTICA                         #
#                                                                                  #
####################################################################################


# indicar a pasta onde esta localizado o ficheiro de dados no menu Session, 
# opcao Set Working Directory, subopcao Choose Directory


# leitura do ficheiro de dados que vai ser guardado no ojbecto lowbwt
lowbwt <- read.table("lowbwt.csv",     # nome do ficheiro e respetia extensao
                     header=T,         # indica que os nomes das colunas estao na 1a linha
                     dec=",",          # indica qual o simbolo usado para identificar as casas decimais
                     sep=";")          # indica qual o simbolo usado para separar as colunas


# algumas instrucoes para ver o conjunto de dados
View(lowbwt)          # mostra os dados numa janela do tipo excel
str(lowbwt)           # estrutura dos dados (tipo de variaveis)
summary(lowbwt)       # resumo de todas as variaveis do conjunto de dados
dim(lowbwt)           # dimensao do conjunto de dados (num. linhas, num. colunas)
names(lowbwt)         # nome das variaveis do conjunto de dados


# verificar se existem valores omissos
sum(is.na(lowbwt))        # numero total de valores omissos de todo o conjunto de dados

sum(is.na(lowbwt$low))    # numero de valores omissos da variavel low do conjunto de dados
sum(is.na(lowbwt[2]))     # igual a linha anterior (em alternativa usa o numero da coluna em vez do nome da variavel)

for (i in 1:11) print(sum(is.na(lowbwt[i])))  # numero de valores omissos por cada uma das 11 variaveis do conjunto de dados


# transformar em fatores as variaveis categoricas e atribuir os nomes das categorias por ordem crescente do valor da variavel
# conselhos: 
# 1) para ter a certeza que os nomes das categorias foi feito de forma correta, fazer uma tabela antes e depois de transformar a variavel
# 2) na pratica, atribuir um nome diferente a variavel transformada para nao perder os dados originais (caso alguma coisa corra mal)

table(lowbwt$low)                                             # tabela da variavel original
lowbwt$low <- factor(lowbwt$low,                              # nome da variavel a transformar
                     labels=c(">= 2500 gr", "< 2500 gr"))     # nomes das categorias
table(lowbwt$low)                                             # tabela da variavel tansformada

table(lowbwt$race)                                            # tabela da variavel original
lowbwt$race <- factor(lowbwt$race,                            # nome da variavel a transformar
                      labels=c("white", "black", "other"))    # nomes das categorias
table(lowbwt$race)                                            # tabela da variavel tansformada

table(lowbwt$smoke)                                           # tabela da variavel original
lowbwt$smoke <- factor(lowbwt$smoke,                          # nome da variavel a transformar
                       labels=c("no", "yes"))                 # nomes das categorias
table(lowbwt$smoke)                                           # tabela da variavel tansformada

table(lowbwt$ptl)                                             # tabela da variavel original
lowbwt$ptl <- factor(lowbwt$ptl)                              # nome da variavel a transformar
table(lowbwt$ptl)                                             # tabela da variavel tansformada

table(lowbwt$ht)                                              # tabela da variavel original
lowbwt$ht <- factor(lowbwt$ht,                                # nome da variavel a transformar
                    labels =c("no", "yes"))                   # nomes das categorias
table(lowbwt$ht)                                              # tabela da variavel tansformada 

table(lowbwt$ui)                                              # tabela da variavel original
lowbwt$ui <- factor(lowbwt$ui,                                # nome da variavel a transformar
                    labels = c("no", "yes"))                  # nomes das categorias
table(lowbwt$ui)                                              # tabela da variavel tansformada

table(lowbwt$ftv)                                             # tabela da variavel original
lowbwt$ftv <- factor(lowbwt$ftv)                              # nome da variavel a transformar
table(lowbwt$ftv)                                             # tabela da variavel tansformada


# novo resumo de todas as variaveis do conjunto de dados
summary(lowbwt)       


# outra forma de ver o conjunto de dados
library(Hmisc)
summary(low ~.,             # cruza a variavel low com todas as outras 
        data=lowbwt)        # nome do conjunto de dados
summary(low ~ smoke+race,   # cruza a variavel low com variaveis smoke e race 
        data=lowbwt,        # nome do conjunto de dados
        fun=table)          # funcao a usar



# ----------------------------------------------------------------------------------
#
#                        Introducao a REGRESSAO LOGISTICA 
#
# ----------------------------------------------------------------------------------


# ----------------------------- AJUSTAR MODELO NULO --------------------------------

fit0 <- glm(low ~ 1,                         # Y ~ lista de variaveis explicativas. ~1 indicia que so se considera a constante (modelo nulo) 
            data=lowbwt,                     # nome do conjunto de dados
            family=binomial(link=logit))     # distribuicao de Y e funcao de ligacao
summary(fit0)
# Nota: este modelo apenas tem o parametro beta 0. Na regressao logistica, habitualmente beta0 nao tem significado.



# ----------------- AJUSTAR MODELO UNIVARIADO: COVARIAVEL SMOKE --------------------

fit1 <- glm(low ~ smoke, 
            data=lowbwt, 
            family=binomial(link=logit))

# -- coeficientes do modelo e sua significancia
summary(fit1)
# Nota: valor p (Wald) de beta1 = 0.0276 -> variavel significativa ao nivel alfa de 5% 

# OR dos parametros
exp(fit1$coefficients)
(ORbeta0 <- exp(fit1$coef[1]))      # Odd de ter bebe baixo peso de quem nao fuma
(ORbeta1 <- exp(fit1$coef[2]))      # OR de quem fuma relativamente a quem não fuma

# Confirmar os valores estimados para os coeficientes
table(lowbwt$smoke, lowbwt$low)
# ORbeta0 = 29/86
library(epitools)
oddsratio.wald(table(lowbwt$smoke, lowbwt$low), 
               rev="both")         # troca categorias das linhas e das colunas
# ORbeta1 = (30*86)/(44*29)


# -- Intervalos de Confianca (IC)

(mc1 <- vcov(fit1))       # matriz de variancias e covariancias do modelo fit1
# na diagonal estao a variancias dos betas

# IC a 95% para beta 0
# sqrt(mc1[1,1]) desvio padrao do coeficiente da variavel smoke
lib0 <- fit1$coef[1] - qnorm(0.975)*sqrt(mc1[1,1])   # limite inferior do IC
lsb0 <- fit1$coef[1] + qnorm(0.975)*sqrt(mc1[1,1])   # limite superior do IC
cbind(lib0, lsb0)                     # junta lado a lado o limite inferior e superior do IC

# IC ORbeta0
exp(cbind(lib0, lsb0))

# IC a 95% para beta 1
# sqrt(mc1[2,2]) desvio padrao do coeficiente da variavel smoke
lib1 <- fit1$coef[2] - qnorm(0.975)*sqrt(mc1[2,2])   # limite inferior do IC
lsb1 <- fit1$coef[2] + qnorm(0.975)*sqrt(mc1[2,2])   # limite superior do IC
cbind(lib1, lsb1)                     # junta lado a lado o limite inferior e superior do IC

# IC ORbeta1
exp(cbind(lib1, lsb1))


# IC para beta 0 e beta 1
confint.default(fit1)      # ICs iguais aos construidos acima
confint(fit1)              # IC baseado no metodo profiling

# IC para ORbeta0 e ORbeta1
exp(confint.default(fit1)) # ICs iguais aos construidos acima
exp(confint(fit1))         # IC baseado no metodo profiling

# -- avaliar significancia geral do modelo
anova(fit1)                        # nao devolve a significancia
anova(fit0, fit1, test="Chisq")    # Teste da razao de verosimilhancas (TVR) entre modelo ajustado e modelo nulo (H0: low ~ 1 vs. H1: low ~ smoke)
# TVR: p-value=0.02737, logo rejeitar modelo nulo.
anova(fit1, test="Chisq")          # Igual a linha anterior se for modelo univariado

# curiosidade
tabela<-table(lowbwt$low, lowbwt$smoke)
chisq.test(tabela)
library(vcd)
assocstats(tabela)   # o valor obtido no TVR = deviance quando se pede anova(fit1)



# ----------------- AJUSTAR MODELO UNIVARIADO: COVARIAVEL RACE --------------------

fit2 <- glm(low ~ race, 
            data=lowbwt, 
            family=binomial(link=logit))

# -- coeficientes do modelo e sua significancia
summary(fit2)
# Notas: 
# 1) Neste caso como a variavel nao e dicotomica, o teste Wald avalia cada coeficiente e nao a significancia da variavel no seu todo
# 2) valor p (Wald) de beta1 = 0.0683 -> variavel dicotomica (X1=1 se raça black, X1=0 c.c.) marginalmente significativa
# 3) valor p (Wald) de beta2 = 0.0674 -> variavel dicotomica (X2=1 se raça other, X2=0 c.c.) marginalmente significativa

# -- avaliar significancia geral do modelo
anova(fit0, fit2, test="Chisq")   # TVR entre modelo ajustado e modelo nulo (H0: low ~ 1 vs. H1: low ~ race)
# TRV: valor p=0.0817 -> modelo marginalmente significativo

# -- avaliar significancia da variavel race
anova(fit0, fit2, test="Chisq")   # TVR entre modelo sem variavel race e com variavel race (H0: low ~ 1 vs. H1: low ~ race)
# TRV: valor p=0.0817 -> variavel race marginalmente significativa
# Nota: se houver missings nao se pode realizar o TRV, mas pode fazer-se o teste de Wald
library(aod)
wald.test(vcov(fit2),    # matriz de variancias e covariancias estimadas
          coef(fit2),    # vector com os coeficientes estimados
          Terms = 2:3)   # posicao no vector anterior dos coefientes associados a variavel a testar

# -- coeficientes estimados e respectivos OR
coef(fit2)         # betas  
exp(coef(fit2))    # ORbetas
# OR(beta2) = OR de mulheres de race black vs mulheres de race white
# OR(beta3) = OR de mulheres de race other (i.e., nao black) vs mulheres de race white

# -- ICs para coeficientes e respectivos OR
confint.default(fit2)        # IC betas
exp(confint.default(fit2))   # IC ORbetas
# Nota: forte indicio de que as categorias preta e outra se podem fundir numa unica categoria, i.e., 
#       criar apenas uma variavel X1 = 0 se race white vs X1=1 c.c.



# -------------- AJUSTAR MODELO MULTIVARIADO: COVARIAVEIS SMOKE E RACE ------------

fit3 <- glm(low ~ smoke + race,   # modelo SEM interacao
            data=lowbwt, 
            family=binomial(link=logit))
# -- coeficientes do modelo e sua significancia
summary(fit3)
# Notas: 
# 1) valor p (Wald) de beta1 = 0.00251 -> variavel smoke significativa
# 2) valor p (Wald) de beta2 = 0.02693 -> variavel dicotomica (X2=1 se race black, X2=0 c.c.) significativa
# 3) valor p (Wald) de beta3 = 0.00562 -> variavel dicotomica (X3=1 se race other, X3=0 c.c.) significativa

# -- avaliar significancia geral do modelo
anova(fit0, fit3, test="Chisq")   # TRV entre modelo ajustado e modelo nulo (H0: low ~ 1 vs. H1: low ~ smoke + race)
# TRV: valor p=0.002 -> modelo significativo

# -- testar significancia individual das variaveis
anova(fit2, fit3, test="Chisq")                 # TVR que compara o modelo fit2 com o fit 3 (H0: low ~ race vs. H1: low ~ smoke + race) para avaliar significancia da var smoke
# TRV: valor p=0.001856 -> variavel smoke significativa
anova(fit1, fit3, test="Chisq")                 # TVR que compara o modelo fit1 com o fit 3 (H0: low ~ smoke vs. H1: low ~ smoke + race) para avaliar significancia da var race
# TRV: valor p=0.007 -> variavel race significativa
wald.test(vcov(fit3), coef(fit3), Terms = 3:4)  # alternativa: Testar o efeito da variável race via teste de Wald
# valor p=0.01 -> variavel smoke significativa



fit4 <- glm(low ~ smoke * race,  # modelo COM interacao
            data=lowbwt, 
            family=binomial(link=logit))

# -- coeficientes do modelo e sua significancia
summary(fit4)
# Notas: 
# 1) valor p (Wald) de beta1 = 0.00343 -> variavel smoke significativa
# 2) valor p (Wald) de beta2 = 0.04412 -> variavel dicotomica (X2=1 se race black, X2=0 c.c.) significativa
# 3) valor p (Wald) de beta3 = 0.00337 -> variavel dicotomica (X3=1 se race other, X3=0 c.c.) significativa
# 4) valor p (Wald) de beta4 = 0.58972 -> variavel dicotomica (X4=1 se fuma e race black, X4=0 c.c.) nao significativa
# 5) valor p (Wald) de beta5 = 0.08359 -> variavel dicotomica (X5=1 se fuma e race other, X5=0 c.c.) marginalmente significativa

# -- avaliar significancia geral do modelo
anova(fit0, fit4, test="Chisq")   # TRV entre modelo ajustado e modelo nulo (H0: low ~ 1 vs. H1: low ~ smoke + race + smoke*race)
# TRV: valor p=0.003 -> modelo significativo

# -- testar significancia da interacao
anova(fit3, fit4, test="Chisq")   # TVR entre (low ~ smoke + race) e (low ~ smoke + race + smoke*race)
# TRV: valor p=0.2063 -> smoke*race não significativa -> vamos manter o fit3

# -- analise de residuos
plot(fit3)                        # devolve varios graficos que permitem avaliar os pressupostos do modelo (alguns nao fazem sentido para o modelo logistico)
cooks.distance(fit3)              # apresenta as distancias de Cook
plot(cooks.distance(fit3))        # grafico de dispersao das distancias de Cook por individuo (i)
barplot(cooks.distance(fit3))     # grafico de barras das distancias de Cook por individuo (i)


# -- ICs para coeficientes e respectivos OR
confint.default(fit3)        # IC betas
exp(confint.default(fit3))   # IC ORbetas
# Nota: forte indicio de que as categorias preta e outra se podem fundir numa unica categoria, i.e., 
#       criar apenas uma variavel X2 = 0 se race white vs X2=1 c.c.

# -- Avaliar se podemos fundir as categorias raceblack e racether numa unica, i.e., b3=b4?
# atraves do teste de Wald
l <- cbind(0, 0, 1, -1)          # vetor com valores diferentes e zero na posicao dos coeficientes a comparar, neste caso b3=b4?
wald.test(b = coef(fit3),        # vector dos coeficientes do modelo ajustado
          Sigma = vcov(fit3),    # matriz de variancias e covariancias do modelo ajustado
          L = l)
# p-value=0.96 pelo que o modelo com a variavel race dicotomizada nao e significativamente diferente do anterior


# -- juntar categorias da variavel race, i.e., race="white", "other"
table(lowbwt$race)    
lowbwt$race2 <- ifelse(lowbwt$race=="white",    # se for = white
                      "white",                  # entao atribui white
                      "other")                  # senao atribui other
table(lowbwt$race2)    # confirmar que a codificacao foi bem feita
# conselho: usar outro nome para a nova variavel para nao perder a variavel original


fit5 <- glm(low ~ smoke + race2,  # modelo SEM interacao
            data=lowbwt, 
            family=binomial(link=logit))

# -- coeficientes do modelo e sua significancia
summary(fit5)
# Notas: 
# 1) valor p (Wald) de beta1 = 0.00225 -> variavel smoke significativa
# 2) valor p (Wald) de beta2 = 0.00254 -> variavel race2 significativa

# -- avaliar significancia geral do modelo
anova(fit0, fit5, test="Chisq")   # TRV entre modelo ajustado e modelo nulo (H0: low ~ 1 vs. H1: low ~ smoke + race2)
# TRV: valor p=0.0006443 -> modelo significativo

# -- Comparar modelo com variavel race com 3 categorias vs. race com 2 categorias
anova(fit5, fit3, test="Chisq")   # TRV entre (low ~ race2) e (low ~ race)
# TRV: valor p=0.9604 -> os modelos nao sao significativamente diferentes -> optar pelo mais simples (low ~ race2)

# -- selecao dos modelos pelo criterio AIC (pode ser usado para modelos nao encaixados)
AIC(fit3)
AIC(fit5)

# -- coeficientes estimados e respectivos OR
coef(fit5)         # betas  
exp(coef(fit5))    # ORbetas
# OR(beta2) = OR de mulheres que fumam vs mulheres que nao fumam terem uma crianca de baixo peso
# OR(beta3) = OR de mulheres de race white vs mulheres de race other terem uma crianca de baixo peso

1/exp(coef(fit5))    # 1/ORbetas
# 1/OR(beta3) = OR de mulheres de race white vs mulheres de race other terem uma crianca de peso normal

# -- ICs para coeficientes e respectivos OR
confint.default(fit5)        # IC betas
exp(confint.default(fit5))   # IC ORbetas

1/exp(confint.default(fit5))   # IC 1/ORbetas: Atencao na 1a columa esta o limite superior e na 2a o limite inferior


# -------------- AJUSTAR MODELO COM VARIAVEIS CONTINUAS ---------------

fit6 <- glm(low ~ smoke+race+lwt, 
            family = binomial(link = logit), 
            data=lowbwt)
# -- coeficientes do modelo e sua significancia
summary(fit6)
# Notas: 
# 1) valor p (Wald) de beta1 = 0.00225 -> variavel smoke significativa
# 2) valor p (Wald) de beta2 = 0.00254 -> variavel race2 significativa

# -- significancia do modelo ajustado
anova(fit0, fit6, test="Chisq")

# -- ICs para coeficientes e respectivos OR
confint.default(fit6)        # IC betas
exp(confint.default(fit6))   # IC ORbetas
# Notas: 
# 1) forte indicio de que as categorias preta e outra se podem fundir numa unica categoria, i.e., 
#    criar apenas uma variavel X2 = 0 se race white vs X2=1 c.c.
# 2) para lwt (var. quantitativa) pode nao fazer sentido IC para o aumento de 1 unidade em lwt


# -- OR e respetivos IC para aumento de 10 libras
# supondo que interessa o aumento de 10 unidades em lwt
a <- 10
exp(a*coef(fit6)[5])                   # OR para um aumento de 10 libras em lwt
ICbeta4 <- confint.default(fit6)[5,]   # IC beta4
exp(a*ICbeta4)                         # IC a 95% para OR para um aumento de 10 libras em lwt

# grafico para aumentos de lwt entre 1 e 80 libras #
(a <- 1:80)
OR_alwt <- exp(a*coef(fit6)[5])       # OR para um aumentos de a libras em lwt
ICbeta4 <- confint.default(fit6)[5,]  # IC beta4
liOR_alwt <- exp(a*ICbeta4[1])        # limite inferior do IC a 95% para OR para aumentos de a libras em lwt
lsOR_alwt <- exp(a*ICbeta4[2])        # limite inferior do IC a 95% para OR para aumentos de a libras em lwt
plot(a,                               # valores do eixo x (os aumentos)
     OR_alwt,                         # valores do eixo y (os OR)
     main="",                         # titulo do grafico
     xlab="Aumento do peso",          # legenda do eixo x
     ylab="OR",                       # legenda do eixo y
     lty=1,                           # tipo de linha (1=continua)
     type="l")                        # tipo de grafico (l=linha)
# adicionar linhas ao grafico correspondentes aos limites do IC
lines(a,                              # valores do eixo x (os aumentos) 
      liOR_alwt,                      # valores do eixo y (limite inferior do IC para OR)
      lty=3)                          # tipo de linha (3=picotada)
lines(a,                              # valores do eixo x (os aumentos)  
      lsOR_alwt,                      # valores do eixo y (limite superior do IC para OR)
      lty=3)                          # tipo de linha (3=picotada)


# como OR<1, facilita a interpretacao inverter o OR (para ser OR>1) 
# a interpretação passara a ser para uma DIMINUICAO de 10 libras 
a <- 10
1/exp(a*coef(fit6)[5])                       # OR para uma diminuicao de 10 libras em lwt
1/exp(a*ICbeta4)                             # Atencao IC errado porque o limite inferior > limite superior
c(1/exp(a*ICbeta4[2]), 1/exp(a*ICbeta4[1]))  # IC a 95% para OR para uma diminuicao de 10 libras em lwt (ignorar linha de cabeçalho)


# -- Estimação da probabilidade para um dado perfil
# perfil: mae fumadora (smoke=yes), de race white e que pesava (lwt) 150 libras no ultimo período menstrual 
library(faraway)
x0 <- c(1, 1, 0, 0, 150)           # valores dos coeficientes para o perfil pretendido
eta0 <- sum(x0*coef(fit6))         # calculo do logit
ilogit(eta0)                       # probabilidade estimada
(cm <- summary(fit6)$cov.unscaled) # matriz de variancias e covariancias
se <- sqrt(t(x0) %*% cm %*% x0)    # desvio padrão associado aos dados do perfil
# IC a 95% para a probabilidade estimada
ilogit(c(eta0-qnorm(0.975)*se,     # limite inferior
         eta0+qnorm(0.975)*se))    # limite superior



# ------ avaliar pressuposto de linearidade (para covariaveis continuas) -----

# -- Metodo dos quartis
Qis <- as.numeric(quantile(lowbwt$lwt, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
lowbwt$lwtCAT<- cut(lowbwt$lwt,            # variavel a categorizar
                    breaks=Qis,            # pontos de corte das classes (nos quartis)
                    right=FALSE,           # classes abertas a direita
                    include.lowest=TRUE)   # a ultima classe e fechada a direita
table(lowbwt$lwtCAT)
# ajustar modelo com a variavel quantitativa categorizada
fit6a <-glm(low ~ smoke+race+lwtCAT, 
         family=binomial("logit"),
         data=lowbwt)
# coeficientes estimados
summary(fit6a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1º beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(fit6a$coef[5:7]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1º beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de lwt categorizada")  # titulo do grafico
lines(lowess(x,y))                            # adiciona uma linha ao grafico a unir os pontos
# Notas: 
# 1) o grafico deve ser linear
# 2) ha alguns desvios da linearidade mas nao tao evidentes para a colocar verdadeiramente em causa 
# 3) deve conjugar-se este resultado com o de outros metodos e so depois decidir


# -- Metodo lowess
plot(lowess(predict(fit6)~lowbwt$lwt),   # grafico de dispersao dos valores preditos vs variavel lwt
     type="l", 
     xlab="lwt", 
     ylab="logOdds")
# Nota: Como o grafico tem um comportamento linear, nao existem razoes para duvidarmos que lwt nao e linear com o logit

# -- Metodo GAM
library(gam)
fit6g <- gam(low~smoke+race+s(lwt), 
             family=binomial("logit"),
             data=lowbwt)
plot(fit6g)
# Nota: Linearidade nao tao evidente como no caso anterior, mas sem grandes desvios que a permitam colocar em causa

# -- Metodo dos polinomios fraccionarios 
library(mfp)
fit6b <-mfp(low~smoke+race+fp(lwt), 
            family = binomial(link = logit), 
            data=lowbwt)
print(fit6b)
summary(fit6b)
# Nota: neste caso o modelo sugerido e igual ao inicial e portanto validamos a linearidade com o logit
# Se fosse sugerida uma transformacao entao:
# 1) comparar os dois modelos usando o valor p calculado a partir de pchisq(deviance(fit6)-deviance(fit6b), gl)
#    com gl=1 no caso de ser uma transformcao simples ou gl=3 no caso de sugerir a transformacao dupla
# 2) comparar tambem os AIC dos dois modelos: extractAIC(fit6); extractAIC(fit6b) 



######################################################################
# --------------- proximas aulas --------------
################################################################

# ---------- Bondade do ajustamento (Avaliar ajustamento do modelo) -------------

# -- Deviance
resumo <- summary(fit6)     # modelo ajustado
(gl <- resumo$df.residual)    # graus de liberdade do qui-quadrado
(ET <- resumo$deviance)       # Deviance
(valorp <- 1-pchisq(ET,gl))   # valor p
paste("Deviance = ", round(ET,4))
paste("df = ", gl)
paste("p.value = ", round(valorp,4))


# -- Teste de Hosmer e Lemeshow (quando ha pelo menos uma covariavel quantitativa)
library(generalhoslem)             # ativar pacote necessario
?generalhoslem
(hl<-logitgof(lowbwt$low,       # valores observados y (dependente)
              fitted(fit6),     # valores ajustados y^ 
              g = 10))          # numero de classes a considerar
hl$expected                        # valores esperados
#Nota: como ha 3 classes com ei<5, devemos correr novo teste com menos grupos

(hl1<-logitgof(lowbwt$low,       # valores observados y (dependente) 
               fitted(fit6),     # valores ajustados y^ 
               g = 4))           # numero de classes a considerar
hl1$expected                        # valores esperados
# como valor p=0.369 podemos concluir que o modelo se ajusta aos dados


# -- R2 de Nagelkerke
(n<-length(lowbwt$low))              # numero de observacoes da variavel resposta
R2N<-(1-exp((fit6$dev-fit6$null)/n))/(1-exp(-fit6$null/n)) 
R2N

# -- R2 de McFadden
R2.MF <- 1-fit6$dev/fit6$null # 35% ganho de informa??o do mf em relacao ao modelo nulo
R2.MF
# 8.4% de ganho de informacao do modelo fit 6 em relacao ao modelo nulo




# ----------------- Capacidade discriminativa (curva ROC) -------------------
library(Epi)
ROC(form=low~smoke+race2+lwt,
    data=lowbwt, 
    plot="ROC",
    PV=T,
    MX=T,
    AUC=T)
# No output pode ver-se que AUC=0.684, o que indica uma capacidade disciminativa pobre
# Para um ponto de corte optimo=0.253 tem-se uma sensibilidade de 88.1%, mas uma especificadade de apenas 40.8%





# ----------------------- Analise de Residuos ------------------------


# -- Analise de residuos via padroes das covariáveis
# Nota: realizar esta análise sempre que o numero de padroes muito inferior ao numero de individuos
# padrao = conjunto de individuos com os mesmos valores nas covariaveis

library(epiR)   # ativar pacote necessario
# transformar a variavel resposta (y) em numerica de zeros e uns
plow <- ifelse(as.numeric(lowbwt$low)==1, 0, 1)
# Extrair os padroes de covariaveis
(modelo.cp <- epi.cp(lowbwt[c("smoke","race","lwt")]))
str(modelo.cp)
# Neste modelo temos 132 padrões e 189 indivíduos

# numero de sucessos por padrao
(nsucpadrao <- as.vector(by(plow, as.factor(modelo.cp$id), FUN = sum)))
plot(nsucpadrao,                                   # variavel a representar 
     main = "Número de sucessos por padrão",       # titulo geral
     xlab = "Padrão",                              # titulo eixo x
     ylab = "N.º de sucessos")                     # titulo eixo y

# probabilidade estimada de cada padrao das covariaveis
(prob.est <- as.vector(tapply(fitted(fit6), as.factor(modelo.cp$id), min)))
plot(prob.est,                                     # variavel a representar 
     main = "Probabilidade estimada por padrão",   # titulo geral
     xlab = "Padrão",                              # titulo eixo x
     ylab = "Probabilidade estimada")              # titulo eixo y

# residuos, deltabetas e deltaquis
(residuos <- epi.cpresids(obs = lowbwt, fit = prob.est, covpattern = modelo.cp))
# Representar probabilidades estimadas vs residuos deviance por padrão
plot(prob.est, 
     residuos$deviance, 
     xlab="Probabilidades estimadas", 
     ylab="Resíduos deviance")
# Nota: Nenhum residuo excessivamente elevado em valor absoluto e que se destaque dos restantes

# representar probabilidades estimadas vs DeltaBeta (distancia de Cook)
plot(prob.est, 
     residuos$sdeltabeta, 
     xlab="Probabilidades Estimadas", 
     ylab="Distância de Cook")
identify(prob.est, 
         residuos$sdeltabeta)       # identificar padroes que se destacam
subset(lowbwt, 
       modelo.cp$id==98)              # numero de individuos com o padrão 98: 2


# representar probabilidades estimadas vs DeltaDev
Delta_Dev <- residuos$deviance^2/(1-residuos$leverage)
plot(prob.est, Delta_Dev, 
     xlab="Probabilidades estimadas", 
     ylab="Delta_Dev")
identify (prob.est, Delta_Dev)       # identificar padrões que se destacam (Deltadev>4)#
subset(lowbwt, modelo.cp$id==115)   # indivíduos com o padrão 58: apenas o 147 #
subset(lowbwt, modelo.cp$id==121)   # indivíduos com o padrão 18: apenas o 155 #
subset(lowbwt, modelo.cp$id==140)   # indivíduos com o padrão 18: apenas o 183 #

# Representar probabilidades estimadas vs Delta_Qui em grafico de bolhas com area proporcional a Delta_Qui
raio <- sqrt(residuos$deltabeta/pi) # dimensionar correctamente por area
symbols(prob.est, residuos$deltachi, circles=raio, inches=0.35, 
        xlab="Probabilidades estimadas", 
        ylab="Alteração no qui-quadrado de Pearson")
text(prob.est, residuos$deltachi, 
     residuos$cpid, cex=0.5) # mostrar padrão correspondente

# Representar probabilidades estimadas vs Delta_D em grafico de bolhas com area proporcional a Delta_Qui
raio <- sqrt(residuos$deltabeta/pi) # dimensionar correctamente por area
symbols(prob.est, Delta_Dev, circles=raio, inches=0.35, 
        xlab="Probabilidades estimadas", 
        ylab="Alteração na Deviance")
text(prob.est, Delta_Dev, residuos$cpid, cex=0.5) # mostrar padrao correspondente



# -- Analise de residuos via individuos
# Nota: quando existem variaveis continuas no modelo quase sempre havera quase
# tantos padroes como covariaveis e esta abordagem pode ser usada

# residuos deviance
rD <- residuals(fit6, type="deviance")  # |residuos deviance| > a 2 podem indicar lack of fit
plot(fitted(fit6), rD, 
     xlab= "Probabilidades estimadas", 
     ylab= "Resíduos Deviance") 
abline(h=2,             # adiciona linha ao grafico, neste caso uma linha horizontal na posicao 2
       lty=2,           # tipo de linha: tracejada
       col="red")       # cor da linha
# Nota: |residuos deviance| < a 2, logo nao ha lack of fit


# distancia de Cook
plot(cooks.distance(fit6), 
     xlab="Indivíduos", 
     ylab="Distância de Cook")
identify(cooks.distance(fit6), 
         labels=rownames(lowbwt)) # identificar os pontos que se destacam
halfnorm(cooks.distance(fit6))    # grafico seminormal 
# Nota: nenhum valor com uma distancia de Cook>0.5


# grafico do leverage vs obs
# Para averiguar a existencia de possiveis observacoes discordantes
h <- influence(fit6)            # valores do leverage
plot(h$hat,
     ylab="leverage",
     xlab="Indivíduos") 
(n <- length(lowbwt$low))       # numero de observacoes da variavel resposta
(p <- length(coef(fit6)))       # numero de parametros no modelo
abline(h=2*(p+1)/n,             # adiciona linha ao grafico, neste caso uma linha horizontal na posicao 2*(p+1)/n
       lty=2,                   # tipo de linha: tracejada
       col="red")               # cor da linha
# Nota: apesar de existirem muitos pontos acima do limite não há nenhum excessivamente elevado nem que se destaque 


# representa os residuos studentizados vs leverage e ainda as distâncias de Cook
library(car)   # ativar pacote necessario
influencePlot(fit6,                                             # modelo ajustado
              id.n = 2,                                         # id.n = n: identifica os n pontos mais influentes em cada uma das 3 medidas
              xlab = "leverage",                                # titulo eixo x
              main = "Pontos influentes",                       # titulo
              sub = "Circulo proporcional à distância de Cook")	# subtitulo
# Nota: so ha uma observacao (68) com leverage acima do limite 3*media(leverage)


# identificar as possiveis observacoes influentes
temp<-influence.measures(fit6)
(lista <- which(apply(temp$is.inf, 1, any)))	 # lista as candidatas a observacoes influentes
summary(temp)                                  # detalhe das candidatas a observacoes influentes
# Nota: a observacao 68 destaca-se na medida leverage (embora o leverage nao seja demasiado elevado)
# As observacoes 68, 76 e 106 destacam-se na medida cov.r


# residuos DfBeta
rdf <- dfbetas(fit6) # residuos dfbeta
# Representacao dos dfbeta vs preditores 
plot(lowbwt$smoke, rdf[,3],   # grafico dfbeta ~ smoke
     xlab="Smoke", 
     ylab="Dfbeta")
plot(lowbwt$race, rdf[,2],    # grafico dfbeta ~ race
     xlab="race", 
     ylab="Dfbeta")
plot(lowbwt$lwt, rdf[,4],     # grafico dfbeta ~ lwt
     xlab="ht", 
     ylab="Dfbeta")
# Nota: nenhum residuo dfbeta excessivamente elevado (>1) ou que se destaque


# Representacao dos dfbeta (standartizados) vs obs
# Para averiguar a existencia de possiveis observacoes discordantes
library(car)
dfbetasPlots(fit6)
# Nota: neste grafico sao representadas linhas a +/- 1 desvio-padro e a +/- 1
# Neste caso nem se veem essas linhas.
# Nenhuma observacao tem um residuo dfbeta excessivamente elevado (>1) ou que se destaque.


# -- Avaliar impacto das observacoes que se destaram na analise de residuos na estimacao dos coeficentes e no deviance do modelo
fit6.1 <- update(fit6,                               # actualiza o modelo estimado
                 subset=rownames(lowbwt)!= 68,       # ignorando a observacao 68
                 data=lowbwt)                        # conjunto de dados
summary(fit6.1)
(fit6$coef-fit6.1$coef)                              # mede a diferenca na estimacao dos parametros
((fit6$coef-fit6.1$coef)*100/fit6$coef)              # mede a influencia (em %) desta observacao na estimacao dos parametros
# Nota: a observacao e influente na estimacao da constante do modelo
((fit6$deviance-fit6.1$deviance)*100/fit6$deviance)  # mede a consistencia (em %)
# Nota: impacto residual na deviance logo a observacao 68 nao e inconsistente
(hl1 <- logitgof(lowbwt$low,              # valores observados y (dependente) 
                  fitted(fit6),           # valores ajustados y^ 
                  g = 4))                 # numero de classes a considerar
(hl1 <- logitgof(lowbwt$low[rownames(lowbwt)!= 68],   # valores observados y (dependente) sem a obs 68
                  fitted(fit6.1),                     # valores ajustados y^ 
                  g = 4))                             # numero de classes a considerar
# Nota: sem grande impacto na bondade de ajustamento
# Conclusao: decide-se manter o individuo




# -------- testar cada preditor relativamente ao modelo com todos -----------

# Para avaliar a significancia dos efeitos
drop1(fit6, test = "Chisq")




# -------- Selecao das variaveis pelos metodos backward e forward -----------

# -- metodo backward para selecionar variaveis
step(fit6, test = "Chisq")

# -- metodo forward para selecionar variaveis
step(fit0, scope=list(lower=fit0, upper=fit6), direction="forward")




# --------------------------------- GRAFICOS ----------------------------------


# -- Representacao dos OR com IC (verosimilhanca de perfil): SO DEVE SER USADA EM MODELOS SEM INTERACAO

# OR e intervalos de confiança baseados na verosimilhança de perfil
cbind(exp(coef(fit6)), exp(confint(fit6)))  # ver os OR e ICs
source ("sjPlotOdds.R")
plotOdds(fit6, axisLimits=c(0, 30.0), gridBreaksAt=2) # grafico


# -- grafico para diminuicoes de lwt entre 1 e 80 libras
d <- 1:80
OR_dlwt <- 1/exp(d*coef(fit6)[5])     # OR para uma diminuicao de d libras em lwt
ICbeta4 <- confint.default(fit6)[5,]  # IC beta4
liOR_dlwt <- 1/exp(d*ICbeta4[2])      # limite inferior do IC a 95% para OR para uma diminuicao de d libras em lwt
lsOR_dlwt <- 1/exp(d*ICbeta4[1])      # limite inferior do IC a 95% para OR para uma diminuicao de d libras em lwt
plot(d,                               # valores do eixo x (as diminuicoes)
     OR_dlwt,                         # valores do eixo y (os OR)
     main="",                         # titulo do grafico
     xlab="Diminuição do peso",          # legenda do eixo x
     ylab="OR",                       # legenda do eixo y
     lty=1,                           # tipo de linha (1=continua)
     type="l")                        # tipo de grafico (l=linha)
# adicionar linhas ao grafico correspondentes aos limites do IC
lines(d,                              # valores do eixo x (as diminuicoes) 
      liOR_dlwt,                      # valores do eixo y (limite inferior do IC para OR)
      lty=3)                          # tipo de linha (3=picotada)
lines(d,                              # valores do eixo x (as diminuicoes)  
      lsOR_dlwt,                      # valores do eixo y (limite superior do IC para OR)
      lty=3)                          # tipo de linha (3=picotada)
# adicionar uma linha correspondente a OR=1
abline(h=1,                # posicao da linha (horizontal com y=1)
       col="red",          # cor da linha
       lty=3,              # tipo de linha: tracejada
       lwd=2)              # espessura da linha



# -- Perfis de probabilidade

# vamos considerar: fumadora vs. nao fumadora, sendo mulher branca, para pesos peso entre 80 e 250
slwt <- seq(80, 250, 1)                                             # valores do peso
# probabilidade prevista para as nao fumadoras
pred.no <- predict(fit6, 
                   data.frame(lwt=slwt, smoke="no", race="white"),  # conjunto de dados
                   type="response")                                 # probabilidade prevista
# grafico da evolucao do perfil de probabilidade de uma mulher branca, nao fumadora com peso entre 80 e 250
plot (slwt, pred.no, 
      type="l", ylim=c(0,1), 
      main="Raça branca", 
      xlab="Peso no último período menstrual", 
      ylab="Probabilidade estimada de ter um bébé abaixo do peso", 
      col="chocolate")

# probabilidade prevista para as fumadoras
pred.yes <- predict (fit6, 
                     data.frame (lwt=slwt, smoke="yes", race="white"),  # conjunto de dados
                     type="response")                                   # probabilidade prevista

# juntar ao grafico anterior a evolucao do perfil de probabilidade de uma mulher branca, fumadora com peso entre 80 e 250
lines(slwt, pred.yes, col="darkcyan")

# adicionar legenda ao grafico
legend("topright", lty=c(1,1), 
       col=c("chocolate", "darkcyan"), 
       c("Não fumadora", "Fumadora"), 
       bty='n')


# -- Perfil de probabilidade com banda de confianca
# vamos considerar: fumadora vs. nao fumadora, sendo mulher branca, para pesos peso entre 80 e 250

# dados com o perfil pretendido
newdata1 <- with(lowbwt, 
                 data.frame(lwt=rep(seq(from = 80, to = 250, length.out = 170),2), smoke="no", race="white"))
# juntar aos dados anteriores as probabilidades previstas
newdata2 <- cbind(newdata1, 
                  predict(fit6, newdata = newdata1, type = "link",se = TRUE))
# juntar aos ados anteriores os limites dos IC
newdata3 <- within(newdata2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - qnorm(0.975) * se.fit)
  UL <- plogis(fit + qnorm(0.975) * se.fit)
})

newdata3 <- within(newdata2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - qnorm(0.975) * se.fit)
  UL <- plogis(fit + qnorm(0.975) * se.fit)
})


# -- grafico da evolucao do perfil de probabilidade de uma mulher branca, nao fumadora com peso entre 80 e 250
ggplot(newdata3, aes(x = lwt, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL,ymax = UL, fill = smoke), alpha = 0.2) + 
  geom_line(aes(colour = smoke), size = 1) +
  xlab("Peso no último período menstrual") +            # titulo do eixo x
  ylab("Probabilidade prevista de ter peso baixo")      # titulo do eixo y



# -- representacao da probabilidade prevista

# Criar data.frame com probabilidade prevista e o peso observado
previstos <- data.frame(probabilidade=fit6$fitted.values, low=lowbwt$low)
# ordenar por ordem crescente de probabilidade o data.frame anterior
previstos <- previstos[order(previstos$probabilidade, decreasing=FALSE),]
# criar uma coluna que identifica o numero da linha
previstos$rank <- 1:nrow(previstos)
# ativar pacotes necessarios
library(ggplot2)
library(cowplot)
# grafico
ggplot(data=previstos,                                  # conjunto de dados
       aes(x=rank, y=probabilidade)) +                  # variaveis a representar no eixo x e y
  geom_point(aes(color=low),                            # variavel y
             alpha=1, shape=4, stroke=2) +              # definir o simbolo a usar, e outras caracteristicas do simbolo 
  xlab("Indice")+                                       # titulo do eixo x
  ylab("Probabilidade prevista de ter peso baixo")      # titulo do eixo y
# Nota: podia ser melhor!
