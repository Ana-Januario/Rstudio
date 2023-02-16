# Rstudio

Nesta secção temos alguns trabalhos realizados em Rstudio, que contemplam análise exploratoria de dados, técnicas para categorização de dados e também criação de modelos. 
Geralmente utilizamos datasets que estão disponíveis no CRAN entretanto, no trabalho de regressão logistica eu carreguei o dataset num ficheiro CSV.
Essa mesma base de dados está disponível no CRAN e está explicada no livro Hosmer, D.W., Lemeshow, S. and Sturdivant, R.X. (2013) Applied Logistic Regression, 3rd ed., New York: Wiley.
Trata-se de um dataframe com 189 linhas e 11 variaveis:

id-Código de Identificação

low-baixo peso ao nascer (1: >= 2500, 2: < 2500 g)

age-idade da mãe (anos)

lwt-Peso da mãe na última menstruação (Libras)

race-Raça (1: Branco, 2: Preto, 3: Outro)

smoking- Tabagismo durante a gravidez (1: Não, 2: Sim)

ptl-História de trabalho de parto prematuro (1: Nenhum, 2: Um, 3: Dois, etc)

ht-História de hipertensão (1: Não, 2: Sim)

ui-Presença de irritabilidade uterina (1: Não, 2: Sim)

ftv-Número de consultas médicas durante o primeiro trimestre (1: Nenhuma, 2: Uma, 3: Duas, etc.)

bwt-Peso de nascimento registrado (gramas)

Já o caso da regressao linear, eu chamei o dataset direto do CRAN.

Em caso de intersse de explorar esses datasets, pode verificar em https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/00Index.html
