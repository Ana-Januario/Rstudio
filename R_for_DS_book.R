#https://r4ds.hadley.nz/data-visualize.html
setwd("C:/Users/Ana Januário/Desktop/databases/R_for_data_science")

#install.packages("tidyverse")

library(tidyverse)

install.packages(
  c("arrow", "babynames", "curl", "duckdb", "gapminder", 
    "ggrepel", "ggridges", "ggthemes", "hexbin", "janitor", "Lahman", 
    "leaflet", "maps", "nycflights13", "openxlsx", "palmerpenguins", 
    "repurrrsive", "tidymodels", "writexl")
)
library(palmerpenguins)
library(ggthemes)

view(penguins)

#considerando pinguins: Como é a relação entre o comprimento da nadadeira e a massa corporal?

#criando grafico com ggplot
ggplot(
  data = penguins,                                       #indicar a base de dados
  mapping = aes(x = flipper_length_mm, y = body_mass_g)  #indicar as variáveis representadas
) +
  geom_point()                                          #Indicar o tipo de grafico que eu quero

#Aparentemente a relção entre compromento da nadadeira e massa corporal é positiva


#vamos modificar a camada aesthetic e add a especie

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point()

#vamos adicionar outra camada, dessa vez um outro objeto geometrico, 
#uma linha que irá representar um modelo linear dos pontos
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +               #grafico de pontos
  geom_smooth(method = "lm")   #linha com base em um modelo linear

#Nós não queremos um modelo linear para cada especie.
#De facto, só queremos que o grafico de pontos seja representado pela especie
#Então podemos mudar o mapeamentos estéticos no nível local 

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")

#também podemos mapear a especie pela forma

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm")

#podemos escolher as legendas e escolher as cores do grafico
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

#Exercicios
#1
nrow(penguins)
ncol(penguins)

?penguins
#2- penguins$bill_depth_mm profundidade do bico?


#3
ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = bill_depth_mm )
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
    scale_color_colorblind()
#sinceramente não parece ter bem uma relação, parece mais que se forma clusters

#4- Para representar uma variável categorica e uma variável numerica
#podemos utilizar o boxplot
ggplot(
  data = penguins,
  mapping = aes(x = species, y = bill_depth_mm, fill = species)
) +
  geom_boxplot() +
  scale_fill_colorblind() +
  labs(x = "Espécie", y = "Profundidade do bico (mm)") +
  theme_minimal()

#Também podemos utilizar o grafico de densidade
ggplot(
  data = penguins,
  mapping = aes(x = bill_depth_mm, fill = species)
) +
  geom_density(alpha = 0.5) +
  scale_fill_colorblind() +
  labs(x = "Profundidade do bico (mm)", y = "Densidade") +
  theme_minimal()

#5- Pq o codigo abaixo da erro?
ggplot(data = penguins) + 
  geom_point()
#pq precisa fornecer as informações de mapeamento estético 
#(aesthetics mapping) para os eixos x e y.

#6
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species), na.rm = T) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

#7 - 

ggplot(
  data = penguins,
  mapping = aes(x = species, y = bill_depth_mm, fill = species)
) +
  geom_boxplot() +
  scale_fill_colorblind() +
  labs(title = "Data come from the palmerpenguins package.",
       x = "Espécie", y = "Profundidade do bico (mm)") +
  theme_minimal()

#8-
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = bill_depth_mm)) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Comprimento da nadadeira (mm)", y = "Massa corporal (g)") +
  theme_minimal()  +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )

#9
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)

#10
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )

#aparentemente são iguais


ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

#No futuro, você também aprenderá sobre o pipe, |>, que permitirá criar esse gráfico com
penguins |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

#Visualizar distribuições
#categoricos
ggplot(penguins, aes(x = species)) +
  geom_bar()

#organizar as barras pela frequencia
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

#numericos
#tamanho das bins do histogram
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)


ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 2000)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram()


#Grafico de densidade

ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()


ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)
