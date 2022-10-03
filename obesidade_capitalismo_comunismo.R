
# Obesidade em países comunistas e capitalistas --------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 02/10/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/obesity -------------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Obesidade é mais comumente medida como índice de Massa Corporal (IMC)
### A Organização Mundial de Saúde define o IMC como "um simples índice de
### peso por altura que é comumente usado para classificar abaixo do peso,
### sobrepeso e obesidade em adultos.

### Os valores de IMC são usados para definir se um indivíduo é considerado
### para estar abaixo do peso, saudável, cokm sobrepeso ou obeso. A Organização
### Mundial de Saúde define essas categorias usando os seguintes pontos de
### corte: IMC entre 25 e 30 é considerado sobrepeso e IMC maior que 30 é
### definido como obeso.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

obe <- read.csv("share-of-deaths-obesity.csv")
view(obe)
names(obe)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

obe <- obe %>%
  select(-Code) %>%
  rename(obesidade_morte = Deaths...Cause..All.causes...Risk..High.body.mass.index...Sex..Both...Age..Age.standardized..Percent.) %>%
  view()

obe1 <- obe %>%
  filter(Entity %in% c("United States", "Japan", "Germany",
                       "China", "Cuba", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(obesidade_morte),
            sd = sd(obesidade_morte), n = n(),
            se = sd/sqrt(n)) %>%
  view()

