
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

obe2 <- obe %>%
    filter(Entity %in% c("United States", "Japan", "Germany",
                       "China", "Cuba", "North Korea")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(obe1, aes(x = fct_reorder(Entity, media), 
                 y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                    size = 0.8, width = 0.2) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677", "#DDCC77",
                              "#117733", "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("Coreia do Norte", "Japão", "China",
                              "Cuba", "Alemanha", "Estados Unidos")) +
  labs(x = "Países", y = "Porcentagem de mortes 
                          atribuídas a obesidade") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))
