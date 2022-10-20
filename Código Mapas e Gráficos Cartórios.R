
#Tomando as livrarias
library(tidyverse)
library(dplyr)
library(data.table)
library(psych)
library(Hmisc)
library(Weighted.Desc.Stat)
library(weights)
library(plotly)
library(wesanderson)
library(RColorBrewer)
library(ggplot2)
library(lessR)
library(readxl)
library(geobr)
library(ggpubr)

#chamando o arquivo
setwd("C:/Users/jorge.ferreira/Desktop/Arq. Jorge/Cartórios/")
df.data <- read_excel("C1.xlsx")

############################  Gráficos (Histograma por Estado)
#Pessoas por Cartório
dados19 <- ggplot(summarise(group_by(df.data, UF), PC2019 = mean(PC2019)), aes(x=UF, y=PC2019)) + geom_col()
dados20 <- ggplot(summarise(group_by(df.data, UF), PC2020 = mean(PC2020)), aes(x=UF, y=PC2020)) + geom_col()
dados21 <- ggplot(summarise(group_by(df.data, UF), PC2021 = mean(PC2021)), aes(x=UF, y=PC2021)) + geom_col()
ggarrange(dados19, dados20, dados21 + rremove("x.text"), 
          labels = c("2019", "2020", "2021"),
          ncol = 2, nrow = 2)

data19 <- ggplot(summarise(group_by(df.data, UF), MRC2019 = mean(MRC2019)), aes(x=UF, y=MRC2019)) + geom_col()
data20 <- ggplot(summarise(group_by(df.data, UF), MRC2020 = mean(MRC2020)), aes(x=UF, y=MRC2020)) + geom_col()
data21 <- ggplot(summarise(group_by(df.data, UF), MRC2021 = mean(MRC2021)), aes(x=UF, y=MRC2021)) + geom_col()
ggarrange(data19, data20, data21 + rremove("x.text"), 
          labels = c("2019", "2020", "2021"),
          ncol = 2, nrow = 2)

dado19 <- ggplot(summarise(group_by(df.data, UF), FM2019 = mean(FM2019)), aes(x=UF, y=FM2019)) + geom_col()
dado20 <- ggplot(summarise(group_by(df.data, UF), FM2020 = mean(FM2020)), aes(x=UF, y=FM2020)) + geom_col()
dado21 <- ggplot(summarise(group_by(df.data, UF), FM2021 = mean(FM2021)), aes(x=UF, y=FM2021)) + geom_col()
ggarrange(dado19, dado20, dado21 + rremove("x.text"),
          labels = c("2019", "2020", "2021"),
          ncol = 2, nrow = 2)

ggplot(summarise(group_by(df.data, UF), PRO2021 = mean(PRO2021)), aes(x=UF, y=PRO2021)) + geom_col()

############################   Mapas
df.data <- as_tibble(df.data)

dados_estados <- df.data %>%
  mutate(abbrev_state = factor(UF)) %>%
  group_by(abbrev_state) %>%
  summarise(n = PC2019)
dados_mapa <- read_state(year=2019, showProgress = F) %>%
  left_join(dados_estados)
dados_mapa %>%
  ggplot() +
  geom_sf(aes(fill=n)) +
  labs(title = "Mapa do Brasil", fill="Pessoas por Cartório em 2019")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
##########
dados_estados <- df.data %>%
  mutate(abbrev_state = factor(UF)) %>%
  group_by(abbrev_state) %>%
  summarise(n = PC2020)
dados_mapa <- read_state(year=2019, showProgress = F) %>%
  left_join(dados_estados)
dados_mapa %>%
  ggplot() +
  geom_sf(aes(fill=n)) +
  labs(title = "Mapa do Brasil", fill="Pessoas por Cartório em 2020")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
##########
dados_estados <- df.data %>%
  mutate(abbrev_state = factor(UF)) %>%
  group_by(abbrev_state) %>%
  summarise(n = PC2021)
dados_mapa <- read_state(year=2019, showProgress = F) %>%
  left_join(dados_estados)
dados_mapa %>%
  ggplot() +
  geom_sf(aes(fill=n)) +
  labs(title = "Mapa do Brasil", fill="Pessoas por Cartório em 2021")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
##########
dados_estados <- df.data %>%
  mutate(abbrev_state = factor(UF)) %>%
  group_by(abbrev_state) %>%
  summarise(n = MRC2019)
dados_mapa <- read_state(year=2019, showProgress = F) %>%
  left_join(dados_estados)
dados_mapa %>%
  ggplot() +
  geom_sf(aes(fill=n)) +
  labs(title = "Mapa do Brasil", fill="Milhões de R$ no PIB(2019) por Cartório em 2019")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
##########
dados_estados <- df.data %>%
  mutate(abbrev_state = factor(UF)) %>%
  group_by(abbrev_state) %>%
  summarise(n = MRC2020)
dados_mapa <- read_state(year=2019, showProgress = F) %>%
  left_join(dados_estados)
dados_mapa %>%
  ggplot() +
  geom_sf(aes(fill=n)) +
  labs(title = "Mapa do Brasil", fill="Milhões de R$ no PIB(2019) por Cartório em 2020")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
##########
dados_estados <- df.data %>%
  mutate(abbrev_state = factor(UF)) %>%
  group_by(abbrev_state) %>%
  summarise(n = MRC2021)
dados_mapa <- read_state(year=2019, showProgress = F) %>%
  left_join(dados_estados)
dados_mapa %>%
  ggplot() +
  geom_sf(aes(fill=n)) +
  labs(title = "Mapa do Brasil", fill="Milhões de R$ no PIB(2019) por Cartório em 2021")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
##########
dados_estados <- df.data %>%
  mutate(abbrev_state = factor(UF)) %>%
  group_by(abbrev_state) %>%
  summarise(n = FM2019)
dados_mapa <- read_state(year=2019, showProgress = F) %>%
  left_join(dados_estados)
dados_mapa %>%
  ggplot() +
  geom_sf(aes(fill=n)) +
  labs(title = "Mapa do Brasil", fill="Faturamento Médio por Cartório em 2019")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
##########
dados_estados <- df.data %>%
  mutate(abbrev_state = factor(UF)) %>%
  group_by(abbrev_state) %>%
  summarise(n = FM2020)
dados_mapa <- read_state(year=2019, showProgress = F) %>%
  left_join(dados_estados)
dados_mapa %>%
  ggplot() +
  geom_sf(aes(fill=n)) +
  labs(title = "Mapa do Brasil", fill="Faturamento Médio por Cartório em 2020")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
##########
dados_estados <- df.data %>%
  mutate(abbrev_state = factor(UF)) %>%
  group_by(abbrev_state) %>%
  summarise(n = FM2021)
dados_mapa <- read_state(year=2019, showProgress = F) %>%
  left_join(dados_estados)
dados_mapa %>%
  ggplot() +
  geom_sf(aes(fill=n)) +
  labs(title = "Mapa do Brasil", fill="Faturamento Médio por Cartório em 2021")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
##########
dados_estados <- df.data %>%
                  mutate(abbrev_state = factor(UF)) %>%
                  group_by(abbrev_state) %>%
                  summarise(n = PRO2021)
dados_mapa <- read_state(year=2019, showProgress = F) %>%
                left_join(dados_estados)
dados_mapa %>%
  ggplot() +
  geom_sf(aes(fill=n)) +
  labs(title = "Mapa do Brasil", fill="Produtividade por Cartório em 2021")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
#############
dados_estados <- df.data %>%
  mutate(abbrev_state = factor(UF)) %>%
  group_by(abbrev_state) %>%
  summarise(n = PF2021)
dados_mapa <- read_state(year=2019, showProgress = F) %>%
  left_join(dados_estados)
dados_mapa %>%
  ggplot() +
  geom_sf(aes(fill=n)) +
  labs(title = "Mapa do Brasil", fill="Pessoas por Notário em 2021")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
##########
dados_estados <- df.data %>%
  mutate(abbrev_state = factor(UF)) %>%
  group_by(abbrev_state) %>%
  summarise(n = MRN2021)
dados_mapa <- read_state(year=2019, showProgress = F) %>%
  left_join(dados_estados)
dados_mapa %>%
  ggplot() +
  geom_sf(aes(fill=n)) +
  labs(title = "Mapa do Brasil", fill="Milhão de R$ no PIB(2019) por Notário em 2021")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


rm(list=ls()) 
