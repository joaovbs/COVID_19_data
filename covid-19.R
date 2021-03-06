# Covid-19

setwd("C:/Users/borge/OneDrive/Documents/COVID-19")

library(readr)
library(ggplot2)
library(dplyr)
options(scipen = 999)

## Importar arquivo .csv


covid<- read_csv2("arquivo_geral.csv")

View(covid)

covid[,1:2]<- lapply(covid[,1:2], as.factor)
covid[,3]<- lapply(covid[,3], as.POSIXct, format = "%Y-%m-%d")
summary(covid)
glimpse(covid)


## Casos
covid.reg<- covid %>%
  group_by(data, regiao) %>%
  summarize(n.casos = sum(casosAcumulados))

covid.reg
tail(covid.reg)
## Gr�fico - primeiro teste com todos estados

ggplot(covid, aes(data, casosAcumulados, col = estado)) + 
  geom_point() + geom_line() + scale_y_log10() + 
  labs(x = "Data", y = "N� de casos") + theme_classic()

## Gr�fico - por regi�o

#tiff("test1.tiff", units="in", width=7, height=4, res=300)

ggplot(covid.reg, aes(data, n.casos, col = regiao)) + 
  geom_point() + geom_line() + scale_y_log10() + 
  labs(x = "Data", y = "N� de casos", col = "Regi�o") + theme_classic() +
  ggtitle("Casos de Covid-19 por regi�o geogr�fica")

#dev.off()

## Mortes

covid.morte<- covid %>%
  filter(obitosAcumulados >= 1)

covid.morte.reg<- covid.morte %>%
  group_by(regiao, data) %>%
  summarize(n.mortes = sum(obitosAcumulados))

covid.morte.reg

## Gr�fico por regi�o

#tiff("test.tiff", units="in", width=7, height=4, res=300)

ggplot(covid.morte.reg, aes(data, n.mortes, col = regiao)) + 
  geom_point() + geom_line() + scale_y_log10() + 
  labs(x = "Data", y = "N� de �bitos", col = "Regi�o") + theme_classic() +
  ggtitle("�bitos por Covid-19 por regi�o geogr�fica")

#dev.off()

## Importar brasil_eua.csv

covid_br_eua<- read_csv2("brasil_eua.csv")

str(covid_br_eua)

covid_br_eua[,1:2]<- lapply(covid_br_eua[,1:2], as.factor)
covid_br_eua[,3]<- lapply(covid_br_eua[,3], as.POSIXct, format = "%Y-%m-%d")
summary(covid_br_eua)
glimpse(covid_br_eua)
View(covid_br_eua)

## Filtrando para m�ltiplas vari�veis e descobrindo os estados com mais casos


covid.top<- covid_br_eua %>%
  group_by(regiao) %>%
  filter(!estado %in% c("NY", "SP", "RJ") & data == '2020-04-29 21:00:00') %>%
  arrange(desc(casosAcumulados))

View(covid.top)

## Gr�fico de casos - NY, SP, RJ, CE, AM, RS, DF

covid.6<- covid_br_eua %>%
  filter(estado %in% c("NY", "SP", "RJ", "CE", "AM", "DF"))

covid.NY<- covid_br_eua %>%
  filter(estado == "NY")

### Subset para incluir Nova Iorque em uma linha separada

ggplot(subset(covid.6, estado %in% c("SP", "RJ", "CE", "AM", "DF")), 
       aes(data, casosAcumulados, col = estado)) + 
  geom_point() + geom_line() + 
  geom_point(data = covid.6 %>% filter(estado == "NY"),
             col = "dimgrey", size =0.8, alpha = 0.4) +
  geom_line(data = covid.6 %>% filter(estado == "NY"), 
            col = "dimgrey", size = 0.6, alpha = 0.4) + 
  scale_y_log10() + 
  annotate("text", x = as.POSIXct("2020-04-30 21:00:00"), 
           y = 220000, label = "NY", size = 3, col = "dimgrey") + 
  labs(x = "Data", y = "N� de casos", col = "Estado") +
  ggtitle("Casos Covid-19 por estado") +
  theme_bw()


## Gr�fico de mortes - NY, SP, RJ, CE, PE, AM, RS, DF

covid.6.mortes<- covid.6 %>%
  filter(obitosAcumulados >= 1)

ggplot(subset(covid.6.mortes, estado %in% c("SP", "RJ", "CE", "PE", "AM", "DF", "RS")), 
       aes(data, obitosAcumulados, col = estado)) + 
  geom_point() + geom_line() + 
  geom_point(data = covid.6.mortes %>% filter(estado == "NY"),
             col = "dimgrey", size =0.8, alpha = 0.4) +
  geom_line(data = covid.6.mortes %>% filter(estado == "NY"), 
            col = "dimgrey", size = 0.6, alpha = 0.4) + 
  scale_y_log10() + 
  annotate("text", x = as.POSIXct("2020-04-30 21:00:00"), 
           y = 12000, label = "NY", size = 3, col = "dimgrey") +
  labs(x = "Data", y = "N� de �bitos", col = "Estado") +
  ggtitle("�bitos de Covid-19 por estado") + 
  theme_bw()
