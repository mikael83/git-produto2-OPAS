############ Produto 2 ###########################
##################################################
###### script desenvolvido por Mikael Lemos ######
###### versão 1.0 - 22.07.2019 ###################
##################################################

######
### Loading / installing packages
######

#install.packages('dplyr')
library('dplyr')

#install.packages("tidyr")
library('tidyr')

#install.packages("data.table")
library('data.table')

#install.packages('stringr')
library('stringr')

# install.packages("tidyverse")
library("tidyverse")

# install.packages("lubridate")
library("lubridate")

#install.packages("Amelia")
library("Amelia")

#install.packages('Rtools')
#library('Rtools')

#install.packages('microbenchmark')
library("microbenchmark")

#install.packages('ggplot2movies')
library("ggplot2movies")

#install.packages('profvis')
library("profvis")

#install.packages('Rcpp')
library("Rcpp")

#install.packages('compiler')
library("compiler")

#install.packages('memoise')
library("memoise")

#install.packages('DiagrammeR')
library("DiagrammeR")

#install.packages('rio')
library("rio")

#install.packages('readr')
library("readr")

#install.packages('data.table')
library("data.table")

#install.packages('feather')
library("feather")

#install.packages('WDI')
library("WDI")


######
### Loading DATA FRAME
######

###########
### GAL ###
###########

##### GAL ####
#### MS mikael
df.GAL_anti_HCV <- read.csv("/Users/mikael.lemos/Desktop/Produtos/SQL/produto2/DBGAL_hepC_anti_HCV.csv")

#### FILTRAR RESULTADO - GAL ####

df.GAL_anti_HCV$RESULTADO <- as.character(df.GAL_anti_HCV$RESULTADO)

df.GAL_anti_HCV_REAGENTE <- filter(df.GAL_anti_HCV, df.GAL_anti_HCV$RESULTADO == "Resultado: Reagente ")

#### FILTRAR RESULTADO - GAL - ID Ãºnico ####

df.GAL_anti_HCV_REAGENTE_id_unico <- distinct(df.GAL_anti_HCV_REAGENTE, ID_PACIENTE, .keep_all = TRUE)


## Amelia - missmap - Datas

gal_hepc_missingdata_datas <- select(df.GAL_anti_HCV_REAGENTE_id_unico, DT_PROCESSAMENTO, DT_COLETA, DT_LIBERACAO, DT_ENCAMINHADO, DT_SOLICITACAO, DT_CADASTRO, DT_RECEBIMENTO)

gal_hepc_missingdata_dt_encaminhado <- select(df.GAL_anti_HCV_REAGENTE_id_unico, DT_ENCAMINHADO)


### Dealing with NA's ###### GAL #######
## Changing Blank and "*" to "NA"  

is.na(gal_hepc_missingdata_datas) <- gal_hepc_missingdata_datas==''  
is.na(gal_hepc_missingdata_datas) <- gal_hepc_missingdata_datas=='*' 
is.na(gal_hepc_missingdata_datas) <- gal_hepc_missingdata_datas=='//'

### Dealing with NA's ###### GAL #######
## Changing Blank and "*" to "NA"  

is.na(gal_hepc_missingdata_dt_encaminhado) <- gal_hepc_missingdata_dt_encaminhado==''  
is.na(gal_hepc_missingdata_dt_encaminhado) <- gal_hepc_missingdata_dt_encaminhado=='*' 
is.na(gal_hepc_missingdata_dt_encaminhado) <- gal_hepc_missingdata_dt_encaminhado=='//'

#### missmap ####

missmap(gal_hepc_missingdata_datas)

### missmap - encaminhado ###

missmap(gal_hepc_missingdata_dt_encaminhado)


#############
### SINAN ###
#############

##### SINAN ####
#### MS mikael
df.SINAN_hepc <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/DBSINAN.csv")

### Dealing with NA's ###### SINAN #######
## Changing Blank and "*" to "NA"  

is.na(df.SINAN_hepc) <- df.SINAN_hepc==''  
is.na(df.SINAN_hepc) <- df.SINAN_hepc=='*' 
is.na(df.SINAN_hepc) <- df.SINAN_hepc=='//'

## Filtro para Hepatite C - somente C ##

#df.SINAN_hepc_filtrado <- df.SINAN_hepc %>% filter(RE_ANTIHCV==1 | ANTIHCV==1 | TP_SOROHCV==1| CLAS_ETIOL==3 ) #| CLAS_ETIOL==6 | CLAS_ETIOL==8 

## Filtro para Hepatite C - Incluindo co-infecÃ§Ãµes ##

#df.SINAN_hepc_filtrado_CI <- df.SINAN_hepc %>% filter(RE_ANTIHCV==1 | ANTIHCV==1 | TP_SOROHCV==1| CLAS_ETIOL==3 | CLAS_ETIOL==6 | CLAS_ETIOL==8 )

######################################
## Filtrar Hepatite C separadamente ##
######################################

## RE_ANTIHCV

df.SINAN_hepc_re_antihcv <- df.SINAN_hepc %>% filter(RE_ANTIHCV==1) 

## ANTIHCV

df.SINAN_hepc_antihcv <- df.SINAN_hepc %>% filter(ANTIHCV==1) 

## TP_SOROHCV

df.SINAN_hepc_sorohcv <- df.SINAN_hepc %>% filter(TP_SOROHCV==1) 

##  CLAS_ETIOL

df.SINAN_hepc_class_etiol <- df.SINAN_hepc %>% filter(CLAS_ETIOL==3) 


## UniÃ£o de todas as tabelas de Hepatite C

df.SINAN_hepc_rbind <- rbind_list(df.SINAN_hepc_re_antihcv, df.SINAN_hepc_antihcv, df.SINAN_hepc_sorohcv, df.SINAN_hepc_class_etiol)

#### FILTRAR RESULTADO - SINAN - ID Ãºnico ####

df.SINAN_hepc_rbind_id_unico <- distinct(df.SINAN_hepc_rbind, ID_PACIENTE, .keep_all = TRUE)


## Amelia - missmap - Datas

df.SINAN.datas <- select(df.SINAN_hepc_rbind_id_unico, DT_NOTIFIC, DT_ENCERRA) 

df.SINAN.DT_NOTIFIC <- select(df.SINAN_hepc_rbind_id_unico, DT_NOTIFIC) 

df.SINAN.DT_ENCERRA <- select(df.SINAN_hepc_rbind_id_unico, DT_ENCERRA) 

#### missmap ####

missmap(df.SINAN.datas)

df.SINAN.DT_ENCERRA$DT_ENCERRA <- as.Date(df.SINAN.DT_ENCERRA$DT_ENCERRA)
df.SINAN.DT_NOTIFIC$DT_NOTIFIC <- as.Date(df.SINAN.DT_NOTIFIC$DT_NOTIFIC)

missmap(df.SINAN.DT_ENCERRA)

missmap(df.SINAN.DT_NOTIFIC)

## nissnap - alternativa

library(reshape2)
library(ggplot2)
# DT_ENCERRA
ggplot_missing <- function(x){
  x %>% is.na %>% melt %>% ggplot(data = ., aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey (name = '', labels = c('Presente', 'Faltante')) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(x = 'VariÃ¡veis na tabela', y = 'ObservaÃ§Ãµes')
}
ggplot_missing(df.SINAN.DT_ENCERRA)

# DT_NOTIFIC
ggplot_missing <- function(x){
  x %>% is.na %>% melt %>% ggplot(data = ., aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey (name = '', labels = c('Presente', 'Faltante')) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(x = 'VariÃ¡veis na tabela', y = 'ObservaÃ§Ãµes')
}
ggplot_missing(df.SINAN.DT_NOTIFIC)

# DATAS
ggplot_missing <- function(x){
  x %>% is.na %>% melt %>% ggplot(data = ., aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey (name = '', labels = c('Presente', 'Faltante')) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(x = 'VariÃ¡veis na tabela', y = 'ObservaÃ§Ãµes')
}
ggplot_missing(df.SINAN.datas)

## SINAN - Extratificação por ano - DT_NOTIFIC ##

df.SINAN_hepc_rbind_id_unico$DT_NOTIFIC <- as.Date.character(df.SINAN_hepc_rbind_id_unico$DT_NOTIFIC)

df.SINAN_hepc_rbind_id_unico <- df.SINAN_hepc_rbind_id_unico %>%
  separate(DT_NOTIFIC, sep="-", into = c("ano_notific", "mes_notific", "dia_notific"))

## 2003

df.SINAN.rbind.id.unico.2003 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2003)

## 2004

df.SINAN.rbind.id.unico.2004 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2004)

## 2005

df.SINAN.rbind.id.unico.2005 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2005)

## 2006

df.SINAN.rbind.id.unico.2006 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2006)

## 2007

df.SINAN.rbind.id.unico.2007 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2007)

## 2008

df.SINAN.rbind.id.unico.2008 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2008)

## 2009

df.SINAN.rbind.id.unico.2009 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2009)

## 2010

df.SINAN.rbind.id.unico.2010 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2010)

## 2011

df.SINAN.rbind.id.unico.2011 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2011)

## 2012

df.SINAN.rbind.id.unico.2012 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2012)

## 2013

df.SINAN.rbind.id.unico.2013 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2013)

## 2014

df.SINAN.rbind.id.unico.2014 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2014)

## 2015

df.SINAN.rbind.id.unico.2015 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2015)

## 2016

df.SINAN.rbind.id.unico.2016 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2016)

## 2017

df.SINAN.rbind.id.unico.2017 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2017)

## 2018

df.SINAN.rbind.id.unico.2018 <- filter(df.SINAN_hepc_rbind_id_unico, df.SINAN_hepc_rbind_id_unico$ano_notific == 2018)

#############
### APAC ####
#############


#############
#### AIH ####
#############


#############
#### BPAI ###
#############


#############
### HORUS ###
#############


#############
#### RAAS ###
#############
