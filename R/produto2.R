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
df.GAL_anti_HCV <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/DBGAL_hepC_anti_HCV.csv")

#### FILTRAR RESULTADO - GAL ####

df.GAL_anti_HCV$RESULTADO <- as.character(df.GAL_anti_HCV$RESULTADO)

df.GAL_anti_HCV_REAGENTE <- filter(df.GAL_anti_HCV, df.GAL_anti_HCV$RESULTADO == "Resultado: Reagente ")


## Amelia - missmap - Datas

gal_hepc_missingdata_datas <- select(df.GAL_anti_HCV_REAGENTE, DT_PROCESSAMENTO, DT_COLETA, DT_LIBERACAO, DT_ENCAMINHADO, DT_SOLICITACAO, DT_CADASTRO, DT_RECEBIMENTO)

gal_hepc_missingdata_dt_encaminhado <- select(df.GAL_anti_HCV_REAGENTE, DT_ENCAMINHADO)

gal_hepc_missingdata_dt_liberacao <- select(df.GAL_anti_HCV_REAGENTE, DT_LIBERACAO )

gal_hepc_missingdata_dt_coleta <- select(df.GAL_anti_HCV_REAGENTE, DT_COLETA)

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

### Dealing with NA's ###### GAL #######
## Changing Blank and "*" to "NA"  

is.na(gal_hepc_missingdata_dt_coleta) <- gal_hepc_missingdata_dt_coleta==''  
is.na(gal_hepc_missingdata_dt_coleta) <- gal_hepc_missingdata_dt_coleta=='*' 
is.na(gal_hepc_missingdata_dt_coleta) <- gal_hepc_missingdata_dt_coleta=='//'

### Dealing with NA's ###### GAL #######
## Changing Blank and "*" to "NA"  

is.na(gal_hepc_missingdata_dt_liberacao) <- gal_hepc_missingdata_dt_liberacao==''  
is.na(gal_hepc_missingdata_dt_liberacao) <- gal_hepc_missingdata_dt_liberacao=='*' 
is.na(gal_hepc_missingdata_dt_liberacao) <- gal_hepc_missingdata_dt_liberacao=='//'

#### missmap ####

missmap(gal_hepc_missingdata_datas)

### missmap - encaminhado ###

missmap(gal_hepc_missingdata_dt_encaminhado)

### missmap - coleta ###

missmap(gal_hepc_missingdata_dt_coleta)

### missmap - liberacao ###

missmap(gal_hepc_missingdata_dt_liberacao)

### Extratificação data GAL - DT_COLETA

df.GAL_anti_HCV_REAGENTE$DT_COLETA <- as.Date.character(df.GAL_anti_HCV_REAGENTE$DT_COLETA)

df.GAL_anti_HCV_REAGENTE <- df.GAL_anti_HCV_REAGENTE %>%
  separate(DT_COLETA, sep="-", into = c("ano_coleta", "mes_coleta", "dia_coleta"))

## 2008

df.GAL.filtrado.hepC.2008 <- filter(df.GAL_anti_HCV_REAGENTE, df.GAL_anti_HCV_REAGENTE$ano_coleta == 2008)

## 2009

df.GAL.filtrado.hepC.2009 <- filter(df.GAL_anti_HCV_REAGENTE, df.GAL_anti_HCV_REAGENTE$ano_coleta == 2009)

## 2010

df.GAL.filtrado.hepC.2010 <- filter(df.GAL_anti_HCV_REAGENTE, df.GAL_anti_HCV_REAGENTE$ano_coleta == 2010)

## 2011

df.GAL.filtrado.hepC.2011 <- filter(df.GAL_anti_HCV_REAGENTE, df.GAL_anti_HCV_REAGENTE$ano_coleta == 2011)

## 2012

df.GAL.filtrado.hepC.2012 <- filter(df.GAL_anti_HCV_REAGENTE, df.GAL_anti_HCV_REAGENTE$ano_coleta == 2012)

## 2013

df.GAL.filtrado.hepC.2013 <- filter(df.GAL_anti_HCV_REAGENTE, df.GAL_anti_HCV_REAGENTE$ano_coleta == 2013)

## 2014

df.GAL.filtrado.hepC.2014 <- filter(df.GAL_anti_HCV_REAGENTE, df.GAL_anti_HCV_REAGENTE$ano_coleta == 2014)

## 2015

df.GAL.filtrado.hepC.2015 <- filter(df.GAL_anti_HCV_REAGENTE, df.GAL_anti_HCV_REAGENTE$ano_coleta == 2015)

## 2016

df.GAL.filtrado.hepC.2016 <- filter(df.GAL_anti_HCV_REAGENTE, df.GAL_anti_HCV_REAGENTE$ano_coleta == 2016)

## 2017

df.GAL.filtrado.hepC.2017 <- filter(df.GAL_anti_HCV_REAGENTE, df.GAL_anti_HCV_REAGENTE$ano_coleta == 2017)

## 2018

df.GAL.filtrado.hepC.2018 <- filter(df.GAL_anti_HCV_REAGENTE, df.GAL_anti_HCV_REAGENTE$ano_coleta == 2018)

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

## Transformar de integer para chr

df.SINAN_hepc$RE_ANTIHCV <- as.character(df.SINAN_hepc$RE_ANTIHCV)
df.SINAN_hepc$ANTIHCV <- as.character(df.SINAN_hepc$ANTIHCV)
df.SINAN_hepc$TP_SOROHCV <- as.character(df.SINAN_hepc$TP_SOROHCV)
df.SINAN_hepc$CLAS_ETIOL <- as.character(df.SINAN_hepc$CLAS_ETIOL)


## Filtro para Hepatite C - somente C ##

df.SINAN_hepc_filtrado <- df.SINAN_hepc %>% filter(RE_ANTIHCV=="1" | ANTIHCV=="1" | TP_SOROHCV=="1"| CLAS_ETIOL=="3") #| CLAS_ETIOL==6 | CLAS_ETIOL==8 )

## Filtro para Hepatite C - Incluindo co-infecÃ§Ãµes ##

df.SINAN_hepc_filtrado_CI <- df.SINAN_hepc %>% filter(RE_ANTIHCV=="1" | ANTIHCV=="1" | TP_SOROHCV=="1"| CLAS_ETIOL=="3" | CLAS_ETIOL=="6" | CLAS_ETIOL=="8")

## Amelia - missmap - Datas

df.SINAN.datas <- select(df.SINAN_hepc_filtrado, DT_NOTIFIC, DT_ENCERRA) 

df.SINAN.DT_NOTIFIC <- select(df.SINAN_hepc_filtrado, DT_NOTIFIC) 

df.SINAN.DT_ENCERRA <- select(df.SINAN_hepc_filtrado, DT_ENCERRA) 

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
    labs(x = 'variáveis na tabela', y = 'Observações')
}
ggplot_missing(df.SINAN.DT_ENCERRA)

# DT_NOTIFIC
ggplot_missing <- function(x){
  x %>% is.na %>% melt %>% ggplot(data = ., aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey (name = '', labels = c('Presente', 'Faltante')) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(x = 'variáveis na tabela', y = 'Observações')
}
ggplot_missing(df.SINAN.DT_NOTIFIC)

# DATAS
ggplot_missing <- function(x){
  x %>% is.na %>% melt %>% ggplot(data = ., aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey (name = '', labels = c('Presente', 'Faltante')) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(x = 'variáveis na tabela', y = 'Observações')
}
ggplot_missing(df.SINAN.datas)

## SINAN - Extratificação por ano - DT_NOTIFIC ##

df.SINAN_hepc_filtrado$DT_NOTIFIC <- as.Date.character(df.SINAN_hepc_filtrado$DT_NOTIFIC)

df.SINAN_hepc_filtrado <- df.SINAN_hepc_filtrado %>%
  separate(DT_NOTIFIC, sep="-", into = c("ano_notific", "mes_notific", "dia_notific"))

## 2003

df.SINAN.filtrado.hepC.2003 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2003)

## 2004

df.SINAN.filtrado.hepC.2004 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2004)

## 2005

df.SINAN.filtrado.hepC.2005 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2005)

## 2006

df.SINAN.filtrado.hepC.2006 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2006)

## 2007

df.SINAN.filtrado.hepC.2007 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2007)

## 2008

df.SINAN.filtrado.hepC.2008 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2008)

## 2009

df.SINAN.filtrado.hepC.2009 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2009)

## 2010

df.SINAN.filtrado.hepC.2010 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2010)

## 2011

df.SINAN.filtrado.hepC.2011 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2011)

## 2012

df.SINAN.filtrado.hepC.2012 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2012)

## 2013

df.SINAN.filtrado.hepC.2013 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2013)

## 2014

df.SINAN.filtrado.hepC.2014 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2014)

## 2015

df.SINAN.filtrado.hepC.2015 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2015)

## 2016

df.SINAN.filtrado.hepC.2016 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2016)

## 2017

df.SINAN.filtrado.hepC.2017 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2017)

## 2018

df.SINAN.filtrado.hepC.2018 <- filter(df.SINAN_hepc_filtrado, df.SINAN_hepc_filtrado$ano_notific == 2018)

#############
### APAC ####
#############

### Extratificação por mês/ano de competência ###

df.APAC.filtrado.hepc.2008 <- filter(df.APAC.filtrado.hepc, df.APAC.filtrado.hepc$ano_inicio == 2008)

df.APAC.filtrado.hepc.2009 <- filter(df.APAC.filtrado.hepc, df.APAC.filtrado.hepc$ano_inicio == 2009)

df.APAC.filtrado.hepc.2010 <- filter(df.APAC.filtrado.hepc, df.APAC.filtrado.hepc$ano_inicio == 2010)

df.APAC.filtrado.hepc.2011 <- filter(df.APAC.filtrado.hepc, df.APAC.filtrado.hepc$ano_inicio == 2011)

df.APAC.filtrado.hepc.2012 <- filter(df.APAC.filtrado.hepc, df.APAC.filtrado.hepc$ano_inicio == 2012)

df.APAC.filtrado.hepc.2013 <- filter(df.APAC.filtrado.hepc, df.APAC.filtrado.hepc$ano_inicio == 2013)

df.APAC.filtrado.hepc.2014 <- filter(df.APAC.filtrado.hepc, df.APAC.filtrado.hepc$ano_inicio == 2014)

df.APAC.filtrado.hepc.2015 <- filter(df.APAC.filtrado.hepc, df.APAC.filtrado.hepc$ano_inicio == 2015)

df.APAC.filtrado.hepc.2016 <- filter(df.APAC.filtrado.hepc, df.APAC.filtrado.hepc$ano_inicio == 2016)

df.APAC.filtrado.hepc.2017 <- filter(df.APAC.filtrado.hepc, df.APAC.filtrado.hepc$ano_inicio == 2017)

df.APAC.filtrado.hepc.2018 <- filter(df.APAC.filtrado.hepc, df.APAC.filtrado.hepc$ano_inicio == 2018)

#############
#### AIH ####
#############

### Extratificação por ano hep C AIH

df.AIH.filtrado.hepC.2008 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2008/AIH_2008_201908131641.csv")

df.AIH.filtrado.hepC.2009 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2009/AIH_2009_201908131714.csv")

df.AIH.filtrado.hepC.2010 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2010/AIH_2010_201908131749.csv")

df.AIH.filtrado.hepC.2011 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2011/AIH_2011_201908141006.csv")

df.AIH.filtrado.hepC.2012 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2012/AIH_2012_201908141044.csv")

df.AIH.filtrado.hepC.2013 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2013/AIH_2013_201908141125.csv")

df.AIH.filtrado.hepC.2014 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2014/AIH_2014_201908141212.csv")

df.AIH.filtrado.hepC.2015 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2015/AIH_2015_201908141259.csv")

df.AIH.filtrado.hepC.2016 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2016/AIH_2016_201908141346.csv")

df.AIH.filtrado.hepC.2017 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2017/AIH_2017_201908141434.csv")

df.AIH.filtrado.hepC.2018 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2018/AIH_2018_201908141524.csv")

#############
#### BPAI ###
#############

### Extratificação por ano hep C BPAI

df.BPAI.filtrado.hepC.2008 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2008/BPAI_2008_201908151202.csv")

df.BPAI.filtrado.hepC.2009 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2009/BPAI_2009_201908151204.csv")

df.BPAI.filtrado.hepC.2010 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2010/BPAI_2010_201908151208.csv")

df.BPAI.filtrado.hepC.2011 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2011/BPAI_2011_201908151212.csv")

df.BPAI.filtrado.hepC.2012 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2012/BPAI_2012_201908151216.csv")

df.BPAI.filtrado.hepC.2013 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2013/BPAI_2013_201908151002.csv")

df.BPAI.filtrado.hepC.2014 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2014/BPAI_2014_201908151002.csv")

df.BPAI.filtrado.hepC.2015 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2015/BPAI_2015_201908151002.csv")

df.BPAI.filtrado.hepC.2016 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2016/BPAI_2016_201908151002.csv")

df.BPAI.filtrado.hepC.2017 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2017/BPAI_2017_201908151002.csv")

df.BPAI.filtrado.hepC.2018 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2018/BPAI_2018_201908151002.csv")

#############
#### SIM ####
#############

### Extratificação por ano hep C SIM

df.SIM.hepC.filtrado <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/TB_CARGA_DO_201908141614.csv")

### Dealing with NA's ###### SIM #######
## Changing Blank and "*" to "NA"  

is.na(df.SIM.hepC.filtrado) <- df.SIM.hepC.filtrado==''  
is.na(df.SIM.hepC.filtrado) <- df.SIM.hepC.filtrado=='*' 
is.na(df.SIM.hepC.filtrado) <- df.SIM.hepC.filtrado=='//'

### Extratificação por ano - DTOBITO ##

df.SIM.hepC.filtrado$DTOBITO <- dmy(df.SIM.hepC.filtrado$DTOBITO)

df.SIM.hepC.filtrado$DTOBITO <- as.Date.character(df.SIM.hepC.filtrado$DTOBITO)

df.SIM.hepC.filtrado <- df.SIM.hepC.filtrado %>%
  separate(DTOBITO, sep="-", into = c("ano_obito", "mes_obito", "dia_obito"))

## 2006

df.SIM.hepC.filtrado.2006 <- filter(df.SIM.hepC.filtrado, df.SIM.hepC.filtrado$ano_obito == 2006)

## 2007

df.SIM.hepC.filtrado.2007 <- filter(df.SIM.hepC.filtrado, df.SIM.hepC.filtrado$ano_obito == 2007)

## 2008

df.SIM.hepC.filtrado.2008 <- filter(df.SIM.hepC.filtrado, df.SIM.hepC.filtrado$ano_obito == 2008)

## 2009

df.SIM.hepC.filtrado.2009 <- filter(df.SIM.hepC.filtrado, df.SIM.hepC.filtrado$ano_obito == 2009)

## 2010

df.SIM.hepC.filtrado.2010 <- filter(df.SIM.hepC.filtrado, df.SIM.hepC.filtrado$ano_obito == 2010)

## 2011

df.SIM.hepC.filtrado.2011 <- filter(df.SIM.hepC.filtrado, df.SIM.hepC.filtrado$ano_obito == 2011)

## 2012

df.SIM.hepC.filtrado.2012 <- filter(df.SIM.hepC.filtrado, df.SIM.hepC.filtrado$ano_obito == 2012)

## 2013

df.SIM.hepC.filtrado.2013 <- filter(df.SIM.hepC.filtrado, df.SIM.hepC.filtrado$ano_obito == 2013)

## 2014

df.SIM.hepC.filtrado.2014 <- filter(df.SIM.hepC.filtrado, df.SIM.hepC.filtrado$ano_obito == 2014)

## 2015

df.SIM.hepC.filtrado.2015 <- filter(df.SIM.hepC.filtrado, df.SIM.hepC.filtrado$ano_obito == 2015)

## 2016

df.SIM.hepC.filtrado.2016 <- filter(df.SIM.hepC.filtrado, df.SIM.hepC.filtrado$ano_obito == 2016)

#############
### HORUS ###
#############

#############
#### RAAS ###
#############