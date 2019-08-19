############ Produto 2 ###########################
##################################################
###### script desenvolvido por Mikael Lemos ######
###### vers?o 1.0 - 22.07.2019 ###################
##################################################

######
### Carregando / instalando pacotes
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

#### MS 
df.GAL_anti_HCV <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/DBGAL_hepC_anti_HCV.csv")

### Mac
df.GAL_anti_HCV <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/tabelas/DBGAL_hepC_anti_HCV.csv")

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

### Extratifica??o data GAL - DT_COLETA

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

#### MS 
df.SINAN_hepc <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/DBSINAN.csv")

#### Mac
df.SINAN_hepc <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/tabelas/DBSINAN.csv")

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

## Filtro para Hepatite C - Incluindo co-infecções ##

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
    labs(x = 'vari?veis na tabela', y = 'Observa??es')
}
ggplot_missing(df.SINAN.DT_ENCERRA)

# DT_NOTIFIC
ggplot_missing <- function(x){
  x %>% is.na %>% melt %>% ggplot(data = ., aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey (name = '', labels = c('Presente', 'Faltante')) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(x = 'vari?veis na tabela', y = 'Observa??es')
}
ggplot_missing(df.SINAN.DT_NOTIFIC)

# DATAS
ggplot_missing <- function(x){
  x %>% is.na %>% melt %>% ggplot(data = ., aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey (name = '', labels = c('Presente', 'Faltante')) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(x = 'vari?veis na tabela', y = 'Observa??es')
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

#### MS 
df.APAC.hepC.filtrado <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/TB_ESPELHO_APAC_201908151746.csv")

#### Mac
df.APAC.hepC.filtrado <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/tabelas/TB_ESPELHO_APAC_201908151746.csv")

### Dealing with NA's ###### SINAN #######
## Changing Blank and "*" to "NA"  

is.na(df.APAC.hepC.filtrado) <- df.APAC.hepC.filtrado==''  
is.na(df.APAC.hepC.filtrado) <- df.APAC.hepC.filtrado=='*' 
is.na(df.APAC.hepC.filtrado) <- df.APAC.hepC.filtrado=='//'

### Extratificação por mês/ano de competência ###

df.APAC.hepC.filtrado$COMPETENCIA <- as.character(df.APAC.hepC.filtrado$COMPETENCIA)

df.APAC.filtrado.hepc.2008 <- filter(df.APAC.hepC.filtrado, df.APAC.hepC.filtrado$COMPETENCIA == "200801" | df.APAC.hepC.filtrado$COMPETENCIA == "200802" | df.APAC.hepC.filtrado$COMPETENCIA == "200803" | df.APAC.hepC.filtrado$COMPETENCIA == "200804" | df.APAC.hepC.filtrado$COMPETENCIA == "200805" | df.APAC.hepC.filtrado$COMPETENCIA == "200806" | df.APAC.hepC.filtrado$COMPETENCIA == "200807" | df.APAC.hepC.filtrado$COMPETENCIA == "200808" | df.APAC.hepC.filtrado$COMPETENCIA == "200809" | df.APAC.hepC.filtrado$COMPETENCIA == "200810" | df.APAC.hepC.filtrado$COMPETENCIA == "200811" | df.APAC.hepC.filtrado$COMPETENCIA == "200812")

df.APAC.filtrado.hepc.2009 <- filter(df.APAC.hepC.filtrado, df.APAC.hepC.filtrado$COMPETENCIA == "200901" | df.APAC.hepC.filtrado$COMPETENCIA == "200902" | df.APAC.hepC.filtrado$COMPETENCIA == "200903" | df.APAC.hepC.filtrado$COMPETENCIA == "200904" | df.APAC.hepC.filtrado$COMPETENCIA == "200905" | df.APAC.hepC.filtrado$COMPETENCIA == "200906" | df.APAC.hepC.filtrado$COMPETENCIA == "200907" | df.APAC.hepC.filtrado$COMPETENCIA == "200908" | df.APAC.hepC.filtrado$COMPETENCIA == "200909" | df.APAC.hepC.filtrado$COMPETENCIA == "200910" | df.APAC.hepC.filtrado$COMPETENCIA == "200911" | df.APAC.hepC.filtrado$COMPETENCIA == "200912")

df.APAC.filtrado.hepc.2010 <- filter(df.APAC.hepC.filtrado, df.APAC.hepC.filtrado$COMPETENCIA == "201001" | df.APAC.hepC.filtrado$COMPETENCIA == "201002" | df.APAC.hepC.filtrado$COMPETENCIA == "201003" | df.APAC.hepC.filtrado$COMPETENCIA == "201004" | df.APAC.hepC.filtrado$COMPETENCIA == "201005" | df.APAC.hepC.filtrado$COMPETENCIA == "201006" | df.APAC.hepC.filtrado$COMPETENCIA == "201007" | df.APAC.hepC.filtrado$COMPETENCIA == "201008" | df.APAC.hepC.filtrado$COMPETENCIA == "201009" | df.APAC.hepC.filtrado$COMPETENCIA == "201010" | df.APAC.hepC.filtrado$COMPETENCIA == "201011" | df.APAC.hepC.filtrado$COMPETENCIA == "201012")

df.APAC.filtrado.hepc.2011 <- filter(df.APAC.hepC.filtrado, df.APAC.hepC.filtrado$COMPETENCIA == "201101" | df.APAC.hepC.filtrado$COMPETENCIA == "201102" | df.APAC.hepC.filtrado$COMPETENCIA == "201103" | df.APAC.hepC.filtrado$COMPETENCIA == "201104" | df.APAC.hepC.filtrado$COMPETENCIA == "201105" | df.APAC.hepC.filtrado$COMPETENCIA == "201106" | df.APAC.hepC.filtrado$COMPETENCIA == "201107" | df.APAC.hepC.filtrado$COMPETENCIA == "201108" | df.APAC.hepC.filtrado$COMPETENCIA == "201109" | df.APAC.hepC.filtrado$COMPETENCIA == "201110" | df.APAC.hepC.filtrado$COMPETENCIA == "201111" | df.APAC.hepC.filtrado$COMPETENCIA == "201112")

df.APAC.filtrado.hepc.2012 <- filter(df.APAC.hepC.filtrado, df.APAC.hepC.filtrado$COMPETENCIA == "201201" | df.APAC.hepC.filtrado$COMPETENCIA == "201202" | df.APAC.hepC.filtrado$COMPETENCIA == "201203" | df.APAC.hepC.filtrado$COMPETENCIA == "201204" | df.APAC.hepC.filtrado$COMPETENCIA == "201205" | df.APAC.hepC.filtrado$COMPETENCIA == "201206" | df.APAC.hepC.filtrado$COMPETENCIA == "201207" | df.APAC.hepC.filtrado$COMPETENCIA == "201208" | df.APAC.hepC.filtrado$COMPETENCIA == "201209" | df.APAC.hepC.filtrado$COMPETENCIA == "201210" | df.APAC.hepC.filtrado$COMPETENCIA == "201211" | df.APAC.hepC.filtrado$COMPETENCIA == "201212")

df.APAC.filtrado.hepc.2013 <- filter(df.APAC.hepC.filtrado, df.APAC.hepC.filtrado$COMPETENCIA == "201301" | df.APAC.hepC.filtrado$COMPETENCIA == "201302" | df.APAC.hepC.filtrado$COMPETENCIA == "201303" | df.APAC.hepC.filtrado$COMPETENCIA == "201304" | df.APAC.hepC.filtrado$COMPETENCIA == "201305" | df.APAC.hepC.filtrado$COMPETENCIA == "201306" | df.APAC.hepC.filtrado$COMPETENCIA == "201307" | df.APAC.hepC.filtrado$COMPETENCIA == "201308" | df.APAC.hepC.filtrado$COMPETENCIA == "201309" | df.APAC.hepC.filtrado$COMPETENCIA == "201310" | df.APAC.hepC.filtrado$COMPETENCIA == "201311" | df.APAC.hepC.filtrado$COMPETENCIA == "201312")

df.APAC.filtrado.hepc.2014 <- filter(df.APAC.hepC.filtrado, df.APAC.hepC.filtrado$COMPETENCIA == "201401" | df.APAC.hepC.filtrado$COMPETENCIA == "201402" | df.APAC.hepC.filtrado$COMPETENCIA == "201403" | df.APAC.hepC.filtrado$COMPETENCIA == "201404" | df.APAC.hepC.filtrado$COMPETENCIA == "201405" | df.APAC.hepC.filtrado$COMPETENCIA == "201406" | df.APAC.hepC.filtrado$COMPETENCIA == "201407" | df.APAC.hepC.filtrado$COMPETENCIA == "201408" | df.APAC.hepC.filtrado$COMPETENCIA == "201409" | df.APAC.hepC.filtrado$COMPETENCIA == "201410" | df.APAC.hepC.filtrado$COMPETENCIA == "201411" | df.APAC.hepC.filtrado$COMPETENCIA == "201412")

df.APAC.filtrado.hepc.2015 <- filter(df.APAC.hepC.filtrado, df.APAC.hepC.filtrado$COMPETENCIA == "201501" | df.APAC.hepC.filtrado$COMPETENCIA == "201502" | df.APAC.hepC.filtrado$COMPETENCIA == "201503" | df.APAC.hepC.filtrado$COMPETENCIA == "201504" | df.APAC.hepC.filtrado$COMPETENCIA == "201505" | df.APAC.hepC.filtrado$COMPETENCIA == "201506" | df.APAC.hepC.filtrado$COMPETENCIA == "201507" | df.APAC.hepC.filtrado$COMPETENCIA == "201508" | df.APAC.hepC.filtrado$COMPETENCIA == "201509" | df.APAC.hepC.filtrado$COMPETENCIA == "201510" | df.APAC.hepC.filtrado$COMPETENCIA == "201511" | df.APAC.hepC.filtrado$COMPETENCIA == "201512")

df.APAC.filtrado.hepc.2016 <- filter(df.APAC.hepC.filtrado, df.APAC.hepC.filtrado$COMPETENCIA == "201601" | df.APAC.hepC.filtrado$COMPETENCIA == "201602" | df.APAC.hepC.filtrado$COMPETENCIA == "201603" | df.APAC.hepC.filtrado$COMPETENCIA == "201604" | df.APAC.hepC.filtrado$COMPETENCIA == "201605" | df.APAC.hepC.filtrado$COMPETENCIA == "201606" | df.APAC.hepC.filtrado$COMPETENCIA == "201607" | df.APAC.hepC.filtrado$COMPETENCIA == "201608" | df.APAC.hepC.filtrado$COMPETENCIA == "201609" | df.APAC.hepC.filtrado$COMPETENCIA == "201610" | df.APAC.hepC.filtrado$COMPETENCIA == "201611" | df.APAC.hepC.filtrado$COMPETENCIA == "201612")

df.APAC.filtrado.hepc.2017 <- filter(df.APAC.hepC.filtrado, df.APAC.hepC.filtrado$COMPETENCIA == "201701" | df.APAC.hepC.filtrado$COMPETENCIA == "201702" | df.APAC.hepC.filtrado$COMPETENCIA == "201703" | df.APAC.hepC.filtrado$COMPETENCIA == "201704" | df.APAC.hepC.filtrado$COMPETENCIA == "201705" | df.APAC.hepC.filtrado$COMPETENCIA == "201706" | df.APAC.hepC.filtrado$COMPETENCIA == "201707" | df.APAC.hepC.filtrado$COMPETENCIA == "201708" | df.APAC.hepC.filtrado$COMPETENCIA == "201709" | df.APAC.hepC.filtrado$COMPETENCIA == "201710" | df.APAC.hepC.filtrado$COMPETENCIA == "201711" | df.APAC.hepC.filtrado$COMPETENCIA == "201712")

df.APAC.filtrado.hepc.2018 <- filter(df.APAC.hepC.filtrado, df.APAC.hepC.filtrado$COMPETENCIA == "201801" | df.APAC.hepC.filtrado$COMPETENCIA == "201802" | df.APAC.hepC.filtrado$COMPETENCIA == "201803" | df.APAC.hepC.filtrado$COMPETENCIA == "201804" | df.APAC.hepC.filtrado$COMPETENCIA == "201805" | df.APAC.hepC.filtrado$COMPETENCIA == "201806" | df.APAC.hepC.filtrado$COMPETENCIA == "201807" | df.APAC.hepC.filtrado$COMPETENCIA == "201808" | df.APAC.hepC.filtrado$COMPETENCIA == "201809" | df.APAC.hepC.filtrado$COMPETENCIA == "201810" | df.APAC.hepC.filtrado$COMPETENCIA == "201811" | df.APAC.hepC.filtrado$COMPETENCIA == "201812")

#############
#### AIH ####
#############

### Extratificação por ano hep C AIH

### MS
df.AIH.filtrado.hepC.2008 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2008/AIH_2008_201908131641.csv")

### Mac
df.AIH.filtrado.hepC.2008 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/AIH_HEPC/2008/AIH_2008_201908131641.csv")

### MS
df.AIH.filtrado.hepC.2009 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2009/AIH_2009_201908131714.csv")

### Mac
df.AIH.filtrado.hepC.2009 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/AIH_HEPC/2009/AIH_2009_201908131714.csv")

### MS
df.AIH.filtrado.hepC.2010 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2010/AIH_2010_201908131749.csv")

### Mac
df.AIH.filtrado.hepC.2010 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/AIH_HEPC/2010/AIH_2010_201908131749.csv")

### MS
df.AIH.filtrado.hepC.2011 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2011/AIH_2011_201908141006.csv")

### Mac
df.AIH.filtrado.hepC.2011 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/AIH_HEPC/2011/AIH_2011_201908141006.csv")

### MS
df.AIH.filtrado.hepC.2012 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2012/AIH_2012_201908141044.csv")

### Mac
df.AIH.filtrado.hepC.2012 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/AIH_HEPC/2012/AIH_2012_201908141044.csv")

### MS
df.AIH.filtrado.hepC.2013 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2013/AIH_2013_201908141125.csv")

### Mac
df.AIH.filtrado.hepC.2013 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/AIH_HEPC/2013/AIH_2013_201908141125.csv")

### MS
df.AIH.filtrado.hepC.2014 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2014/AIH_2014_201908141212.csv")

### Mac
df.AIH.filtrado.hepC.2014 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/AIH_HEPC/2014/AIH_2014_201908141212.csv")

### MS
df.AIH.filtrado.hepC.2015 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2015/AIH_2015_201908141259.csv")

### Mac
df.AIH.filtrado.hepC.2015 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/AIH_HEPC/2015/AIH_2015_201908141259.csv")

### MS
df.AIH.filtrado.hepC.2016 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2016/AIH_2016_201908141346.csv")

### Mac
df.AIH.filtrado.hepC.2016 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/AIH_HEPC/2016/AIH_2016_201908141346.csv")

### MS
df.AIH.filtrado.hepC.2017 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2017/AIH_2017_201908141434.csv")

### Mac
df.AIH.filtrado.hepC.2017 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/AIH_HEPC/2017/AIH_2017_201908141434.csv")

### MS
df.AIH.filtrado.hepC.2018 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/AIH_HEPC/2018/AIH_2018_201908141524.csv")

### Mac
df.AIH.filtrado.hepC.2018 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/AIH_HEPC/2018/AIH_2018_201908141524.csv")

#############
#### BPAI ###
#############

### Extratificação por ano hep C BPAI

### MS
df.BPAI.filtrado.hepC.2008 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2008/BPAI_2008_201908151202.csv")

### Mac 
df.BPAI.filtrado.hepC.2008 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/BPAI_HEPC/BPAI_2008/BPAI_2008_201908151202.csv")

### MS
df.BPAI.filtrado.hepC.2009 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2009/BPAI_2009_201908151204.csv")

### Mac 
df.BPAI.filtrado.hepC.2009 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/BPAI_HEPC/BPAI_2009/BPAI_2009_201908151204.csv")

### MS
df.BPAI.filtrado.hepC.2010 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2010/BPAI_2010_201908151208.csv")

### Mac 
df.BPAI.filtrado.hepC.2010 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/BPAI_HEPC/BPAI_2010/BPAI_2010_201908151208.csv")

### MS
df.BPAI.filtrado.hepC.2011 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2011/BPAI_2011_201908151212.csv")

### Mac 
df.BPAI.filtrado.hepC.2011 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/BPAI_HEPC/BPAI_2011/BPAI_2011_201908151212.csv")

### MS
df.BPAI.filtrado.hepC.2012 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2012/BPAI_2012_201908151216.csv")

### Mac 
df.BPAI.filtrado.hepC.2012 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/BPAI_HEPC/BPAI_2012/BPAI_2012_201908151216.csv")

### MS
df.BPAI.filtrado.hepC.2013 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2013/BPAI_2013_201908151002.csv")

### Mac 
df.BPAI.filtrado.hepC.2013 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/BPAI_HEPC/BPAI_2013/BPAI_2013_201908151002.csv")

### MS
df.BPAI.filtrado.hepC.2014 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2014/BPAI_2014_201908151002.csv")

### Mac 
df.BPAI.filtrado.hepC.2014 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/BPAI_HEPC/BPAI_2014/BPAI_2014_201908151002.csv")

### MS
df.BPAI.filtrado.hepC.2015 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2015/BPAI_2015_201908151002.csv")

### Mac 
df.BPAI.filtrado.hepC.2015 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/BPAI_HEPC/BPAI_2015/BPAI_2015_201908151002.csv")

### MS
df.BPAI.filtrado.hepC.2016 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2016/BPAI_2016_201908151002.csv")

### Mac 
df.BPAI.filtrado.hepC.2016 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/BPAI_HEPC/BPAI_2016/BPAI_2016_201908151002.csv")

### MS
df.BPAI.filtrado.hepC.2017 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2017/BPAI_2017_201908151002.csv")

### Mac 
df.BPAI.filtrado.hepC.2017 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/BPAI_HEPC/BPAI_2017/BPAI_2017_201908151002.csv")

### MS
df.BPAI.filtrado.hepC.2018 <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/BPAI_HEPC/BPAI_2018/BPAI_2018_201908151002.csv")

### Mac 
df.BPAI.filtrado.hepC.2018 <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/BPAI_HEPC/BPAI_2018/BPAI_2018_201908151002.csv")

#############
#### SIM ####
#############

### Extratificação por ano hep C SIM

### MS
df.SIM.hepC.filtrado <- read.csv("/Users/mikael.lemos/Desktop/Produtos/produto2/TB_CARGA_DO_201908141614.csv")

### Mac
df.SIM.hepC.filtrado <- read.csv("/Volumes/MIKAEL/Produtos/git-produto2-OPAS/tabelas/TB_CARGA_DO_201908141614.csv")

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

##########################################################
### Preparando lista de códigos IBGE com 6 e 7 digitos ###
##########################################################

# Extrair código IBGE completo
cod_mun_IBGE <- readxl::read_xls("/Users/mikaellemos/Downloads/DTB_2018/RELATORIO_DTB_BRASIL_MUNICIPIO.xls")

############# tabela completa - COD IBGE 7 digitos ################

# retirar úktimo digito do código completo 7 dígitos 
cod_mun_completo <- substr(cod_mun_IBGE$`Código Município Completo`, 1, 6)

# transformar lista de cidades 2015 com 6 digitos em df
cod_mun_completo <- as.data.frame(cod_mun_completo)

# tabela completa IBGE contendo todos os municipios em 2018 com 6 e 7 digitos (5570)
cod_mun_IBGE_6d_7d <- cbind(cod_mun_completo, cod_mun_IBGE)

#########
## GAL ##
#########

#2008 - MUN
df.GAL.2008_mun_br <- select(df.GAL.filtrado.hepC.2008, CO_MUNICIPIO_RESIDENCIA)
df.GAL.2008_mun <- table(df.GAL.2008_mun_br)
df.GAL.2008_mun <- as.data.frame(df.GAL.2008_mun)
# Acrescentar informações IBGE 7d
df.GAL.2008_mun  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2008_mun, by.x = "cod_mun_completo", by.y = "df.GAL.2008_mun_br" , all.y = TRUE)
write.csv(df.GAL.2008_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2008_mun.csv', row.names=FALSE)

#2008 - UF
df.GAL.2008_UF_br <- select(df.GAL.filtrado.hepC.2008, CO_UF_REQUISICAO)
df.GAL.2008_UF <- table(df.GAL.2008_UF_br)
df.GAL.2008_UF <- as.data.frame(df.GAL.2008_UF)
# Acrescentar informações IBGE 7d
df.GAL.2008_UF  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2008_UF, by.x = "UF", by.y = "df.GAL.2008_UF_br" , all.y = TRUE)
df.GAL.2008_UF <- distinct(df.GAL.2008_UF, UF, .keep_all = TRUE)
write.csv(df.GAL.2008_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2008_UF.csv', row.names=FALSE)

#2009 - MUN
df.GAL.2009_mun_br <- select(df.GAL.filtrado.hepC.2009, CO_MUNICIPIO_RESIDENCIA)
df.GAL.2009_mun <- table(df.GAL.2009_mun_br)
df.GAL.2009_mun <- as.data.frame(df.GAL.2009_mun)
# Acrescentar informações IBGE 7d
df.GAL.2009_mun  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2009_mun, by.x = "cod_mun_completo", by.y = "df.GAL.2009_mun_br" , all.y = TRUE)
write.csv(df.GAL.2009_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2009_mun.csv', row.names=FALSE)

#2009 - UF
df.GAL.2009_UF_br <- select(df.GAL.filtrado.hepC.2009, CO_UF_REQUISICAO)
df.GAL.2009_UF <- table(df.GAL.2009_UF_br)
df.GAL.2009_UF <- as.data.frame(df.GAL.2009_UF)
# Acrescentar informações IBGE 7d
df.GAL.2009_UF  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2009_UF, by.x = "UF", by.y = "df.GAL.2009_UF_br" , all.y = TRUE)
df.GAL.2009_UF <- distinct(df.GAL.2009_UF, UF, .keep_all = TRUE)
write.csv(df.GAL.2009_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2009_UF.csv', row.names=FALSE)

#2010 - MUN
df.GAL.2010_mun_br <- select(df.GAL.filtrado.hepC.2010, CO_MUNICIPIO_RESIDENCIA)
df.GAL.2010_mun <- table(df.GAL.2010_mun_br)
df.GAL.2010_mun <- as.data.frame(df.GAL.2010_mun)
# Acrescentar informações IBGE 7d
df.GAL.2010_mun  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2010_mun, by.x = "cod_mun_completo", by.y = "df.GAL.2010_mun_br" , all.y = TRUE)
write.csv(df.GAL.2010_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2010_mun.csv', row.names=FALSE)

#2010 - UF
df.GAL.2010_UF_br <- select(df.GAL.filtrado.hepC.2010, CO_UF_REQUISICAO)
df.GAL.2010_UF <- table(df.GAL.2010_UF_br)
df.GAL.2010_UF <- as.data.frame(df.GAL.2010_UF)
# Acrescentar informações IBGE 7d
df.GAL.2010_UF  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2010_UF, by.x = "UF", by.y = "df.GAL.2010_UF_br" , all.y = TRUE)
df.GAL.2010_UF <- distinct(df.GAL.2010_UF, UF, .keep_all = TRUE)
write.csv(df.GAL.2010_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2010_UF.csv', row.names=FALSE)

#2011 - MUN
df.GAL.2011_mun_br <- select(df.GAL.filtrado.hepC.2011, CO_MUNICIPIO_RESIDENCIA)
df.GAL.2011_mun <- table(df.GAL.2011_mun_br)
df.GAL.2011_mun <- as.data.frame(df.GAL.2011_mun)
# Acrescentar informações IBGE 7d
df.GAL.2011_mun  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2011_mun, by.x = "cod_mun_completo", by.y = "df.GAL.2011_mun_br" , all.y = TRUE)
write.csv(df.GAL.2011_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2011_mun.csv', row.names=FALSE)

#2011 - UF
df.GAL.2011_UF_br <- select(df.GAL.filtrado.hepC.2011, CO_UF_REQUISICAO)
df.GAL.2011_UF <- table(df.GAL.2011_UF_br)
df.GAL.2011_UF <- as.data.frame(df.GAL.2011_UF)
# Acrescentar informações IBGE 7d
df.GAL.2011_UF  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2011_UF, by.x = "UF", by.y = "df.GAL.2011_UF_br" , all.y = TRUE)
df.GAL.2011_UF <- distinct(df.GAL.2011_UF, UF, .keep_all = TRUE)
write.csv(df.GAL.2011_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2011_UF.csv', row.names=FALSE)

#2012 - MUN
df.GAL.2012_mun_br <- select(df.GAL.filtrado.hepC.2012, CO_MUNICIPIO_RESIDENCIA)
df.GAL.2012_mun <- table(df.GAL.2012_mun_br)
df.GAL.2012_mun <- as.data.frame(df.GAL.2012_mun)
# Acrescentar informações IBGE 7d
df.GAL.2012_mun  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2012_mun, by.x = "cod_mun_completo", by.y = "df.GAL.2012_mun_br" , all.y = TRUE)
write.csv(df.GAL.2012_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2012_mun.csv', row.names=FALSE)

#2012 - UF
df.GAL.2012_UF_br <- select(df.GAL.filtrado.hepC.2012, CO_UF_REQUISICAO)
df.GAL.2012_UF <- table(df.GAL.2012_UF_br)
df.GAL.2012_UF <- as.data.frame(df.GAL.2012_UF)
# Acrescentar informações IBGE 7d
df.GAL.2012_UF  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2012_UF, by.x = "UF", by.y = "df.GAL.2012_UF_br" , all.y = TRUE)
df.GAL.2012_UF <- distinct(df.GAL.2012_UF, UF, .keep_all = TRUE)
write.csv(df.GAL.2012_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2012_UF.csv', row.names=FALSE)

#2013 - MUN
df.GAL.2013_mun_br <- select(df.GAL.filtrado.hepC.2013, CO_MUNICIPIO_RESIDENCIA)
df.GAL.2013_mun <- table(df.GAL.2013_mun_br)
df.GAL.2013_mun <- as.data.frame(df.GAL.2013_mun)
# Acrescentar informações IBGE 7d
df.GAL.2013_mun  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2013_mun, by.x = "cod_mun_completo", by.y = "df.GAL.2013_mun_br" , all.y = TRUE)
write.csv(df.GAL.2013_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2013_mun.csv', row.names=FALSE)

#2013 - UF
df.GAL.2013_UF_br <- select(df.GAL.filtrado.hepC.2013, CO_UF_REQUISICAO)
df.GAL.2013_UF <- table(df.GAL.2013_UF_br)
df.GAL.2013_UF <- as.data.frame(df.GAL.2013_UF)
# Acrescentar informações IBGE 7d
df.GAL.2013_UF  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2013_UF, by.x = "UF", by.y = "df.GAL.2013_UF_br" , all.y = TRUE)
df.GAL.2013_UF <- distinct(df.GAL.2013_UF, UF, .keep_all = TRUE)
write.csv(df.GAL.2013_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2013_UF.csv', row.names=FALSE)

#2014 - MUN
df.GAL.2014_mun_br <- select(df.GAL.filtrado.hepC.2014, CO_MUNICIPIO_RESIDENCIA)
df.GAL.2014_mun <- table(df.GAL.2014_mun_br)
df.GAL.2014_mun <- as.data.frame(df.GAL.2014_mun)
# Acrescentar informações IBGE 7d
df.GAL.2014_mun  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2014_mun, by.x = "cod_mun_completo", by.y = "df.GAL.2014_mun_br" , all.y = TRUE)
write.csv(df.GAL.2014_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2014_mun.csv', row.names=FALSE)

#2014 - UF
df.GAL.2014_UF_br <- select(df.GAL.filtrado.hepC.2014, CO_UF_REQUISICAO)
df.GAL.2014_UF <- table(df.GAL.2014_UF_br)
df.GAL.2014_UF <- as.data.frame(df.GAL.2014_UF)
# Acrescentar informações IBGE 7d
df.GAL.2014_UF  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2014_UF, by.x = "UF", by.y = "df.GAL.2014_UF_br" , all.y = TRUE)
df.GAL.2014_UF <- distinct(df.GAL.2014_UF, UF, .keep_all = TRUE)
write.csv(df.GAL.2014_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2014_UF.csv', row.names=FALSE)

#2015 - MUN
df.GAL.2015_mun_br <- select(df.GAL.filtrado.hepC.2015, CO_MUNICIPIO_RESIDENCIA)
df.GAL.2015_mun <- table(df.GAL.2015_mun_br)
df.GAL.2015_mun <- as.data.frame(df.GAL.2015_mun)
# Acrescentar informações IBGE 7d
df.GAL.2015_mun  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2015_mun, by.x = "cod_mun_completo", by.y = "df.GAL.2015_mun_br" , all.y = TRUE)
write.csv(df.GAL.2015_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2015_mun.csv', row.names=FALSE)

#2015 - UF
df.GAL.2015_UF_br <- select(df.GAL.filtrado.hepC.2015, CO_UF_REQUISICAO)
df.GAL.2015_UF <- table(df.GAL.2015_UF_br)
df.GAL.2015_UF <- as.data.frame(df.GAL.2015_UF)
# Acrescentar informações IBGE 7d
df.GAL.2015_UF  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2015_UF, by.x = "UF", by.y = "df.GAL.2015_UF_br" , all.y = TRUE)
df.GAL.2015_UF <- distinct(df.GAL.2015_UF, UF, .keep_all = TRUE)
write.csv(df.GAL.2015_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2015_UF.csv', row.names=FALSE)

#2016 - MUN
df.GAL.2016_mun_br <- select(df.GAL.filtrado.hepC.2016, CO_MUNICIPIO_RESIDENCIA)
df.GAL.2016_mun <- table(df.GAL.2016_mun_br)
df.GAL.2016_mun <- as.data.frame(df.GAL.2016_mun)
# Acrescentar informações IBGE 7d
df.GAL.2016_mun  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2016_mun, by.x = "cod_mun_completo", by.y = "df.GAL.2016_mun_br" , all.y = TRUE)
write.csv(df.GAL.2016_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2016_mun.csv', row.names=FALSE)

#2016 - UF
df.GAL.2016_UF_br <- select(df.GAL.filtrado.hepC.2016, CO_UF_REQUISICAO)
df.GAL.2016_UF <- table(df.GAL.2016_UF_br)
df.GAL.2016_UF <- as.data.frame(df.GAL.2016_UF)
# Acrescentar informações IBGE 7d
df.GAL.2016_UF  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2016_UF, by.x = "UF", by.y = "df.GAL.2016_UF_br" , all.y = TRUE)
df.GAL.2016_UF <- distinct(df.GAL.2016_UF, UF, .keep_all = TRUE)
write.csv(df.GAL.2016_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2016_UF.csv', row.names=FALSE)

#2017 - MUN
df.GAL.2017_mun_br <- select(df.GAL.filtrado.hepC.2017, CO_MUNICIPIO_RESIDENCIA)
df.GAL.2017_mun <- table(df.GAL.2017_mun_br)
df.GAL.2017_mun <- as.data.frame(df.GAL.2017_mun)
# Acrescentar informações IBGE 7d
df.GAL.2017_mun  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2017_mun, by.x = "cod_mun_completo", by.y = "df.GAL.2017_mun_br" , all.y = TRUE)
write.csv(df.GAL.2017_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2017_mun.csv', row.names=FALSE)

#2017 - UF
df.GAL.2017_UF_br <- select(df.GAL.filtrado.hepC.2017, CO_UF_REQUISICAO)
df.GAL.2017_UF <- table(df.GAL.2017_UF_br)
df.GAL.2017_UF <- as.data.frame(df.GAL.2017_UF)
# Acrescentar informações IBGE 7d
df.GAL.2017_UF  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2017_UF, by.x = "UF", by.y = "df.GAL.2017_UF_br" , all.y = TRUE)
df.GAL.2017_UF <- distinct(df.GAL.2017_UF, UF, .keep_all = TRUE)
write.csv(df.GAL.2017_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2017_UF.csv', row.names=FALSE)

#2018 - MUN
df.GAL.2018_mun_br <- select(df.GAL.filtrado.hepC.2018, CO_MUNICIPIO_RESIDENCIA)
df.GAL.2018_mun <- table(df.GAL.2018_mun_br)
df.GAL.2018_mun <- as.data.frame(df.GAL.2018_mun)
# Acrescentar informações IBGE 7d
df.GAL.2018_mun  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2018_mun, by.x = "cod_mun_completo", by.y = "df.GAL.2018_mun_br" , all.y = TRUE)
write.csv(df.GAL.2018_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2018_mun.csv', row.names=FALSE)

#2018 - UF
df.GAL.2018_UF_br <- select(df.GAL.filtrado.hepC.2018, CO_UF_REQUISICAO)
df.GAL.2018_UF <- table(df.GAL.2018_UF_br)
df.GAL.2018_UF <- as.data.frame(df.GAL.2018_UF)
# Acrescentar informações IBGE 7d
df.GAL.2018_UF  <- merge(cod_mun_IBGE_6d_7d,df.GAL.2018_UF, by.x = "UF", by.y = "df.GAL.2018_UF_br" , all.y = TRUE)
df.GAL.2018_UF <- distinct(df.GAL.2018_UF, UF, .keep_all = TRUE)
write.csv(df.GAL.2018_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.GAL.2018_UF.csv', row.names=FALSE)

#############
### SINAN ###
#############

#2008 - MUN
df.SINAN.2008_mun_br <- select(df.SINAN.filtrado.hepC.2008, ID_MUNICIP)
df.SINAN.2008_mun <- table(df.SINAN.2008_mun_br)
df.SINAN.2008_mun <- as.data.frame(df.SINAN.2008_mun)
# Acrescentar informações IBGE 7d
df.SINAN.2008_mun  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2008_mun, by.x = "cod_mun_completo", by.y = "df.SINAN.2008_mun_br" , all.y = TRUE)
write.csv(df.SINAN.2008_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2008_mun.csv', row.names=FALSE)

#2008 - UF
df.SINAN.2008_UF_br <- select(df.SINAN.filtrado.hepC.2008, SG_UF_NOT)
df.SINAN.2008_UF <- table(df.SINAN.2008_UF_br)
df.SINAN.2008_UF <- as.data.frame(df.SINAN.2008_UF)
# Acrescentar informações IBGE 7d
df.SINAN.2008_UF  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2008_UF, by.x = "UF", by.y = "df.SINAN.2008_UF_br" , all.y = TRUE)
df.SINAN.2008_UF <- distinct(df.SINAN.2008_UF, UF, .keep_all = TRUE)
write.csv(df.SINAN.2008_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2008_UF.csv', row.names=FALSE)

#2009 - MUN
df.SINAN.2009_mun_br <- select(df.SINAN.filtrado.hepC.2009, ID_MUNICIP)
df.SINAN.2009_mun <- table(df.SINAN.2009_mun_br)
df.SINAN.2009_mun <- as.data.frame(df.SINAN.2009_mun)
# Acrescentar informações IBGE 7d
df.SINAN.2009_mun  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2009_mun, by.x = "cod_mun_completo", by.y = "df.SINAN.2009_mun_br" , all.y = TRUE)
write.csv(df.SINAN.2009_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2009_mun.csv', row.names=FALSE)

#2009 - UF
df.SINAN.2009_UF_br <- select(df.SINAN.filtrado.hepC.2009, SG_UF_NOT)
df.SINAN.2009_UF <- table(df.SINAN.2009_UF_br)
df.SINAN.2009_UF <- as.data.frame(df.SINAN.2009_UF)
# Acrescentar informações IBGE 7d
df.SINAN.2009_UF  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2009_UF, by.x = "UF", by.y = "df.SINAN.2009_UF_br" , all.y = TRUE)
df.SINAN.2009_UF <- distinct(df.SINAN.2009_UF, UF, .keep_all = TRUE)
write.csv(df.SINAN.2009_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2009_UF.csv', row.names=FALSE)

#2010 - MUN
df.SINAN.2010_mun_br <- select(df.SINAN.filtrado.hepC.2010, ID_MUNICIP)
df.SINAN.2010_mun <- table(df.SINAN.2010_mun_br)
df.SINAN.2010_mun <- as.data.frame(df.SINAN.2010_mun)
# Acrescentar informações IBGE 7d
df.SINAN.2010_mun  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2010_mun, by.x = "cod_mun_completo", by.y = "df.SINAN.2010_mun_br" , all.y = TRUE)
write.csv(df.SINAN.2010_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2010_mun.csv', row.names=FALSE)

#2010 - UF
df.SINAN.2010_UF_br <- select(df.SINAN.filtrado.hepC.2010, SG_UF_NOT)
df.SINAN.2010_UF <- table(df.SINAN.2010_UF_br)
df.SINAN.2010_UF <- as.data.frame(df.SINAN.2010_UF)
# Acrescentar informações IBGE 7d
df.SINAN.2010_UF  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2010_UF, by.x = "UF", by.y = "df.SINAN.2010_UF_br" , all.y = TRUE)
df.SINAN.2010_UF <- distinct(df.SINAN.2010_UF, UF, .keep_all = TRUE)
write.csv(df.SINAN.2010_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2010_UF.csv', row.names=FALSE)

#2011 - MUN
df.SINAN.2011_mun_br <- select(df.SINAN.filtrado.hepC.2011, ID_MUNICIP)
df.SINAN.2011_mun <- table(df.SINAN.2011_mun_br)
df.SINAN.2011_mun <- as.data.frame(df.SINAN.2011_mun)
# Acrescentar informações IBGE 7d
df.SINAN.2011_mun  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2011_mun, by.x = "cod_mun_completo", by.y = "df.SINAN.2011_mun_br" , all.y = TRUE)
write.csv(df.SINAN.2011_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2011_mun.csv', row.names=FALSE)

#2011 - UF
df.SINAN.2011_UF_br <- select(df.SINAN.filtrado.hepC.2011, SG_UF_NOT)
df.SINAN.2011_UF <- table(df.SINAN.2011_UF_br)
df.SINAN.2011_UF <- as.data.frame(df.SINAN.2011_UF)
# Acrescentar informações IBGE 7d
df.SINAN.2011_UF  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2011_UF, by.x = "UF", by.y = "df.SINAN.2011_UF_br" , all.y = TRUE)
df.SINAN.2011_UF <- distinct(df.SINAN.2011_UF, UF, .keep_all = TRUE)
write.csv(df.SINAN.2011_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2011_UF.csv', row.names=FALSE)

#2012 - MUN
df.SINAN.2012_mun_br <- select(df.SINAN.filtrado.hepC.2012, ID_MUNICIP)
df.SINAN.2012_mun <- table(df.SINAN.2012_mun_br)
df.SINAN.2012_mun <- as.data.frame(df.SINAN.2012_mun)
# Acrescentar informações IBGE 7d
df.SINAN.2012_mun  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2012_mun, by.x = "cod_mun_completo", by.y = "df.SINAN.2012_mun_br" , all.y = TRUE)
write.csv(df.SINAN.2012_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2012_mun.csv', row.names=FALSE)

#2012 - UF
df.SINAN.2012_UF_br <- select(df.SINAN.filtrado.hepC.2012, SG_UF_NOT)
df.SINAN.2012_UF <- table(df.SINAN.2012_UF_br)
df.SINAN.2012_UF <- as.data.frame(df.SINAN.2012_UF)
# Acrescentar informações IBGE 7d
df.SINAN.2012_UF  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2012_UF, by.x = "UF", by.y = "df.SINAN.2012_UF_br" , all.y = TRUE)
df.SINAN.2012_UF <- distinct(df.SINAN.2012_UF, UF, .keep_all = TRUE)
write.csv(df.SINAN.2012_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2012_UF.csv', row.names=FALSE)

#2013 - MUN
df.SINAN.2013_mun_br <- select(df.SINAN.filtrado.hepC.2013, ID_MUNICIP)
df.SINAN.2013_mun <- table(df.SINAN.2013_mun_br)
df.SINAN.2013_mun <- as.data.frame(df.SINAN.2013_mun)
# Acrescentar informações IBGE 7d
df.SINAN.2013_mun  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2013_mun, by.x = "cod_mun_completo", by.y = "df.SINAN.2013_mun_br" , all.y = TRUE)
write.csv(df.SINAN.2013_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2013_mun.csv', row.names=FALSE)

#2013 - UF
df.SINAN.2013_UF_br <- select(df.SINAN.filtrado.hepC.2013, SG_UF_NOT)
df.SINAN.2013_UF <- table(df.SINAN.2013_UF_br)
df.SINAN.2013_UF <- as.data.frame(df.SINAN.2013_UF)
# Acrescentar informações IBGE 7d
df.SINAN.2013_UF  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2013_UF, by.x = "UF", by.y = "df.SINAN.2013_UF_br" , all.y = TRUE)
df.SINAN.2013_UF <- distinct(df.SINAN.2013_UF, UF, .keep_all = TRUE)
write.csv(df.SINAN.2013_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2013_UF.csv', row.names=FALSE)

#2014 - MUN
df.SINAN.2014_mun_br <- select(df.SINAN.filtrado.hepC.2014, ID_MUNICIP)
df.SINAN.2014_mun <- table(df.SINAN.2014_mun_br)
df.SINAN.2014_mun <- as.data.frame(df.SINAN.2014_mun)
# Acrescentar informações IBGE 7d
df.SINAN.2014_mun  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2014_mun, by.x = "cod_mun_completo", by.y = "df.SINAN.2014_mun_br" , all.y = TRUE)
write.csv(df.SINAN.2014_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2014_mun.csv', row.names=FALSE)

#2014 - UF
df.SINAN.2014_UF_br <- select(df.SINAN.filtrado.hepC.2014, SG_UF_NOT)
df.SINAN.2014_UF <- table(df.SINAN.2014_UF_br)
df.SINAN.2014_UF <- as.data.frame(df.SINAN.2014_UF)
# Acrescentar informações IBGE 7d
df.SINAN.2014_UF  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2014_UF, by.x = "UF", by.y = "df.SINAN.2014_UF_br" , all.y = TRUE)
df.SINAN.2014_UF <- distinct(df.SINAN.2014_UF, UF, .keep_all = TRUE)
write.csv(df.SINAN.2014_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2014_UF.csv', row.names=FALSE)

#2015 - MUN
df.SINAN.2015_mun_br <- select(df.SINAN.filtrado.hepC.2015, ID_MUNICIP)
df.SINAN.2015_mun <- table(df.SINAN.2015_mun_br)
df.SINAN.2015_mun <- as.data.frame(df.SINAN.2015_mun)
# Acrescentar informações IBGE 7d
df.SINAN.2015_mun  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2015_mun, by.x = "cod_mun_completo", by.y = "df.SINAN.2015_mun_br" , all.y = TRUE)
write.csv(df.SINAN.2015_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2015_mun.csv', row.names=FALSE)

#2015 - UF
df.SINAN.2015_UF_br <- select(df.SINAN.filtrado.hepC.2015, SG_UF_NOT)
df.SINAN.2015_UF <- table(df.SINAN.2015_UF_br)
df.SINAN.2015_UF <- as.data.frame(df.SINAN.2015_UF)
# Acrescentar informações IBGE 7d
df.SINAN.2015_UF  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2015_UF, by.x = "UF", by.y = "df.SINAN.2015_UF_br" , all.y = TRUE)
df.SINAN.2015_UF <- distinct(df.SINAN.2015_UF, UF, .keep_all = TRUE)
write.csv(df.SINAN.2015_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2015_UF.csv', row.names=FALSE)

#2016 - MUN
df.SINAN.2016_mun_br <- select(df.SINAN.filtrado.hepC.2016, ID_MUNICIP)
df.SINAN.2016_mun <- table(df.SINAN.2016_mun_br)
df.SINAN.2016_mun <- as.data.frame(df.SINAN.2016_mun)
# Acrescentar informações IBGE 7d
df.SINAN.2016_mun  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2016_mun, by.x = "cod_mun_completo", by.y = "df.SINAN.2016_mun_br" , all.y = TRUE)
write.csv(df.SINAN.2016_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2016_mun.csv', row.names=FALSE)

#2016 - UF
df.SINAN.2016_UF_br <- select(df.SINAN.filtrado.hepC.2016, SG_UF_NOT)
df.SINAN.2016_UF <- table(df.SINAN.2016_UF_br)
df.SINAN.2016_UF <- as.data.frame(df.SINAN.2016_UF)
# Acrescentar informações IBGE 7d
df.SINAN.2016_UF  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2016_UF, by.x = "UF", by.y = "df.SINAN.2016_UF_br" , all.y = TRUE)
df.SINAN.2016_UF <- distinct(df.SINAN.2016_UF, UF, .keep_all = TRUE)
write.csv(df.SINAN.2016_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2016_UF.csv', row.names=FALSE)

#2017 - MUN
df.SINAN.2017_mun_br <- select(df.SINAN.filtrado.hepC.2017, ID_MUNICIP)
df.SINAN.2017_mun <- table(df.SINAN.2017_mun_br)
df.SINAN.2017_mun <- as.data.frame(df.SINAN.2017_mun)
# Acrescentar informações IBGE 7d
df.SINAN.2017_mun  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2017_mun, by.x = "cod_mun_completo", by.y = "df.SINAN.2017_mun_br" , all.y = TRUE)
write.csv(df.SINAN.2017_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2017_mun.csv', row.names=FALSE)

#2017 - UF
df.SINAN.2017_UF_br <- select(df.SINAN.filtrado.hepC.2017, SG_UF_NOT)
df.SINAN.2017_UF <- table(df.SINAN.2017_UF_br)
df.SINAN.2017_UF <- as.data.frame(df.SINAN.2017_UF)
# Acrescentar informações IBGE 7d
df.SINAN.2017_UF  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2017_UF, by.x = "UF", by.y = "df.SINAN.2017_UF_br" , all.y = TRUE)
df.SINAN.2017_UF <- distinct(df.SINAN.2017_UF, UF, .keep_all = TRUE)
write.csv(df.SINAN.2017_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2017_UF.csv', row.names=FALSE)

#2018 - MUN
df.SINAN.2018_mun_br <- select(df.SINAN.filtrado.hepC.2018, ID_MUNICIP)
df.SINAN.2018_mun <- table(df.SINAN.2018_mun_br)
df.SINAN.2018_mun <- as.data.frame(df.SINAN.2018_mun)
# Acrescentar informações IBGE 7d
df.SINAN.2018_mun  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2018_mun, by.x = "cod_mun_completo", by.y = "df.SINAN.2018_mun_br" , all.y = TRUE)
write.csv(df.SINAN.2018_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2018_mun.csv', row.names=FALSE)

#2018 - UF
df.SINAN.2018_UF_br <- select(df.SINAN.filtrado.hepC.2018, SG_UF_NOT)
df.SINAN.2018_UF <- table(df.SINAN.2018_UF_br)
df.SINAN.2018_UF <- as.data.frame(df.SINAN.2018_UF)
# Acrescentar informações IBGE 7d
df.SINAN.2018_UF  <- merge(cod_mun_IBGE_6d_7d,df.SINAN.2018_UF, by.x = "UF", by.y = "df.SINAN.2018_UF_br" , all.y = TRUE)
df.SINAN.2018_UF <- distinct(df.SINAN.2018_UF, UF, .keep_all = TRUE)
write.csv(df.SINAN.2018_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SINAN.2018_UF.csv', row.names=FALSE)

#############
### APAC ####
#############

#2008 - MUN
df.APAC.2008_mun_br <- select(df.APAC.filtrado.hepc.2008, CO_MUNICIPIO_HOSPITAL)
df.APAC.2008_mun <- table(df.APAC.2008_mun_br)
df.APAC.2008_mun <- as.data.frame(df.APAC.2008_mun)
# Acrescentar informações IBGE 7d
df.APAC.2008_mun  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2008_mun, by.x = "cod_mun_completo", by.y = "df.APAC.2008_mun_br" , all.y = TRUE)
write.csv(df.APAC.2008_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2008_mun.csv', row.names=FALSE)

#2008 - UF
df.APAC.2008_UF_br <- select(df.APAC.filtrado.hepc.2008, UF_HOSPITAL)
df.APAC.2008_UF <- table(df.APAC.2008_UF_br)
df.APAC.2008_UF <- as.data.frame(df.APAC.2008_UF)
# Acrescentar informações IBGE 7d
df.APAC.2008_UF  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2008_UF, by.x = "UF", by.y = "df.APAC.2008_UF_br" , all.y = TRUE)
df.APAC.2008_UF <- distinct(df.APAC.2008_UF, UF, .keep_all = TRUE)
write.csv(df.APAC.2008_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2008_UF.csv', row.names=FALSE)

#2009 - MUN
df.APAC.2009_mun_br <- select(df.APAC.filtrado.hepc.2009, CO_MUNICIPIO_HOSPITAL)
df.APAC.2009_mun <- table(df.APAC.2009_mun_br)
df.APAC.2009_mun <- as.data.frame(df.APAC.2009_mun)
# Acrescentar informações IBGE 7d
df.APAC.2009_mun  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2009_mun, by.x = "cod_mun_completo", by.y = "df.APAC.2009_mun_br" , all.y = TRUE)
write.csv(df.APAC.2009_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2009_mun.csv', row.names=FALSE)

#2009 - UF
df.APAC.2009_UF_br <- select(df.APAC.filtrado.hepc.2009, UF_HOSPITAL)
df.APAC.2009_UF <- table(df.APAC.2009_UF_br)
df.APAC.2009_UF <- as.data.frame(df.APAC.2009_UF)
# Acrescentar informações IBGE 7d
df.APAC.2009_UF  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2009_UF, by.x = "UF", by.y = "df.APAC.2009_UF_br" , all.y = TRUE)
df.APAC.2009_UF <- distinct(df.APAC.2009_UF, UF, .keep_all = TRUE)
write.csv(df.APAC.2009_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2009_UF.csv', row.names=FALSE)

#2010 - MUN
df.APAC.2010_mun_br <- select(df.APAC.filtrado.hepc.2010, CO_MUNICIPIO_HOSPITAL)
df.APAC.2010_mun <- table(df.APAC.2010_mun_br)
df.APAC.2010_mun <- as.data.frame(df.APAC.2010_mun)
# Acrescentar informações IBGE 7d
df.APAC.2010_mun  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2010_mun, by.x = "cod_mun_completo", by.y = "df.APAC.2010_mun_br" , all.y = TRUE)
write.csv(df.APAC.2010_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2010_mun.csv', row.names=FALSE)

#2010 - UF
df.APAC.2010_UF_br <- select(df.APAC.filtrado.hepc.2010, UF_HOSPITAL)
df.APAC.2010_UF <- table(df.APAC.2010_UF_br)
df.APAC.2010_UF <- as.data.frame(df.APAC.2010_UF)
# Acrescentar informações IBGE 7d
df.APAC.2010_UF  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2010_UF, by.x = "UF", by.y = "df.APAC.2010_UF_br" , all.y = TRUE)
df.APAC.2010_UF <- distinct(df.APAC.2010_UF, UF, .keep_all = TRUE)
write.csv(df.APAC.2010_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2010_UF.csv', row.names=FALSE)

#2011 - MUN
df.APAC.2011_mun_br <- select(df.APAC.filtrado.hepc.2011, CO_MUNICIPIO_HOSPITAL)
df.APAC.2011_mun <- table(df.APAC.2011_mun_br)
df.APAC.2011_mun <- as.data.frame(df.APAC.2011_mun)
# Acrescentar informações IBGE 7d
df.APAC.2011_mun  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2011_mun, by.x = "cod_mun_completo", by.y = "df.APAC.2011_mun_br" , all.y = TRUE)
write.csv(df.APAC.2011_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2011_mun.csv', row.names=FALSE)

#2011 - UF
df.APAC.2011_UF_br <- select(df.APAC.filtrado.hepc.2011, UF_HOSPITAL)
df.APAC.2011_UF <- table(df.APAC.2011_UF_br)
df.APAC.2011_UF <- as.data.frame(df.APAC.2011_UF)
# Acrescentar informações IBGE 7d
df.APAC.2011_UF  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2011_UF, by.x = "UF", by.y = "df.APAC.2011_UF_br" , all.y = TRUE)
df.APAC.2011_UF <- distinct(df.APAC.2011_UF, UF, .keep_all = TRUE)
write.csv(df.APAC.2011_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2011_UF.csv', row.names=FALSE)

#2012 - MUN
df.APAC.2012_mun_br <- select(df.APAC.filtrado.hepc.2012, CO_MUNICIPIO_HOSPITAL)
df.APAC.2012_mun <- table(df.APAC.2012_mun_br)
df.APAC.2012_mun <- as.data.frame(df.APAC.2012_mun)
# Acrescentar informações IBGE 7d
df.APAC.2012_mun  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2012_mun, by.x = "cod_mun_completo", by.y = "df.APAC.2012_mun_br" , all.y = TRUE)
write.csv(df.APAC.2012_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2012_mun.csv', row.names=FALSE)

#2012 - UF
df.APAC.2012_UF_br <- select(df.APAC.filtrado.hepc.2012, UF_HOSPITAL)
df.APAC.2012_UF <- table(df.APAC.2012_UF_br)
df.APAC.2012_UF <- as.data.frame(df.APAC.2012_UF)
# Acrescentar informações IBGE 7d
df.APAC.2012_UF  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2012_UF, by.x = "UF", by.y = "df.APAC.2012_UF_br" , all.y = TRUE)
df.APAC.2012_UF <- distinct(df.APAC.2012_UF, UF, .keep_all = TRUE)
write.csv(df.APAC.2012_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2012_UF.csv', row.names=FALSE)

#2013 - MUN
df.APAC.2013_mun_br <- select(df.APAC.filtrado.hepc.2013, CO_MUNICIPIO_HOSPITAL)
df.APAC.2013_mun <- table(df.APAC.2013_mun_br)
df.APAC.2013_mun <- as.data.frame(df.APAC.2013_mun)
# Acrescentar informações IBGE 7d
df.APAC.2013_mun  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2013_mun, by.x = "cod_mun_completo", by.y = "df.APAC.2013_mun_br" , all.y = TRUE)
write.csv(df.APAC.2013_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2013_mun.csv', row.names=FALSE)

#2013 - UF
df.APAC.2013_UF_br <- select(df.APAC.filtrado.hepc.2013, UF_HOSPITAL)
df.APAC.2013_UF <- table(df.APAC.2013_UF_br)
df.APAC.2013_UF <- as.data.frame(df.APAC.2013_UF)
# Acrescentar informações IBGE 7d
df.APAC.2013_UF  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2013_UF, by.x = "UF", by.y = "df.APAC.2013_UF_br" , all.y = TRUE)
df.APAC.2013_UF <- distinct(df.APAC.2013_UF, UF, .keep_all = TRUE)
write.csv(df.APAC.2013_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2013_UF.csv', row.names=FALSE)

#2014 - MUN
df.APAC.2014_mun_br <- select(df.APAC.filtrado.hepc.2014, CO_MUNICIPIO_HOSPITAL)
df.APAC.2014_mun <- table(df.APAC.2014_mun_br)
df.APAC.2014_mun <- as.data.frame(df.APAC.2014_mun)
# Acrescentar informações IBGE 7d
df.APAC.2014_mun  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2014_mun, by.x = "cod_mun_completo", by.y = "df.APAC.2014_mun_br" , all.y = TRUE)
write.csv(df.APAC.2014_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2014_mun.csv', row.names=FALSE)

#2014 - UF
df.APAC.2014_UF_br <- select(df.APAC.filtrado.hepc.2014, UF_HOSPITAL)
df.APAC.2014_UF <- table(df.APAC.2014_UF_br)
df.APAC.2014_UF <- as.data.frame(df.APAC.2014_UF)
# Acrescentar informações IBGE 7d
df.APAC.2014_UF  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2014_UF, by.x = "UF", by.y = "df.APAC.2014_UF_br" , all.y = TRUE)
df.APAC.2014_UF <- distinct(df.APAC.2014_UF, UF, .keep_all = TRUE)
write.csv(df.APAC.2014_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2014_UF.csv', row.names=FALSE)

#2015 - MUN
df.APAC.2015_mun_br <- select(df.APAC.filtrado.hepc.2015, CO_MUNICIPIO_HOSPITAL)
df.APAC.2015_mun <- table(df.APAC.2015_mun_br)
df.APAC.2015_mun <- as.data.frame(df.APAC.2015_mun)
# Acrescentar informações IBGE 7d
df.APAC.2015_mun  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2015_mun, by.x = "cod_mun_completo", by.y = "df.APAC.2015_mun_br" , all.y = TRUE)
write.csv(df.APAC.2015_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2015_mun.csv', row.names=FALSE)

#2015 - UF
df.APAC.2015_UF_br <- select(df.APAC.filtrado.hepc.2015, UF_HOSPITAL)
df.APAC.2015_UF <- table(df.APAC.2015_UF_br)
df.APAC.2015_UF <- as.data.frame(df.APAC.2015_UF)
# Acrescentar informações IBGE 7d
df.APAC.2015_UF  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2015_UF, by.x = "UF", by.y = "df.APAC.2015_UF_br" , all.y = TRUE)
df.APAC.2015_UF <- distinct(df.APAC.2015_UF, UF, .keep_all = TRUE)
write.csv(df.APAC.2015_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2015_UF.csv', row.names=FALSE)

#2016 - MUN
df.APAC.2016_mun_br <- select(df.APAC.filtrado.hepc.2016, CO_MUNICIPIO_HOSPITAL)
df.APAC.2016_mun <- table(df.APAC.2016_mun_br)
df.APAC.2016_mun <- as.data.frame(df.APAC.2016_mun)
# Acrescentar informações IBGE 7d
df.APAC.2016_mun  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2016_mun, by.x = "cod_mun_completo", by.y = "df.APAC.2016_mun_br" , all.y = TRUE)
write.csv(df.APAC.2016_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2016_mun.csv', row.names=FALSE)

#2016 - UF
df.APAC.2016_UF_br <- select(df.APAC.filtrado.hepc.2016, UF_HOSPITAL)
df.APAC.2016_UF <- table(df.APAC.2016_UF_br)
df.APAC.2016_UF <- as.data.frame(df.APAC.2016_UF)
# Acrescentar informações IBGE 7d
df.APAC.2016_UF  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2016_UF, by.x = "UF", by.y = "df.APAC.2016_UF_br" , all.y = TRUE)
df.APAC.2016_UF <- distinct(df.APAC.2016_UF, UF, .keep_all = TRUE)
write.csv(df.APAC.2016_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2016_UF.csv', row.names=FALSE)

#2017 - MUN
df.APAC.2017_mun_br <- select(df.APAC.filtrado.hepc.2017, CO_MUNICIPIO_HOSPITAL)
df.APAC.2017_mun <- table(df.APAC.2017_mun_br)
df.APAC.2017_mun <- as.data.frame(df.APAC.2017_mun)
# Acrescentar informações IBGE 7d
df.APAC.2017_mun  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2017_mun, by.x = "cod_mun_completo", by.y = "df.APAC.2017_mun_br" , all.y = TRUE)
write.csv(df.APAC.2017_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2017_mun.csv', row.names=FALSE)

#2017 - UF
df.APAC.2017_UF_br <- select(df.APAC.filtrado.hepc.2017, UF_HOSPITAL)
df.APAC.2017_UF <- table(df.APAC.2017_UF_br)
df.APAC.2017_UF <- as.data.frame(df.APAC.2017_UF)
# Acrescentar informações IBGE 7d
df.APAC.2017_UF  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2017_UF, by.x = "UF", by.y = "df.APAC.2017_UF_br" , all.y = TRUE)
df.APAC.2017_UF <- distinct(df.APAC.2017_UF, UF, .keep_all = TRUE)
write.csv(df.APAC.2017_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2017_UF.csv', row.names=FALSE)

#2018 - MUN
df.APAC.2018_mun_br <- select(df.APAC.filtrado.hepc.2018, CO_MUNICIPIO_HOSPITAL)
df.APAC.2018_mun <- table(df.APAC.2018_mun_br)
df.APAC.2018_mun <- as.data.frame(df.APAC.2018_mun)
# Acrescentar informações IBGE 7d
df.APAC.2018_mun  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2018_mun, by.x = "cod_mun_completo", by.y = "df.APAC.2018_mun_br" , all.y = TRUE)
write.csv(df.APAC.2018_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2018_mun.csv', row.names=FALSE)

#2018 - UF
df.APAC.2018_UF_br <- select(df.APAC.filtrado.hepc.2018, UF_HOSPITAL)
df.APAC.2018_UF <- table(df.APAC.2018_UF_br)
df.APAC.2018_UF <- as.data.frame(df.APAC.2018_UF)
# Acrescentar informações IBGE 7d
df.APAC.2018_UF  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2018_UF, by.x = "UF", by.y = "df.APAC.2018_UF_br" , all.y = TRUE)
df.APAC.2018_UF <- distinct(df.APAC.2018_UF, UF, .keep_all = TRUE)
write.csv(df.APAC.2018_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2018_UF.csv', row.names=FALSE)

#############
#### BPAI ###
#############
