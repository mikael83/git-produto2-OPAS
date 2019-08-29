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

#install.packages('eeptools')
library("eeptools")


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
## Mac
cod_mun_IBGE <- readxl::read_xls("/Users/mikaellemos/Downloads/DTB_2018/RELATORIO_DTB_BRASIL_MUNICIPIO.xls")

## MS
cod_mun_IBGE <- readxl::read_xls("/Users/mikael.lemos/Downloads/RELATORIO_DTB_BRASIL_MUNICIPIO.xls")

############# tabela completa - COD IBGE 7 digitos ################

# retirar úktimo digito do código completo 7 dígitos 
cod_mun_completo <- substr(cod_mun_IBGE$`Código Município Completo`, 1, 6)

# transformar lista de cidades 2015 com 6 digitos em df
cod_mun_completo <- as.data.frame(cod_mun_completo)

# tabela completa IBGE contendo todos os municipios em 2018 com 6 e 7 digitos (5570)
cod_mun_IBGE_6d_7d <- cbind(cod_mun_completo, cod_mun_IBGE)

write.csv(cod_mun_IBGE_6d_7d, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/cod_mun_IBGE_6d_7d.csv', row.names=FALSE)

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

df.APAC.2008_UF_br$UF_HOSPITAL <- as.character(df.APAC.2008_UF_br$UF_HOSPITAL)

df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "RO"] <- "11"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "AC"] <- "12"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "AM"] <- "13"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "RR"] <- "14"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "PA"] <- "15"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "AP"] <- "16"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "TO"] <- "17"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "MA"] <- "21"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "PI"] <- "22"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "CE"] <- "23"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "RN"] <- "24"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "PB"] <- "25"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "PE"] <- "26"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "AL"] <- "27"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "SE"] <- "28"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "BA"] <- "29"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "MG"] <- "31"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "ES"] <- "32"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "RJ"] <- "33"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "SP"] <- "35"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "PR"] <- "41"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "SC"] <- "42"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "RS"] <- "43"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "MS"] <- "50"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "MT"] <- "51"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "GO"] <- "52"
df.APAC.2008_UF_br$UF_HOSPITAL[df.APAC.2008_UF_br$UF_HOSPITAL == "DF"] <- "53"

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

df.APAC.2009_UF_br$UF_HOSPITAL <- as.character(df.APAC.2009_UF_br$UF_HOSPITAL)

df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "RO"] <- "11"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "AC"] <- "12"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "AM"] <- "13"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "RR"] <- "14"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "PA"] <- "15"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "AP"] <- "16"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "TO"] <- "17"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "MA"] <- "21"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "PI"] <- "22"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "CE"] <- "23"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "RN"] <- "24"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "PB"] <- "25"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "PE"] <- "26"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "AL"] <- "27"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "SE"] <- "28"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "BA"] <- "29"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "MG"] <- "31"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "ES"] <- "32"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "RJ"] <- "33"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "SP"] <- "35"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "PR"] <- "41"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "SC"] <- "42"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "RS"] <- "43"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "MS"] <- "50"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "MT"] <- "51"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "GO"] <- "52"
df.APAC.2009_UF_br$UF_HOSPITAL[df.APAC.2009_UF_br$UF_HOSPITAL == "DF"] <- "53"

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

df.APAC.2010_UF_br$UF_HOSPITAL <- as.character(df.APAC.2010_UF_br$UF_HOSPITAL)

df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "RO"] <- "11"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "AC"] <- "12"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "AM"] <- "13"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "RR"] <- "14"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "PA"] <- "15"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "AP"] <- "16"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "TO"] <- "17"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "MA"] <- "21"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "PI"] <- "22"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "CE"] <- "23"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "RN"] <- "24"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "PB"] <- "25"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "PE"] <- "26"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "AL"] <- "27"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "SE"] <- "28"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "BA"] <- "29"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "MG"] <- "31"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "ES"] <- "32"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "RJ"] <- "33"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "SP"] <- "35"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "PR"] <- "41"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "SC"] <- "42"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "RS"] <- "43"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "MS"] <- "50"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "MT"] <- "51"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "GO"] <- "52"
df.APAC.2010_UF_br$UF_HOSPITAL[df.APAC.2010_UF_br$UF_HOSPITAL == "DF"] <- "53"

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

df.APAC.2011_UF_br$UF_HOSPITAL <- as.character(df.APAC.2011_UF_br$UF_HOSPITAL)

df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "RO"] <- "11"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "AC"] <- "12"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "AM"] <- "13"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "RR"] <- "14"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "PA"] <- "15"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "AP"] <- "16"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "TO"] <- "17"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "MA"] <- "21"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "PI"] <- "22"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "CE"] <- "23"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "RN"] <- "24"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "PB"] <- "25"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "PE"] <- "26"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "AL"] <- "27"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "SE"] <- "28"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "BA"] <- "29"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "MG"] <- "31"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "ES"] <- "32"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "RJ"] <- "33"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "SP"] <- "35"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "PR"] <- "41"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "SC"] <- "42"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "RS"] <- "43"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "MS"] <- "50"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "MT"] <- "51"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "GO"] <- "52"
df.APAC.2011_UF_br$UF_HOSPITAL[df.APAC.2011_UF_br$UF_HOSPITAL == "DF"] <- "53"

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

df.APAC.2012_UF_br$UF_HOSPITAL <- as.character(df.APAC.2012_UF_br$UF_HOSPITAL)

df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "RO"] <- "11"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "AC"] <- "12"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "AM"] <- "13"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "RR"] <- "14"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "PA"] <- "15"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "AP"] <- "16"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "TO"] <- "17"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "MA"] <- "21"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "PI"] <- "22"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "CE"] <- "23"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "RN"] <- "24"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "PB"] <- "25"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "PE"] <- "26"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "AL"] <- "27"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "SE"] <- "28"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "BA"] <- "29"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "MG"] <- "31"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "ES"] <- "32"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "RJ"] <- "33"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "SP"] <- "35"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "PR"] <- "41"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "SC"] <- "42"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "RS"] <- "43"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "MS"] <- "50"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "MT"] <- "51"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "GO"] <- "52"
df.APAC.2012_UF_br$UF_HOSPITAL[df.APAC.2012_UF_br$UF_HOSPITAL == "DF"] <- "53"

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

df.APAC.2013_UF_br$UF_HOSPITAL <- as.character(df.APAC.2013_UF_br$UF_HOSPITAL)

df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "RO"] <- "11"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "AC"] <- "12"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "AM"] <- "13"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "RR"] <- "14"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "PA"] <- "15"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "AP"] <- "16"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "TO"] <- "17"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "MA"] <- "21"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "PI"] <- "22"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "CE"] <- "23"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "RN"] <- "24"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "PB"] <- "25"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "PE"] <- "26"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "AL"] <- "27"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "SE"] <- "28"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "BA"] <- "29"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "MG"] <- "31"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "ES"] <- "32"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "RJ"] <- "33"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "SP"] <- "35"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "PR"] <- "41"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "SC"] <- "42"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "RS"] <- "43"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "MS"] <- "50"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "MT"] <- "51"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "GO"] <- "52"
df.APAC.2013_UF_br$UF_HOSPITAL[df.APAC.2013_UF_br$UF_HOSPITAL == "DF"] <- "53"

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

df.APAC.2014_UF_br$UF_HOSPITAL <- as.character(df.APAC.2014_UF_br$UF_HOSPITAL)

df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "RO"] <- "11"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "AC"] <- "12"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "AM"] <- "13"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "RR"] <- "14"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "PA"] <- "15"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "AP"] <- "16"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "TO"] <- "17"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "MA"] <- "21"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "PI"] <- "22"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "CE"] <- "23"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "RN"] <- "24"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "PB"] <- "25"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "PE"] <- "26"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "AL"] <- "27"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "SE"] <- "28"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "BA"] <- "29"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "MG"] <- "31"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "ES"] <- "32"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "RJ"] <- "33"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "SP"] <- "35"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "PR"] <- "41"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "SC"] <- "42"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "RS"] <- "43"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "MS"] <- "50"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "MT"] <- "51"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "GO"] <- "52"
df.APAC.2014_UF_br$UF_HOSPITAL[df.APAC.2014_UF_br$UF_HOSPITAL == "DF"] <- "53"

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

df.APAC.2015_UF_br$UF_HOSPITAL <- as.character(df.APAC.2015_UF_br$UF_HOSPITAL)

df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "RO"] <- "11"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "AC"] <- "12"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "AM"] <- "13"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "RR"] <- "14"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "PA"] <- "15"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "AP"] <- "16"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "TO"] <- "17"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "MA"] <- "21"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "PI"] <- "22"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "CE"] <- "23"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "RN"] <- "24"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "PB"] <- "25"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "PE"] <- "26"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "AL"] <- "27"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "SE"] <- "28"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "BA"] <- "29"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "MG"] <- "31"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "ES"] <- "32"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "RJ"] <- "33"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "SP"] <- "35"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "PR"] <- "41"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "SC"] <- "42"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "RS"] <- "43"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "MS"] <- "50"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "MT"] <- "51"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "GO"] <- "52"
df.APAC.2015_UF_br$UF_HOSPITAL[df.APAC.2015_UF_br$UF_HOSPITAL == "DF"] <- "53"

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

df.APAC.2016_UF_br$UF_HOSPITAL <- as.character(df.APAC.2016_UF_br$UF_HOSPITAL)

df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "RO"] <- "11"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "AC"] <- "12"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "AM"] <- "13"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "RR"] <- "14"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "PA"] <- "15"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "AP"] <- "16"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "TO"] <- "17"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "MA"] <- "21"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "PI"] <- "22"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "CE"] <- "23"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "RN"] <- "24"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "PB"] <- "25"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "PE"] <- "26"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "AL"] <- "27"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "SE"] <- "28"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "BA"] <- "29"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "MG"] <- "31"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "ES"] <- "32"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "RJ"] <- "33"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "SP"] <- "35"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "PR"] <- "41"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "SC"] <- "42"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "RS"] <- "43"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "MS"] <- "50"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "MT"] <- "51"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "GO"] <- "52"
df.APAC.2016_UF_br$UF_HOSPITAL[df.APAC.2016_UF_br$UF_HOSPITAL == "DF"] <- "53"

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

df.APAC.2017_UF_br$UF_HOSPITAL <- as.character(df.APAC.2017_UF_br$UF_HOSPITAL)

df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "RO"] <- "11"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "AC"] <- "12"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "AM"] <- "13"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "RR"] <- "14"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "PA"] <- "15"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "AP"] <- "16"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "TO"] <- "17"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "MA"] <- "21"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "PI"] <- "22"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "CE"] <- "23"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "RN"] <- "24"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "PB"] <- "25"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "PE"] <- "26"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "AL"] <- "27"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "SE"] <- "28"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "BA"] <- "29"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "MG"] <- "31"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "ES"] <- "32"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "RJ"] <- "33"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "SP"] <- "35"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "PR"] <- "41"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "SC"] <- "42"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "RS"] <- "43"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "MS"] <- "50"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "MT"] <- "51"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "GO"] <- "52"
df.APAC.2017_UF_br$UF_HOSPITAL[df.APAC.2017_UF_br$UF_HOSPITAL == "DF"] <- "53"

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

df.APAC.2018_UF_br$UF_HOSPITAL <- as.character(df.APAC.2018_UF_br$UF_HOSPITAL)

df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "RO"] <- "11"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "AC"] <- "12"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "AM"] <- "13"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "RR"] <- "14"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "PA"] <- "15"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "AP"] <- "16"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "TO"] <- "17"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "MA"] <- "21"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "PI"] <- "22"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "CE"] <- "23"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "RN"] <- "24"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "PB"] <- "25"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "PE"] <- "26"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "AL"] <- "27"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "SE"] <- "28"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "BA"] <- "29"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "MG"] <- "31"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "ES"] <- "32"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "RJ"] <- "33"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "SP"] <- "35"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "PR"] <- "41"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "SC"] <- "42"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "RS"] <- "43"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "MS"] <- "50"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "MT"] <- "51"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "GO"] <- "52"
df.APAC.2018_UF_br$UF_HOSPITAL[df.APAC.2018_UF_br$UF_HOSPITAL == "DF"] <- "53"

df.APAC.2018_UF <- table(df.APAC.2018_UF_br)
df.APAC.2018_UF <- as.data.frame(df.APAC.2018_UF)
# Acrescentar informações IBGE 7d
df.APAC.2018_UF  <- merge(cod_mun_IBGE_6d_7d,df.APAC.2018_UF, by.x = "UF", by.y = "df.APAC.2018_UF_br" , all.y = TRUE)
df.APAC.2018_UF <- distinct(df.APAC.2018_UF, UF, .keep_all = TRUE)
write.csv(df.APAC.2018_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.APAC.2018_UF.csv', row.names=FALSE)

#############
#### BPAI ###
#############

#2008 - MUN
df.BPAI.2008_mun_br <- select(df.BPAI.filtrado.hepC.2008, CO_MUNICIPIO)
df.BPAI.2008_mun <- table(df.BPAI.2008_mun_br)
df.BPAI.2008_mun <- as.data.frame(df.BPAI.2008_mun)
# Acrescentar informações IBGE 7d
df.BPAI.2008_mun  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2008_mun, by.x = "cod_mun_completo", by.y = "df.BPAI.2008_mun_br" , all.y = TRUE)
write.csv(df.BPAI.2008_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2008_mun.csv', row.names=FALSE)

#2008 - UF
df.BPAI.2008_UF_br <- select(df.BPAI.filtrado.hepC.2008, UF)

df.BPAI.2008_UF_br$UF <- as.character(df.BPAI.2008_UF_br$UF)

df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "RO"] <- "11"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "AC"] <- "12"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "AM"] <- "13"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "RR"] <- "14"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "PA"] <- "15"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "AP"] <- "16"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "TO"] <- "17"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "MA"] <- "21"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "PI"] <- "22"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "CE"] <- "23"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "RN"] <- "24"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "PB"] <- "25"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "PE"] <- "26"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "AL"] <- "27"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "SE"] <- "28"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "BA"] <- "29"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "MG"] <- "31"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "ES"] <- "32"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "RJ"] <- "33"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "SP"] <- "35"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "PR"] <- "41"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "SC"] <- "42"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "RS"] <- "43"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "MS"] <- "50"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "MT"] <- "51"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "GO"] <- "52"
df.BPAI.2008_UF_br$UF[df.BPAI.2008_UF_br$UF == "DF"] <- "53"

df.BPAI.2008_UF <- table(df.BPAI.2008_UF_br)
df.BPAI.2008_UF <- as.data.frame(df.BPAI.2008_UF)

# Acrescentar informações IBGE 7d
df.BPAI.2008_UF  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2008_UF, by.x = "UF", by.y = "df.BPAI.2008_UF_br" , all.y = TRUE)
df.BPAI.2008_UF <- distinct(df.BPAI.2008_UF, UF, .keep_all = TRUE)
write.csv(df.BPAI.2008_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2008_UF.csv', row.names=FALSE)

#2009 - MUN
df.BPAI.2009_mun_br <- select(df.BPAI.filtrado.hepC.2009, CO_MUNICIPIO)
df.BPAI.2009_mun <- table(df.BPAI.2009_mun_br)
df.BPAI.2009_mun <- as.data.frame(df.BPAI.2009_mun)
# Acrescentar informações IBGE 7d
df.BPAI.2009_mun  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2009_mun, by.x = "cod_mun_completo", by.y = "df.BPAI.2009_mun_br" , all.y = TRUE)
write.csv(df.BPAI.2009_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2009_mun.csv', row.names=FALSE)

#2009 - UF
df.BPAI.2009_UF_br <- select(df.BPAI.filtrado.hepC.2009, UF)

df.BPAI.2009_UF_br$UF <- as.character(df.BPAI.2009_UF_br$UF)

df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "RO"] <- "11"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "AC"] <- "12"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "AM"] <- "13"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "RR"] <- "14"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "PA"] <- "15"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "AP"] <- "16"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "TO"] <- "17"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "MA"] <- "21"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "PI"] <- "22"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "CE"] <- "23"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "RN"] <- "24"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "PB"] <- "25"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "PE"] <- "26"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "AL"] <- "27"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "SE"] <- "28"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "BA"] <- "29"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "MG"] <- "31"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "ES"] <- "32"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "RJ"] <- "33"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "SP"] <- "35"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "PR"] <- "41"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "SC"] <- "42"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "RS"] <- "43"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "MS"] <- "50"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "MT"] <- "51"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "GO"] <- "52"
df.BPAI.2009_UF_br$UF[df.BPAI.2009_UF_br$UF == "DF"] <- "53"

df.BPAI.2009_UF <- table(df.BPAI.2009_UF_br)
df.BPAI.2009_UF <- as.data.frame(df.BPAI.2009_UF)
# Acrescentar informações IBGE 7d
df.BPAI.2009_UF  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2009_UF, by.x = "UF", by.y = "df.BPAI.2009_UF_br" , all.y = TRUE)
df.BPAI.2009_UF <- distinct(df.BPAI.2009_UF, UF, .keep_all = TRUE)
write.csv(df.BPAI.2009_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2009_UF.csv', row.names=FALSE)

#2010 - MUN
df.BPAI.2010_mun_br <- select(df.BPAI.filtrado.hepC.2010, CO_MUNICIPIO)
df.BPAI.2010_mun <- table(df.BPAI.2010_mun_br)
df.BPAI.2010_mun <- as.data.frame(df.BPAI.2010_mun)
# Acrescentar informações IBGE 7d
df.BPAI.2010_mun  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2010_mun, by.x = "cod_mun_completo", by.y = "df.BPAI.2010_mun_br" , all.y = TRUE)
write.csv(df.BPAI.2010_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2010_mun.csv', row.names=FALSE)

#2010 - UF
df.BPAI.2010_UF_br <- select(df.BPAI.filtrado.hepC.2010, UF)

df.BPAI.2010_UF_br$UF <- as.character(df.BPAI.2010_UF_br$UF)

df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "RO"] <- "11"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "AC"] <- "12"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "AM"] <- "13"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "RR"] <- "14"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "PA"] <- "15"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "AP"] <- "16"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "TO"] <- "17"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "MA"] <- "21"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "PI"] <- "22"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "CE"] <- "23"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "RN"] <- "24"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "PB"] <- "25"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "PE"] <- "26"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "AL"] <- "27"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "SE"] <- "28"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "BA"] <- "29"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "MG"] <- "31"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "ES"] <- "32"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "RJ"] <- "33"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "SP"] <- "35"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "PR"] <- "41"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "SC"] <- "42"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "RS"] <- "43"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "MS"] <- "50"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "MT"] <- "51"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "GO"] <- "52"
df.BPAI.2010_UF_br$UF[df.BPAI.2010_UF_br$UF == "DF"] <- "53"

df.BPAI.2010_UF <- table(df.BPAI.2010_UF_br)
df.BPAI.2010_UF <- as.data.frame(df.BPAI.2010_UF)
# Acrescentar informações IBGE 7d
df.BPAI.2010_UF  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2010_UF, by.x = "UF", by.y = "df.BPAI.2010_UF_br" , all.y = TRUE)
df.BPAI.2010_UF <- distinct(df.BPAI.2010_UF, UF, .keep_all = TRUE)
write.csv(df.BPAI.2010_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2010_UF.csv', row.names=FALSE)

#2011 - MUN
df.BPAI.2011_mun_br <- select(df.BPAI.filtrado.hepC.2011, CO_MUNICIPIO)
df.BPAI.2011_mun <- table(df.BPAI.2011_mun_br)
df.BPAI.2011_mun <- as.data.frame(df.BPAI.2011_mun)
# Acrescentar informações IBGE 7d
df.BPAI.2011_mun  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2011_mun, by.x = "cod_mun_completo", by.y = "df.BPAI.2011_mun_br" , all.y = TRUE)
write.csv(df.BPAI.2011_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2011_mun.csv', row.names=FALSE)

#2011 - UF
df.BPAI.2011_UF_br <- select(df.BPAI.filtrado.hepC.2011, UF)

df.BPAI.2011_UF_br$UF <- as.character(df.BPAI.2011_UF_br$UF)

df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "RO"] <- "11"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "AC"] <- "12"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "AM"] <- "13"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "RR"] <- "14"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "PA"] <- "15"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "AP"] <- "16"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "TO"] <- "17"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "MA"] <- "21"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "PI"] <- "22"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "CE"] <- "23"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "RN"] <- "24"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "PB"] <- "25"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "PE"] <- "26"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "AL"] <- "27"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "SE"] <- "28"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "BA"] <- "29"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "MG"] <- "31"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "ES"] <- "32"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "RJ"] <- "33"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "SP"] <- "35"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "PR"] <- "41"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "SC"] <- "42"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "RS"] <- "43"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "MS"] <- "50"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "MT"] <- "51"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "GO"] <- "52"
df.BPAI.2011_UF_br$UF[df.BPAI.2011_UF_br$UF == "DF"] <- "53"

df.BPAI.2011_UF <- table(df.BPAI.2011_UF_br)
df.BPAI.2011_UF <- as.data.frame(df.BPAI.2011_UF)
# Acrescentar informações IBGE 7d
df.BPAI.2011_UF  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2011_UF, by.x = "UF", by.y = "df.BPAI.2011_UF_br" , all.y = TRUE)
df.BPAI.2011_UF <- distinct(df.BPAI.2011_UF, UF, .keep_all = TRUE)
write.csv(df.BPAI.2011_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2011_UF.csv', row.names=FALSE)

#2012 - MUN
df.BPAI.2012_mun_br <- select(df.BPAI.filtrado.hepC.2012, CO_MUNICIPIO)
df.BPAI.2012_mun <- table(df.BPAI.2012_mun_br)
df.BPAI.2012_mun <- as.data.frame(df.BPAI.2012_mun)
# Acrescentar informações IBGE 7d
df.BPAI.2012_mun  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2012_mun, by.x = "cod_mun_completo", by.y = "df.BPAI.2012_mun_br" , all.y = TRUE)
write.csv(df.BPAI.2012_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2012_mun.csv', row.names=FALSE)

#2012 - UF
df.BPAI.2012_UF_br <- select(df.BPAI.filtrado.hepC.2012, UF)

df.BPAI.2012_UF_br$UF <- as.character(df.BPAI.2012_UF_br$UF)

df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "RO"] <- "11"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "AC"] <- "12"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "AM"] <- "13"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "RR"] <- "14"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "PA"] <- "15"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "AP"] <- "16"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "TO"] <- "17"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "MA"] <- "21"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "PI"] <- "22"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "CE"] <- "23"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "RN"] <- "24"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "PB"] <- "25"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "PE"] <- "26"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "AL"] <- "27"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "SE"] <- "28"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "BA"] <- "29"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "MG"] <- "31"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "ES"] <- "32"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "RJ"] <- "33"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "SP"] <- "35"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "PR"] <- "41"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "SC"] <- "42"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "RS"] <- "43"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "MS"] <- "50"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "MT"] <- "51"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "GO"] <- "52"
df.BPAI.2012_UF_br$UF[df.BPAI.2012_UF_br$UF == "DF"] <- "53"

df.BPAI.2012_UF <- table(df.BPAI.2012_UF_br)
df.BPAI.2012_UF <- as.data.frame(df.BPAI.2012_UF)
# Acrescentar informações IBGE 7d
df.BPAI.2012_UF  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2012_UF, by.x = "UF", by.y = "df.BPAI.2012_UF_br" , all.y = TRUE)
df.BPAI.2012_UF <- distinct(df.BPAI.2012_UF, UF, .keep_all = TRUE)
write.csv(df.BPAI.2012_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2012_UF.csv', row.names=FALSE)

#2013 - MUN
df.BPAI.2013_mun_br <- select(df.BPAI.filtrado.hepC.2013, CO_MUNICIPIO)
df.BPAI.2013_mun <- table(df.BPAI.2013_mun_br)
df.BPAI.2013_mun <- as.data.frame(df.BPAI.2013_mun)
# Acrescentar informações IBGE 7d
df.BPAI.2013_mun  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2013_mun, by.x = "cod_mun_completo", by.y = "df.BPAI.2013_mun_br" , all.y = TRUE)
write.csv(df.BPAI.2013_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2013_mun.csv', row.names=FALSE)

#2013 - UF
df.BPAI.2013_UF_br <- select(df.BPAI.filtrado.hepC.2013, UF)

df.BPAI.2013_UF_br$UF <- as.character(df.BPAI.2013_UF_br$UF)

df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "RO"] <- "11"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "AC"] <- "12"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "AM"] <- "13"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "RR"] <- "14"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "PA"] <- "15"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "AP"] <- "16"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "TO"] <- "17"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "MA"] <- "21"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "PI"] <- "22"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "CE"] <- "23"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "RN"] <- "24"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "PB"] <- "25"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "PE"] <- "26"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "AL"] <- "27"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "SE"] <- "28"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "BA"] <- "29"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "MG"] <- "31"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "ES"] <- "32"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "RJ"] <- "33"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "SP"] <- "35"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "PR"] <- "41"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "SC"] <- "42"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "RS"] <- "43"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "MS"] <- "50"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "MT"] <- "51"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "GO"] <- "52"
df.BPAI.2013_UF_br$UF[df.BPAI.2013_UF_br$UF == "DF"] <- "53"

df.BPAI.2013_UF <- table(df.BPAI.2013_UF_br)
df.BPAI.2013_UF <- as.data.frame(df.BPAI.2013_UF)
# Acrescentar informações IBGE 7d
df.BPAI.2013_UF  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2013_UF, by.x = "UF", by.y = "df.BPAI.2013_UF_br" , all.y = TRUE)
df.BPAI.2013_UF <- distinct(df.BPAI.2013_UF, UF, .keep_all = TRUE)
write.csv(df.BPAI.2013_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2013_UF.csv', row.names=FALSE)

#2014 - MUN
df.BPAI.2014_mun_br <- select(df.BPAI.filtrado.hepC.2014, CO_MUNICIPIO)
df.BPAI.2014_mun <- table(df.BPAI.2014_mun_br)
df.BPAI.2014_mun <- as.data.frame(df.BPAI.2014_mun)
# Acrescentar informações IBGE 7d
df.BPAI.2014_mun  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2014_mun, by.x = "cod_mun_completo", by.y = "df.BPAI.2014_mun_br" , all.y = TRUE)
write.csv(df.BPAI.2014_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2014_mun.csv', row.names=FALSE)

#2014 - UF
df.BPAI.2014_UF_br <- select(df.BPAI.filtrado.hepC.2014, UF)

df.BPAI.2014_UF_br$UF <- as.character(df.BPAI.2014_UF_br$UF)

df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "RO"] <- "11"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "AC"] <- "12"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "AM"] <- "13"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "RR"] <- "14"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "PA"] <- "15"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "AP"] <- "16"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "TO"] <- "17"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "MA"] <- "21"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "PI"] <- "22"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "CE"] <- "23"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "RN"] <- "24"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "PB"] <- "25"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "PE"] <- "26"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "AL"] <- "27"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "SE"] <- "28"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "BA"] <- "29"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "MG"] <- "31"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "ES"] <- "32"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "RJ"] <- "33"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "SP"] <- "35"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "PR"] <- "41"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "SC"] <- "42"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "RS"] <- "43"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "MS"] <- "50"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "MT"] <- "51"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "GO"] <- "52"
df.BPAI.2014_UF_br$UF[df.BPAI.2014_UF_br$UF == "DF"] <- "53"

df.BPAI.2014_UF <- table(df.BPAI.2014_UF_br)
df.BPAI.2014_UF <- as.data.frame(df.BPAI.2014_UF)
# Acrescentar informações IBGE 7d
df.BPAI.2014_UF  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2014_UF, by.x = "UF", by.y = "df.BPAI.2014_UF_br" , all.y = TRUE)
df.BPAI.2014_UF <- distinct(df.BPAI.2014_UF, UF, .keep_all = TRUE)
write.csv(df.BPAI.2014_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2014_UF.csv', row.names=FALSE)

#2015 - MUN
df.BPAI.2015_mun_br <- select(df.BPAI.filtrado.hepC.2015, CO_MUNICIPIO)
df.BPAI.2015_mun <- table(df.BPAI.2015_mun_br)
df.BPAI.2015_mun <- as.data.frame(df.BPAI.2015_mun)
# Acrescentar informações IBGE 7d
df.BPAI.2015_mun  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2015_mun, by.x = "cod_mun_completo", by.y = "df.BPAI.2015_mun_br" , all.y = TRUE)
write.csv(df.BPAI.2015_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2015_mun.csv', row.names=FALSE)

#2015 - UF
df.BPAI.2015_UF_br <- select(df.BPAI.filtrado.hepC.2015, UF)

df.BPAI.2015_UF_br$UF <- as.character(df.BPAI.2015_UF_br$UF)

df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "RO"] <- "11"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "AC"] <- "12"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "AM"] <- "13"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "RR"] <- "14"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "PA"] <- "15"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "AP"] <- "16"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "TO"] <- "17"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "MA"] <- "21"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "PI"] <- "22"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "CE"] <- "23"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "RN"] <- "24"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "PB"] <- "25"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "PE"] <- "26"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "AL"] <- "27"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "SE"] <- "28"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "BA"] <- "29"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "MG"] <- "31"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "ES"] <- "32"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "RJ"] <- "33"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "SP"] <- "35"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "PR"] <- "41"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "SC"] <- "42"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "RS"] <- "43"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "MS"] <- "50"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "MT"] <- "51"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "GO"] <- "52"
df.BPAI.2015_UF_br$UF[df.BPAI.2015_UF_br$UF == "DF"] <- "53"

df.BPAI.2015_UF <- table(df.BPAI.2015_UF_br)
df.BPAI.2015_UF <- as.data.frame(df.BPAI.2015_UF)
# Acrescentar informações IBGE 7d
df.BPAI.2015_UF  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2015_UF, by.x = "UF", by.y = "df.BPAI.2015_UF_br" , all.y = TRUE)
df.BPAI.2015_UF <- distinct(df.BPAI.2015_UF, UF, .keep_all = TRUE)
write.csv(df.BPAI.2015_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2015_UF.csv', row.names=FALSE)

#2016 - MUN
df.BPAI.2016_mun_br <- select(df.BPAI.filtrado.hepC.2016, CO_MUNICIPIO)
df.BPAI.2016_mun <- table(df.BPAI.2016_mun_br)
df.BPAI.2016_mun <- as.data.frame(df.BPAI.2016_mun)
# Acrescentar informações IBGE 7d
df.BPAI.2016_mun  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2016_mun, by.x = "cod_mun_completo", by.y = "df.BPAI.2016_mun_br" , all.y = TRUE)
write.csv(df.BPAI.2016_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2016_mun.csv', row.names=FALSE)

#2016 - UF
df.BPAI.2016_UF_br <- select(df.BPAI.filtrado.hepC.2016, UF)

df.BPAI.2016_UF_br$UF <- as.character(df.BPAI.2016_UF_br$UF)

df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "RO"] <- "11"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "AC"] <- "12"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "AM"] <- "13"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "RR"] <- "14"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "PA"] <- "15"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "AP"] <- "16"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "TO"] <- "17"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "MA"] <- "21"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "PI"] <- "22"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "CE"] <- "23"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "RN"] <- "24"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "PB"] <- "25"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "PE"] <- "26"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "AL"] <- "27"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "SE"] <- "28"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "BA"] <- "29"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "MG"] <- "31"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "ES"] <- "32"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "RJ"] <- "33"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "SP"] <- "35"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "PR"] <- "41"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "SC"] <- "42"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "RS"] <- "43"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "MS"] <- "50"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "MT"] <- "51"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "GO"] <- "52"
df.BPAI.2016_UF_br$UF[df.BPAI.2016_UF_br$UF == "DF"] <- "53"

df.BPAI.2016_UF <- table(df.BPAI.2016_UF_br)
df.BPAI.2016_UF <- as.data.frame(df.BPAI.2016_UF)
# Acrescentar informações IBGE 7d
df.BPAI.2016_UF  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2016_UF, by.x = "UF", by.y = "df.BPAI.2016_UF_br" , all.y = TRUE)
df.BPAI.2016_UF <- distinct(df.BPAI.2016_UF, UF, .keep_all = TRUE)
write.csv(df.BPAI.2016_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2016_UF.csv', row.names=FALSE)

#2017 - MUN
df.BPAI.2017_mun_br <- select(df.BPAI.filtrado.hepC.2017, CO_MUNICIPIO)
df.BPAI.2017_mun <- table(df.BPAI.2017_mun_br)
df.BPAI.2017_mun <- as.data.frame(df.BPAI.2017_mun)
# Acrescentar informações IBGE 7d
df.BPAI.2017_mun  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2017_mun, by.x = "cod_mun_completo", by.y = "df.BPAI.2017_mun_br" , all.y = TRUE)
write.csv(df.BPAI.2017_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2017_mun.csv', row.names=FALSE)

#2017 - UF
df.BPAI.2017_UF_br <- select(df.BPAI.filtrado.hepC.2017, UF)

df.BPAI.2017_UF_br$UF <- as.character(df.BPAI.2017_UF_br$UF)

df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "RO"] <- "11"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "AC"] <- "12"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "AM"] <- "13"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "RR"] <- "14"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "PA"] <- "15"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "AP"] <- "16"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "TO"] <- "17"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "MA"] <- "21"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "PI"] <- "22"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "CE"] <- "23"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "RN"] <- "24"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "PB"] <- "25"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "PE"] <- "26"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "AL"] <- "27"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "SE"] <- "28"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "BA"] <- "29"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "MG"] <- "31"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "ES"] <- "32"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "RJ"] <- "33"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "SP"] <- "35"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "PR"] <- "41"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "SC"] <- "42"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "RS"] <- "43"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "MS"] <- "50"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "MT"] <- "51"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "GO"] <- "52"
df.BPAI.2017_UF_br$UF[df.BPAI.2017_UF_br$UF == "DF"] <- "53"

df.BPAI.2017_UF <- table(df.BPAI.2017_UF_br)
df.BPAI.2017_UF <- as.data.frame(df.BPAI.2017_UF)
# Acrescentar informações IBGE 7d
df.BPAI.2017_UF  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2017_UF, by.x = "UF", by.y = "df.BPAI.2017_UF_br" , all.y = TRUE)
df.BPAI.2017_UF <- distinct(df.BPAI.2017_UF, UF, .keep_all = TRUE)
write.csv(df.BPAI.2017_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2017_UF.csv', row.names=FALSE)

#2018 - MUN
df.BPAI.2018_mun_br <- select(df.BPAI.filtrado.hepC.2018, CO_MUNICIPIO)
df.BPAI.2018_mun <- table(df.BPAI.2018_mun_br)
df.BPAI.2018_mun <- as.data.frame(df.BPAI.2018_mun)
# Acrescentar informações IBGE 7d
df.BPAI.2018_mun  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2018_mun, by.x = "cod_mun_completo", by.y = "df.BPAI.2018_mun_br" , all.y = TRUE)
write.csv(df.BPAI.2018_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2018_mun.csv', row.names=FALSE)

#2018 - UF
df.BPAI.2018_UF_br <- select(df.BPAI.filtrado.hepC.2018, UF)

df.BPAI.2018_UF_br$UF <- as.character(df.BPAI.2018_UF_br$UF)

df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "RO"] <- "11"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "AC"] <- "12"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "AM"] <- "13"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "RR"] <- "14"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "PA"] <- "15"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "AP"] <- "16"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "TO"] <- "17"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "MA"] <- "21"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "PI"] <- "22"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "CE"] <- "23"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "RN"] <- "24"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "PB"] <- "25"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "PE"] <- "26"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "AL"] <- "27"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "SE"] <- "28"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "BA"] <- "29"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "MG"] <- "31"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "ES"] <- "32"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "RJ"] <- "33"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "SP"] <- "35"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "PR"] <- "41"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "SC"] <- "42"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "RS"] <- "43"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "MS"] <- "50"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "MT"] <- "51"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "GO"] <- "52"
df.BPAI.2018_UF_br$UF[df.BPAI.2018_UF_br$UF == "DF"] <- "53"

df.BPAI.2018_UF <- table(df.BPAI.2018_UF_br)
df.BPAI.2018_UF <- as.data.frame(df.BPAI.2018_UF)
# Acrescentar informações IBGE 7d
df.BPAI.2018_UF  <- merge(cod_mun_IBGE_6d_7d,df.BPAI.2018_UF, by.x = "UF", by.y = "df.BPAI.2018_UF_br" , all.y = TRUE)
df.BPAI.2018_UF <- distinct(df.BPAI.2018_UF, UF, .keep_all = TRUE)
write.csv(df.BPAI.2018_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.BPAI.2018_UF.csv', row.names=FALSE)

#############
#### SIM ###
#############

#2008 - MUN
df.SIM.2008_mun_br <- select(df.SIM.hepC.filtrado.2008, CODMUNOCOR)
df.SIM.2008_mun <- table(df.SIM.2008_mun_br)
df.SIM.2008_mun <- as.data.frame(df.SIM.2008_mun)
# Acrescentar informações IBGE 7d
df.SIM.2008_mun  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2008_mun, by.x = "cod_mun_completo", by.y = "df.SIM.2008_mun_br" , all.y = TRUE)
write.csv(df.SIM.2008_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2008_mun.csv', row.names=FALSE)

#2008 - UF
df.SIM.hepC.filtrado.2008$UF <- substr(df.SIM.hepC.filtrado.2008$CODMUNOCOR , 1, 2)
df.SIM.2008_UF_br <- select(df.SIM.hepC.filtrado.2008 , UF)
df.SIM.2008_UF <- table(df.SIM.2008_UF_br)
df.SIM.2008_UF <- as.data.frame(df.SIM.2008_UF)
# Acrescentar informações IBGE 7d
df.SIM.2008_UF  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2008_UF, by.x = "UF", by.y = "df.SIM.2008_UF_br" , all.y = TRUE)
df.SIM.2008_UF <- distinct(df.SIM.2008_UF, UF, .keep_all = TRUE)
write.csv(df.SIM.2008_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2008_UF.csv', row.names=FALSE)

#2009 - MUN
df.SIM.2009_mun_br <- select(df.SIM.hepC.filtrado.2009, CODMUNOCOR)
df.SIM.2009_mun <- table(df.SIM.2009_mun_br)
df.SIM.2009_mun <- as.data.frame(df.SIM.2009_mun)
# Acrescentar informações IBGE 7d
df.SIM.2009_mun  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2009_mun, by.x = "cod_mun_completo", by.y = "df.SIM.2009_mun_br" , all.y = TRUE)
write.csv(df.SIM.2009_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2009_mun.csv', row.names=FALSE)

#2009 - UF
df.SIM.hepC.filtrado.2009$UF <- substr(df.SIM.hepC.filtrado.2009$CODMUNOCOR , 1, 2)
df.SIM.2009_UF_br <- select(df.SIM.hepC.filtrado.2009, UF)
df.SIM.2009_UF <- table(df.SIM.2009_UF_br)
df.SIM.2009_UF <- as.data.frame(df.SIM.2009_UF)
# Acrescentar informações IBGE 7d
df.SIM.2009_UF  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2009_UF, by.x = "UF", by.y = "df.SIM.2009_UF_br" , all.y = TRUE)
df.SIM.2009_UF <- distinct(df.SIM.2009_UF, UF, .keep_all = TRUE)
write.csv(df.SIM.2009_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2009_UF.csv', row.names=FALSE)

#2010 - MUN
df.SIM.2010_mun_br <- select(df.SIM.hepC.filtrado.2010, CODMUNOCOR)
df.SIM.2010_mun <- table(df.SIM.2010_mun_br)
df.SIM.2010_mun <- as.data.frame(df.SIM.2010_mun)
# Acrescentar informações IBGE 7d
df.SIM.2010_mun  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2010_mun, by.x = "cod_mun_completo", by.y = "df.SIM.2010_mun_br" , all.y = TRUE)
write.csv(df.SIM.2010_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2010_mun.csv', row.names=FALSE)

#2010 - UF
df.SIM.hepC.filtrado.2010$UF <- substr(df.SIM.hepC.filtrado.2010$CODMUNOCOR , 1, 2)
df.SIM.2010_UF_br <- select(df.SIM.hepC.filtrado.2010 , UF)
df.SIM.2010_UF <- table(df.SIM.2010_UF_br)
df.SIM.2010_UF <- as.data.frame(df.SIM.2010_UF)
# Acrescentar informações IBGE 7d
df.SIM.2010_UF  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2010_UF, by.x = "UF", by.y = "df.SIM.2010_UF_br" , all.y = TRUE)
df.SIM.2010_UF <- distinct(df.SIM.2010_UF, UF, .keep_all = TRUE)
write.csv(df.SIM.2010_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2010_UF.csv', row.names=FALSE)

#2011 - MUN
df.SIM.2011_mun_br <- select(df.SIM.hepC.filtrado.2011, CODMUNOCOR)
df.SIM.2011_mun <- table(df.SIM.2011_mun_br)
df.SIM.2011_mun <- as.data.frame(df.SIM.2011_mun)
# Acrescentar informações IBGE 7d
df.SIM.2011_mun  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2011_mun, by.x = "cod_mun_completo", by.y = "df.SIM.2011_mun_br" , all.y = TRUE)
write.csv(df.SIM.2011_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2011_mun.csv', row.names=FALSE)

#2011 - UF
df.SIM.hepC.filtrado.2011$UF <- substr(df.SIM.hepC.filtrado.2011$CODMUNOCOR , 1, 2)
df.SIM.2011_UF_br <- select(df.SIM.hepC.filtrado.2011, UF)
df.SIM.2011_UF <- table(df.SIM.2011_UF_br)
df.SIM.2011_UF <- as.data.frame(df.SIM.2011_UF)
# Acrescentar informações IBGE 7d
df.SIM.2011_UF  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2011_UF, by.x = "UF", by.y = "df.SIM.2011_UF_br" , all.y = TRUE)
df.SIM.2011_UF <- distinct(df.SIM.2011_UF, UF, .keep_all = TRUE)
write.csv(df.SIM.2011_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2011_UF.csv', row.names=FALSE)

#2012 - MUN
df.SIM.2012_mun_br <- select(df.SIM.hepC.filtrado.2012, CODMUNOCOR)
df.SIM.2012_mun <- table(df.SIM.2012_mun_br)
df.SIM.2012_mun <- as.data.frame(df.SIM.2012_mun)
# Acrescentar informações IBGE 7d
df.SIM.2012_mun  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2012_mun, by.x = "cod_mun_completo", by.y = "df.SIM.2012_mun_br" , all.y = TRUE)
write.csv(df.SIM.2012_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2012_mun.csv', row.names=FALSE)

#2012 - UF
df.SIM.hepC.filtrado.2012$UF <- substr(df.SIM.hepC.filtrado.2012$CODMUNOCOR , 1, 2)
df.SIM.2012_UF_br <- select(df.SIM.hepC.filtrado.2012, UF)
df.SIM.2012_UF <- table(df.SIM.2012_UF_br)
df.SIM.2012_UF <- as.data.frame(df.SIM.2012_UF)
# Acrescentar informações IBGE 7d
df.SIM.2012_UF  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2012_UF, by.x = "UF", by.y = "df.SIM.2012_UF_br" , all.y = TRUE)
df.SIM.2012_UF <- distinct(df.SIM.2012_UF, UF, .keep_all = TRUE)
write.csv(df.SIM.2012_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2012_UF.csv', row.names=FALSE)

#2013 - MUN
df.SIM.2013_mun_br <- select(df.SIM.hepC.filtrado.2013, CODMUNOCOR)
df.SIM.2013_mun <- table(df.SIM.2013_mun_br)
df.SIM.2013_mun <- as.data.frame(df.SIM.2013_mun)
# Acrescentar informações IBGE 7d
df.SIM.2013_mun  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2013_mun, by.x = "cod_mun_completo", by.y = "df.SIM.2013_mun_br" , all.y = TRUE)
write.csv(df.SIM.2013_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2013_mun.csv', row.names=FALSE)

#2013 - UF
df.SIM.hepC.filtrado.2013$UF <- substr(df.SIM.hepC.filtrado.2013$CODMUNOCOR , 1, 2)
df.SIM.2013_UF_br <- select(df.SIM.hepC.filtrado.2013, UF)
df.SIM.2013_UF <- table(df.SIM.2013_UF_br)
df.SIM.2013_UF <- as.data.frame(df.SIM.2013_UF)
# Acrescentar informações IBGE 7d
df.SIM.2013_UF  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2013_UF, by.x = "UF", by.y = "df.SIM.2013_UF_br" , all.y = TRUE)
df.SIM.2013_UF <- distinct(df.SIM.2013_UF, UF, .keep_all = TRUE)
write.csv(df.SIM.2013_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2013_UF.csv', row.names=FALSE)

#2014 - MUN
df.SIM.2014_mun_br <- select(df.SIM.hepC.filtrado.2014, CODMUNOCOR)
df.SIM.2014_mun <- table(df.SIM.2014_mun_br)
df.SIM.2014_mun <- as.data.frame(df.SIM.2014_mun)
# Acrescentar informações IBGE 7d
df.SIM.2014_mun  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2014_mun, by.x = "cod_mun_completo", by.y = "df.SIM.2014_mun_br" , all.y = TRUE)
write.csv(df.SIM.2014_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2014_mun.csv', row.names=FALSE)

#2014 - UF
df.SIM.hepC.filtrado.2014$UF <- substr(df.SIM.hepC.filtrado.2014$CODMUNOCOR , 1, 2)
df.SIM.2014_UF_br <- select(df.SIM.hepC.filtrado.2014, UF)
df.SIM.2014_UF <- table(df.SIM.2014_UF_br)
df.SIM.2014_UF <- as.data.frame(df.SIM.2014_UF)
# Acrescentar informações IBGE 7d
df.SIM.2014_UF  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2014_UF, by.x = "UF", by.y = "df.SIM.2014_UF_br" , all.y = TRUE)
df.SIM.2014_UF <- distinct(df.SIM.2014_UF, UF, .keep_all = TRUE)
write.csv(df.SIM.2014_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2014_UF.csv', row.names=FALSE)

#2015 - MUN
df.SIM.2015_mun_br <- select(df.SIM.hepC.filtrado.2015, CODMUNOCOR)
df.SIM.2015_mun <- table(df.SIM.2015_mun_br)
df.SIM.2015_mun <- as.data.frame(df.SIM.2015_mun)
# Acrescentar informações IBGE 7d
df.SIM.2015_mun  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2015_mun, by.x = "cod_mun_completo", by.y = "df.SIM.2015_mun_br" , all.y = TRUE)
write.csv(df.SIM.2015_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2015_mun.csv', row.names=FALSE)

#2015 - UF
df.SIM.hepC.filtrado.2015$UF <- substr(df.SIM.hepC.filtrado.2015$CODMUNOCOR , 1, 2)
df.SIM.2015_UF_br <- select(df.SIM.hepC.filtrado.2015, UF)
df.SIM.2015_UF <- table(df.SIM.2015_UF_br)
df.SIM.2015_UF <- as.data.frame(df.SIM.2015_UF)
# Acrescentar informações IBGE 7d
df.SIM.2015_UF  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2015_UF, by.x = "UF", by.y = "df.SIM.2015_UF_br" , all.y = TRUE)
df.SIM.2015_UF <- distinct(df.SIM.2015_UF, UF, .keep_all = TRUE)
write.csv(df.SIM.2015_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2015_UF.csv', row.names=FALSE)

#2016 - MUN
df.SIM.2016_mun_br <- select(df.SIM.hepC.filtrado.2016, CODMUNOCOR)
df.SIM.2016_mun <- table(df.SIM.2016_mun_br)
df.SIM.2016_mun <- as.data.frame(df.SIM.2016_mun)
# Acrescentar informações IBGE 7d
df.SIM.2016_mun  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2016_mun, by.x = "cod_mun_completo", by.y = "df.SIM.2016_mun_br" , all.y = TRUE)
write.csv(df.SIM.2016_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2016_mun.csv', row.names=FALSE)

#2016 - UF
df.SIM.hepC.filtrado.2016$UF <- substr(df.SIM.hepC.filtrado.2016$CODMUNOCOR , 1, 2)
df.SIM.2016_UF_br <- select(df.SIM.hepC.filtrado.2016, UF)
df.SIM.2016_UF <- table(df.SIM.2016_UF_br)
df.SIM.2016_UF <- as.data.frame(df.SIM.2016_UF)
# Acrescentar informações IBGE 7d
df.SIM.2016_UF  <- merge(cod_mun_IBGE_6d_7d,df.SIM.2016_UF, by.x = "UF", by.y = "df.SIM.2016_UF_br" , all.y = TRUE)
df.SIM.2016_UF <- distinct(df.SIM.2016_UF, UF, .keep_all = TRUE)
write.csv(df.SIM.2016_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.SIM.2016_UF.csv', row.names=FALSE)

##########
## AIH ##
#########

#2008 - MUN
df.AIH.2008_mun_br <- select(df.AIH.filtrado.hepC.2008, NU_MUN_HOSP)
df.AIH.2008_mun <- table(df.AIH.2008_mun_br)
df.AIH.2008_mun <- as.data.frame(df.AIH.2008_mun)
# Acrescentar informações IBGE 7d
df.AIH.2008_mun  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2008_mun, by.x = "cod_mun_completo", by.y = "df.AIH.2008_mun_br" , all.y = TRUE)
write.csv(df.AIH.2008_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2008_mun.csv', row.names=FALSE)

#2008 - UF
df.AIH.2008_UF_br <- select(df.AIH.filtrado.hepC.2008, SG_UF)

df.AIH.2008_UF_br$SG_UF <- as.character(df.AIH.2008_UF_br$SG_UF)

df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "RO"] <- "11"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "AC"] <- "12"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "AM"] <- "13"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "RR"] <- "14"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "PA"] <- "15"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "AP"] <- "16"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "TO"] <- "17"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "MA"] <- "21"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "PI"] <- "22"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "CE"] <- "23"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "RN"] <- "24"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "PB"] <- "25"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "PE"] <- "26"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "AL"] <- "27"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "SE"] <- "28"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "BA"] <- "29"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "MG"] <- "31"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "ES"] <- "32"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "RJ"] <- "33"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "SP"] <- "35"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "PR"] <- "41"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "SC"] <- "42"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "RS"] <- "43"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "MS"] <- "50"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "MT"] <- "51"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "GO"] <- "52"
df.AIH.2008_UF_br$SG_UF[df.AIH.2008_UF_br$SG_UF == "DF"] <- "53"

df.AIH.2008_UF <- table(df.AIH.2008_UF_br)
df.AIH.2008_UF <- as.data.frame(df.AIH.2008_UF)

# Acrescentar informações IBGE 7d
df.AIH.2008_UF  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2008_UF, by.x = "UF", by.y = "df.AIH.2008_UF_br" , all.y = TRUE)
df.AIH.2008_UF <- distinct(df.AIH.2008_UF, UF, .keep_all = TRUE)
write.csv(df.AIH.2008_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2008_UF.csv', row.names=FALSE)

#2009 - MUN
df.AIH.2009_mun_br <- select(df.AIH.filtrado.hepC.2009, NU_MUN_HOSP)
df.AIH.2009_mun <- table(df.AIH.2009_mun_br)
df.AIH.2009_mun <- as.data.frame(df.AIH.2009_mun)
# Acrescentar informações IBGE 7d
df.AIH.2009_mun  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2009_mun, by.x = "cod_mun_completo", by.y = "df.AIH.2009_mun_br" , all.y = TRUE)
write.csv(df.AIH.2009_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2009_mun.csv', row.names=FALSE)

#2009 - UF
df.AIH.2009_UF_br <- select(df.AIH.filtrado.hepC.2009, SG_UF)

df.AIH.2009_UF_br$SG_UF <- as.character(df.AIH.2009_UF_br$SG_UF)

df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "RO"] <- "11"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "AC"] <- "12"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "AM"] <- "13"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "RR"] <- "14"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "PA"] <- "15"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "AP"] <- "16"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "TO"] <- "17"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "MA"] <- "21"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "PI"] <- "22"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "CE"] <- "23"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "RN"] <- "24"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "PB"] <- "25"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "PE"] <- "26"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "AL"] <- "27"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "SE"] <- "28"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "BA"] <- "29"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "MG"] <- "31"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "ES"] <- "32"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "RJ"] <- "33"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "SP"] <- "35"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "PR"] <- "41"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "SC"] <- "42"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "RS"] <- "43"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "MS"] <- "50"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "MT"] <- "51"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "GO"] <- "52"
df.AIH.2009_UF_br$SG_UF[df.AIH.2009_UF_br$SG_UF == "DF"] <- "53"

df.AIH.2009_UF <- table(df.AIH.2009_UF_br)
df.AIH.2009_UF <- as.data.frame(df.AIH.2009_UF)

# Acrescentar informações IBGE 7d
df.AIH.2009_UF  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2009_UF, by.x = "UF", by.y = "df.AIH.2009_UF_br" , all.y = TRUE)
df.AIH.2009_UF <- distinct(df.AIH.2009_UF, UF, .keep_all = TRUE)
write.csv(df.AIH.2009_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2009_UF.csv', row.names=FALSE)

#2010 - MUN
df.AIH.2010_mun_br <- select(df.AIH.filtrado.hepC.2010, NU_MUN_HOSP)
df.AIH.2010_mun <- table(df.AIH.2010_mun_br)
df.AIH.2010_mun <- as.data.frame(df.AIH.2010_mun)
# Acrescentar informações IBGE 7d
df.AIH.2010_mun  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2010_mun, by.x = "cod_mun_completo", by.y = "df.AIH.2010_mun_br" , all.y = TRUE)
write.csv(df.AIH.2010_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2010_mun.csv', row.names=FALSE)

#2010 - UF
df.AIH.2010_UF_br <- select(df.AIH.filtrado.hepC.2010, SG_UF)

df.AIH.2010_UF_br$SG_UF <- as.character(df.AIH.2010_UF_br$SG_UF)

df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "RO"] <- "11"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "AC"] <- "12"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "AM"] <- "13"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "RR"] <- "14"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "PA"] <- "15"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "AP"] <- "16"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "TO"] <- "17"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "MA"] <- "21"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "PI"] <- "22"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "CE"] <- "23"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "RN"] <- "24"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "PB"] <- "25"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "PE"] <- "26"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "AL"] <- "27"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "SE"] <- "28"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "BA"] <- "29"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "MG"] <- "31"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "ES"] <- "32"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "RJ"] <- "33"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "SP"] <- "35"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "PR"] <- "41"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "SC"] <- "42"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "RS"] <- "43"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "MS"] <- "50"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "MT"] <- "51"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "GO"] <- "52"
df.AIH.2010_UF_br$SG_UF[df.AIH.2010_UF_br$SG_UF == "DF"] <- "53"

df.AIH.2010_UF <- table(df.AIH.2010_UF_br)
df.AIH.2010_UF <- as.data.frame(df.AIH.2010_UF)

# Acrescentar informações IBGE 7d
df.AIH.2010_UF  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2010_UF, by.x = "UF", by.y = "df.AIH.2010_UF_br" , all.y = TRUE)
df.AIH.2010_UF <- distinct(df.AIH.2010_UF, UF, .keep_all = TRUE)
write.csv(df.AIH.2010_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2010_UF.csv', row.names=FALSE)

#2011 - MUN
df.AIH.2011_mun_br <- select(df.AIH.filtrado.hepC.2011, NU_MUN_HOSP)
df.AIH.2011_mun <- table(df.AIH.2011_mun_br)
df.AIH.2011_mun <- as.data.frame(df.AIH.2011_mun)
# Acrescentar informações IBGE 7d
df.AIH.2011_mun  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2011_mun, by.x = "cod_mun_completo", by.y = "df.AIH.2011_mun_br" , all.y = TRUE)
write.csv(df.AIH.2011_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2011_mun.csv', row.names=FALSE)

#2011 - UF
df.AIH.2011_UF_br <- select(df.AIH.filtrado.hepC.2011, SG_UF)

df.AIH.2011_UF_br$SG_UF <- as.character(df.AIH.2011_UF_br$SG_UF)

df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "RO"] <- "11"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "AC"] <- "12"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "AM"] <- "13"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "RR"] <- "14"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "PA"] <- "15"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "AP"] <- "16"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "TO"] <- "17"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "MA"] <- "21"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "PI"] <- "22"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "CE"] <- "23"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "RN"] <- "24"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "PB"] <- "25"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "PE"] <- "26"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "AL"] <- "27"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "SE"] <- "28"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "BA"] <- "29"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "MG"] <- "31"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "ES"] <- "32"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "RJ"] <- "33"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "SP"] <- "35"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "PR"] <- "41"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "SC"] <- "42"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "RS"] <- "43"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "MS"] <- "50"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "MT"] <- "51"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "GO"] <- "52"
df.AIH.2011_UF_br$SG_UF[df.AIH.2011_UF_br$SG_UF == "DF"] <- "53"

df.AIH.2011_UF <- table(df.AIH.2011_UF_br)
df.AIH.2011_UF <- as.data.frame(df.AIH.2011_UF)

# Acrescentar informações IBGE 7d
df.AIH.2011_UF  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2011_UF, by.x = "UF", by.y = "df.AIH.2011_UF_br" , all.y = TRUE)
df.AIH.2011_UF <- distinct(df.AIH.2011_UF, UF, .keep_all = TRUE)
write.csv(df.AIH.2011_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2011_UF.csv', row.names=FALSE)

#2012 - MUN
df.AIH.2012_mun_br <- select(df.AIH.filtrado.hepC.2012, NU_MUN_HOSP)
df.AIH.2012_mun <- table(df.AIH.2012_mun_br)
df.AIH.2012_mun <- as.data.frame(df.AIH.2012_mun)
# Acrescentar informações IBGE 7d
df.AIH.2012_mun  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2012_mun, by.x = "cod_mun_completo", by.y = "df.AIH.2012_mun_br" , all.y = TRUE)
write.csv(df.AIH.2012_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2012_mun.csv', row.names=FALSE)

#2012 - UF
df.AIH.2012_UF_br <- select(df.AIH.filtrado.hepC.2012, SG_UF)

df.AIH.2012_UF_br$SG_UF <- as.character(df.AIH.2012_UF_br$SG_UF)

df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "RO"] <- "11"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "AC"] <- "12"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "AM"] <- "13"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "RR"] <- "14"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "PA"] <- "15"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "AP"] <- "16"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "TO"] <- "17"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "MA"] <- "21"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "PI"] <- "22"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "CE"] <- "23"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "RN"] <- "24"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "PB"] <- "25"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "PE"] <- "26"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "AL"] <- "27"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "SE"] <- "28"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "BA"] <- "29"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "MG"] <- "31"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "ES"] <- "32"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "RJ"] <- "33"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "SP"] <- "35"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "PR"] <- "41"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "SC"] <- "42"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "RS"] <- "43"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "MS"] <- "50"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "MT"] <- "51"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "GO"] <- "52"
df.AIH.2012_UF_br$SG_UF[df.AIH.2012_UF_br$SG_UF == "DF"] <- "53"

df.AIH.2012_UF <- table(df.AIH.2012_UF_br)
df.AIH.2012_UF <- as.data.frame(df.AIH.2012_UF)

# Acrescentar informações IBGE 7d
df.AIH.2012_UF  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2012_UF, by.x = "UF", by.y = "df.AIH.2012_UF_br" , all.y = TRUE)
df.AIH.2012_UF <- distinct(df.AIH.2012_UF, UF, .keep_all = TRUE)
write.csv(df.AIH.2012_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2012_UF.csv', row.names=FALSE)

#2013 - MUN
df.AIH.2013_mun_br <- select(df.AIH.filtrado.hepC.2013, NU_MUN_HOSP)
df.AIH.2013_mun <- table(df.AIH.2013_mun_br)
df.AIH.2013_mun <- as.data.frame(df.AIH.2013_mun)
# Acrescentar informações IBGE 7d
df.AIH.2013_mun  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2013_mun, by.x = "cod_mun_completo", by.y = "df.AIH.2013_mun_br" , all.y = TRUE)
write.csv(df.AIH.2013_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2013_mun.csv', row.names=FALSE)

#2013 - UF
df.AIH.2013_UF_br <- select(df.AIH.filtrado.hepC.2013, SG_UF)

df.AIH.2013_UF_br$SG_UF <- as.character(df.AIH.2013_UF_br$SG_UF)

df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "RO"] <- "11"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "AC"] <- "12"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "AM"] <- "13"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "RR"] <- "14"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "PA"] <- "15"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "AP"] <- "16"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "TO"] <- "17"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "MA"] <- "21"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "PI"] <- "22"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "CE"] <- "23"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "RN"] <- "24"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "PB"] <- "25"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "PE"] <- "26"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "AL"] <- "27"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "SE"] <- "28"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "BA"] <- "29"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "MG"] <- "31"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "ES"] <- "32"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "RJ"] <- "33"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "SP"] <- "35"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "PR"] <- "41"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "SC"] <- "42"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "RS"] <- "43"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "MS"] <- "50"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "MT"] <- "51"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "GO"] <- "52"
df.AIH.2013_UF_br$SG_UF[df.AIH.2013_UF_br$SG_UF == "DF"] <- "53"

df.AIH.2013_UF <- table(df.AIH.2013_UF_br)
df.AIH.2013_UF <- as.data.frame(df.AIH.2013_UF)

# Acrescentar informações IBGE 7d
df.AIH.2013_UF  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2013_UF, by.x = "UF", by.y = "df.AIH.2013_UF_br" , all.y = TRUE)
df.AIH.2013_UF <- distinct(df.AIH.2013_UF, UF, .keep_all = TRUE)
write.csv(df.AIH.2013_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2013_UF.csv', row.names=FALSE)

#2014 - MUN
df.AIH.2014_mun_br <- select(df.AIH.filtrado.hepC.2014, NU_MUN_HOSP)
df.AIH.2014_mun <- table(df.AIH.2014_mun_br)
df.AIH.2014_mun <- as.data.frame(df.AIH.2014_mun)
# Acrescentar informações IBGE 7d
df.AIH.2014_mun  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2014_mun, by.x = "cod_mun_completo", by.y = "df.AIH.2014_mun_br" , all.y = TRUE)
write.csv(df.AIH.2014_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2014_mun.csv', row.names=FALSE)

#2014 - UF
df.AIH.2014_UF_br <- select(df.AIH.filtrado.hepC.2014, SG_UF)

df.AIH.2014_UF_br$SG_UF <- as.character(df.AIH.2014_UF_br$SG_UF)

df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "RO"] <- "11"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "AC"] <- "12"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "AM"] <- "13"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "RR"] <- "14"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "PA"] <- "15"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "AP"] <- "16"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "TO"] <- "17"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "MA"] <- "21"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "PI"] <- "22"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "CE"] <- "23"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "RN"] <- "24"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "PB"] <- "25"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "PE"] <- "26"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "AL"] <- "27"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "SE"] <- "28"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "BA"] <- "29"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "MG"] <- "31"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "ES"] <- "32"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "RJ"] <- "33"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "SP"] <- "35"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "PR"] <- "41"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "SC"] <- "42"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "RS"] <- "43"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "MS"] <- "50"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "MT"] <- "51"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "GO"] <- "52"
df.AIH.2014_UF_br$SG_UF[df.AIH.2014_UF_br$SG_UF == "DF"] <- "53"

df.AIH.2014_UF <- table(df.AIH.2014_UF_br)
df.AIH.2014_UF <- as.data.frame(df.AIH.2014_UF)

# Acrescentar informações IBGE 7d
df.AIH.2014_UF  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2014_UF, by.x = "UF", by.y = "df.AIH.2014_UF_br" , all.y = TRUE)
df.AIH.2014_UF <- distinct(df.AIH.2014_UF, UF, .keep_all = TRUE)
write.csv(df.AIH.2014_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2014_UF.csv', row.names=FALSE)

#2015 - MUN
df.AIH.2015_mun_br <- select(df.AIH.filtrado.hepC.2015, NU_MUN_HOSP)
df.AIH.2015_mun <- table(df.AIH.2015_mun_br)
df.AIH.2015_mun <- as.data.frame(df.AIH.2015_mun)
# Acrescentar informações IBGE 7d
df.AIH.2015_mun  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2015_mun, by.x = "cod_mun_completo", by.y = "df.AIH.2015_mun_br" , all.y = TRUE)
write.csv(df.AIH.2015_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2015_mun.csv', row.names=FALSE)

#2015 - UF
df.AIH.2015_UF_br <- select(df.AIH.filtrado.hepC.2015, SG_UF)

df.AIH.2015_UF_br$SG_UF <- as.character(df.AIH.2015_UF_br$SG_UF)

df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "RO"] <- "11"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "AC"] <- "12"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "AM"] <- "13"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "RR"] <- "14"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "PA"] <- "15"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "AP"] <- "16"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "TO"] <- "17"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "MA"] <- "21"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "PI"] <- "22"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "CE"] <- "23"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "RN"] <- "24"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "PB"] <- "25"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "PE"] <- "26"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "AL"] <- "27"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "SE"] <- "28"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "BA"] <- "29"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "MG"] <- "31"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "ES"] <- "32"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "RJ"] <- "33"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "SP"] <- "35"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "PR"] <- "41"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "SC"] <- "42"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "RS"] <- "43"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "MS"] <- "50"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "MT"] <- "51"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "GO"] <- "52"
df.AIH.2015_UF_br$SG_UF[df.AIH.2015_UF_br$SG_UF == "DF"] <- "53"

df.AIH.2015_UF <- table(df.AIH.2015_UF_br)
df.AIH.2015_UF <- as.data.frame(df.AIH.2015_UF)

# Acrescentar informações IBGE 7d
df.AIH.2015_UF  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2015_UF, by.x = "UF", by.y = "df.AIH.2015_UF_br" , all.y = TRUE)
df.AIH.2015_UF <- distinct(df.AIH.2015_UF, UF, .keep_all = TRUE)
write.csv(df.AIH.2015_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2015_UF.csv', row.names=FALSE)

#2016 - MUN
df.AIH.2016_mun_br <- select(df.AIH.filtrado.hepC.2016, NU_MUN_HOSP)
df.AIH.2016_mun <- table(df.AIH.2016_mun_br)
df.AIH.2016_mun <- as.data.frame(df.AIH.2016_mun)
# Acrescentar informações IBGE 7d
df.AIH.2016_mun  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2016_mun, by.x = "cod_mun_completo", by.y = "df.AIH.2016_mun_br" , all.y = TRUE)
write.csv(df.AIH.2016_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2016_mun.csv', row.names=FALSE)

#2016 - UF
df.AIH.2016_UF_br <- select(df.AIH.filtrado.hepC.2016, SG_UF)

df.AIH.2016_UF_br$SG_UF <- as.character(df.AIH.2016_UF_br$SG_UF)

df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "RO"] <- "11"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "AC"] <- "12"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "AM"] <- "13"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "RR"] <- "14"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "PA"] <- "15"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "AP"] <- "16"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "TO"] <- "17"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "MA"] <- "21"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "PI"] <- "22"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "CE"] <- "23"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "RN"] <- "24"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "PB"] <- "25"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "PE"] <- "26"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "AL"] <- "27"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "SE"] <- "28"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "BA"] <- "29"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "MG"] <- "31"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "ES"] <- "32"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "RJ"] <- "33"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "SP"] <- "35"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "PR"] <- "41"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "SC"] <- "42"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "RS"] <- "43"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "MS"] <- "50"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "MT"] <- "51"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "GO"] <- "52"
df.AIH.2016_UF_br$SG_UF[df.AIH.2016_UF_br$SG_UF == "DF"] <- "53"

df.AIH.2016_UF <- table(df.AIH.2016_UF_br)
df.AIH.2016_UF <- as.data.frame(df.AIH.2016_UF)

# Acrescentar informações IBGE 7d
df.AIH.2016_UF  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2016_UF, by.x = "UF", by.y = "df.AIH.2016_UF_br" , all.y = TRUE)
df.AIH.2016_UF <- distinct(df.AIH.2016_UF, UF, .keep_all = TRUE)
write.csv(df.AIH.2016_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2016_UF.csv', row.names=FALSE)

#2017 - MUN
df.AIH.2017_mun_br <- select(df.AIH.filtrado.hepC.2017, NU_MUN_HOSP)
df.AIH.2017_mun <- table(df.AIH.2017_mun_br)
df.AIH.2017_mun <- as.data.frame(df.AIH.2017_mun)
# Acrescentar informações IBGE 7d
df.AIH.2017_mun  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2017_mun, by.x = "cod_mun_completo", by.y = "df.AIH.2017_mun_br" , all.y = TRUE)
write.csv(df.AIH.2017_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2017_mun.csv', row.names=FALSE)

#2017 - UF
df.AIH.2017_UF_br <- select(df.AIH.filtrado.hepC.2017, SG_UF)

df.AIH.2017_UF_br$SG_UF <- as.character(df.AIH.2017_UF_br$SG_UF)

df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "RO"] <- "11"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "AC"] <- "12"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "AM"] <- "13"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "RR"] <- "14"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "PA"] <- "15"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "AP"] <- "16"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "TO"] <- "17"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "MA"] <- "21"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "PI"] <- "22"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "CE"] <- "23"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "RN"] <- "24"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "PB"] <- "25"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "PE"] <- "26"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "AL"] <- "27"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "SE"] <- "28"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "BA"] <- "29"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "MG"] <- "31"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "ES"] <- "32"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "RJ"] <- "33"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "SP"] <- "35"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "PR"] <- "41"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "SC"] <- "42"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "RS"] <- "43"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "MS"] <- "50"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "MT"] <- "51"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "GO"] <- "52"
df.AIH.2017_UF_br$SG_UF[df.AIH.2017_UF_br$SG_UF == "DF"] <- "53"

df.AIH.2017_UF <- table(df.AIH.2017_UF_br)
df.AIH.2017_UF <- as.data.frame(df.AIH.2017_UF)

# Acrescentar informações IBGE 7d
df.AIH.2017_UF  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2017_UF, by.x = "UF", by.y = "df.AIH.2017_UF_br" , all.y = TRUE)
df.AIH.2017_UF <- distinct(df.AIH.2017_UF, UF, .keep_all = TRUE)
write.csv(df.AIH.2017_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2017_UF.csv', row.names=FALSE)

#2018 - MUN
df.AIH.2018_mun_br <- select(df.AIH.filtrado.hepC.2018, NU_MUN_HOSP)
df.AIH.2018_mun <- table(df.AIH.2018_mun_br)
df.AIH.2018_mun <- as.data.frame(df.AIH.2018_mun)
# Acrescentar informações IBGE 7d
df.AIH.2018_mun  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2018_mun, by.x = "cod_mun_completo", by.y = "df.AIH.2018_mun_br" , all.y = TRUE)
write.csv(df.AIH.2018_mun, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2018_mun.csv', row.names=FALSE)

#2018 - UF
df.AIH.2018_UF_br <- select(df.AIH.filtrado.hepC.2018, SG_UF)

df.AIH.2018_UF_br$SG_UF <- as.character(df.AIH.2018_UF_br$SG_UF)

df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "RO"] <- "11"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "AC"] <- "12"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "AM"] <- "13"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "RR"] <- "14"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "PA"] <- "15"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "AP"] <- "16"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "TO"] <- "17"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "MA"] <- "21"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "PI"] <- "22"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "CE"] <- "23"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "RN"] <- "24"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "PB"] <- "25"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "PE"] <- "26"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "AL"] <- "27"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "SE"] <- "28"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "BA"] <- "29"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "MG"] <- "31"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "ES"] <- "32"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "RJ"] <- "33"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "SP"] <- "35"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "PR"] <- "41"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "SC"] <- "42"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "RS"] <- "43"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "MS"] <- "50"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "MT"] <- "51"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "GO"] <- "52"
df.AIH.2018_UF_br$SG_UF[df.AIH.2018_UF_br$SG_UF == "DF"] <- "53"

df.AIH.2018_UF <- table(df.AIH.2018_UF_br)
df.AIH.2018_UF <- as.data.frame(df.AIH.2018_UF)

# Acrescentar informações IBGE 7d
df.AIH.2018_UF  <- merge(cod_mun_IBGE_6d_7d,df.AIH.2018_UF, by.x = "UF", by.y = "df.AIH.2018_UF_br" , all.y = TRUE)
df.AIH.2018_UF <- distinct(df.AIH.2018_UF, UF, .keep_all = TRUE)
write.csv(df.AIH.2018_UF, file = '/Users/mikaellemos/Produto2/tabelas_intermediarias/df.AIH.2018_UF.csv', row.names=FALSE)


#######################
# Geração das tabelas #
#######################

########
# APAC #
########

## UF ##

# 2008

UF_APAC_2008 <- select(df.APAC.2008_UF, UF = Nome_UF, Freq)

UF_APAC_2008$Porcentagem <- paste(round(UF_APAC_2008$Freq / sum(UF_APAC_2008$Freq) * 100,digits=2),"%",sep="")

UF_APAC_2008 <- arrange(UF_APAC_2008, desc(Freq))

UF_APAC_2008<- na.omit(UF_APAC_2008)

write.csv(UF_APAC_2008, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_APAC_2008.csv', row.names=FALSE)

# 2009

UF_APAC_2009 <- select(df.APAC.2009_UF, UF = Nome_UF, Freq)

UF_APAC_2009$Porcentagem <- paste(round(UF_APAC_2009$Freq / sum(UF_APAC_2009 $Freq) * 100,digits=2),"%",sep="")

UF_APAC_2009 <- arrange(UF_APAC_2009, desc(Freq))

UF_APAC_2009<- na.omit(UF_APAC_2009)

write.csv(UF_APAC_2009, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_APAC_2009.csv', row.names=FALSE)

# 2010

UF_APAC_2010 <- select(df.APAC.2010_UF, UF = Nome_UF, Freq)

UF_APAC_2010$Porcentagem <- paste(round(UF_APAC_2010$Freq / sum(UF_APAC_2010 $Freq) * 100,digits=2),"%",sep="")

UF_APAC_2010 <- arrange(UF_APAC_2010, desc(Freq))

UF_APAC_2010<- na.omit(UF_APAC_2010)

write.csv(UF_APAC_2010, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_APAC_2010.csv', row.names=FALSE)

# 2011

UF_APAC_2011 <- select(df.APAC.2011_UF, UF = Nome_UF, Freq)

UF_APAC_2011$Porcentagem <- paste(round(UF_APAC_2011$Freq / sum(UF_APAC_2011 $Freq) * 100,digits=2),"%",sep="")

UF_APAC_2011 <- arrange(UF_APAC_2011, desc(Freq))

UF_APAC_2011<- na.omit(UF_APAC_2011)

write.csv(UF_APAC_2011, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_APAC_2011.csv', row.names=FALSE)

# 2012

UF_APAC_2012 <- select(df.APAC.2012_UF, UF = Nome_UF, Freq)

UF_APAC_2012$Porcentagem <- paste(round(UF_APAC_2012$Freq / sum(UF_APAC_2012 $Freq) * 100,digits=2),"%",sep="")

UF_APAC_2012 <- arrange(UF_APAC_2012, desc(Freq))

UF_APAC_2012<- na.omit(UF_APAC_2012)

write.csv(UF_APAC_2012, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_APAC_2012.csv', row.names=FALSE)

# 2013

UF_APAC_2013 <- select(df.APAC.2013_UF, UF = Nome_UF, Freq)

UF_APAC_2013$Porcentagem <- paste(round(UF_APAC_2013$Freq / sum(UF_APAC_2013 $Freq) * 100,digits=2),"%",sep="")

UF_APAC_2013 <- arrange(UF_APAC_2013, desc(Freq))

UF_APAC_2013<- na.omit(UF_APAC_2013)

write.csv(UF_APAC_2013, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_APAC_2013.csv', row.names=FALSE)

# 2014

UF_APAC_2014 <- select(df.APAC.2014_UF, UF = Nome_UF, Freq)

UF_APAC_2014$Porcentagem <- paste(round(UF_APAC_2014$Freq / sum(UF_APAC_2014 $Freq) * 100,digits=2),"%",sep="")

UF_APAC_2014 <- arrange(UF_APAC_2014, desc(Freq))

UF_APAC_2014<- na.omit(UF_APAC_2014)

write.csv(UF_APAC_2014, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_APAC_2014.csv', row.names=FALSE)

# 2015

UF_APAC_2015 <- select(df.APAC.2015_UF, UF = Nome_UF, Freq)

UF_APAC_2015$Porcentagem <- paste(round(UF_APAC_2015$Freq / sum(UF_APAC_2015 $Freq) * 100,digits=2),"%",sep="")

UF_APAC_2015 <- arrange(UF_APAC_2015, desc(Freq))

UF_APAC_2015<- na.omit(UF_APAC_2015)

write.csv(UF_APAC_2015, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_APAC_2015.csv', row.names=FALSE)

# 2016

UF_APAC_2016 <- select(df.APAC.2016_UF, UF = Nome_UF, Freq)

UF_APAC_2016$Porcentagem <- paste(round(UF_APAC_2016$Freq / sum(UF_APAC_2016 $Freq) * 100,digits=2),"%",sep="")

UF_APAC_2016 <- arrange(UF_APAC_2016, desc(Freq))

UF_APAC_2016<- na.omit(UF_APAC_2016)

write.csv(UF_APAC_2016, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_APAC_2016.csv', row.names=FALSE)

# 2017

UF_APAC_2017 <- select(df.APAC.2017_UF, UF = Nome_UF, Freq)

UF_APAC_2017$Porcentagem <- paste(round(UF_APAC_2017$Freq / sum(UF_APAC_2017 $Freq) * 100,digits=2),"%",sep="")

UF_APAC_2017 <- arrange(UF_APAC_2017, desc(Freq))

UF_APAC_2017<- na.omit(UF_APAC_2017)

write.csv(UF_APAC_2017, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_APAC_2017.csv', row.names=FALSE)

# 2018

UF_APAC_2018 <- select(df.APAC.2018_UF, UF = Nome_UF, Freq)

UF_APAC_2018$Porcentagem <- paste(round(UF_APAC_2018$Freq / sum(UF_APAC_2018 $Freq) * 100,digits=2),"%",sep="")

UF_APAC_2018 <- arrange(UF_APAC_2018, desc(Freq))

UF_APAC_2018<- na.omit(UF_APAC_2018)

write.csv(UF_APAC_2018, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_APAC_2018.csv', row.names=FALSE)

## MUN ##

# 2008

mun_APAC_2008 <- select(df.APAC.2008_mun, Município = Nome_Município, Freq)

mun_APAC_2008$Porcentagem <- paste(round(mun_APAC_2008$Freq / sum(mun_APAC_2008 $Freq) * 100,digits=2),"%",sep="")

mun_APAC_2008 <- arrange(mun_APAC_2008, desc(Freq))
 
mun_APAC_2008<- na.omit(mun_APAC_2008)

write.csv(mun_APAC_2008, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_APAC_2008.csv', row.names=FALSE)

# 2009

mun_APAC_2009 <- select(df.APAC.2009_mun, Município = Nome_Município, Freq)

mun_APAC_2009$Porcentagem <- paste(round(mun_APAC_2009$Freq / sum(mun_APAC_2009 $Freq) * 100,digits=2),"%",sep="")

mun_APAC_2009 <- arrange(mun_APAC_2009, desc(Freq))

mun_APAC_2009<- na.omit(mun_APAC_2009)

write.csv(mun_APAC_2009, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_APAC_2009.csv', row.names=FALSE)

# 2010

mun_APAC_2010 <- select(df.APAC.2010_mun, Município = Nome_Município, Freq)

mun_APAC_2010$Porcentagem <- paste(round(mun_APAC_2010$Freq / sum(mun_APAC_2010 $Freq) * 100,digits=2),"%",sep="")

mun_APAC_2010 <- arrange(mun_APAC_2010, desc(Freq))

mun_APAC_2010<- na.omit(mun_APAC_2010)

write.csv(mun_APAC_2010, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_APAC_2010.csv', row.names=FALSE)

# 2011

mun_APAC_2011 <- select(df.APAC.2011_mun, Município = Nome_Município, Freq)

mun_APAC_2011$Porcentagem <- paste(round(mun_APAC_2011$Freq / sum(mun_APAC_2011 $Freq) * 100,digits=2),"%",sep="")

mun_APAC_2011 <- arrange(mun_APAC_2011, desc(Freq))

mun_APAC_2011<- na.omit(mun_APAC_2011)

write.csv(mun_APAC_2011, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_APAC_2011.csv', row.names=FALSE)

# 2012

mun_APAC_2012 <- select(df.APAC.2012_mun, Município = Nome_Município, Freq)

mun_APAC_2012$Porcentagem <- paste(round(mun_APAC_2012$Freq / sum(mun_APAC_2012 $Freq) * 100,digits=2),"%",sep="")

mun_APAC_2012 <- arrange(mun_APAC_2012, desc(Freq))

mun_APAC_2012<- na.omit(mun_APAC_2012)

write.csv(mun_APAC_2012, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_APAC_2012.csv', row.names=FALSE)

# 2013

mun_APAC_2013 <- select(df.APAC.2013_mun, Município = Nome_Município, Freq)

mun_APAC_2013$Porcentagem <- paste(round(mun_APAC_2013$Freq / sum(mun_APAC_2013 $Freq) * 100,digits=2),"%",sep="")

mun_APAC_2013 <- arrange(mun_APAC_2013, desc(Freq))

mun_APAC_2013<- na.omit(mun_APAC_2013)

write.csv(mun_APAC_2013, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_APAC_2013.csv', row.names=FALSE)

# 2014

mun_APAC_2014 <- select(df.APAC.2014_mun, Município = Nome_Município, Freq)

mun_APAC_2014$Porcentagem <- paste(round(mun_APAC_2014$Freq / sum(mun_APAC_2014 $Freq) * 100,digits=2),"%",sep="")

mun_APAC_2014 <- arrange(mun_APAC_2014, desc(Freq))

mun_APAC_2014<- na.omit(mun_APAC_2014)

write.csv(mun_APAC_2014, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_APAC_2014.csv', row.names=FALSE)

# 2015

mun_APAC_2015 <- select(df.APAC.2015_mun, Município = Nome_Município, Freq)

mun_APAC_2015$Porcentagem <- paste(round(mun_APAC_2015$Freq / sum(mun_APAC_2015 $Freq) * 100,digits=2),"%",sep="")

mun_APAC_2015 <- arrange(mun_APAC_2015, desc(Freq))

mun_APAC_2015<- na.omit(mun_APAC_2015)

write.csv(mun_APAC_2015, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_APAC_2015.csv', row.names=FALSE)

# 2016

mun_APAC_2016 <- select(df.APAC.2016_mun, Município = Nome_Município, Freq)

mun_APAC_2016$Porcentagem <- paste(round(mun_APAC_2016$Freq / sum(mun_APAC_2016 $Freq) * 100,digits=2),"%",sep="")

mun_APAC_2016 <- arrange(mun_APAC_2016, desc(Freq))

mun_APAC_2016<- na.omit(mun_APAC_2016)

write.csv(mun_APAC_2016, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_APAC_2016.csv', row.names=FALSE)

# 2017

mun_APAC_2017 <- select(df.APAC.2017_mun, Município = Nome_Município, Freq)

mun_APAC_2017$Porcentagem <- paste(round(mun_APAC_2017$Freq / sum(mun_APAC_2017 $Freq) * 100,digits=2),"%",sep="")

mun_APAC_2017 <- arrange(mun_APAC_2017, desc(Freq))

mun_APAC_2017<- na.omit(mun_APAC_2017)

write.csv(mun_APAC_2017, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_APAC_2017.csv', row.names=FALSE)

# 2018

mun_APAC_2018 <- select(df.APAC.2018_mun, Município = Nome_Município, Freq)

mun_APAC_2018$Porcentagem <- paste(round(mun_APAC_2018$Freq / sum(mun_APAC_2018 $Freq) * 100,digits=2),"%",sep="")

mun_APAC_2018 <- arrange(mun_APAC_2018, desc(Freq))

mun_APAC_2018<- na.omit(mun_APAC_2018)

write.csv(mun_APAC_2018, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_APAC_2018.csv', row.names=FALSE)

########
# BPAI #
########

## UF ##

# 2008

UF_BPAI_2008 <- select(df.BPAI.2008_UF, UF = Nome_UF, Freq)

UF_BPAI_2008$Porcentagem <- paste(round(UF_BPAI_2008$Freq / sum(UF_BPAI_2008 $Freq) * 100,digits=2),"%",sep="")

UF_BPAI_2008 <- arrange(UF_BPAI_2008, desc(Freq))

UF_BPAI_2008<- na.omit(UF_BPAI_2008)

write.csv(UF_BPAI_2008, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_BPAI_2008.csv', row.names=FALSE)

# 2009

UF_BPAI_2009 <- select(df.BPAI.2009_UF, UF = Nome_UF, Freq)

UF_BPAI_2009$Porcentagem <- paste(round(UF_BPAI_2009$Freq / sum(UF_BPAI_2009 $Freq) * 100,digits=2),"%",sep="")

UF_BPAI_2009 <- arrange(UF_BPAI_2009, desc(Freq))

UF_BPAI_2009<- na.omit(UF_BPAI_2009)

write.csv(UF_BPAI_2009, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_BPAI_2009.csv', row.names=FALSE)

# 2010

UF_BPAI_2010 <- select(df.BPAI.2010_UF, UF = Nome_UF, Freq)

UF_BPAI_2010$Porcentagem <- paste(round(UF_BPAI_2010$Freq / sum(UF_BPAI_2010 $Freq) * 100,digits=2),"%",sep="")

UF_BPAI_2010 <- arrange(UF_BPAI_2010, desc(Freq))

UF_BPAI_2010<- na.omit(UF_BPAI_2010)

write.csv(UF_BPAI_2010, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_BPAI_2010.csv', row.names=FALSE)

# 2011

UF_BPAI_2011 <- select(df.BPAI.2011_UF, UF = Nome_UF, Freq)

UF_BPAI_2011$Porcentagem <- paste(round(UF_BPAI_2011$Freq / sum(UF_BPAI_2011 $Freq) * 100,digits=2),"%",sep="")

UF_BPAI_2011 <- arrange(UF_BPAI_2011, desc(Freq))

UF_BPAI_2011<- na.omit(UF_BPAI_2011)

write.csv(UF_BPAI_2011, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_BPAI_2011.csv', row.names=FALSE)

# 2012

UF_BPAI_2012 <- select(df.BPAI.2012_UF, UF = Nome_UF, Freq)

UF_BPAI_2012$Porcentagem <- paste(round(UF_BPAI_2012$Freq / sum(UF_BPAI_2012 $Freq) * 100,digits=2),"%",sep="")

UF_BPAI_2012 <- arrange(UF_BPAI_2012, desc(Freq))

UF_BPAI_2012<- na.omit(UF_BPAI_2012)

write.csv(UF_BPAI_2012, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_BPAI_2012.csv', row.names=FALSE)

# 2013

UF_BPAI_2013 <- select(df.BPAI.2013_UF, UF = Nome_UF, Freq)

UF_BPAI_2013$Porcentagem <- paste(round(UF_BPAI_2013$Freq / sum(UF_BPAI_2013 $Freq) * 100,digits=2),"%",sep="")

UF_BPAI_2013 <- arrange(UF_BPAI_2013, desc(Freq))

UF_BPAI_2013<- na.omit(UF_BPAI_2013)

write.csv(UF_BPAI_2013, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_BPAI_2013.csv', row.names=FALSE)

# 2014

UF_BPAI_2014 <- select(df.BPAI.2014_UF, UF = Nome_UF, Freq)

UF_BPAI_2014$Porcentagem <- paste(round(UF_BPAI_2014$Freq / sum(UF_BPAI_2014 $Freq) * 100,digits=2),"%",sep="")

UF_BPAI_2014 <- arrange(UF_BPAI_2014, desc(Freq))

UF_BPAI_2014<- na.omit(UF_BPAI_2014)

write.csv(UF_BPAI_2014, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_BPAI_2014.csv', row.names=FALSE)

# 2015

UF_BPAI_2015 <- select(df.BPAI.2015_UF, UF = Nome_UF, Freq)

UF_BPAI_2015$Porcentagem <- paste(round(UF_BPAI_2015$Freq / sum(UF_BPAI_2015 $Freq) * 100,digits=2),"%",sep="")

UF_BPAI_2015 <- arrange(UF_BPAI_2015, desc(Freq))

UF_BPAI_2015<- na.omit(UF_BPAI_2015)

write.csv(UF_BPAI_2015, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_BPAI_2015.csv', row.names=FALSE)

# 2016

UF_BPAI_2016 <- select(df.BPAI.2016_UF, UF = Nome_UF, Freq)

UF_BPAI_2016$Porcentagem <- paste(round(UF_BPAI_2016$Freq / sum(UF_BPAI_2016 $Freq) * 100,digits=2),"%",sep="")

UF_BPAI_2016 <- arrange(UF_BPAI_2016, desc(Freq))

UF_BPAI_2016<- na.omit(UF_BPAI_2016)

write.csv(UF_BPAI_2016, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_BPAI_2016.csv', row.names=FALSE)

# 2017

UF_BPAI_2017 <- select(df.BPAI.2017_UF, UF = Nome_UF, Freq)

UF_BPAI_2017$Porcentagem <- paste(round(UF_BPAI_2017$Freq / sum(UF_BPAI_2017 $Freq) * 100,digits=2),"%",sep="")

UF_BPAI_2017 <- arrange(UF_BPAI_2017, desc(Freq))

UF_BPAI_2017<- na.omit(UF_BPAI_2017)

write.csv(UF_BPAI_2017, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_BPAI_2017.csv', row.names=FALSE)

# 2018

UF_BPAI_2018 <- select(df.BPAI.2018_UF, UF = Nome_UF, Freq)

UF_BPAI_2018$Porcentagem <- paste(round(UF_BPAI_2018$Freq / sum(UF_BPAI_2018 $Freq) * 100,digits=2),"%",sep="")

UF_BPAI_2018 <- arrange(UF_BPAI_2018, desc(Freq))

UF_BPAI_2018<- na.omit(UF_BPAI_2018)

write.csv(UF_BPAI_2018, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_BPAI_2018.csv', row.names=FALSE)

## MUN ##

# 2008

mun_BPAI_2008 <- select(df.BPAI.2008_mun, Município = Nome_Município, Freq)

mun_BPAI_2008$Porcentagem <- paste(round(mun_BPAI_2008$Freq / sum(mun_BPAI_2008 $Freq) * 100,digits=2),"%",sep="")

mun_BPAI_2008 <- arrange(mun_BPAI_2008, desc(Freq))

mun_BPAI_2008<- na.omit(mun_BPAI_2008)

write.csv(mun_BPAI_2008, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_BPAI_2008.csv', row.names=FALSE)

# 2009

mun_BPAI_2009 <- select(df.BPAI.2009_mun, Município = Nome_Município, Freq)

mun_BPAI_2009$Porcentagem <- paste(round(mun_BPAI_2009$Freq / sum(mun_BPAI_2009 $Freq) * 100,digits=2),"%",sep="")

mun_BPAI_2009 <- arrange(mun_BPAI_2009, desc(Freq))

mun_BPAI_2009<- na.omit(mun_BPAI_2009)

write.csv(mun_BPAI_2009, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_BPAI_2009.csv', row.names=FALSE)

# 2010

mun_BPAI_2010 <- select(df.BPAI.2010_mun, Município = Nome_Município, Freq)

mun_BPAI_2010$Porcentagem <- paste(round(mun_BPAI_2010$Freq / sum(mun_BPAI_2010 $Freq) * 100,digits=2),"%",sep="")

mun_BPAI_2010 <- arrange(mun_BPAI_2010, desc(Freq))

mun_BPAI_2010<- na.omit(mun_BPAI_2010)

write.csv(mun_BPAI_2010, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_BPAI_2010.csv', row.names=FALSE)

# 2011

mun_BPAI_2011 <- select(df.BPAI.2011_mun, Município = Nome_Município, Freq)

mun_BPAI_2011$Porcentagem <- paste(round(mun_BPAI_2011$Freq / sum(mun_BPAI_2011 $Freq) * 100,digits=2),"%",sep="")

mun_BPAI_2011 <- arrange(mun_BPAI_2011, desc(Freq))

mun_BPAI_2011<- na.omit(mun_BPAI_2011)

write.csv(mun_BPAI_2011, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_BPAI_2011.csv', row.names=FALSE)

# 2012

mun_BPAI_2012 <- select(df.BPAI.2012_mun, Município = Nome_Município, Freq)

mun_BPAI_2012$Porcentagem <- paste(round(mun_BPAI_2012$Freq / sum(mun_BPAI_2012 $Freq) * 100,digits=2),"%",sep="")

mun_BPAI_2012 <- arrange(mun_BPAI_2012, desc(Freq))

mun_BPAI_2012<- na.omit(mun_BPAI_2012)

write.csv(mun_BPAI_2012, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_BPAI_2012.csv', row.names=FALSE)

# 2013

mun_BPAI_2013 <- select(df.BPAI.2013_mun, Município = Nome_Município, Freq)

mun_BPAI_2013$Porcentagem <- paste(round(mun_BPAI_2013$Freq / sum(mun_BPAI_2013 $Freq) * 100,digits=2),"%",sep="")

mun_BPAI_2013 <- arrange(mun_BPAI_2013, desc(Freq))

mun_BPAI_2013<- na.omit(mun_BPAI_2013)

write.csv(mun_BPAI_2013, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_BPAI_2013.csv', row.names=FALSE)

# 2014

mun_BPAI_2014 <- select(df.BPAI.2014_mun, Município = Nome_Município, Freq)

mun_BPAI_2014$Porcentagem <- paste(round(mun_BPAI_2014$Freq / sum(mun_BPAI_2014 $Freq) * 100,digits=2),"%",sep="")

mun_BPAI_2014 <- arrange(mun_BPAI_2014, desc(Freq))

mun_BPAI_2014<- na.omit(mun_BPAI_2014)

write.csv(mun_BPAI_2014, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_BPAI_2014.csv', row.names=FALSE)

# 2015

mun_BPAI_2015 <- select(df.BPAI.2015_mun, Município = Nome_Município, Freq)

mun_BPAI_2015$Porcentagem <- paste(round(mun_BPAI_2015$Freq / sum(mun_BPAI_2015 $Freq) * 100,digits=2),"%",sep="")

mun_BPAI_2015 <- arrange(mun_BPAI_2015, desc(Freq))

mun_BPAI_2015<- na.omit(mun_BPAI_2015)

write.csv(mun_BPAI_2015, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_BPAI_2015.csv', row.names=FALSE)

# 2016

mun_BPAI_2016 <- select(df.BPAI.2016_mun, Município = Nome_Município, Freq)

mun_BPAI_2016$Porcentagem <- paste(round(mun_BPAI_2016$Freq / sum(mun_BPAI_2016 $Freq) * 100,digits=2),"%",sep="")

mun_BPAI_2016 <- arrange(mun_BPAI_2016, desc(Freq))

mun_BPAI_2016<- na.omit(mun_BPAI_2016)

write.csv(mun_BPAI_2016, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_BPAI_2016.csv', row.names=FALSE)

# 2017

mun_BPAI_2017 <- select(df.BPAI.2017_mun, Município = Nome_Município, Freq)

mun_BPAI_2017$Porcentagem <- paste(round(mun_BPAI_2017$Freq / sum(mun_BPAI_2017 $Freq) * 100,digits=2),"%",sep="")

mun_BPAI_2017 <- arrange(mun_BPAI_2017, desc(Freq))

mun_BPAI_2017<- na.omit(mun_BPAI_2017)

write.csv(mun_BPAI_2017, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_BPAI_2017.csv', row.names=FALSE)

# 2018

mun_BPAI_2018 <- select(df.BPAI.2018_mun, Município = Nome_Município, Freq)

mun_BPAI_2018$Porcentagem <- paste(round(mun_BPAI_2018$Freq / sum(mun_BPAI_2018 $Freq) * 100,digits=2),"%",sep="")

mun_BPAI_2018 <- arrange(mun_BPAI_2018, desc(Freq))

mun_BPAI_2018<- na.omit(mun_BPAI_2018)

write.csv(mun_BPAI_2018, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_BPAI_2018.csv', row.names=FALSE)

########
# AIH  #
########

## UF ##

# 2008

UF_AIH_2008 <- select(df.AIH.2008_UF, UF = Nome_UF, Freq)

UF_AIH_2008$Porcentagem <- paste(round(UF_AIH_2008$Freq / sum(UF_AIH_2008 $Freq) * 100,digits=2),"%",sep="")

UF_AIH_2008 <- arrange(UF_AIH_2008, desc(Freq))

UF_AIH_2008<- na.omit(UF_AIH_2008)

write.csv(UF_AIH_2008, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_AIH_2008.csv', row.names=FALSE)

# 2009

UF_AIH_2009 <- select(df.AIH.2009_UF, UF = Nome_UF, Freq)

UF_AIH_2009$Porcentagem <- paste(round(UF_AIH_2009$Freq / sum(UF_AIH_2009 $Freq) * 100,digits=2),"%",sep="")

UF_AIH_2009 <- arrange(UF_AIH_2009, desc(Freq))

UF_AIH_2009<- na.omit(UF_AIH_2009)

write.csv(UF_AIH_2009, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_AIH_2009.csv', row.names=FALSE)

# 2010

UF_AIH_2010 <- select(df.AIH.2010_UF, UF = Nome_UF, Freq)

UF_AIH_2010$Porcentagem <- paste(round(UF_AIH_2010$Freq / sum(UF_AIH_2010 $Freq) * 100,digits=2),"%",sep="")

UF_AIH_2010 <- arrange(UF_AIH_2010, desc(Freq))

UF_AIH_2010<- na.omit(UF_AIH_2010)

write.csv(UF_AIH_2010, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_AIH_2010.csv', row.names=FALSE)

# 2011

UF_AIH_2011 <- select(df.AIH.2011_UF, UF = Nome_UF, Freq)

UF_AIH_2011$Porcentagem <- paste(round(UF_AIH_2011$Freq / sum(UF_AIH_2011 $Freq) * 100,digits=2),"%",sep="")

UF_AIH_2011 <- arrange(UF_AIH_2011, desc(Freq))

UF_AIH_2011<- na.omit(UF_AIH_2011)

write.csv(UF_AIH_2011, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_AIH_2011.csv', row.names=FALSE)

# 2012

UF_AIH_2012 <- select(df.AIH.2012_UF, UF = Nome_UF, Freq)

UF_AIH_2012$Porcentagem <- paste(round(UF_AIH_2012$Freq / sum(UF_AIH_2012 $Freq) * 100,digits=2),"%",sep="")

UF_AIH_2012 <- arrange(UF_AIH_2012, desc(Freq))

UF_AIH_2012<- na.omit(UF_AIH_2012)

write.csv(UF_AIH_2012, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_AIH_2012.csv', row.names=FALSE)

# 2013

UF_AIH_2013 <- select(df.AIH.2013_UF, UF = Nome_UF, Freq)

UF_AIH_2013$Porcentagem <- paste(round(UF_AIH_2013$Freq / sum(UF_AIH_2013 $Freq) * 100,digits=2),"%",sep="")

UF_AIH_2013 <- arrange(UF_AIH_2013, desc(Freq))

UF_AIH_2013<- na.omit(UF_AIH_2013)

write.csv(UF_AIH_2013, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_AIH_2013.csv', row.names=FALSE)

# 2014

UF_AIH_2014 <- select(df.AIH.2014_UF, UF = Nome_UF, Freq)

UF_AIH_2014$Porcentagem <- paste(round(UF_AIH_2014$Freq / sum(UF_AIH_2014 $Freq) * 100,digits=2),"%",sep="")

UF_AIH_2014 <- arrange(UF_AIH_2014, desc(Freq))

UF_AIH_2014<- na.omit(UF_AIH_2014)

write.csv(UF_AIH_2014, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_AIH_2014.csv', row.names=FALSE)

# 2015

UF_AIH_2015 <- select(df.AIH.2015_UF, UF = Nome_UF, Freq)

UF_AIH_2015$Porcentagem <- paste(round(UF_AIH_2015$Freq / sum(UF_AIH_2015 $Freq) * 100,digits=2),"%",sep="")

UF_AIH_2015 <- arrange(UF_AIH_2015, desc(Freq))

UF_AIH_2015<- na.omit(UF_AIH_2015)

write.csv(UF_AIH_2015, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_AIH_2015.csv', row.names=FALSE)

# 2016

UF_AIH_2016 <- select(df.AIH.2016_UF, UF = Nome_UF, Freq)

UF_AIH_2016$Porcentagem <- paste(round(UF_AIH_2016$Freq / sum(UF_AIH_2016 $Freq) * 100,digits=2),"%",sep="")

UF_AIH_2016 <- arrange(UF_AIH_2016, desc(Freq))

UF_AIH_2016<- na.omit(UF_AIH_2016)

write.csv(UF_AIH_2016, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_AIH_2016.csv', row.names=FALSE)

# 2017

UF_AIH_2017 <- select(df.AIH.2017_UF, UF = Nome_UF, Freq)

UF_AIH_2017$Porcentagem <- paste(round(UF_AIH_2017$Freq / sum(UF_AIH_2017 $Freq) * 100,digits=2),"%",sep="")

UF_AIH_2017 <- arrange(UF_AIH_2017, desc(Freq))

UF_AIH_2017<- na.omit(UF_AIH_2017)

write.csv(UF_AIH_2017, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_AIH_2017.csv', row.names=FALSE)

# 2018

UF_AIH_2018 <- select(df.AIH.2018_UF, UF = Nome_UF, Freq)

UF_AIH_2018$Porcentagem <- paste(round(UF_AIH_2018$Freq / sum(UF_AIH_2018 $Freq) * 100,digits=2),"%",sep="")

UF_AIH_2018 <- arrange(UF_AIH_2018, desc(Freq))

UF_AIH_2018<- na.omit(UF_AIH_2018)

write.csv(UF_AIH_2018, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_AIH_2018.csv', row.names=FALSE)

## MUN ##

# 2008

mun_AIH_2008 <- select(df.AIH.2008_mun, Município = Nome_Município, Freq)

mun_AIH_2008$Porcentagem <- paste(round(mun_AIH_2008$Freq / sum(mun_AIH_2008 $Freq) * 100,digits=2),"%",sep="")

mun_AIH_2008 <- arrange(mun_AIH_2008, desc(Freq))

mun_AIH_2008 <- na.omit(mun_AIH_2008)

write.csv(mun_AIH_2008, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_AIH_2008.csv', row.names=FALSE)

# 2009

mun_AIH_2009 <- select(df.AIH.2009_mun, Município = Nome_Município, Freq)

mun_AIH_2009$Porcentagem <- paste(round(mun_AIH_2009$Freq / sum(mun_AIH_2009 $Freq) * 100,digits=2),"%",sep="")

mun_AIH_2009 <- arrange(mun_AIH_2009, desc(Freq))

mun_AIH_2009 <- na.omit(mun_AIH_2009)

write.csv(mun_AIH_2009, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_AIH_2009.csv', row.names=FALSE)

# 2010

mun_AIH_2010 <- select(df.AIH.2010_mun, Município = Nome_Município, Freq)

mun_AIH_2010$Porcentagem <- paste(round(mun_AIH_2010$Freq / sum(mun_AIH_2010 $Freq) * 100,digits=2),"%",sep="")

mun_AIH_2010 <- arrange(mun_AIH_2010, desc(Freq))

mun_AIH_2010 <- na.omit(mun_AIH_2010)

write.csv(mun_AIH_2010, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_AIH_2010.csv', row.names=FALSE)

# 2011

mun_AIH_2011 <- select(df.AIH.2011_mun, Município = Nome_Município, Freq)

mun_AIH_2011$Porcentagem <- paste(round(mun_AIH_2011$Freq / sum(mun_AIH_2011 $Freq) * 100,digits=2),"%",sep="")

mun_AIH_2011 <- arrange(mun_AIH_2011, desc(Freq))

mun_AIH_2011 <- na.omit(mun_AIH_2011)

write.csv(mun_AIH_2011, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_AIH_2011.csv', row.names=FALSE)

# 2012

mun_AIH_2012 <- select(df.AIH.2012_mun, Município = Nome_Município, Freq)

mun_AIH_2012$Porcentagem <- paste(round(mun_AIH_2012$Freq / sum(mun_AIH_2012 $Freq) * 100,digits=2),"%",sep="")

mun_AIH_2012 <- arrange(mun_AIH_2012, desc(Freq))

mun_AIH_2012 <- na.omit(mun_AIH_2012)

write.csv(mun_AIH_2012, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_AIH_2012.csv', row.names=FALSE)

# 2013

mun_AIH_2013 <- select(df.AIH.2013_mun, Município = Nome_Município, Freq)

mun_AIH_2013$Porcentagem <- paste(round(mun_AIH_2013$Freq / sum(mun_AIH_2013 $Freq) * 100,digits=2),"%",sep="")

mun_AIH_2013 <- arrange(mun_AIH_2013, desc(Freq))

mun_AIH_2013 <- na.omit(mun_AIH_2013)

write.csv(mun_AIH_2013, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_AIH_2013.csv', row.names=FALSE)

# 2014

mun_AIH_2014 <- select(df.AIH.2014_mun, Município = Nome_Município, Freq)

mun_AIH_2014$Porcentagem <- paste(round(mun_AIH_2014$Freq / sum(mun_AIH_2014 $Freq) * 100,digits=2),"%",sep="")

mun_AIH_2014 <- arrange(mun_AIH_2014, desc(Freq))

mun_AIH_2014 <- na.omit(mun_AIH_2014)

write.csv(mun_AIH_2014, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_AIH_2014.csv', row.names=FALSE)


# 2015

mun_AIH_2015 <- select(df.AIH.2015_mun, Município = Nome_Município, Freq)

mun_AIH_2015$Porcentagem <- paste(round(mun_AIH_2015$Freq / sum(mun_AIH_2015 $Freq) * 100,digits=2),"%",sep="")

mun_AIH_2015 <- arrange(mun_AIH_2015, desc(Freq))

mun_AIH_2015 <- na.omit(mun_AIH_2015)

write.csv(mun_AIH_2015, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_AIH_2015.csv', row.names=FALSE)

# 2016

mun_AIH_2016 <- select(df.AIH.2016_mun, Município = Nome_Município, Freq)

mun_AIH_2016$Porcentagem <- paste(round(mun_AIH_2016$Freq / sum(mun_AIH_2016 $Freq) * 100,digits=2),"%",sep="")

mun_AIH_2016 <- arrange(mun_AIH_2016, desc(Freq))

mun_AIH_2016 <- na.omit(mun_AIH_2016)

write.csv(mun_AIH_2016, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_AIH_2016.csv', row.names=FALSE)

# 2017

mun_AIH_2017 <- select(df.AIH.2017_mun, Município = Nome_Município, Freq)

mun_AIH_2017$Porcentagem <- paste(round(mun_AIH_2017$Freq / sum(mun_AIH_2017 $Freq) * 100,digits=2),"%",sep="")

mun_AIH_2017 <- arrange(mun_AIH_2017, desc(Freq))

mun_AIH_2017 <- na.omit(mun_AIH_2017)

write.csv(mun_AIH_2017, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_AIH_2017.csv', row.names=FALSE)

# 2018

mun_AIH_2018 <- select(df.AIH.2018_mun, Município = Nome_Município, Freq)

mun_AIH_2018$Porcentagem <- paste(round(mun_AIH_2018$Freq / sum(mun_AIH_2018 $Freq) * 100,digits=2),"%",sep="")

mun_AIH_2018 <- arrange(mun_AIH_2018, desc(Freq))

mun_AIH_2018 <- na.omit(mun_AIH_2018)

write.csv(mun_AIH_2018, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_AIH_2018.csv', row.names=FALSE)

#######
# GAL #
#######

## UF ##

# 2008

UF_GAL_2008 <- select(df.GAL.2008_UF, UF = Nome_UF, Freq)

UF_GAL_2008$Porcentagem <- paste(round(UF_GAL_2008$Freq / sum(UF_GAL_2008 $Freq) * 100,digits=2),"%",sep="")

UF_GAL_2008 <- arrange(UF_GAL_2008, desc(Freq))

UF_GAL_2008<- na.omit(UF_GAL_2008)

write.csv(UF_GAL_2008, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_GAL_2008.csv', row.names=FALSE)

# 2009

UF_GAL_2009 <- select(df.GAL.2009_UF, UF = Nome_UF, Freq)

UF_GAL_2009$Porcentagem <- paste(round(UF_GAL_2009$Freq / sum(UF_GAL_2009 $Freq) * 100,digits=2),"%",sep="")

UF_GAL_2009 <- arrange(UF_GAL_2009, desc(Freq))

UF_GAL_2009<- na.omit(UF_GAL_2009)

write.csv(UF_GAL_2009, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_GAL_2009.csv', row.names=FALSE)

# 2010

UF_GAL_2010 <- select(df.GAL.2010_UF, UF = Nome_UF, Freq)

UF_GAL_2010$Porcentagem <- paste(round(UF_GAL_2010$Freq / sum(UF_GAL_2010 $Freq) * 100,digits=2),"%",sep="")

UF_GAL_2010 <- arrange(UF_GAL_2010, desc(Freq))

UF_GAL_2010<- na.omit(UF_GAL_2010)

write.csv(UF_GAL_2010, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_GAL_2010.csv', row.names=FALSE)

# 2011

UF_GAL_2011 <- select(df.GAL.2011_UF, UF = Nome_UF, Freq)

UF_GAL_2011$Porcentagem <- paste(round(UF_GAL_2011$Freq / sum(UF_GAL_2011 $Freq) * 100,digits=2),"%",sep="")

UF_GAL_2011 <- arrange(UF_GAL_2011, desc(Freq))

UF_GAL_2011<- na.omit(UF_GAL_2011)

write.csv(UF_GAL_2011, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_GAL_2011.csv', row.names=FALSE)

# 2012

UF_GAL_2012 <- select(df.GAL.2012_UF, UF = Nome_UF, Freq)

UF_GAL_2012$Porcentagem <- paste(round(UF_GAL_2012$Freq / sum(UF_GAL_2012 $Freq) * 100,digits=2),"%",sep="")

UF_GAL_2012 <- arrange(UF_GAL_2012, desc(Freq))

UF_GAL_2012<- na.omit(UF_GAL_2012)

write.csv(UF_GAL_2012, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_GAL_2012.csv', row.names=FALSE)

# 2013

UF_GAL_2013 <- select(df.GAL.2013_UF, UF = Nome_UF, Freq)

UF_GAL_2013$Porcentagem <- paste(round(UF_GAL_2013$Freq / sum(UF_GAL_2013 $Freq) * 100,digits=2),"%",sep="")

UF_GAL_2013 <- arrange(UF_GAL_2013, desc(Freq))

UF_GAL_2013<- na.omit(UF_GAL_2013)

write.csv(UF_GAL_2013, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_GAL_2013.csv', row.names=FALSE)

# 2014

UF_GAL_2014 <- select(df.GAL.2014_UF, UF = Nome_UF, Freq)

UF_GAL_2014$Porcentagem <- paste(round(UF_GAL_2014$Freq / sum(UF_GAL_2014 $Freq) * 100,digits=2),"%",sep="")

UF_GAL_2014 <- arrange(UF_GAL_2014, desc(Freq))

UF_GAL_2014<- na.omit(UF_GAL_2014)

write.csv(UF_GAL_2014, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_GAL_2014.csv', row.names=FALSE)

# 2015

UF_GAL_2015 <- select(df.GAL.2015_UF, UF = Nome_UF, Freq)

UF_GAL_2015$Porcentagem <- paste(round(UF_GAL_2015$Freq / sum(UF_GAL_2015 $Freq) * 100,digits=2),"%",sep="")

UF_GAL_2015 <- arrange(UF_GAL_2015, desc(Freq))

UF_GAL_2015<- na.omit(UF_GAL_2015)

write.csv(UF_GAL_2015, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_GAL_2015.csv', row.names=FALSE)

# 2016

UF_GAL_2016 <- select(df.GAL.2016_UF, UF = Nome_UF, Freq)

UF_GAL_2016$Porcentagem <- paste(round(UF_GAL_2016$Freq / sum(UF_GAL_2016 $Freq) * 100,digits=2),"%",sep="")

UF_GAL_2016 <- arrange(UF_GAL_2016, desc(Freq))

UF_GAL_2016<- na.omit(UF_GAL_2016)

write.csv(UF_GAL_2016, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_GAL_2016.csv', row.names=FALSE)

# 2017

UF_GAL_2017 <- select(df.GAL.2017_UF, UF = Nome_UF, Freq)

UF_GAL_2017$Porcentagem <- paste(round(UF_GAL_2017$Freq / sum(UF_GAL_2017 $Freq) * 100,digits=2),"%",sep="")

UF_GAL_2017 <- arrange(UF_GAL_2017, desc(Freq))

UF_GAL_2017<- na.omit(UF_GAL_2017)

write.csv(UF_GAL_2017, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_GAL_2017.csv', row.names=FALSE)

# 2018

UF_GAL_2018 <- select(df.GAL.2018_UF, UF = Nome_UF, Freq)

UF_GAL_2018$Porcentagem <- paste(round(UF_GAL_2018$Freq / sum(UF_GAL_2018 $Freq) * 100,digits=2),"%",sep="")

UF_GAL_2018 <- arrange(UF_GAL_2018, desc(Freq))

UF_GAL_2018<- na.omit(UF_GAL_2018)

write.csv(UF_GAL_2018, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_GAL_2018.csv', row.names=FALSE)

## MUN ##

# 2008

mun_GAL_2008 <- select(df.GAL.2008_mun, Município = Nome_Município, Freq)

mun_GAL_2008$Porcentagem <- paste(round(mun_GAL_2008$Freq / sum(mun_GAL_2008 $Freq) * 100,digits=2),"%",sep="")

mun_GAL_2008 <- arrange(mun_GAL_2008, desc(Freq))

mun_GAL_2008<- na.omit(mun_GAL_2008)

write.csv(mun_GAL_2008, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_GAL_2008.csv', row.names=FALSE)

# 2009

mun_GAL_2009 <- select(df.GAL.2009_mun, Município = Nome_Município, Freq)

mun_GAL_2009$Porcentagem <- paste(round(mun_GAL_2009$Freq / sum(mun_GAL_2009 $Freq) * 100,digits=2),"%",sep="")

mun_GAL_2009 <- arrange(mun_GAL_2009, desc(Freq))

mun_GAL_2009<- na.omit(mun_GAL_2009)

write.csv(mun_GAL_2009, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_GAL_2009.csv', row.names=FALSE)

# 2010

mun_GAL_2010 <- select(df.GAL.2010_mun, Município = Nome_Município, Freq)

mun_GAL_2010$Porcentagem <- paste(round(mun_GAL_2010$Freq / sum(mun_GAL_2010 $Freq) * 100,digits=2),"%",sep="")

mun_GAL_2010 <- arrange(mun_GAL_2010, desc(Freq))

mun_GAL_2010<- na.omit(mun_GAL_2010)

write.csv(mun_GAL_2010, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_GAL_2010.csv', row.names=FALSE)

# 2011

mun_GAL_2011 <- select(df.GAL.2011_mun, Município = Nome_Município, Freq)

mun_GAL_2011$Porcentagem <- paste(round(mun_GAL_2011$Freq / sum(mun_GAL_2011 $Freq) * 100,digits=2),"%",sep="")

mun_GAL_2011 <- arrange(mun_GAL_2011, desc(Freq))

mun_GAL_2011<- na.omit(mun_GAL_2011)

write.csv(mun_GAL_2011, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_GAL_2011.csv', row.names=FALSE)

# 2012

mun_GAL_2012 <- select(df.GAL.2012_mun, Município = Nome_Município, Freq)

mun_GAL_2012$Porcentagem <- paste(round(mun_GAL_2012$Freq / sum(mun_GAL_2012 $Freq) * 100,digits=2),"%",sep="")

mun_GAL_2012 <- arrange(mun_GAL_2012, desc(Freq))

mun_GAL_2012<- na.omit(mun_GAL_2012)

write.csv(mun_GAL_2012, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_GAL_2012.csv', row.names=FALSE)

# 2013

mun_GAL_2013 <- select(df.GAL.2013_mun, Município = Nome_Município, Freq)

mun_GAL_2013$Porcentagem <- paste(round(mun_GAL_2013$Freq / sum(mun_GAL_2013 $Freq) * 100,digits=2),"%",sep="")

mun_GAL_2013 <- arrange(mun_GAL_2013, desc(Freq))

mun_GAL_2013<- na.omit(mun_GAL_2013)

write.csv(mun_GAL_2013, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_GAL_2013.csv', row.names=FALSE)

# 2014

mun_GAL_2014 <- select(df.GAL.2014_mun, Município = Nome_Município, Freq)

mun_GAL_2014$Porcentagem <- paste(round(mun_GAL_2014$Freq / sum(mun_GAL_2014 $Freq) * 100,digits=2),"%",sep="")

mun_GAL_2014 <- arrange(mun_GAL_2014, desc(Freq))

mun_GAL_2014<- na.omit(mun_GAL_2014)

write.csv(mun_GAL_2014, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_GAL_2014.csv', row.names=FALSE)

# 2015

mun_GAL_2015 <- select(df.GAL.2015_mun, Município = Nome_Município, Freq)

mun_GAL_2015$Porcentagem <- paste(round(mun_GAL_2015$Freq / sum(mun_GAL_2015 $Freq) * 100,digits=2),"%",sep="")

mun_GAL_2015 <- arrange(mun_GAL_2015, desc(Freq))

mun_GAL_2015<- na.omit(mun_GAL_2015)

write.csv(mun_GAL_2015, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_GAL_2015.csv', row.names=FALSE)

# 2016

mun_GAL_2016 <- select(df.GAL.2016_mun, Município = Nome_Município, Freq)

mun_GAL_2016$Porcentagem <- paste(round(mun_GAL_2016$Freq / sum(mun_GAL_2016 $Freq) * 100,digits=2),"%",sep="")

mun_GAL_2016 <- arrange(mun_GAL_2016, desc(Freq))

mun_GAL_2016<- na.omit(mun_GAL_2016)

write.csv(mun_GAL_2016, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_GAL_2016.csv', row.names=FALSE)

# 2017

mun_GAL_2017 <- select(df.GAL.2017_mun, Município = Nome_Município, Freq)

mun_GAL_2017$Porcentagem <- paste(round(mun_GAL_2017$Freq / sum(mun_GAL_2017 $Freq) * 100,digits=2),"%",sep="")

mun_GAL_2017 <- arrange(mun_GAL_2017, desc(Freq))

mun_GAL_2017<- na.omit(mun_GAL_2017)

write.csv(mun_GAL_2017, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_GAL_2017.csv', row.names=FALSE)

# 2018

mun_GAL_2018 <- select(df.GAL.2018_mun, Município = Nome_Município, Freq)

mun_GAL_2018$Porcentagem <- paste(round(mun_GAL_2018$Freq / sum(mun_GAL_2018 $Freq) * 100,digits=2),"%",sep="")

mun_GAL_2018 <- arrange(mun_GAL_2018, desc(Freq))

mun_GAL_2018<- na.omit(mun_GAL_2018)

write.csv(mun_GAL_2018, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_GAL_2018.csv', row.names=FALSE)

#########
# SINAN #
#########

## UF ##

# 2008

UF_SINAN_2008 <- select(df.SINAN.2008_UF, UF = Nome_UF, Freq)

UF_SINAN_2008$Porcentagem <- paste(round(UF_SINAN_2008$Freq / sum(UF_SINAN_2008 $Freq) * 100,digits=2),"%",sep="")

UF_SINAN_2008 <- arrange(UF_SINAN_2008, desc(Freq))

UF_SINAN_2008<- na.omit(UF_SINAN_2008)

write.csv(UF_SINAN_2008, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SINAN_2008.csv', row.names=FALSE)

# 2009

UF_SINAN_2009 <- select(df.SINAN.2009_UF, UF = Nome_UF, Freq)

UF_SINAN_2009$Porcentagem <- paste(round(UF_SINAN_2009$Freq / sum(UF_SINAN_2009 $Freq) * 100,digits=2),"%",sep="")

UF_SINAN_2009 <- arrange(UF_SINAN_2009, desc(Freq))

UF_SINAN_2009<- na.omit(UF_SINAN_2009)

write.csv(UF_SINAN_2009, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SINAN_2009.csv', row.names=FALSE)

# 2010

UF_SINAN_2010 <- select(df.SINAN.2010_UF, UF = Nome_UF, Freq)

UF_SINAN_2010$Porcentagem <- paste(round(UF_SINAN_2010$Freq / sum(UF_SINAN_2010 $Freq) * 100,digits=2),"%",sep="")

UF_SINAN_2010 <- arrange(UF_SINAN_2010, desc(Freq))

UF_SINAN_2010<- na.omit(UF_SINAN_2010)

write.csv(UF_SINAN_2010, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SINAN_2010.csv', row.names=FALSE)

# 2011

UF_SINAN_2011 <- select(df.SINAN.2011_UF, UF = Nome_UF, Freq)

UF_SINAN_2011$Porcentagem <- paste(round(UF_SINAN_2011$Freq / sum(UF_SINAN_2011 $Freq) * 100,digits=2),"%",sep="")

UF_SINAN_2011 <- arrange(UF_SINAN_2011, desc(Freq))

UF_SINAN_2011<- na.omit(UF_SINAN_2011)

write.csv(UF_SINAN_2011, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SINAN_2011.csv', row.names=FALSE)

# 2012

UF_SINAN_2012 <- select(df.SINAN.2012_UF, UF = Nome_UF, Freq)

UF_SINAN_2012$Porcentagem <- paste(round(UF_SINAN_2012$Freq / sum(UF_SINAN_2012 $Freq) * 100,digits=2),"%",sep="")

UF_SINAN_2012 <- arrange(UF_SINAN_2012, desc(Freq))

UF_SINAN_2012<- na.omit(UF_SINAN_2012)

write.csv(UF_SINAN_2012, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SINAN_2012.csv', row.names=FALSE)

# 2013

UF_SINAN_2013 <- select(df.SINAN.2013_UF, UF = Nome_UF, Freq)

UF_SINAN_2013$Porcentagem <- paste(round(UF_SINAN_2013$Freq / sum(UF_SINAN_2013 $Freq) * 100,digits=2),"%",sep="")

UF_SINAN_2013 <- arrange(UF_SINAN_2013, desc(Freq))

UF_SINAN_2013<- na.omit(UF_SINAN_2013)

write.csv(UF_SINAN_2013, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SINAN_2013.csv', row.names=FALSE)

# 2014

UF_SINAN_2014 <- select(df.SINAN.2014_UF, UF = Nome_UF, Freq)

UF_SINAN_2014$Porcentagem <- paste(round(UF_SINAN_2014$Freq / sum(UF_SINAN_2014 $Freq) * 100,digits=2),"%",sep="")

UF_SINAN_2014 <- arrange(UF_SINAN_2014, desc(Freq))

UF_SINAN_2014<- na.omit(UF_SINAN_2014)

write.csv(UF_SINAN_2014, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SINAN_2014.csv', row.names=FALSE)

# 2015

UF_SINAN_2015 <- select(df.SINAN.2015_UF, UF = Nome_UF, Freq)

UF_SINAN_2015$Porcentagem <- paste(round(UF_SINAN_2015$Freq / sum(UF_SINAN_2015 $Freq) * 100,digits=2),"%",sep="")

UF_SINAN_2015 <- arrange(UF_SINAN_2015, desc(Freq))

UF_SINAN_2015<- na.omit(UF_SINAN_2015)

write.csv(UF_SINAN_2015, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SINAN_2015.csv', row.names=FALSE)

# 2016

UF_SINAN_2016 <- select(df.SINAN.2016_UF, UF = Nome_UF, Freq)

UF_SINAN_2016$Porcentagem <- paste(round(UF_SINAN_2016$Freq / sum(UF_SINAN_2016 $Freq) * 100,digits=2),"%",sep="")

UF_SINAN_2016 <- arrange(UF_SINAN_2016, desc(Freq))

UF_SINAN_2016<- na.omit(UF_SINAN_2016)

write.csv(UF_SINAN_2016, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SINAN_2016.csv', row.names=FALSE)

# 2017

UF_SINAN_2017 <- select(df.SINAN.2017_UF, UF = Nome_UF, Freq)

UF_SINAN_2017$Porcentagem <- paste(round(UF_SINAN_2017$Freq / sum(UF_SINAN_2017 $Freq) * 100,digits=2),"%",sep="")

UF_SINAN_2017 <- arrange(UF_SINAN_2017, desc(Freq))

UF_SINAN_2017<- na.omit(UF_SINAN_2017)

write.csv(UF_SINAN_2017, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SINAN_2017.csv', row.names=FALSE)

# 2018

UF_SINAN_2018 <- select(df.SINAN.2018_UF, UF = Nome_UF, Freq)

UF_SINAN_2018$Porcentagem <- paste(round(UF_SINAN_2018$Freq / sum(UF_SINAN_2018 $Freq) * 100,digits=2),"%",sep="")

UF_SINAN_2018 <- arrange(UF_SINAN_2018, desc(Freq))

UF_SINAN_2018<- na.omit(UF_SINAN_2018)

write.csv(UF_SINAN_2018, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SINAN_2018.csv', row.names=FALSE)

## MUN ##

# 2008

mun_SINAN_2008 <- select(df.SINAN.2008_mun, Município = Nome_Município, Freq)

mun_SINAN_2008$Porcentagem <- paste(round(mun_SINAN_2008$Freq / sum(mun_SINAN_2008 $Freq) * 100,digits=2),"%",sep="")

mun_SINAN_2008 <- arrange(mun_SINAN_2008, desc(Freq))

mun_SINAN_2008<- na.omit(mun_SINAN_2008)

write.csv(mun_SINAN_2008, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SINAN_2008.csv', row.names=FALSE)

# 2009

mun_SINAN_2009 <- select(df.SINAN.2009_mun, Município = Nome_Município, Freq)

mun_SINAN_2009$Porcentagem <- paste(round(mun_SINAN_2009$Freq / sum(mun_SINAN_2009 $Freq) * 100,digits=2),"%",sep="")

mun_SINAN_2009 <- arrange(mun_SINAN_2009, desc(Freq))

mun_SINAN_2009<- na.omit(mun_SINAN_2009)

write.csv(mun_SINAN_2009, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SINAN_2009.csv', row.names=FALSE)

# 2010

mun_SINAN_2010 <- select(df.SINAN.2010_mun, Município = Nome_Município, Freq)

mun_SINAN_2010$Porcentagem <- paste(round(mun_SINAN_2010$Freq / sum(mun_SINAN_2010 $Freq) * 100,digits=2),"%",sep="")

mun_SINAN_2010 <- arrange(mun_SINAN_2010, desc(Freq))

mun_SINAN_2010<- na.omit(mun_SINAN_2010)

write.csv(mun_SINAN_2010, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SINAN_2010.csv', row.names=FALSE)

# 2011

mun_SINAN_2011 <- select(df.SINAN.2011_mun, Município = Nome_Município, Freq)

mun_SINAN_2011$Porcentagem <- paste(round(mun_SINAN_2011$Freq / sum(mun_SINAN_2011 $Freq) * 100,digits=2),"%",sep="")

mun_SINAN_2011 <- arrange(mun_SINAN_2011, desc(Freq))

mun_SINAN_2011<- na.omit(mun_SINAN_2011)

write.csv(mun_SINAN_2011, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SINAN_2011.csv', row.names=FALSE)

# 2012

mun_SINAN_2012 <- select(df.SINAN.2012_mun, Município = Nome_Município, Freq)

mun_SINAN_2012$Porcentagem <- paste(round(mun_SINAN_2012$Freq / sum(mun_SINAN_2012 $Freq) * 100,digits=2),"%",sep="")

mun_SINAN_2012 <- arrange(mun_SINAN_2012, desc(Freq))

mun_SINAN_2012<- na.omit(mun_SINAN_2012)

write.csv(mun_SINAN_2012, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SINAN_2012.csv', row.names=FALSE)

# 2013

mun_SINAN_2013 <- select(df.SINAN.2013_mun, Município = Nome_Município, Freq)

mun_SINAN_2013$Porcentagem <- paste(round(mun_SINAN_2013$Freq / sum(mun_SINAN_2013 $Freq) * 100,digits=2),"%",sep="")

mun_SINAN_2013 <- arrange(mun_SINAN_2013, desc(Freq))

mun_SINAN_2013<- na.omit(mun_SINAN_2013)

write.csv(mun_SINAN_2013, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SINAN_2013.csv', row.names=FALSE)

# 2014

mun_SINAN_2014 <- select(df.SINAN.2014_mun, Município = Nome_Município, Freq)

mun_SINAN_2014$Porcentagem <- paste(round(mun_SINAN_2014$Freq / sum(mun_SINAN_2014 $Freq) * 100,digits=2),"%",sep="")

mun_SINAN_2014 <- arrange(mun_SINAN_2014, desc(Freq))

mun_SINAN_2014<- na.omit(mun_SINAN_2014)

write.csv(mun_SINAN_2014, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SINAN_2014.csv', row.names=FALSE)

# 2015

mun_SINAN_2015 <- select(df.SINAN.2015_mun, Município = Nome_Município, Freq)

mun_SINAN_2015$Porcentagem <- paste(round(mun_SINAN_2015$Freq / sum(mun_SINAN_2015 $Freq) * 100,digits=2),"%",sep="")

mun_SINAN_2015 <- arrange(mun_SINAN_2015, desc(Freq))

mun_SINAN_2015<- na.omit(mun_SINAN_2015)

write.csv(mun_SINAN_2015, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SINAN_2015.csv', row.names=FALSE)

# 2016

mun_SINAN_2016 <- select(df.SINAN.2016_mun, Município = Nome_Município, Freq)

mun_SINAN_2016$Porcentagem <- paste(round(mun_SINAN_2016$Freq / sum(mun_SINAN_2016 $Freq) * 100,digits=2),"%",sep="")

mun_SINAN_2016 <- arrange(mun_SINAN_2016, desc(Freq))

mun_SINAN_2016<- na.omit(mun_SINAN_2016)

write.csv(mun_SINAN_2016, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SINAN_2016.csv', row.names=FALSE)

# 2017

mun_SINAN_2017 <- select(df.SINAN.2017_mun, Município = Nome_Município, Freq)

mun_SINAN_2017$Porcentagem <- paste(round(mun_SINAN_2017$Freq / sum(mun_SINAN_2017 $Freq) * 100,digits=2),"%",sep="")

mun_SINAN_2017 <- arrange(mun_SINAN_2017, desc(Freq))

mun_SINAN_2017<- na.omit(mun_SINAN_2017)

write.csv(mun_SINAN_2017, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SINAN_2017.csv', row.names=FALSE)

# 2018

mun_SINAN_2018 <- select(df.SINAN.2018_mun, Município = Nome_Município, Freq)

mun_SINAN_2018$Porcentagem <- paste(round(mun_SINAN_2018$Freq / sum(mun_SINAN_2018 $Freq) * 100,digits=2),"%",sep="")

mun_SINAN_2018 <- arrange(mun_SINAN_2018, desc(Freq))

mun_SINAN_2018<- na.omit(mun_SINAN_2018)

write.csv(mun_SINAN_2018, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SINAN_2018.csv', row.names=FALSE)

#######
# SIM #
#######

## UF ##

# 2008

UF_SIM_2008 <- select(df.SIM.2008_UF, UF = Nome_UF, Freq)

UF_SIM_2008$Porcentagem <- paste(round(UF_SIM_2008$Freq / sum(UF_SIM_2008 $Freq) * 100,digits=2),"%",sep="")

UF_SIM_2008 <- arrange(UF_SIM_2008, desc(Freq))

UF_SIM_2008<- na.omit(UF_SIM_2008)

write.csv(UF_SIM_2008, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SIM_2008.csv', row.names=FALSE)

# 2009

UF_SIM_2009 <- select(df.SIM.2009_UF, UF = Nome_UF, Freq)

UF_SIM_2009$Porcentagem <- paste(round(UF_SIM_2009$Freq / sum(UF_SIM_2009 $Freq) * 100,digits=2),"%",sep="")

UF_SIM_2009 <- arrange(UF_SIM_2009, desc(Freq))

UF_SIM_2009<- na.omit(UF_SIM_2009)

write.csv(UF_SIM_2009, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SIM_2009.csv', row.names=FALSE)

# 2010

UF_SIM_2010 <- select(df.SIM.2010_UF, UF = Nome_UF, Freq)

UF_SIM_2010$Porcentagem <- paste(round(UF_SIM_2010$Freq / sum(UF_SIM_2010 $Freq) * 100,digits=2),"%",sep="")

UF_SIM_2010 <- arrange(UF_SIM_2010, desc(Freq))

UF_SIM_2010<- na.omit(UF_SIM_2010)

write.csv(UF_SIM_2010, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SIM_2010.csv', row.names=FALSE)

# 2011

UF_SIM_2011 <- select(df.SIM.2011_UF, UF = Nome_UF, Freq)

UF_SIM_2011$Porcentagem <- paste(round(UF_SIM_2011$Freq / sum(UF_SIM_2011 $Freq) * 100,digits=2),"%",sep="")

UF_SIM_2011 <- arrange(UF_SIM_2011, desc(Freq))

UF_SIM_2011<- na.omit(UF_SIM_2011)

write.csv(UF_SIM_2011, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SIM_2011.csv', row.names=FALSE)

# 2012

UF_SIM_2012 <- select(df.SIM.2012_UF, UF = Nome_UF, Freq)

UF_SIM_2012$Porcentagem <- paste(round(UF_SIM_2012$Freq / sum(UF_SIM_2012 $Freq) * 100,digits=2),"%",sep="")

UF_SIM_2012 <- arrange(UF_SIM_2012, desc(Freq))

UF_SIM_2012<- na.omit(UF_SIM_2012)

write.csv(UF_SIM_2012, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SIM_2012.csv', row.names=FALSE)

# 2013

UF_SIM_2013 <- select(df.SIM.2013_UF, UF = Nome_UF, Freq)

UF_SIM_2013$Porcentagem <- paste(round(UF_SIM_2013$Freq / sum(UF_SIM_2013 $Freq) * 100,digits=2),"%",sep="")

UF_SIM_2013 <- arrange(UF_SIM_2013, desc(Freq))

UF_SIM_2013<- na.omit(UF_SIM_2013)

write.csv(UF_SIM_2013, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SIM_2013.csv', row.names=FALSE)

# 2014

UF_SIM_2014 <- select(df.SIM.2014_UF, UF = Nome_UF, Freq)

UF_SIM_2014$Porcentagem <- paste(round(UF_SIM_2014$Freq / sum(UF_SIM_2014 $Freq) * 100,digits=2),"%",sep="")

UF_SIM_2014 <- arrange(UF_SIM_2014, desc(Freq))

UF_SIM_2014<- na.omit(UF_SIM_2014)

write.csv(UF_SIM_2014, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SIM_2014.csv', row.names=FALSE)

# 2015

UF_SIM_2015 <- select(df.SIM.2015_UF, UF = Nome_UF, Freq)

UF_SIM_2015$Porcentagem <- paste(round(UF_SIM_2015$Freq / sum(UF_SIM_2015 $Freq) * 100,digits=2),"%",sep="")

UF_SIM_2015 <- arrange(UF_SIM_2015, desc(Freq))

UF_SIM_2015<- na.omit(UF_SIM_2015)

write.csv(UF_SIM_2015, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SIM_2015.csv', row.names=FALSE)

# 2016

UF_SIM_2016 <- select(df.SIM.2016_UF, UF = Nome_UF, Freq)

UF_SIM_2016$Porcentagem <- paste(round(UF_SIM_2016$Freq / sum(UF_SIM_2016 $Freq) * 100,digits=2),"%",sep="")

UF_SIM_2016 <- arrange(UF_SIM_2016, desc(Freq))

UF_SIM_2016<- na.omit(UF_SIM_2016)

write.csv(UF_SIM_2016, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/UF_SIM_2016.csv', row.names=FALSE)

## MUN ##

# 2008

mun_SIM_2008 <- select(df.SIM.2008_mun, Município = Nome_Município, Freq)

mun_SIM_2008$Porcentagem <- paste(round(mun_SIM_2008$Freq / sum(mun_SIM_2008 $Freq) * 100,digits=2),"%",sep="")

mun_SIM_2008 <- arrange(mun_SIM_2008, desc(Freq))

mun_SIM_2008<- na.omit(mun_SIM_2008)

write.csv(mun_SIM_2008, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SIM_2008.csv', row.names=FALSE)

# 2009

mun_SIM_2009 <- select(df.SIM.2009_mun, Município = Nome_Município, Freq)

mun_SIM_2009$Porcentagem <- paste(round(mun_SIM_2009$Freq / sum(mun_SIM_2009 $Freq) * 100,digits=2),"%",sep="")

mun_SIM_2009 <- arrange(mun_SIM_2009, desc(Freq))

mun_SIM_2009<- na.omit(mun_SIM_2009)

write.csv(mun_SIM_2009, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SIM_2009.csv', row.names=FALSE)

# 2010

mun_SIM_2010 <- select(df.SIM.2010_mun, Município = Nome_Município, Freq)

mun_SIM_2010$Porcentagem <- paste(round(mun_SIM_2010$Freq / sum(mun_SIM_2010 $Freq) * 100,digits=2),"%",sep="")

mun_SIM_2010 <- arrange(mun_SIM_2010, desc(Freq))

mun_SIM_2010<- na.omit(mun_SIM_2010)

write.csv(mun_SIM_2010, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SIM_2010.csv', row.names=FALSE)

# 2011

mun_SIM_2011 <- select(df.SIM.2011_mun, Município = Nome_Município, Freq)

mun_SIM_2011$Porcentagem <- paste(round(mun_SIM_2011$Freq / sum(mun_SIM_2011 $Freq) * 100,digits=2),"%",sep="")

mun_SIM_2011 <- arrange(mun_SIM_2011, desc(Freq))

mun_SIM_2011<- na.omit(mun_SIM_2011)

write.csv(mun_SIM_2011, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SIM_2011.csv', row.names=FALSE)

# 2012

mun_SIM_2012 <- select(df.SIM.2012_mun, Município = Nome_Município, Freq)

mun_SIM_2012$Porcentagem <- paste(round(mun_SIM_2012$Freq / sum(mun_SIM_2012 $Freq) * 100,digits=2),"%",sep="")

mun_SIM_2012 <- arrange(mun_SIM_2012, desc(Freq))

mun_SIM_2012<- na.omit(mun_SIM_2012)

write.csv(mun_SIM_2012, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SIM_2012.csv', row.names=FALSE)

# 2013

mun_SIM_2013 <- select(df.SIM.2013_mun, Município = Nome_Município, Freq)

mun_SIM_2013$Porcentagem <- paste(round(mun_SIM_2013$Freq / sum(mun_SIM_2013 $Freq) * 100,digits=2),"%",sep="")

mun_SIM_2013 <- arrange(mun_SIM_2013, desc(Freq))

mun_SIM_2013<- na.omit(mun_SIM_2013)

write.csv(mun_SIM_2013, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SIM_2013.csv', row.names=FALSE)

# 2014

mun_SIM_2014 <- select(df.SIM.2014_mun, Município = Nome_Município, Freq)

mun_SIM_2014$Porcentagem <- paste(round(mun_SIM_2014$Freq / sum(mun_SIM_2014 $Freq) * 100,digits=2),"%",sep="")

mun_SIM_2014 <- arrange(mun_SIM_2014, desc(Freq))

mun_SIM_2014<- na.omit(mun_SIM_2014)

write.csv(mun_SIM_2014, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SIM_2014.csv', row.names=FALSE)

# 2015

mun_SIM_2015 <- select(df.SIM.2015_mun, Município = Nome_Município, Freq)

mun_SIM_2015$Porcentagem <- paste(round(mun_SIM_2015$Freq / sum(mun_SIM_2015 $Freq) * 100,digits=2),"%",sep="")

mun_SIM_2015 <- arrange(mun_SIM_2015, desc(Freq))

mun_SIM_2015<- na.omit(mun_SIM_2015)

write.csv(mun_SIM_2015, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SIM_2015.csv', row.names=FALSE)

# 2016

mun_SIM_2016 <- select(df.SIM.2016_mun, Município = Nome_Município, Freq)

mun_SIM_2016$Porcentagem <- paste(round(mun_SIM_2016$Freq / sum(mun_SIM_2016 $Freq) * 100,digits=2),"%",sep="")

mun_SIM_2016 <- arrange(mun_SIM_2016, desc(Freq))

mun_SIM_2016<- na.omit(mun_SIM_2016)

write.csv(mun_SIM_2016, file = '/Volumes/Mikael_backup3/produtos_OPAS/produto2/tabelas_produto/mun_SIM_2016.csv', row.names=FALSE)

########################
## Scatter plots (UF) ##
########################

##### Preparando tabelas por banco ######

## APAC

UF_APAC_2008$ano <- "2008" 
UF_APAC_2009$ano <- "2009" 
UF_APAC_2010$ano <- "2010" 
UF_APAC_2011$ano <- "2011" 
UF_APAC_2012$ano <- "2012" 
UF_APAC_2013$ano <- "2013" 
UF_APAC_2014$ano <- "2014"
UF_APAC_2015$ano <- "2015" 
UF_APAC_2016$ano <- "2016"
UF_APAC_2017$ano <- "2017"
UF_APAC_2018$ano <- "2018" 

UF_APAC_2008$UF[UF_APAC_2008$UF == "São Paulo"] <- "SP"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Rio Grande do Sul"] <- "RS"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Rio de Janeiro"] <- "RJ"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Minas Gerais"] <- "MG"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Santa Catarina"] <- "SC"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Paraná"] <- "PR"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Pernambuco"] <- "PE"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Amazonas"] <- "AM"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Espírito Santo"] <- "ES"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Goiás"] <- "GO"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Ceará"] <- "CE"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Pará"] <- "PA"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Acre"] <- "AC"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Bahia"] <- "BA"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Mato Grosso do Sul"] <- "MS"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Maranhão"] <- "MA"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Rondônia"] <- "RO"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Paraíba"] <- "PB"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Alagoas"] <- "AL"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Rio Grande do Norte"] <- "RN"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Piauí"] <- "PI"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Amapá"] <- "AP"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Distrito Federal"] <- "DF"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Mato Grosso"] <- "MT"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Roraima"] <- "RR"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Sergipe"] <- "SE"
UF_APAC_2008$UF[UF_APAC_2008$UF == "Tocantins"] <- "TO"

UF_APAC_2009$UF[UF_APAC_2009$UF == "São Paulo"] <- "SP"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Rio Grande do Sul"] <- "RS"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Rio de Janeiro"] <- "RJ"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Minas Gerais"] <- "MG"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Santa Catarina"] <- "SC"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Paraná"] <- "PR"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Pernambuco"] <- "PE"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Amazonas"] <- "AM"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Espírito Santo"] <- "ES"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Goiás"] <- "GO"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Ceará"] <- "CE"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Pará"] <- "PA"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Acre"] <- "AC"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Bahia"] <- "BA"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Mato Grosso do Sul"] <- "MS"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Maranhão"] <- "MA"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Rondônia"] <- "RO"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Paraíba"] <- "PB"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Alagoas"] <- "AL"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Rio Grande do Norte"] <- "RN"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Piauí"] <- "PI"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Amapá"] <- "AP"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Distrito Federal"] <- "DF"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Mato Grosso"] <- "MT"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Roraima"] <- "RR"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Sergipe"] <- "SE"
UF_APAC_2009$UF[UF_APAC_2009$UF == "Tocantins"] <- "TO"

UF_APAC_2010$UF[UF_APAC_2010$UF == "São Paulo"] <- "SP"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Rio Grande do Sul"] <- "RS"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Rio de Janeiro"] <- "RJ"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Minas Gerais"] <- "MG"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Santa Catarina"] <- "SC"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Paraná"] <- "PR"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Pernambuco"] <- "PE"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Amazonas"] <- "AM"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Espírito Santo"] <- "ES"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Goiás"] <- "GO"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Ceará"] <- "CE"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Pará"] <- "PA"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Acre"] <- "AC"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Bahia"] <- "BA"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Mato Grosso do Sul"] <- "MS"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Maranhão"] <- "MA"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Rondônia"] <- "RO"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Paraíba"] <- "PB"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Alagoas"] <- "AL"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Rio Grande do Norte"] <- "RN"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Piauí"] <- "PI"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Amapá"] <- "AP"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Distrito Federal"] <- "DF"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Mato Grosso"] <- "MT"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Roraima"] <- "RR"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Sergipe"] <- "SE"
UF_APAC_2010$UF[UF_APAC_2010$UF == "Tocantins"] <- "TO"

UF_APAC_2011$UF[UF_APAC_2011$UF == "São Paulo"] <- "SP"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Rio Grande do Sul"] <- "RS"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Rio de Janeiro"] <- "RJ"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Minas Gerais"] <- "MG"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Santa Catarina"] <- "SC"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Paraná"] <- "PR"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Pernambuco"] <- "PE"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Amazonas"] <- "AM"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Espírito Santo"] <- "ES"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Goiás"] <- "GO"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Ceará"] <- "CE"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Pará"] <- "PA"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Acre"] <- "AC"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Bahia"] <- "BA"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Mato Grosso do Sul"] <- "MS"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Maranhão"] <- "MA"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Rondônia"] <- "RO"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Paraíba"] <- "PB"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Alagoas"] <- "AL"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Rio Grande do Norte"] <- "RN"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Piauí"] <- "PI"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Amapá"] <- "AP"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Distrito Federal"] <- "DF"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Mato Grosso"] <- "MT"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Roraima"] <- "RR"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Sergipe"] <- "SE"
UF_APAC_2011$UF[UF_APAC_2011$UF == "Tocantins"] <- "TO"

UF_APAC_2012$UF[UF_APAC_2012$UF == "São Paulo"] <- "SP"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Rio Grande do Sul"] <- "RS"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Rio de Janeiro"] <- "RJ"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Minas Gerais"] <- "MG"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Santa Catarina"] <- "SC"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Paraná"] <- "PR"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Pernambuco"] <- "PE"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Amazonas"] <- "AM"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Espírito Santo"] <- "ES"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Goiás"] <- "GO"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Ceará"] <- "CE"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Pará"] <- "PA"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Acre"] <- "AC"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Bahia"] <- "BA"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Mato Grosso do Sul"] <- "MS"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Maranhão"] <- "MA"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Rondônia"] <- "RO"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Paraíba"] <- "PB"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Alagoas"] <- "AL"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Rio Grande do Norte"] <- "RN"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Piauí"] <- "PI"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Amapá"] <- "AP"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Distrito Federal"] <- "DF"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Mato Grosso"] <- "MT"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Roraima"] <- "RR"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Sergipe"] <- "SE"
UF_APAC_2012$UF[UF_APAC_2012$UF == "Tocantins"] <- "TO"


UF_APAC_2013$UF[UF_APAC_2013$UF == "São Paulo"] <- "SP"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Rio Grande do Sul"] <- "RS"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Rio de Janeiro"] <- "RJ"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Minas Gerais"] <- "MG"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Santa Catarina"] <- "SC"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Paraná"] <- "PR"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Pernambuco"] <- "PE"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Amazonas"] <- "AM"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Espírito Santo"] <- "ES"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Goiás"] <- "GO"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Ceará"] <- "CE"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Pará"] <- "PA"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Acre"] <- "AC"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Bahia"] <- "BA"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Mato Grosso do Sul"] <- "MS"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Maranhão"] <- "MA"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Rondônia"] <- "RO"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Paraíba"] <- "PB"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Alagoas"] <- "AL"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Rio Grande do Norte"] <- "RN"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Piauí"] <- "PI"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Amapá"] <- "AP"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Distrito Federal"] <- "DF"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Mato Grosso"] <- "MT"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Roraima"] <- "RR"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Sergipe"] <- "SE"
UF_APAC_2013$UF[UF_APAC_2013$UF == "Tocantins"] <- "TO"

UF_APAC_2014$UF[UF_APAC_2014$UF == "São Paulo"] <- "SP"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Rio Grande do Sul"] <- "RS"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Rio de Janeiro"] <- "RJ"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Minas Gerais"] <- "MG"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Santa Catarina"] <- "SC"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Paraná"] <- "PR"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Pernambuco"] <- "PE"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Amazonas"] <- "AM"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Espírito Santo"] <- "ES"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Goiás"] <- "GO"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Ceará"] <- "CE"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Pará"] <- "PA"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Acre"] <- "AC"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Bahia"] <- "BA"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Mato Grosso do Sul"] <- "MS"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Maranhão"] <- "MA"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Rondônia"] <- "RO"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Paraíba"] <- "PB"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Alagoas"] <- "AL"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Rio Grande do Norte"] <- "RN"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Piauí"] <- "PI"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Amapá"] <- "AP"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Distrito Federal"] <- "DF"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Mato Grosso"] <- "MT"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Roraima"] <- "RR"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Sergipe"] <- "SE"
UF_APAC_2014$UF[UF_APAC_2014$UF == "Tocantins"] <- "TO"


UF_APAC_2015$UF[UF_APAC_2015$UF == "São Paulo"] <- "SP"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Rio Grande do Sul"] <- "RS"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Rio de Janeiro"] <- "RJ"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Minas Gerais"] <- "MG"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Santa Catarina"] <- "SC"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Paraná"] <- "PR"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Pernambuco"] <- "PE"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Amazonas"] <- "AM"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Espírito Santo"] <- "ES"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Goiás"] <- "GO"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Ceará"] <- "CE"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Pará"] <- "PA"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Acre"] <- "AC"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Bahia"] <- "BA"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Mato Grosso do Sul"] <- "MS"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Maranhão"] <- "MA"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Rondônia"] <- "RO"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Paraíba"] <- "PB"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Alagoas"] <- "AL"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Rio Grande do Norte"] <- "RN"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Piauí"] <- "PI"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Amapá"] <- "AP"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Distrito Federal"] <- "DF"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Mato Grosso"] <- "MT"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Roraima"] <- "RR"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Sergipe"] <- "SE"
UF_APAC_2015$UF[UF_APAC_2015$UF == "Tocantins"] <- "TO"

UF_APAC_2016$UF[UF_APAC_2016$UF == "São Paulo"] <- "SP"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Rio Grande do Sul"] <- "RS"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Rio de Janeiro"] <- "RJ"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Minas Gerais"] <- "MG"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Santa Catarina"] <- "SC"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Paraná"] <- "PR"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Pernambuco"] <- "PE"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Amazonas"] <- "AM"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Espírito Santo"] <- "ES"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Goiás"] <- "GO"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Ceará"] <- "CE"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Pará"] <- "PA"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Acre"] <- "AC"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Bahia"] <- "BA"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Mato Grosso do Sul"] <- "MS"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Maranhão"] <- "MA"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Rondônia"] <- "RO"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Paraíba"] <- "PB"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Alagoas"] <- "AL"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Rio Grande do Norte"] <- "RN"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Piauí"] <- "PI"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Amapá"] <- "AP"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Distrito Federal"] <- "DF"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Mato Grosso"] <- "MT"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Roraima"] <- "RR"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Sergipe"] <- "SE"
UF_APAC_2016$UF[UF_APAC_2016$UF == "Tocantins"] <- "TO"

UF_APAC_2017$UF[UF_APAC_2017$UF == "São Paulo"] <- "SP"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Rio Grande do Sul"] <- "RS"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Rio de Janeiro"] <- "RJ"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Minas Gerais"] <- "MG"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Santa Catarina"] <- "SC"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Paraná"] <- "PR"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Pernambuco"] <- "PE"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Amazonas"] <- "AM"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Espírito Santo"] <- "ES"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Goiás"] <- "GO"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Ceará"] <- "CE"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Pará"] <- "PA"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Acre"] <- "AC"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Bahia"] <- "BA"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Mato Grosso do Sul"] <- "MS"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Maranhão"] <- "MA"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Rondônia"] <- "RO"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Paraíba"] <- "PB"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Alagoas"] <- "AL"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Rio Grande do Norte"] <- "RN"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Piauí"] <- "PI"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Amapá"] <- "AP"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Distrito Federal"] <- "DF"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Mato Grosso"] <- "MT"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Roraima"] <- "RR"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Sergipe"] <- "SE"
UF_APAC_2017$UF[UF_APAC_2017$UF == "Tocantins"] <- "TO"

UF_APAC_2018$UF[UF_APAC_2018$UF == "São Paulo"] <- "SP"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Rio Grande do Sul"] <- "RS"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Rio de Janeiro"] <- "RJ"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Minas Gerais"] <- "MG"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Santa Catarina"] <- "SC"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Paraná"] <- "PR"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Pernambuco"] <- "PE"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Amazonas"] <- "AM"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Espírito Santo"] <- "ES"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Goiás"] <- "GO"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Ceará"] <- "CE"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Pará"] <- "PA"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Acre"] <- "AC"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Bahia"] <- "BA"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Mato Grosso do Sul"] <- "MS"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Maranhão"] <- "MA"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Rondônia"] <- "RO"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Paraíba"] <- "PB"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Alagoas"] <- "AL"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Rio Grande do Norte"] <- "RN"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Piauí"] <- "PI"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Amapá"] <- "AP"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Distrito Federal"] <- "DF"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Mato Grosso"] <- "MT"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Roraima"] <- "RR"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Sergipe"] <- "SE"
UF_APAC_2018$UF[UF_APAC_2018$UF == "Tocantins"] <- "TO"

## rbind

UF_APAC_sc <- do.call("rbind", list(UF_APAC_2008, UF_APAC_2009, UF_APAC_2010, UF_APAC_2011, UF_APAC_2012, UF_APAC_2013, UF_APAC_2014, UF_APAC_2015, UF_APAC_2016, UF_APAC_2017, UF_APAC_2018))


## AIH

UF_AIH_2008$ano <- "2008" 
UF_AIH_2009$ano <- "2009" 
UF_AIH_2010$ano <- "2010" 
UF_AIH_2011$ano <- "2011" 
UF_AIH_2012$ano <- "2012" 
UF_AIH_2013$ano <- "2013" 
UF_AIH_2014$ano <- "2014"
UF_AIH_2015$ano <- "2015" 
UF_AIH_2016$ano <- "2016"
UF_AIH_2017$ano <- "2017"
UF_AIH_2018$ano <- "2018" 

UF_AIH_2008$UF[UF_AIH_2008$UF == "São Paulo"] <- "SP"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Rio Grande do Sul"] <- "RS"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Rio de Janeiro"] <- "RJ"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Minas Gerais"] <- "MG"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Santa Catarina"] <- "SC"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Paraná"] <- "PR"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Pernambuco"] <- "PE"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Amazonas"] <- "AM"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Espírito Santo"] <- "ES"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Goiás"] <- "GO"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Ceará"] <- "CE"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Pará"] <- "PA"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Acre"] <- "AC"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Bahia"] <- "BA"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Mato Grosso do Sul"] <- "MS"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Maranhão"] <- "MA"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Rondônia"] <- "RO"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Paraíba"] <- "PB"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Alagoas"] <- "AL"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Rio Grande do Norte"] <- "RN"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Piauí"] <- "PI"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Amapá"] <- "AP"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Distrito Federal"] <- "DF"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Mato Grosso"] <- "MT"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Roraima"] <- "RR"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Sergipe"] <- "SE"
UF_AIH_2008$UF[UF_AIH_2008$UF == "Tocantins"] <- "TO"

UF_AIH_2009$UF[UF_AIH_2009$UF == "São Paulo"] <- "SP"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Rio Grande do Sul"] <- "RS"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Rio de Janeiro"] <- "RJ"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Minas Gerais"] <- "MG"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Santa Catarina"] <- "SC"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Paraná"] <- "PR"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Pernambuco"] <- "PE"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Amazonas"] <- "AM"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Espírito Santo"] <- "ES"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Goiás"] <- "GO"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Ceará"] <- "CE"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Pará"] <- "PA"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Acre"] <- "AC"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Bahia"] <- "BA"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Mato Grosso do Sul"] <- "MS"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Maranhão"] <- "MA"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Rondônia"] <- "RO"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Paraíba"] <- "PB"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Alagoas"] <- "AL"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Rio Grande do Norte"] <- "RN"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Piauí"] <- "PI"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Amapá"] <- "AP"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Distrito Federal"] <- "DF"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Mato Grosso"] <- "MT"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Roraima"] <- "RR"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Sergipe"] <- "SE"
UF_AIH_2009$UF[UF_AIH_2009$UF == "Tocantins"] <- "TO"

UF_AIH_2010$UF[UF_AIH_2010$UF == "São Paulo"] <- "SP"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Rio Grande do Sul"] <- "RS"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Rio de Janeiro"] <- "RJ"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Minas Gerais"] <- "MG"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Santa Catarina"] <- "SC"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Paraná"] <- "PR"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Pernambuco"] <- "PE"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Amazonas"] <- "AM"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Espírito Santo"] <- "ES"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Goiás"] <- "GO"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Ceará"] <- "CE"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Pará"] <- "PA"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Acre"] <- "AC"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Bahia"] <- "BA"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Mato Grosso do Sul"] <- "MS"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Maranhão"] <- "MA"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Rondônia"] <- "RO"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Paraíba"] <- "PB"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Alagoas"] <- "AL"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Rio Grande do Norte"] <- "RN"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Piauí"] <- "PI"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Amapá"] <- "AP"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Distrito Federal"] <- "DF"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Mato Grosso"] <- "MT"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Roraima"] <- "RR"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Sergipe"] <- "SE"
UF_AIH_2010$UF[UF_AIH_2010$UF == "Tocantins"] <- "TO"

UF_AIH_2011$UF[UF_AIH_2011$UF == "São Paulo"] <- "SP"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Rio Grande do Sul"] <- "RS"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Rio de Janeiro"] <- "RJ"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Minas Gerais"] <- "MG"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Santa Catarina"] <- "SC"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Paraná"] <- "PR"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Pernambuco"] <- "PE"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Amazonas"] <- "AM"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Espírito Santo"] <- "ES"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Goiás"] <- "GO"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Ceará"] <- "CE"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Pará"] <- "PA"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Acre"] <- "AC"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Bahia"] <- "BA"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Mato Grosso do Sul"] <- "MS"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Maranhão"] <- "MA"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Rondônia"] <- "RO"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Paraíba"] <- "PB"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Alagoas"] <- "AL"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Rio Grande do Norte"] <- "RN"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Piauí"] <- "PI"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Amapá"] <- "AP"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Distrito Federal"] <- "DF"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Mato Grosso"] <- "MT"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Roraima"] <- "RR"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Sergipe"] <- "SE"
UF_AIH_2011$UF[UF_AIH_2011$UF == "Tocantins"] <- "TO"

UF_AIH_2012$UF[UF_AIH_2012$UF == "São Paulo"] <- "SP"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Rio Grande do Sul"] <- "RS"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Rio de Janeiro"] <- "RJ"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Minas Gerais"] <- "MG"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Santa Catarina"] <- "SC"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Paraná"] <- "PR"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Pernambuco"] <- "PE"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Amazonas"] <- "AM"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Espírito Santo"] <- "ES"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Goiás"] <- "GO"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Ceará"] <- "CE"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Pará"] <- "PA"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Acre"] <- "AC"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Bahia"] <- "BA"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Mato Grosso do Sul"] <- "MS"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Maranhão"] <- "MA"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Rondônia"] <- "RO"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Paraíba"] <- "PB"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Alagoas"] <- "AL"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Rio Grande do Norte"] <- "RN"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Piauí"] <- "PI"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Amapá"] <- "AP"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Distrito Federal"] <- "DF"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Mato Grosso"] <- "MT"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Roraima"] <- "RR"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Sergipe"] <- "SE"
UF_AIH_2012$UF[UF_AIH_2012$UF == "Tocantins"] <- "TO"


UF_AIH_2013$UF[UF_AIH_2013$UF == "São Paulo"] <- "SP"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Rio Grande do Sul"] <- "RS"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Rio de Janeiro"] <- "RJ"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Minas Gerais"] <- "MG"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Santa Catarina"] <- "SC"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Paraná"] <- "PR"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Pernambuco"] <- "PE"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Amazonas"] <- "AM"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Espírito Santo"] <- "ES"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Goiás"] <- "GO"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Ceará"] <- "CE"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Pará"] <- "PA"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Acre"] <- "AC"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Bahia"] <- "BA"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Mato Grosso do Sul"] <- "MS"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Maranhão"] <- "MA"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Rondônia"] <- "RO"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Paraíba"] <- "PB"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Alagoas"] <- "AL"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Rio Grande do Norte"] <- "RN"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Piauí"] <- "PI"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Amapá"] <- "AP"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Distrito Federal"] <- "DF"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Mato Grosso"] <- "MT"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Roraima"] <- "RR"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Sergipe"] <- "SE"
UF_AIH_2013$UF[UF_AIH_2013$UF == "Tocantins"] <- "TO"

UF_AIH_2014$UF[UF_AIH_2014$UF == "São Paulo"] <- "SP"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Rio Grande do Sul"] <- "RS"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Rio de Janeiro"] <- "RJ"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Minas Gerais"] <- "MG"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Santa Catarina"] <- "SC"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Paraná"] <- "PR"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Pernambuco"] <- "PE"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Amazonas"] <- "AM"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Espírito Santo"] <- "ES"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Goiás"] <- "GO"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Ceará"] <- "CE"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Pará"] <- "PA"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Acre"] <- "AC"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Bahia"] <- "BA"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Mato Grosso do Sul"] <- "MS"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Maranhão"] <- "MA"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Rondônia"] <- "RO"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Paraíba"] <- "PB"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Alagoas"] <- "AL"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Rio Grande do Norte"] <- "RN"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Piauí"] <- "PI"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Amapá"] <- "AP"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Distrito Federal"] <- "DF"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Mato Grosso"] <- "MT"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Roraima"] <- "RR"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Sergipe"] <- "SE"
UF_AIH_2014$UF[UF_AIH_2014$UF == "Tocantins"] <- "TO"


UF_AIH_2015$UF[UF_AIH_2015$UF == "São Paulo"] <- "SP"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Rio Grande do Sul"] <- "RS"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Rio de Janeiro"] <- "RJ"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Minas Gerais"] <- "MG"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Santa Catarina"] <- "SC"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Paraná"] <- "PR"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Pernambuco"] <- "PE"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Amazonas"] <- "AM"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Espírito Santo"] <- "ES"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Goiás"] <- "GO"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Ceará"] <- "CE"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Pará"] <- "PA"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Acre"] <- "AC"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Bahia"] <- "BA"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Mato Grosso do Sul"] <- "MS"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Maranhão"] <- "MA"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Rondônia"] <- "RO"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Paraíba"] <- "PB"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Alagoas"] <- "AL"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Rio Grande do Norte"] <- "RN"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Piauí"] <- "PI"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Amapá"] <- "AP"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Distrito Federal"] <- "DF"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Mato Grosso"] <- "MT"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Roraima"] <- "RR"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Sergipe"] <- "SE"
UF_AIH_2015$UF[UF_AIH_2015$UF == "Tocantins"] <- "TO"

UF_AIH_2016$UF[UF_AIH_2016$UF == "São Paulo"] <- "SP"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Rio Grande do Sul"] <- "RS"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Rio de Janeiro"] <- "RJ"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Minas Gerais"] <- "MG"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Santa Catarina"] <- "SC"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Paraná"] <- "PR"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Pernambuco"] <- "PE"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Amazonas"] <- "AM"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Espírito Santo"] <- "ES"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Goiás"] <- "GO"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Ceará"] <- "CE"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Pará"] <- "PA"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Acre"] <- "AC"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Bahia"] <- "BA"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Mato Grosso do Sul"] <- "MS"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Maranhão"] <- "MA"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Rondônia"] <- "RO"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Paraíba"] <- "PB"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Alagoas"] <- "AL"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Rio Grande do Norte"] <- "RN"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Piauí"] <- "PI"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Amapá"] <- "AP"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Distrito Federal"] <- "DF"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Mato Grosso"] <- "MT"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Roraima"] <- "RR"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Sergipe"] <- "SE"
UF_AIH_2016$UF[UF_AIH_2016$UF == "Tocantins"] <- "TO"

UF_AIH_2017$UF[UF_AIH_2017$UF == "São Paulo"] <- "SP"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Rio Grande do Sul"] <- "RS"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Rio de Janeiro"] <- "RJ"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Minas Gerais"] <- "MG"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Santa Catarina"] <- "SC"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Paraná"] <- "PR"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Pernambuco"] <- "PE"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Amazonas"] <- "AM"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Espírito Santo"] <- "ES"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Goiás"] <- "GO"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Ceará"] <- "CE"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Pará"] <- "PA"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Acre"] <- "AC"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Bahia"] <- "BA"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Mato Grosso do Sul"] <- "MS"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Maranhão"] <- "MA"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Rondônia"] <- "RO"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Paraíba"] <- "PB"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Alagoas"] <- "AL"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Rio Grande do Norte"] <- "RN"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Piauí"] <- "PI"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Amapá"] <- "AP"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Distrito Federal"] <- "DF"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Mato Grosso"] <- "MT"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Roraima"] <- "RR"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Sergipe"] <- "SE"
UF_AIH_2017$UF[UF_AIH_2017$UF == "Tocantins"] <- "TO"

UF_AIH_2018$UF[UF_AIH_2018$UF == "São Paulo"] <- "SP"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Rio Grande do Sul"] <- "RS"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Rio de Janeiro"] <- "RJ"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Minas Gerais"] <- "MG"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Santa Catarina"] <- "SC"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Paraná"] <- "PR"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Pernambuco"] <- "PE"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Amazonas"] <- "AM"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Espírito Santo"] <- "ES"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Goiás"] <- "GO"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Ceará"] <- "CE"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Pará"] <- "PA"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Acre"] <- "AC"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Bahia"] <- "BA"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Mato Grosso do Sul"] <- "MS"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Maranhão"] <- "MA"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Rondônia"] <- "RO"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Paraíba"] <- "PB"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Alagoas"] <- "AL"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Rio Grande do Norte"] <- "RN"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Piauí"] <- "PI"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Amapá"] <- "AP"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Distrito Federal"] <- "DF"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Mato Grosso"] <- "MT"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Roraima"] <- "RR"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Sergipe"] <- "SE"
UF_AIH_2018$UF[UF_AIH_2018$UF == "Tocantins"] <- "TO"

## rbind

UF_AIH_sc <- do.call("rbind", list(UF_AIH_2008, UF_AIH_2009, UF_AIH_2010, UF_AIH_2011, UF_AIH_2012, UF_AIH_2013, UF_AIH_2014, UF_AIH_2015, UF_AIH_2016, UF_AIH_2017, UF_AIH_2018))

## BPAI

UF_BPAI_2008$ano <- "2008" 
UF_BPAI_2009$ano <- "2009" 
UF_BPAI_2010$ano <- "2010" 
UF_BPAI_2011$ano <- "2011" 
UF_BPAI_2012$ano <- "2012" 
UF_BPAI_2013$ano <- "2013" 
UF_BPAI_2014$ano <- "2014"
UF_BPAI_2015$ano <- "2015" 
UF_BPAI_2016$ano <- "2016"
UF_BPAI_2017$ano <- "2017"
UF_BPAI_2018$ano <- "2018" 


UF_BPAI_2008$UF[UF_BPAI_2008$UF == "São Paulo"] <- "SP"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Rio Grande do Sul"] <- "RS"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Rio de Janeiro"] <- "RJ"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Minas Gerais"] <- "MG"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Santa Catarina"] <- "SC"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Paraná"] <- "PR"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Pernambuco"] <- "PE"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Amazonas"] <- "AM"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Espírito Santo"] <- "ES"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Goiás"] <- "GO"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Ceará"] <- "CE"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Pará"] <- "PA"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Acre"] <- "AC"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Bahia"] <- "BA"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Mato Grosso do Sul"] <- "MS"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Maranhão"] <- "MA"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Rondônia"] <- "RO"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Paraíba"] <- "PB"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Alagoas"] <- "AL"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Rio Grande do Norte"] <- "RN"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Piauí"] <- "PI"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Amapá"] <- "AP"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Distrito Federal"] <- "DF"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Mato Grosso"] <- "MT"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Roraima"] <- "RR"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Sergipe"] <- "SE"
UF_BPAI_2008$UF[UF_BPAI_2008$UF == "Tocantins"] <- "TO"

UF_BPAI_2009$UF[UF_BPAI_2009$UF == "São Paulo"] <- "SP"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Rio Grande do Sul"] <- "RS"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Rio de Janeiro"] <- "RJ"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Minas Gerais"] <- "MG"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Santa Catarina"] <- "SC"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Paraná"] <- "PR"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Pernambuco"] <- "PE"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Amazonas"] <- "AM"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Espírito Santo"] <- "ES"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Goiás"] <- "GO"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Ceará"] <- "CE"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Pará"] <- "PA"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Acre"] <- "AC"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Bahia"] <- "BA"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Mato Grosso do Sul"] <- "MS"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Maranhão"] <- "MA"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Rondônia"] <- "RO"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Paraíba"] <- "PB"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Alagoas"] <- "AL"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Rio Grande do Norte"] <- "RN"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Piauí"] <- "PI"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Amapá"] <- "AP"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Distrito Federal"] <- "DF"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Mato Grosso"] <- "MT"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Roraima"] <- "RR"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Sergipe"] <- "SE"
UF_BPAI_2009$UF[UF_BPAI_2009$UF == "Tocantins"] <- "TO"

UF_BPAI_2010$UF[UF_BPAI_2010$UF == "São Paulo"] <- "SP"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Rio Grande do Sul"] <- "RS"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Rio de Janeiro"] <- "RJ"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Minas Gerais"] <- "MG"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Santa Catarina"] <- "SC"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Paraná"] <- "PR"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Pernambuco"] <- "PE"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Amazonas"] <- "AM"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Espírito Santo"] <- "ES"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Goiás"] <- "GO"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Ceará"] <- "CE"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Pará"] <- "PA"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Acre"] <- "AC"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Bahia"] <- "BA"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Mato Grosso do Sul"] <- "MS"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Maranhão"] <- "MA"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Rondônia"] <- "RO"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Paraíba"] <- "PB"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Alagoas"] <- "AL"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Rio Grande do Norte"] <- "RN"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Piauí"] <- "PI"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Amapá"] <- "AP"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Distrito Federal"] <- "DF"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Mato Grosso"] <- "MT"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Roraima"] <- "RR"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Sergipe"] <- "SE"
UF_BPAI_2010$UF[UF_BPAI_2010$UF == "Tocantins"] <- "TO"

UF_BPAI_2011$UF[UF_BPAI_2011$UF == "São Paulo"] <- "SP"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Rio Grande do Sul"] <- "RS"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Rio de Janeiro"] <- "RJ"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Minas Gerais"] <- "MG"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Santa Catarina"] <- "SC"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Paraná"] <- "PR"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Pernambuco"] <- "PE"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Amazonas"] <- "AM"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Espírito Santo"] <- "ES"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Goiás"] <- "GO"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Ceará"] <- "CE"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Pará"] <- "PA"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Acre"] <- "AC"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Bahia"] <- "BA"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Mato Grosso do Sul"] <- "MS"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Maranhão"] <- "MA"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Rondônia"] <- "RO"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Paraíba"] <- "PB"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Alagoas"] <- "AL"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Rio Grande do Norte"] <- "RN"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Piauí"] <- "PI"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Amapá"] <- "AP"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Distrito Federal"] <- "DF"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Mato Grosso"] <- "MT"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Roraima"] <- "RR"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Sergipe"] <- "SE"
UF_BPAI_2011$UF[UF_BPAI_2011$UF == "Tocantins"] <- "TO"

UF_BPAI_2012$UF[UF_BPAI_2012$UF == "São Paulo"] <- "SP"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Rio Grande do Sul"] <- "RS"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Rio de Janeiro"] <- "RJ"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Minas Gerais"] <- "MG"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Santa Catarina"] <- "SC"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Paraná"] <- "PR"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Pernambuco"] <- "PE"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Amazonas"] <- "AM"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Espírito Santo"] <- "ES"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Goiás"] <- "GO"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Ceará"] <- "CE"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Pará"] <- "PA"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Acre"] <- "AC"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Bahia"] <- "BA"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Mato Grosso do Sul"] <- "MS"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Maranhão"] <- "MA"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Rondônia"] <- "RO"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Paraíba"] <- "PB"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Alagoas"] <- "AL"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Rio Grande do Norte"] <- "RN"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Piauí"] <- "PI"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Amapá"] <- "AP"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Distrito Federal"] <- "DF"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Mato Grosso"] <- "MT"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Roraima"] <- "RR"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Sergipe"] <- "SE"
UF_BPAI_2012$UF[UF_BPAI_2012$UF == "Tocantins"] <- "TO"


UF_BPAI_2013$UF[UF_BPAI_2013$UF == "São Paulo"] <- "SP"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Rio Grande do Sul"] <- "RS"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Rio de Janeiro"] <- "RJ"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Minas Gerais"] <- "MG"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Santa Catarina"] <- "SC"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Paraná"] <- "PR"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Pernambuco"] <- "PE"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Amazonas"] <- "AM"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Espírito Santo"] <- "ES"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Goiás"] <- "GO"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Ceará"] <- "CE"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Pará"] <- "PA"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Acre"] <- "AC"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Bahia"] <- "BA"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Mato Grosso do Sul"] <- "MS"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Maranhão"] <- "MA"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Rondônia"] <- "RO"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Paraíba"] <- "PB"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Alagoas"] <- "AL"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Rio Grande do Norte"] <- "RN"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Piauí"] <- "PI"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Amapá"] <- "AP"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Distrito Federal"] <- "DF"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Mato Grosso"] <- "MT"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Roraima"] <- "RR"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Sergipe"] <- "SE"
UF_BPAI_2013$UF[UF_BPAI_2013$UF == "Tocantins"] <- "TO"

UF_BPAI_2014$UF[UF_BPAI_2014$UF == "São Paulo"] <- "SP"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Rio Grande do Sul"] <- "RS"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Rio de Janeiro"] <- "RJ"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Minas Gerais"] <- "MG"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Santa Catarina"] <- "SC"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Paraná"] <- "PR"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Pernambuco"] <- "PE"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Amazonas"] <- "AM"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Espírito Santo"] <- "ES"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Goiás"] <- "GO"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Ceará"] <- "CE"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Pará"] <- "PA"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Acre"] <- "AC"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Bahia"] <- "BA"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Mato Grosso do Sul"] <- "MS"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Maranhão"] <- "MA"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Rondônia"] <- "RO"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Paraíba"] <- "PB"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Alagoas"] <- "AL"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Rio Grande do Norte"] <- "RN"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Piauí"] <- "PI"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Amapá"] <- "AP"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Distrito Federal"] <- "DF"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Mato Grosso"] <- "MT"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Roraima"] <- "RR"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Sergipe"] <- "SE"
UF_BPAI_2014$UF[UF_BPAI_2014$UF == "Tocantins"] <- "TO"


UF_BPAI_2015$UF[UF_BPAI_2015$UF == "São Paulo"] <- "SP"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Rio Grande do Sul"] <- "RS"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Rio de Janeiro"] <- "RJ"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Minas Gerais"] <- "MG"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Santa Catarina"] <- "SC"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Paraná"] <- "PR"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Pernambuco"] <- "PE"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Amazonas"] <- "AM"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Espírito Santo"] <- "ES"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Goiás"] <- "GO"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Ceará"] <- "CE"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Pará"] <- "PA"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Acre"] <- "AC"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Bahia"] <- "BA"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Mato Grosso do Sul"] <- "MS"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Maranhão"] <- "MA"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Rondônia"] <- "RO"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Paraíba"] <- "PB"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Alagoas"] <- "AL"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Rio Grande do Norte"] <- "RN"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Piauí"] <- "PI"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Amapá"] <- "AP"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Distrito Federal"] <- "DF"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Mato Grosso"] <- "MT"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Roraima"] <- "RR"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Sergipe"] <- "SE"
UF_BPAI_2015$UF[UF_BPAI_2015$UF == "Tocantins"] <- "TO"

UF_BPAI_2016$UF[UF_BPAI_2016$UF == "São Paulo"] <- "SP"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Rio Grande do Sul"] <- "RS"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Rio de Janeiro"] <- "RJ"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Minas Gerais"] <- "MG"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Santa Catarina"] <- "SC"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Paraná"] <- "PR"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Pernambuco"] <- "PE"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Amazonas"] <- "AM"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Espírito Santo"] <- "ES"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Goiás"] <- "GO"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Ceará"] <- "CE"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Pará"] <- "PA"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Acre"] <- "AC"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Bahia"] <- "BA"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Mato Grosso do Sul"] <- "MS"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Maranhão"] <- "MA"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Rondônia"] <- "RO"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Paraíba"] <- "PB"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Alagoas"] <- "AL"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Rio Grande do Norte"] <- "RN"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Piauí"] <- "PI"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Amapá"] <- "AP"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Distrito Federal"] <- "DF"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Mato Grosso"] <- "MT"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Roraima"] <- "RR"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Sergipe"] <- "SE"
UF_BPAI_2016$UF[UF_BPAI_2016$UF == "Tocantins"] <- "TO"

UF_BPAI_2017$UF[UF_BPAI_2017$UF == "São Paulo"] <- "SP"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Rio Grande do Sul"] <- "RS"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Rio de Janeiro"] <- "RJ"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Minas Gerais"] <- "MG"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Santa Catarina"] <- "SC"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Paraná"] <- "PR"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Pernambuco"] <- "PE"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Amazonas"] <- "AM"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Espírito Santo"] <- "ES"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Goiás"] <- "GO"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Ceará"] <- "CE"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Pará"] <- "PA"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Acre"] <- "AC"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Bahia"] <- "BA"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Mato Grosso do Sul"] <- "MS"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Maranhão"] <- "MA"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Rondônia"] <- "RO"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Paraíba"] <- "PB"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Alagoas"] <- "AL"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Rio Grande do Norte"] <- "RN"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Piauí"] <- "PI"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Amapá"] <- "AP"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Distrito Federal"] <- "DF"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Mato Grosso"] <- "MT"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Roraima"] <- "RR"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Sergipe"] <- "SE"
UF_BPAI_2017$UF[UF_BPAI_2017$UF == "Tocantins"] <- "TO"

UF_BPAI_2018$UF[UF_BPAI_2018$UF == "São Paulo"] <- "SP"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Rio Grande do Sul"] <- "RS"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Rio de Janeiro"] <- "RJ"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Minas Gerais"] <- "MG"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Santa Catarina"] <- "SC"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Paraná"] <- "PR"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Pernambuco"] <- "PE"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Amazonas"] <- "AM"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Espírito Santo"] <- "ES"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Goiás"] <- "GO"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Ceará"] <- "CE"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Pará"] <- "PA"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Acre"] <- "AC"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Bahia"] <- "BA"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Mato Grosso do Sul"] <- "MS"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Maranhão"] <- "MA"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Rondônia"] <- "RO"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Paraíba"] <- "PB"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Alagoas"] <- "AL"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Rio Grande do Norte"] <- "RN"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Piauí"] <- "PI"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Amapá"] <- "AP"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Distrito Federal"] <- "DF"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Mato Grosso"] <- "MT"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Roraima"] <- "RR"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Sergipe"] <- "SE"
UF_BPAI_2018$UF[UF_BPAI_2018$UF == "Tocantins"] <- "TO"


## rbind

UF_BPAI_sc <- do.call("rbind", list(UF_BPAI_2008, UF_BPAI_2009, UF_BPAI_2010, UF_BPAI_2011, UF_BPAI_2012, UF_BPAI_2013, UF_BPAI_2014, UF_BPAI_2015, UF_BPAI_2016, UF_BPAI_2017, UF_BPAI_2018))

## GAL

UF_GAL_2008$ano <- "2008" 
UF_GAL_2009$ano <- "2009" 
UF_GAL_2010$ano <- "2010" 
UF_GAL_2011$ano <- "2011" 
UF_GAL_2012$ano <- "2012" 
UF_GAL_2013$ano <- "2013" 
UF_GAL_2014$ano <- "2014"
UF_GAL_2015$ano <- "2015" 
UF_GAL_2016$ano <- "2016"
UF_GAL_2017$ano <- "2017"
UF_GAL_2018$ano <- "2018" 

UF_GAL_2008$UF[UF_GAL_2008$UF == "São Paulo"] <- "SP"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Rio Grande do Sul"] <- "RS"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Rio de Janeiro"] <- "RJ"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Minas Gerais"] <- "MG"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Santa Catarina"] <- "SC"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Paraná"] <- "PR"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Pernambuco"] <- "PE"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Amazonas"] <- "AM"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Espírito Santo"] <- "ES"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Goiás"] <- "GO"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Ceará"] <- "CE"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Pará"] <- "PA"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Acre"] <- "AC"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Bahia"] <- "BA"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Mato Grosso do Sul"] <- "MS"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Maranhão"] <- "MA"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Rondônia"] <- "RO"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Paraíba"] <- "PB"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Alagoas"] <- "AL"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Rio Grande do Norte"] <- "RN"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Piauí"] <- "PI"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Amapá"] <- "AP"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Distrito Federal"] <- "DF"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Mato Grosso"] <- "MT"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Roraima"] <- "RR"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Sergipe"] <- "SE"
UF_GAL_2008$UF[UF_GAL_2008$UF == "Tocantins"] <- "TO"

UF_GAL_2009$UF[UF_GAL_2009$UF == "São Paulo"] <- "SP"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Rio Grande do Sul"] <- "RS"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Rio de Janeiro"] <- "RJ"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Minas Gerais"] <- "MG"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Santa Catarina"] <- "SC"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Paraná"] <- "PR"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Pernambuco"] <- "PE"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Amazonas"] <- "AM"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Espírito Santo"] <- "ES"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Goiás"] <- "GO"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Ceará"] <- "CE"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Pará"] <- "PA"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Acre"] <- "AC"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Bahia"] <- "BA"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Mato Grosso do Sul"] <- "MS"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Maranhão"] <- "MA"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Rondônia"] <- "RO"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Paraíba"] <- "PB"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Alagoas"] <- "AL"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Rio Grande do Norte"] <- "RN"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Piauí"] <- "PI"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Amapá"] <- "AP"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Distrito Federal"] <- "DF"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Mato Grosso"] <- "MT"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Roraima"] <- "RR"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Sergipe"] <- "SE"
UF_GAL_2009$UF[UF_GAL_2009$UF == "Tocantins"] <- "TO"

UF_GAL_2010$UF[UF_GAL_2010$UF == "São Paulo"] <- "SP"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Rio Grande do Sul"] <- "RS"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Rio de Janeiro"] <- "RJ"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Minas Gerais"] <- "MG"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Santa Catarina"] <- "SC"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Paraná"] <- "PR"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Pernambuco"] <- "PE"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Amazonas"] <- "AM"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Espírito Santo"] <- "ES"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Goiás"] <- "GO"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Ceará"] <- "CE"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Pará"] <- "PA"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Acre"] <- "AC"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Bahia"] <- "BA"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Mato Grosso do Sul"] <- "MS"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Maranhão"] <- "MA"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Rondônia"] <- "RO"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Paraíba"] <- "PB"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Alagoas"] <- "AL"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Rio Grande do Norte"] <- "RN"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Piauí"] <- "PI"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Amapá"] <- "AP"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Distrito Federal"] <- "DF"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Mato Grosso"] <- "MT"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Roraima"] <- "RR"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Sergipe"] <- "SE"
UF_GAL_2010$UF[UF_GAL_2010$UF == "Tocantins"] <- "TO"

UF_GAL_2011$UF[UF_GAL_2011$UF == "São Paulo"] <- "SP"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Rio Grande do Sul"] <- "RS"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Rio de Janeiro"] <- "RJ"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Minas Gerais"] <- "MG"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Santa Catarina"] <- "SC"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Paraná"] <- "PR"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Pernambuco"] <- "PE"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Amazonas"] <- "AM"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Espírito Santo"] <- "ES"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Goiás"] <- "GO"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Ceará"] <- "CE"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Pará"] <- "PA"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Acre"] <- "AC"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Bahia"] <- "BA"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Mato Grosso do Sul"] <- "MS"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Maranhão"] <- "MA"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Rondônia"] <- "RO"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Paraíba"] <- "PB"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Alagoas"] <- "AL"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Rio Grande do Norte"] <- "RN"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Piauí"] <- "PI"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Amapá"] <- "AP"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Distrito Federal"] <- "DF"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Mato Grosso"] <- "MT"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Roraima"] <- "RR"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Sergipe"] <- "SE"
UF_GAL_2011$UF[UF_GAL_2011$UF == "Tocantins"] <- "TO"

UF_GAL_2012$UF[UF_GAL_2012$UF == "São Paulo"] <- "SP"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Rio Grande do Sul"] <- "RS"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Rio de Janeiro"] <- "RJ"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Minas Gerais"] <- "MG"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Santa Catarina"] <- "SC"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Paraná"] <- "PR"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Pernambuco"] <- "PE"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Amazonas"] <- "AM"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Espírito Santo"] <- "ES"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Goiás"] <- "GO"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Ceará"] <- "CE"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Pará"] <- "PA"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Acre"] <- "AC"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Bahia"] <- "BA"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Mato Grosso do Sul"] <- "MS"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Maranhão"] <- "MA"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Rondônia"] <- "RO"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Paraíba"] <- "PB"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Alagoas"] <- "AL"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Rio Grande do Norte"] <- "RN"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Piauí"] <- "PI"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Amapá"] <- "AP"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Distrito Federal"] <- "DF"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Mato Grosso"] <- "MT"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Roraima"] <- "RR"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Sergipe"] <- "SE"
UF_GAL_2012$UF[UF_GAL_2012$UF == "Tocantins"] <- "TO"


UF_GAL_2013$UF[UF_GAL_2013$UF == "São Paulo"] <- "SP"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Rio Grande do Sul"] <- "RS"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Rio de Janeiro"] <- "RJ"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Minas Gerais"] <- "MG"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Santa Catarina"] <- "SC"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Paraná"] <- "PR"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Pernambuco"] <- "PE"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Amazonas"] <- "AM"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Espírito Santo"] <- "ES"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Goiás"] <- "GO"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Ceará"] <- "CE"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Pará"] <- "PA"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Acre"] <- "AC"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Bahia"] <- "BA"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Mato Grosso do Sul"] <- "MS"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Maranhão"] <- "MA"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Rondônia"] <- "RO"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Paraíba"] <- "PB"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Alagoas"] <- "AL"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Rio Grande do Norte"] <- "RN"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Piauí"] <- "PI"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Amapá"] <- "AP"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Distrito Federal"] <- "DF"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Mato Grosso"] <- "MT"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Roraima"] <- "RR"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Sergipe"] <- "SE"
UF_GAL_2013$UF[UF_GAL_2013$UF == "Tocantins"] <- "TO"

UF_GAL_2014$UF[UF_GAL_2014$UF == "São Paulo"] <- "SP"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Rio Grande do Sul"] <- "RS"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Rio de Janeiro"] <- "RJ"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Minas Gerais"] <- "MG"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Santa Catarina"] <- "SC"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Paraná"] <- "PR"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Pernambuco"] <- "PE"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Amazonas"] <- "AM"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Espírito Santo"] <- "ES"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Goiás"] <- "GO"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Ceará"] <- "CE"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Pará"] <- "PA"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Acre"] <- "AC"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Bahia"] <- "BA"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Mato Grosso do Sul"] <- "MS"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Maranhão"] <- "MA"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Rondônia"] <- "RO"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Paraíba"] <- "PB"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Alagoas"] <- "AL"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Rio Grande do Norte"] <- "RN"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Piauí"] <- "PI"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Amapá"] <- "AP"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Distrito Federal"] <- "DF"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Mato Grosso"] <- "MT"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Roraima"] <- "RR"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Sergipe"] <- "SE"
UF_GAL_2014$UF[UF_GAL_2014$UF == "Tocantins"] <- "TO"


UF_GAL_2015$UF[UF_GAL_2015$UF == "São Paulo"] <- "SP"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Rio Grande do Sul"] <- "RS"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Rio de Janeiro"] <- "RJ"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Minas Gerais"] <- "MG"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Santa Catarina"] <- "SC"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Paraná"] <- "PR"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Pernambuco"] <- "PE"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Amazonas"] <- "AM"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Espírito Santo"] <- "ES"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Goiás"] <- "GO"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Ceará"] <- "CE"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Pará"] <- "PA"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Acre"] <- "AC"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Bahia"] <- "BA"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Mato Grosso do Sul"] <- "MS"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Maranhão"] <- "MA"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Rondônia"] <- "RO"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Paraíba"] <- "PB"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Alagoas"] <- "AL"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Rio Grande do Norte"] <- "RN"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Piauí"] <- "PI"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Amapá"] <- "AP"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Distrito Federal"] <- "DF"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Mato Grosso"] <- "MT"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Roraima"] <- "RR"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Sergipe"] <- "SE"
UF_GAL_2015$UF[UF_GAL_2015$UF == "Tocantins"] <- "TO"

UF_GAL_2016$UF[UF_GAL_2016$UF == "São Paulo"] <- "SP"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Rio Grande do Sul"] <- "RS"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Rio de Janeiro"] <- "RJ"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Minas Gerais"] <- "MG"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Santa Catarina"] <- "SC"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Paraná"] <- "PR"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Pernambuco"] <- "PE"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Amazonas"] <- "AM"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Espírito Santo"] <- "ES"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Goiás"] <- "GO"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Ceará"] <- "CE"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Pará"] <- "PA"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Acre"] <- "AC"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Bahia"] <- "BA"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Mato Grosso do Sul"] <- "MS"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Maranhão"] <- "MA"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Rondônia"] <- "RO"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Paraíba"] <- "PB"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Alagoas"] <- "AL"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Rio Grande do Norte"] <- "RN"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Piauí"] <- "PI"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Amapá"] <- "AP"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Distrito Federal"] <- "DF"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Mato Grosso"] <- "MT"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Roraima"] <- "RR"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Sergipe"] <- "SE"
UF_GAL_2016$UF[UF_GAL_2016$UF == "Tocantins"] <- "TO"

UF_GAL_2017$UF[UF_GAL_2017$UF == "São Paulo"] <- "SP"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Rio Grande do Sul"] <- "RS"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Rio de Janeiro"] <- "RJ"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Minas Gerais"] <- "MG"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Santa Catarina"] <- "SC"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Paraná"] <- "PR"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Pernambuco"] <- "PE"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Amazonas"] <- "AM"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Espírito Santo"] <- "ES"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Goiás"] <- "GO"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Ceará"] <- "CE"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Pará"] <- "PA"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Acre"] <- "AC"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Bahia"] <- "BA"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Mato Grosso do Sul"] <- "MS"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Maranhão"] <- "MA"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Rondônia"] <- "RO"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Paraíba"] <- "PB"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Alagoas"] <- "AL"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Rio Grande do Norte"] <- "RN"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Piauí"] <- "PI"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Amapá"] <- "AP"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Distrito Federal"] <- "DF"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Mato Grosso"] <- "MT"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Roraima"] <- "RR"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Sergipe"] <- "SE"
UF_GAL_2017$UF[UF_GAL_2017$UF == "Tocantins"] <- "TO"

UF_GAL_2018$UF[UF_GAL_2018$UF == "São Paulo"] <- "SP"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Rio Grande do Sul"] <- "RS"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Rio de Janeiro"] <- "RJ"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Minas Gerais"] <- "MG"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Santa Catarina"] <- "SC"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Paraná"] <- "PR"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Pernambuco"] <- "PE"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Amazonas"] <- "AM"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Espírito Santo"] <- "ES"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Goiás"] <- "GO"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Ceará"] <- "CE"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Pará"] <- "PA"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Acre"] <- "AC"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Bahia"] <- "BA"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Mato Grosso do Sul"] <- "MS"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Maranhão"] <- "MA"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Rondônia"] <- "RO"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Paraíba"] <- "PB"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Alagoas"] <- "AL"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Rio Grande do Norte"] <- "RN"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Piauí"] <- "PI"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Amapá"] <- "AP"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Distrito Federal"] <- "DF"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Mato Grosso"] <- "MT"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Roraima"] <- "RR"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Sergipe"] <- "SE"
UF_GAL_2018$UF[UF_GAL_2018$UF == "Tocantins"] <- "TO"

## rbind

UF_GAL_sc <- do.call("rbind", list(UF_GAL_2008, UF_GAL_2009, UF_GAL_2010, UF_GAL_2011, UF_GAL_2012, UF_GAL_2013, UF_GAL_2014, UF_GAL_2015, UF_GAL_2016, UF_GAL_2017, UF_GAL_2018))

## SINAN

UF_SINAN_2008$ano <- "2008" 
UF_SINAN_2009$ano <- "2009" 
UF_SINAN_2010$ano <- "2010" 
UF_SINAN_2011$ano <- "2011" 
UF_SINAN_2012$ano <- "2012" 
UF_SINAN_2013$ano <- "2013" 
UF_SINAN_2014$ano <- "2014"
UF_SINAN_2015$ano <- "2015" 
UF_SINAN_2016$ano <- "2016"
UF_SINAN_2017$ano <- "2017"
UF_SINAN_2018$ano <- "2018" 

UF_SINAN_2008$UF[UF_SINAN_2008$UF == "São Paulo"] <- "SP"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Rio Grande do Sul"] <- "RS"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Rio de Janeiro"] <- "RJ"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Minas Gerais"] <- "MG"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Santa Catarina"] <- "SC"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Paraná"] <- "PR"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Pernambuco"] <- "PE"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Amazonas"] <- "AM"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Espírito Santo"] <- "ES"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Goiás"] <- "GO"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Ceará"] <- "CE"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Pará"] <- "PA"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Acre"] <- "AC"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Bahia"] <- "BA"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Mato Grosso do Sul"] <- "MS"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Maranhão"] <- "MA"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Rondônia"] <- "RO"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Paraíba"] <- "PB"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Alagoas"] <- "AL"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Rio Grande do Norte"] <- "RN"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Piauí"] <- "PI"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Amapá"] <- "AP"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Distrito Federal"] <- "DF"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Mato Grosso"] <- "MT"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Roraima"] <- "RR"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Sergipe"] <- "SE"
UF_SINAN_2008$UF[UF_SINAN_2008$UF == "Tocantins"] <- "TO"

UF_SINAN_2009$UF[UF_SINAN_2009$UF == "São Paulo"] <- "SP"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Rio Grande do Sul"] <- "RS"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Rio de Janeiro"] <- "RJ"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Minas Gerais"] <- "MG"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Santa Catarina"] <- "SC"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Paraná"] <- "PR"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Pernambuco"] <- "PE"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Amazonas"] <- "AM"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Espírito Santo"] <- "ES"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Goiás"] <- "GO"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Ceará"] <- "CE"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Pará"] <- "PA"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Acre"] <- "AC"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Bahia"] <- "BA"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Mato Grosso do Sul"] <- "MS"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Maranhão"] <- "MA"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Rondônia"] <- "RO"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Paraíba"] <- "PB"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Alagoas"] <- "AL"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Rio Grande do Norte"] <- "RN"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Piauí"] <- "PI"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Amapá"] <- "AP"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Distrito Federal"] <- "DF"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Mato Grosso"] <- "MT"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Roraima"] <- "RR"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Sergipe"] <- "SE"
UF_SINAN_2009$UF[UF_SINAN_2009$UF == "Tocantins"] <- "TO"

UF_SINAN_2010$UF[UF_SINAN_2010$UF == "São Paulo"] <- "SP"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Rio Grande do Sul"] <- "RS"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Rio de Janeiro"] <- "RJ"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Minas Gerais"] <- "MG"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Santa Catarina"] <- "SC"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Paraná"] <- "PR"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Pernambuco"] <- "PE"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Amazonas"] <- "AM"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Espírito Santo"] <- "ES"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Goiás"] <- "GO"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Ceará"] <- "CE"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Pará"] <- "PA"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Acre"] <- "AC"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Bahia"] <- "BA"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Mato Grosso do Sul"] <- "MS"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Maranhão"] <- "MA"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Rondônia"] <- "RO"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Paraíba"] <- "PB"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Alagoas"] <- "AL"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Rio Grande do Norte"] <- "RN"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Piauí"] <- "PI"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Amapá"] <- "AP"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Distrito Federal"] <- "DF"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Mato Grosso"] <- "MT"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Roraima"] <- "RR"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Sergipe"] <- "SE"
UF_SINAN_2010$UF[UF_SINAN_2010$UF == "Tocantins"] <- "TO"

UF_SINAN_2011$UF[UF_SINAN_2011$UF == "São Paulo"] <- "SP"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Rio Grande do Sul"] <- "RS"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Rio de Janeiro"] <- "RJ"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Minas Gerais"] <- "MG"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Santa Catarina"] <- "SC"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Paraná"] <- "PR"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Pernambuco"] <- "PE"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Amazonas"] <- "AM"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Espírito Santo"] <- "ES"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Goiás"] <- "GO"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Ceará"] <- "CE"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Pará"] <- "PA"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Acre"] <- "AC"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Bahia"] <- "BA"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Mato Grosso do Sul"] <- "MS"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Maranhão"] <- "MA"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Rondônia"] <- "RO"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Paraíba"] <- "PB"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Alagoas"] <- "AL"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Rio Grande do Norte"] <- "RN"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Piauí"] <- "PI"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Amapá"] <- "AP"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Distrito Federal"] <- "DF"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Mato Grosso"] <- "MT"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Roraima"] <- "RR"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Sergipe"] <- "SE"
UF_SINAN_2011$UF[UF_SINAN_2011$UF == "Tocantins"] <- "TO"

UF_SINAN_2012$UF[UF_SINAN_2012$UF == "São Paulo"] <- "SP"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Rio Grande do Sul"] <- "RS"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Rio de Janeiro"] <- "RJ"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Minas Gerais"] <- "MG"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Santa Catarina"] <- "SC"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Paraná"] <- "PR"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Pernambuco"] <- "PE"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Amazonas"] <- "AM"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Espírito Santo"] <- "ES"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Goiás"] <- "GO"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Ceará"] <- "CE"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Pará"] <- "PA"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Acre"] <- "AC"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Bahia"] <- "BA"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Mato Grosso do Sul"] <- "MS"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Maranhão"] <- "MA"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Rondônia"] <- "RO"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Paraíba"] <- "PB"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Alagoas"] <- "AL"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Rio Grande do Norte"] <- "RN"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Piauí"] <- "PI"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Amapá"] <- "AP"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Distrito Federal"] <- "DF"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Mato Grosso"] <- "MT"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Roraima"] <- "RR"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Sergipe"] <- "SE"
UF_SINAN_2012$UF[UF_SINAN_2012$UF == "Tocantins"] <- "TO"


UF_SINAN_2013$UF[UF_SINAN_2013$UF == "São Paulo"] <- "SP"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Rio Grande do Sul"] <- "RS"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Rio de Janeiro"] <- "RJ"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Minas Gerais"] <- "MG"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Santa Catarina"] <- "SC"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Paraná"] <- "PR"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Pernambuco"] <- "PE"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Amazonas"] <- "AM"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Espírito Santo"] <- "ES"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Goiás"] <- "GO"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Ceará"] <- "CE"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Pará"] <- "PA"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Acre"] <- "AC"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Bahia"] <- "BA"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Mato Grosso do Sul"] <- "MS"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Maranhão"] <- "MA"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Rondônia"] <- "RO"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Paraíba"] <- "PB"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Alagoas"] <- "AL"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Rio Grande do Norte"] <- "RN"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Piauí"] <- "PI"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Amapá"] <- "AP"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Distrito Federal"] <- "DF"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Mato Grosso"] <- "MT"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Roraima"] <- "RR"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Sergipe"] <- "SE"
UF_SINAN_2013$UF[UF_SINAN_2013$UF == "Tocantins"] <- "TO"

UF_SINAN_2014$UF[UF_SINAN_2014$UF == "São Paulo"] <- "SP"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Rio Grande do Sul"] <- "RS"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Rio de Janeiro"] <- "RJ"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Minas Gerais"] <- "MG"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Santa Catarina"] <- "SC"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Paraná"] <- "PR"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Pernambuco"] <- "PE"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Amazonas"] <- "AM"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Espírito Santo"] <- "ES"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Goiás"] <- "GO"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Ceará"] <- "CE"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Pará"] <- "PA"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Acre"] <- "AC"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Bahia"] <- "BA"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Mato Grosso do Sul"] <- "MS"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Maranhão"] <- "MA"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Rondônia"] <- "RO"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Paraíba"] <- "PB"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Alagoas"] <- "AL"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Rio Grande do Norte"] <- "RN"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Piauí"] <- "PI"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Amapá"] <- "AP"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Distrito Federal"] <- "DF"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Mato Grosso"] <- "MT"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Roraima"] <- "RR"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Sergipe"] <- "SE"
UF_SINAN_2014$UF[UF_SINAN_2014$UF == "Tocantins"] <- "TO"


UF_SINAN_2015$UF[UF_SINAN_2015$UF == "São Paulo"] <- "SP"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Rio Grande do Sul"] <- "RS"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Rio de Janeiro"] <- "RJ"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Minas Gerais"] <- "MG"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Santa Catarina"] <- "SC"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Paraná"] <- "PR"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Pernambuco"] <- "PE"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Amazonas"] <- "AM"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Espírito Santo"] <- "ES"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Goiás"] <- "GO"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Ceará"] <- "CE"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Pará"] <- "PA"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Acre"] <- "AC"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Bahia"] <- "BA"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Mato Grosso do Sul"] <- "MS"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Maranhão"] <- "MA"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Rondônia"] <- "RO"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Paraíba"] <- "PB"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Alagoas"] <- "AL"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Rio Grande do Norte"] <- "RN"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Piauí"] <- "PI"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Amapá"] <- "AP"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Distrito Federal"] <- "DF"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Mato Grosso"] <- "MT"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Roraima"] <- "RR"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Sergipe"] <- "SE"
UF_SINAN_2015$UF[UF_SINAN_2015$UF == "Tocantins"] <- "TO"

UF_SINAN_2016$UF[UF_SINAN_2016$UF == "São Paulo"] <- "SP"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Rio Grande do Sul"] <- "RS"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Rio de Janeiro"] <- "RJ"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Minas Gerais"] <- "MG"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Santa Catarina"] <- "SC"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Paraná"] <- "PR"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Pernambuco"] <- "PE"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Amazonas"] <- "AM"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Espírito Santo"] <- "ES"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Goiás"] <- "GO"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Ceará"] <- "CE"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Pará"] <- "PA"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Acre"] <- "AC"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Bahia"] <- "BA"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Mato Grosso do Sul"] <- "MS"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Maranhão"] <- "MA"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Rondônia"] <- "RO"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Paraíba"] <- "PB"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Alagoas"] <- "AL"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Rio Grande do Norte"] <- "RN"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Piauí"] <- "PI"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Amapá"] <- "AP"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Distrito Federal"] <- "DF"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Mato Grosso"] <- "MT"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Roraima"] <- "RR"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Sergipe"] <- "SE"
UF_SINAN_2016$UF[UF_SINAN_2016$UF == "Tocantins"] <- "TO"

UF_SINAN_2017$UF[UF_SINAN_2017$UF == "São Paulo"] <- "SP"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Rio Grande do Sul"] <- "RS"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Rio de Janeiro"] <- "RJ"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Minas Gerais"] <- "MG"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Santa Catarina"] <- "SC"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Paraná"] <- "PR"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Pernambuco"] <- "PE"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Amazonas"] <- "AM"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Espírito Santo"] <- "ES"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Goiás"] <- "GO"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Ceará"] <- "CE"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Pará"] <- "PA"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Acre"] <- "AC"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Bahia"] <- "BA"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Mato Grosso do Sul"] <- "MS"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Maranhão"] <- "MA"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Rondônia"] <- "RO"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Paraíba"] <- "PB"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Alagoas"] <- "AL"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Rio Grande do Norte"] <- "RN"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Piauí"] <- "PI"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Amapá"] <- "AP"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Distrito Federal"] <- "DF"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Mato Grosso"] <- "MT"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Roraima"] <- "RR"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Sergipe"] <- "SE"
UF_SINAN_2017$UF[UF_SINAN_2017$UF == "Tocantins"] <- "TO"

UF_SINAN_2018$UF[UF_SINAN_2018$UF == "São Paulo"] <- "SP"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Rio Grande do Sul"] <- "RS"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Rio de Janeiro"] <- "RJ"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Minas Gerais"] <- "MG"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Santa Catarina"] <- "SC"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Paraná"] <- "PR"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Pernambuco"] <- "PE"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Amazonas"] <- "AM"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Espírito Santo"] <- "ES"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Goiás"] <- "GO"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Ceará"] <- "CE"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Pará"] <- "PA"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Acre"] <- "AC"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Bahia"] <- "BA"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Mato Grosso do Sul"] <- "MS"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Maranhão"] <- "MA"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Rondônia"] <- "RO"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Paraíba"] <- "PB"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Alagoas"] <- "AL"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Rio Grande do Norte"] <- "RN"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Piauí"] <- "PI"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Amapá"] <- "AP"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Distrito Federal"] <- "DF"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Mato Grosso"] <- "MT"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Roraima"] <- "RR"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Sergipe"] <- "SE"
UF_SINAN_2018$UF[UF_SINAN_2018$UF == "Tocantins"] <- "TO"

UF_SINAN_sc <- do.call("rbind", list(UF_SINAN_2008, UF_SINAN_2009, UF_SINAN_2010, UF_SINAN_2011, UF_SINAN_2012, UF_SINAN_2013, UF_SINAN_2014, UF_SINAN_2015, UF_SINAN_2016, UF_SINAN_2017, UF_SINAN_2018))

## SIM

UF_SIM_2008$ano <- "2008" 
UF_SIM_2009$ano <- "2009" 
UF_SIM_2010$ano <- "2010" 
UF_SIM_2011$ano <- "2011" 
UF_SIM_2012$ano <- "2012" 
UF_SIM_2013$ano <- "2013" 
UF_SIM_2014$ano <- "2014"
UF_SIM_2015$ano <- "2015" 
UF_SIM_2016$ano <- "2016"

UF_SIM_2008$UF[UF_SIM_2008$UF == "São Paulo"] <- "SP"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Rio Grande do Sul"] <- "RS"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Rio de Janeiro"] <- "RJ"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Minas Gerais"] <- "MG"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Santa Catarina"] <- "SC"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Paraná"] <- "PR"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Pernambuco"] <- "PE"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Amazonas"] <- "AM"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Espírito Santo"] <- "ES"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Goiás"] <- "GO"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Ceará"] <- "CE"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Pará"] <- "PA"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Acre"] <- "AC"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Bahia"] <- "BA"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Mato Grosso do Sul"] <- "MS"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Maranhão"] <- "MA"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Rondônia"] <- "RO"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Paraíba"] <- "PB"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Alagoas"] <- "AL"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Rio Grande do Norte"] <- "RN"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Piauí"] <- "PI"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Amapá"] <- "AP"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Distrito Federal"] <- "DF"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Mato Grosso"] <- "MT"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Roraima"] <- "RR"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Sergipe"] <- "SE"
UF_SIM_2008$UF[UF_SIM_2008$UF == "Tocantins"] <- "TO"

UF_SIM_2009$UF[UF_SIM_2009$UF == "São Paulo"] <- "SP"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Rio Grande do Sul"] <- "RS"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Rio de Janeiro"] <- "RJ"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Minas Gerais"] <- "MG"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Santa Catarina"] <- "SC"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Paraná"] <- "PR"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Pernambuco"] <- "PE"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Amazonas"] <- "AM"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Espírito Santo"] <- "ES"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Goiás"] <- "GO"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Ceará"] <- "CE"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Pará"] <- "PA"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Acre"] <- "AC"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Bahia"] <- "BA"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Mato Grosso do Sul"] <- "MS"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Maranhão"] <- "MA"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Rondônia"] <- "RO"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Paraíba"] <- "PB"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Alagoas"] <- "AL"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Rio Grande do Norte"] <- "RN"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Piauí"] <- "PI"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Amapá"] <- "AP"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Distrito Federal"] <- "DF"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Mato Grosso"] <- "MT"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Roraima"] <- "RR"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Sergipe"] <- "SE"
UF_SIM_2009$UF[UF_SIM_2009$UF == "Tocantins"] <- "TO"

UF_SIM_2010$UF[UF_SIM_2010$UF == "São Paulo"] <- "SP"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Rio Grande do Sul"] <- "RS"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Rio de Janeiro"] <- "RJ"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Minas Gerais"] <- "MG"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Santa Catarina"] <- "SC"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Paraná"] <- "PR"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Pernambuco"] <- "PE"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Amazonas"] <- "AM"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Espírito Santo"] <- "ES"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Goiás"] <- "GO"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Ceará"] <- "CE"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Pará"] <- "PA"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Acre"] <- "AC"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Bahia"] <- "BA"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Mato Grosso do Sul"] <- "MS"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Maranhão"] <- "MA"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Rondônia"] <- "RO"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Paraíba"] <- "PB"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Alagoas"] <- "AL"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Rio Grande do Norte"] <- "RN"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Piauí"] <- "PI"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Amapá"] <- "AP"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Distrito Federal"] <- "DF"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Mato Grosso"] <- "MT"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Roraima"] <- "RR"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Sergipe"] <- "SE"
UF_SIM_2010$UF[UF_SIM_2010$UF == "Tocantins"] <- "TO"

UF_SIM_2011$UF[UF_SIM_2011$UF == "São Paulo"] <- "SP"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Rio Grande do Sul"] <- "RS"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Rio de Janeiro"] <- "RJ"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Minas Gerais"] <- "MG"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Santa Catarina"] <- "SC"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Paraná"] <- "PR"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Pernambuco"] <- "PE"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Amazonas"] <- "AM"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Espírito Santo"] <- "ES"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Goiás"] <- "GO"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Ceará"] <- "CE"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Pará"] <- "PA"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Acre"] <- "AC"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Bahia"] <- "BA"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Mato Grosso do Sul"] <- "MS"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Maranhão"] <- "MA"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Rondônia"] <- "RO"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Paraíba"] <- "PB"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Alagoas"] <- "AL"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Rio Grande do Norte"] <- "RN"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Piauí"] <- "PI"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Amapá"] <- "AP"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Distrito Federal"] <- "DF"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Mato Grosso"] <- "MT"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Roraima"] <- "RR"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Sergipe"] <- "SE"
UF_SIM_2011$UF[UF_SIM_2011$UF == "Tocantins"] <- "TO"

UF_SIM_2012$UF[UF_SIM_2012$UF == "São Paulo"] <- "SP"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Rio Grande do Sul"] <- "RS"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Rio de Janeiro"] <- "RJ"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Minas Gerais"] <- "MG"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Santa Catarina"] <- "SC"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Paraná"] <- "PR"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Pernambuco"] <- "PE"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Amazonas"] <- "AM"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Espírito Santo"] <- "ES"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Goiás"] <- "GO"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Ceará"] <- "CE"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Pará"] <- "PA"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Acre"] <- "AC"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Bahia"] <- "BA"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Mato Grosso do Sul"] <- "MS"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Maranhão"] <- "MA"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Rondônia"] <- "RO"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Paraíba"] <- "PB"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Alagoas"] <- "AL"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Rio Grande do Norte"] <- "RN"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Piauí"] <- "PI"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Amapá"] <- "AP"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Distrito Federal"] <- "DF"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Mato Grosso"] <- "MT"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Roraima"] <- "RR"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Sergipe"] <- "SE"
UF_SIM_2012$UF[UF_SIM_2012$UF == "Tocantins"] <- "TO"


UF_SIM_2013$UF[UF_SIM_2013$UF == "São Paulo"] <- "SP"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Rio Grande do Sul"] <- "RS"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Rio de Janeiro"] <- "RJ"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Minas Gerais"] <- "MG"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Santa Catarina"] <- "SC"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Paraná"] <- "PR"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Pernambuco"] <- "PE"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Amazonas"] <- "AM"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Espírito Santo"] <- "ES"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Goiás"] <- "GO"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Ceará"] <- "CE"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Pará"] <- "PA"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Acre"] <- "AC"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Bahia"] <- "BA"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Mato Grosso do Sul"] <- "MS"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Maranhão"] <- "MA"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Rondônia"] <- "RO"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Paraíba"] <- "PB"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Alagoas"] <- "AL"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Rio Grande do Norte"] <- "RN"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Piauí"] <- "PI"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Amapá"] <- "AP"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Distrito Federal"] <- "DF"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Mato Grosso"] <- "MT"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Roraima"] <- "RR"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Sergipe"] <- "SE"
UF_SIM_2013$UF[UF_SIM_2013$UF == "Tocantins"] <- "TO"

UF_SIM_2014$UF[UF_SIM_2014$UF == "São Paulo"] <- "SP"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Rio Grande do Sul"] <- "RS"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Rio de Janeiro"] <- "RJ"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Minas Gerais"] <- "MG"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Santa Catarina"] <- "SC"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Paraná"] <- "PR"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Pernambuco"] <- "PE"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Amazonas"] <- "AM"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Espírito Santo"] <- "ES"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Goiás"] <- "GO"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Ceará"] <- "CE"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Pará"] <- "PA"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Acre"] <- "AC"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Bahia"] <- "BA"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Mato Grosso do Sul"] <- "MS"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Maranhão"] <- "MA"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Rondônia"] <- "RO"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Paraíba"] <- "PB"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Alagoas"] <- "AL"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Rio Grande do Norte"] <- "RN"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Piauí"] <- "PI"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Amapá"] <- "AP"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Distrito Federal"] <- "DF"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Mato Grosso"] <- "MT"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Roraima"] <- "RR"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Sergipe"] <- "SE"
UF_SIM_2014$UF[UF_SIM_2014$UF == "Tocantins"] <- "TO"


UF_SIM_2015$UF[UF_SIM_2015$UF == "São Paulo"] <- "SP"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Rio Grande do Sul"] <- "RS"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Rio de Janeiro"] <- "RJ"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Minas Gerais"] <- "MG"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Santa Catarina"] <- "SC"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Paraná"] <- "PR"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Pernambuco"] <- "PE"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Amazonas"] <- "AM"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Espírito Santo"] <- "ES"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Goiás"] <- "GO"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Ceará"] <- "CE"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Pará"] <- "PA"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Acre"] <- "AC"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Bahia"] <- "BA"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Mato Grosso do Sul"] <- "MS"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Maranhão"] <- "MA"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Rondônia"] <- "RO"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Paraíba"] <- "PB"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Alagoas"] <- "AL"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Rio Grande do Norte"] <- "RN"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Piauí"] <- "PI"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Amapá"] <- "AP"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Distrito Federal"] <- "DF"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Mato Grosso"] <- "MT"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Roraima"] <- "RR"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Sergipe"] <- "SE"
UF_SIM_2015$UF[UF_SIM_2015$UF == "Tocantins"] <- "TO"

UF_SIM_2016$UF[UF_SIM_2016$UF == "São Paulo"] <- "SP"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Rio Grande do Sul"] <- "RS"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Rio de Janeiro"] <- "RJ"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Minas Gerais"] <- "MG"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Santa Catarina"] <- "SC"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Paraná"] <- "PR"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Pernambuco"] <- "PE"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Amazonas"] <- "AM"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Espírito Santo"] <- "ES"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Goiás"] <- "GO"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Ceará"] <- "CE"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Pará"] <- "PA"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Acre"] <- "AC"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Bahia"] <- "BA"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Mato Grosso do Sul"] <- "MS"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Maranhão"] <- "MA"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Rondônia"] <- "RO"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Paraíba"] <- "PB"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Alagoas"] <- "AL"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Rio Grande do Norte"] <- "RN"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Piauí"] <- "PI"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Amapá"] <- "AP"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Distrito Federal"] <- "DF"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Mato Grosso"] <- "MT"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Roraima"] <- "RR"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Sergipe"] <- "SE"
UF_SIM_2016$UF[UF_SIM_2016$UF == "Tocantins"] <- "TO"

UF_SIM_sc <- do.call("rbind", list(UF_SIM_2008, UF_SIM_2009, UF_SIM_2010, UF_SIM_2011, UF_SIM_2012, UF_SIM_2013, UF_SIM_2014, UF_SIM_2015, UF_SIM_2016))

#### Adicionando total por ano - Brasil ###
#AIH
total_AIH <- UF_AIH_sc %>% group_by(ano) %>% summarise(total = sum(Freq), na.rm = TRUE) 
total_AIH$na.rm = "Brasil"
total_AIH$Porcentagem = "100%"
total_AIH <- select(total_AIH, UF = na.rm, Freq = total, Porcentagem, ano )
UF_AIH_sc <- do.call("rbind", list(UF_AIH_sc, total_AIH))

#APAC
total_APAC <- UF_APAC_sc %>% group_by(ano) %>% summarise(total = sum(Freq), na.rm = TRUE) 
total_APAC$na.rm = "Brasil"
total_APAC$Porcentagem = "100%"
total_APAC <- select(total_APAC, UF = na.rm, Freq = total, Porcentagem, ano )
UF_APAC_sc <- do.call("rbind", list(UF_APAC_sc, total_APAC))

#BPAI
total_BPAI <- UF_BPAI_sc %>% group_by(ano) %>% summarise(total = sum(Freq), na.rm = TRUE) 
total_BPAI$na.rm = "Brasil"
total_BPAI$Porcentagem = "100%"
total_BPAI <- select(total_BPAI, UF = na.rm, Freq = total, Porcentagem, ano )
UF_BPAI_sc <- do.call("rbind", list(UF_BPAI_sc, total_BPAI))

#GAL
total_GAL <- UF_GAL_sc %>% group_by(ano) %>% summarise(total = sum(Freq), na.rm = TRUE) 
total_GAL$na.rm = "Brasil"
total_GAL$Porcentagem = "100%"
total_GAL <- select(total_GAL, UF = na.rm, Freq = total, Porcentagem, ano )
UF_GAL_sc <- do.call("rbind", list(UF_GAL_sc, total_GAL))

#SIM
total_SIM <- UF_SIM_sc %>% group_by(ano) %>% summarise(total = sum(Freq), na.rm = TRUE) 
total_SIM$na.rm = "Brasil"
total_SIM$Porcentagem = "100%"
total_SIM <- select(total_SIM, UF = na.rm, Freq = total, Porcentagem, ano )
UF_SIM_sc <- do.call("rbind", list(UF_SIM_sc, total_SIM))

#SINAN
total_SINAN <- UF_SINAN_sc %>% group_by(ano) %>% summarise(total = sum(Freq), na.rm = TRUE) 
total_SINAN$na.rm = "Brasil"
total_SINAN$Porcentagem = "100%"
total_SINAN <- select(total_SINAN, UF = na.rm, Freq = total, Porcentagem, ano )
UF_SINAN_sc <- do.call("rbind", list(UF_SINAN_sc, total_SINAN))

######################
# scatter plots - UF #
######################

### AIH ###

# com SP
################# scatter plot linhas ########################
ggplot(data = UF_AIH_sc, aes(x = ano, 
                              y = Freq, 
                              group=UF, 
                              color=UF)) +
  geom_line() +
  geom_point() + 
  labs( 
    y="Frequência", 
    x="Ano"
  ) + geom_text(aes(label=UF),hjust=0, vjust=0, check_overlap = TRUE, size = 3) 
#############################################################

UF_AIH_sc_SP <- subset(UF_AIH_sc,  UF!="SP")

# sem SP
################# scatter plot linhas ########################
ggplot(data = UF_AIH_sc_SP, aes(x = ano, 
                             y = Freq, 
                             group=UF, 
                             color=UF)) +
  geom_line() +
  geom_point() + 
  labs( 
    y="Frequência", 
    x="Ano"
  ) + geom_text(aes(label=UF),hjust=0, vjust=0, check_overlap = TRUE, size = 3) 
#############################################################

### BPAI ###

# com SP
################# scatter plot linhas ########################
ggplot(data = UF_BPAI_sc, aes(x = ano, 
                             y = Freq, 
                             group=UF, 
                             color=UF)) +
  geom_line() +
  geom_point() + 
  labs( 
    y="Frequência", 
    x="Ano"
  ) + geom_text(aes(label=UF),hjust=0, vjust=0, check_overlap = TRUE, size = 3) 
#############################################################

UF_BPAI_sc_SP <- subset(UF_BPAI_sc,  UF!="SP")

# sem SP
################# scatter plot linhas ########################
ggplot(data = UF_BPAI_sc_SP, aes(x = ano, 
                                y = Freq, 
                                group=UF, 
                                color=UF)) +
  geom_line() +
  geom_point() + 
  labs( 
    y="Frequência", 
    x="Ano"
  ) + geom_text(aes(label=UF),hjust=0, vjust=0, check_overlap = TRUE, size = 3) 
#############################################################

### APAC ###

# com SP
################# scatter plot linhas ########################
ggplot(data = UF_APAC_sc, aes(x = ano, 
                              y = Freq, 
                              group=UF, 
                              color=UF)) +
  geom_line() +
  geom_point() + 
  labs( 
    y="Frequência", 
    x="Ano"
  ) + geom_text(aes(label=UF),hjust=0, vjust=0, check_overlap = TRUE, size = 3) 
#############################################################

UF_APAC_sc_SP <- subset(UF_APAC_sc,  UF!="SP")

# sem SP
################# scatter plot linhas ########################
ggplot(data = UF_APAC_sc_SP, aes(x = ano, 
                                 y = Freq, 
                                 group=UF, 
                                 color=UF)) +
  geom_line() +
  geom_point() + 
  labs( 
    y="Frequência", 
    x="Ano"
  ) + geom_text(aes(label=UF),hjust=0, vjust=0, check_overlap = TRUE, size = 3) 
#############################################################

### GAL ###

# com SP
################# scatter plot linhas ########################
ggplot(data = UF_GAL_sc, aes(x = ano, 
                              y = Freq, 
                              group=UF, 
                              color=UF)) +
  geom_line() +
  geom_point() + 
  labs( 
    y="Frequência", 
    x="Ano"
  ) + geom_text(aes(label=UF),hjust=0, vjust=0, check_overlap = TRUE, size = 3) 
#############################################################

UF_GAL_sc_SP <- subset(UF_GAL_sc,  UF!="SP")

# sem SP
################# scatter plot linhas ########################
ggplot(data = UF_GAL_sc_SP, aes(x = ano, 
                                 y = Freq, 
                                 group=UF, 
                                 color=UF)) +
  geom_line() +
  geom_point() + 
  labs( 
    y="Frequência", 
    x="Ano"
  ) + geom_text(aes(label=UF),hjust=0, vjust=0, check_overlap = TRUE, size = 3) 
#############################################################

### SINAN ###

# com SP
################# scatter plot linhas ########################
ggplot(data = UF_SINAN_sc, aes(x = ano, 
                             y = Freq, 
                             group=UF, 
                             color=UF)) +
  geom_line() +
  geom_point() + 
  labs( 
    y="Frequência", 
    x="Ano"
  ) + geom_text(aes(label=UF),hjust=0, vjust=0, check_overlap = TRUE, size = 3) 
#############################################################

UF_SINAN_sc_SP_RS <- subset(UF_SINAN_sc,  UF!="SP" & UF!="RS")

# sem SP e RS
################# scatter plot linhas ########################
ggplot(data = UF_SINAN_sc_SP_RS, aes(x = ano, 
                                y = Freq, 
                                group=UF, 
                                color=UF)) +
  geom_line() +
  geom_point() + 
  labs( 
    y="Frequência", 
    x="Ano"
  ) + geom_text(aes(label=UF),hjust=0, vjust=0, check_overlap = TRUE, size = 3) 
#############################################################

### SIM ###

# com SP
################# scatter plot linhas ########################
ggplot(data = UF_SIM_sc, aes(x = ano, 
                               y = Freq, 
                               group=UF, 
                               color=UF)) +
  geom_line() +
  geom_point() + 
  labs( 
    y="Frequência", 
    x="Ano"
  ) + geom_text(aes(label=UF),hjust=0, vjust=0, check_overlap = TRUE, size = 3) 
#############################################################

UF_SIM_sc_SP <- subset(UF_SIM_sc,  UF!="SP" )

# sem SP 
################# scatter plot linhas ########################
ggplot(data = UF_SIM_sc_SP, aes(x = ano, 
                                     y = Freq, 
                                     group=UF, 
                                     color=UF)) +
  geom_line() +
  geom_point() + 
  labs( 
    y="Frequência", 
    x="Ano"
  ) + geom_text(aes(label=UF),hjust=0, vjust=0, check_overlap = TRUE, size = 3) 
#############################################################






################# scatter plots pontos ########################
gg <- ggplot(UF_BPAI_sc, aes(x=ano, y=Freq)) + 
  geom_point(aes(col=UF, size=Freq)) + 
  geom_smooth(method="loess", se=F) + 
  ylim(c(0, 600000)) + 
  labs(subtitle="UF", 
       y="Frequência", 
       x="Ano", 
       title="Scatterplot") 
plot(gg)
#############################################################




