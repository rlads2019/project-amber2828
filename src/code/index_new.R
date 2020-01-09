library(dplyr)
library(readr)
library(tibble)
library(stringr)
pa <- function(x){
  if(grepl("[0-9]",x))
  x <- as.numeric(str_replace(x,"%",""))
  return (x)
}
cal <- function(x)
{

  if(grepl("107.csv",x)){
    file <- read_csv(x)
    colnames(file)<-file[1,]
    file <- file[-1,]
    colnames(file)[[9]]<-"other"
    colnames(file)[[10]]<-"total"
    colnames(file)[[11]]<-"adopt1"
    colnames(file)[[12]]<-"adopt2"
    colnames(file)[[14]]<-"death"
    colnames(file)[[21]]<-"personnel1"
    colnames(file)[[22]]<-"personnel2"
    file<-as.tibble(lapply(file,pa))
    file <- mutate(file,"adopt" = 100*((adopt1 + adopt2) / total),
                   "adopt_threat(%)" = 100 - 100*((adopt1 + adopt2) / total),
                   "death_threat(%)" = 100*death / total,
             "reliability(%)" = 100*((adopt1 + adopt2) / total) - 80 + 10 - 100*death / total,
             "animals_per_person" = total / (personnel1 + personnel2)) %>% 
              select(c(1,10,20,23:27))
    return (file)
      }
  else if(grepl(".csv",x)){
    file <- read_csv(x)
    colnames(file)[[4]]<-"adopt"
    colnames(file)[[6]]<-"death"
    file<-as.tibble(lapply(file,pa))
    file <- select(file,c(1,2,4,6)) %>% 
      mutate("adopt_threat(%)" = 100 - adopt,"death_threat(%)" = death,"reliability(%)" = adopt - 80 + 10 - death) %>% 
      select(c(1,2,3,5:7))
    return(file)
  } 
}
#使用前記得用setwd()把console路徑設到repository的資料夾
direction <- dir('./')
fileset<-lapply(sort(direction),cal)
names(fileset) <- direction
#fileset裡有算好的數據,照csv檔名命名好了
fileset<-fileset[-6]
print(fileset)
#檢視用,可刪
#adopt_threat是認領養風險指標,death_threat是死亡風險指標,reliability是可靠度指標
#只有107.csvj才有在養佔可留容比例跟animals_per_person(每個人力須照顧的動物數量)

