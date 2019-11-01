library(readxl)
library(tidyverse)
library(RSQLite)
library(DBI)

Con_All = read_excel("Top MA Donors 2016-2020(2).xlsx",sheet = "Direct Contributions & JFC Dist")

library(tidyverse)
library(magrittr)
library(RecordLinkage)
test = Con_All %>% select(Fecoccemp,fectransid) 
colnames(test) = c("name","id")
test %<>% mutate(like = levenshteinDist(test$name[1],name)) %>% arrange(name)

test$name = gsub(x = test$name, pattern = "\\W",replacement = " ")

repeat{
  sub = 0
  for(i in 1:(length(test$name)-1)){
    up = nchar(test$name[i])
    down = nchar(test$name[i+1])
    if(test$name[i]!=test$name[i+1] & levenshteinDist(test$name[i],test$name[i+1]) <=3){
      if(up >down){
        test$name[i]=test$name[i+1]
        sub = sub+1
      }
      if(up <=down){
        test$name[i+1]=test$name[i]
        sub = sub+1
      }
    }
  }
  s = sample(x = seq(1,length(test$name),10),size = 1)
  test %<>% mutate(like = levenshteinDist(test$name[s],name)) %>% arrange(name)
  if(sub == 0){
    break
  }
}

test = test %>% select(-"like")
colnames(test) = c("Fecoccemp","fectransid")
Con_All = Con_All %>% select(-"Fecoccemp")
Con_All = left_join(Con_All,test,by = 'fectransid')