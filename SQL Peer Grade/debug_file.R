for(i in 1:(length(test$name)-1)){
  up = nchar(test$name[i])
  down = nchar(test$name[i+1])
  if(test$name[i]!=test$name[i+1] & levenshteinSim(test$name[i],test$name[i+1]) >= 0.8){
    if(up >down){
      test$name[i]=test$name[i+1]
    }
    if(up <=down&test$name[i]!=test$name[i+1]){
      test$name[i+1]=test$name[i]
    }
  }
}
  
  