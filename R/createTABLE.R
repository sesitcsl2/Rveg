

createTABLE <- function(SpLIST,RelNew,DATA2){

  TABLE<-data.frame(ShortName=SpLIST[,2],Value=0)
  zz<-c()
  for (i in 2:length(colnames(DATA2))) {
    RelNewKrs<-data.frame(ShortName=SpLIST[,2],Value=0)
    zzz<-DATA2[,c(1,i)]
    RelNewKrs[RelNewKrs[,1]%in%zzz[,1],][,2] <- zzz[,2]
    TABLE<-data.frame(TABLE,RelNewKrs[,2])
    zz<-c(zz,i)
  }
  TABLE<-TABLE[,-c(2)]
  TABLE<-data.frame(TABLE,RelNew[,2])
  TABLE[] <- lapply(TABLE, as.character)
  TABLEexp<-TABLE[apply(TABLE[,-1], 1, function(x) sum(x !="0" ))>0,]
  colnames(TABLEexp)[2:(length(zz)+2)]<-c(1:(length(zz)+1))
  return(TABLEexp)
  print(TABLEexp)
}
