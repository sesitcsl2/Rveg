

createTABLE <- function(SpLIST,RelNew,DATA2){
  TABLE<-data.frame(number=row.names(SpLIST),ShortName=SpLIST[,2],Value=0)
  #TABLE<-TABLE[order(TABLE$number),]
  zz<-c()
  for (i in 2:length(colnames(DATA2))) {
    RelNewKrs<-data.frame(ShortName=SpLIST[,2],Value=0)
    zzz<-DATA2[,c(1,i)]
    zzz<-zzz[order(as.numeric(row.names(zzz))),]
    RelNewKrs<-RelNewKrs[order(as.numeric(row.names(RelNewKrs))),]
    RelNewKrs[RelNewKrs[,1]%in%zzz[,1],][,2] <- zzz[,2]
    TABLE<-data.frame(TABLE,RelNewKrs[,2])
    zz<-c(zz,i)
  }
  TABLE<-TABLE[,-c(3)]
  #RelNew<-RelNew[order(RelNew$ShortName),]
  TABLE<-data.frame(TABLE,RelNew[,2])
  TABLE[] <- lapply(TABLE, as.character)
  TABLEexp<-TABLE[apply(TABLE[,c(-1,-2)], 1, function(x) sum(x !="0" ))>0,]
  TABLEexp<-TABLEexp[,-c(1)]
  colnames(TABLEexp)[2:(length(zz)+2)]<-c(1:(length(zz)+1))
  return(TABLEexp)
  print(TABLEexp)
}
