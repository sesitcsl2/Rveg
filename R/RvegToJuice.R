#'
#' RvegToJuice
#'
#' Export Rveg database to Juice software compatible format
#'
#' @param Data name of your Rveg database
#' @param checklist path to your custom species checklist
#' @param export name of your exported csv file
#'
#' @returns csv file which is readible by Juice
#'
#' @export
#'

RvegToJuice <- function(Data, checklist = "default", export = "export") {
  Data_rel <- read.csv(paste0(Data,"REL.csv"),row.names = 1)
  Data_head <- read.csv(paste0(Data,"HEAD.csv"),row.names = 1)
  if (checklist == "default"){
    checklist<- paste0(path.package("Rveg"),"/extdata/DANIHELKA2012rko.txt")
    }
  Splist <- read.delim(checklist, sep = "\t")

  x <- NULL
  y <- NULL
  z <- NULL

  for (i in 1:nrow(Data_rel)) {
    n <- substr(Data_rel[i,1],9,9)
    if (n == 3) {
      n <- 2
    } else if (n == 2) {
      n <- 4
    } else if (n == 1) {
      n <- 6
    } else if (n == 0) {
      n <- 9
    } else if (n == "J") {
      n <- 7
    }
    y <- c(y,n)
    x <- c(x,Splist[Splist[,2]==substr(Data_rel[i,1],1,7),3])
    z[[i]] <- as.vector(Data_rel[i,2:ncol(Data_rel)])

  }

  tt<-data.frame(matrix(unlist(z), nrow = length(z),byrow=T))
  ttt <- data.frame(x,y)
  tt[tt==0] <- "."
  tzt <- c(NA,NA,1:ncol(tt))
  tttt <- cbind(ttt,tt)
  tttt <- rbind(tzt,tttt)
  tty <- paste0("Export from ", Data)
  ttz <- paste0("Number of relev\u00e9s:",ncol(tt))

  write(x = paste0(tty,"\n",ttz,"\n"),file = paste0(export, ".csv")) # possible encoding problem?
  write.table(tttt,file = paste0(export, ".csv"),row.names = F,col.names =F,na = "",sep = ",",quote = F,append = T) # fileEncoding = "Windows-1252"

}


#'
#' TvToRveg
#'
#' Export Turboveg csv file to Rveg database compatible format
#'
#' @param tv path to Turboveg csv export
#' @param export name of your exported database
#'
#' @returns csv file
#'
#' @export
#'
#'

tvToRveg <- function(tv, export = "export"){

  data <- read.csv(tv)
  lim = as.numeric(rownames(data[data[,1]=="",]))

  tvhead <- data [1:(lim-1),]
  tvrel <- data [(lim+1):nrow(data),]

  rvrel<- data.frame( ShortName = character(),stringsAsFactors = F)
  rvhead <- data.frame(ShortName = c("ID","DATE","SpringDATE","LOCALITY","FieldCODE","Authors",
                                  "PlotSize","Latitude","Longitude","Accuracy",
                                  "E3","E2","E1","Ejuv","E0","Note",data[22:lim-1,1]),
                    stringsAsFactors = FALSE)

  # header
  for (i in 3:ncol(data)) {
  rvhead[rvhead$ShortName=="ID",i-1] <- tvhead[tvhead[,1]=="Releve number",i]
  rvhead[rvhead$ShortName=="DATE",i-1] <- tvhead[tvhead[,1]=="Date (year/month/day)",i]
  rvhead[rvhead$ShortName=="LOCALITY",i-1] <- tvhead[tvhead[,1]=="Releve number",i]
  rvhead[rvhead$ShortName=="FieldCODE",i-1] <- tvhead[tvhead[,1]=="Releve number",i]
  rvhead[rvhead$ShortName=="Authors",i-1] <- "unknown"
  rvhead[rvhead$ShortName=="PlotSize",i-1] <- tvhead[tvhead[,1]=="Releve area (m2)",i]
  rvhead[rvhead$ShortName=="Latitude",i-1] <- "unknown"
  rvhead[rvhead$ShortName=="Longitude",i-1] <- "unknown"
  rvhead[rvhead$ShortName=="E3",i-1] <- tvhead[tvhead[,1]=="Cover tree layer (%)",i]
  rvhead[rvhead$ShortName=="E2",i-1] <- tvhead[tvhead[,1]=="Cover shrub layer (%)",i]
  rvhead[rvhead$ShortName=="E1",i-1] <- tvhead[tvhead[,1]=="Cover herb layer (%)",i]
  rvhead[rvhead$ShortName=="Note",i-1] <- tvhead[tvhead[,1]=="Remarks",i]
  rvhead[17:(17+lim-22),i-1] <- tvhead[22:lim-1,i]
  } # header end

  SpLIST <- read.delim("DANIHELKA2012rko.txt", sep = "\t")
  aaa <- paste(SpLIST[, 2], 3, sep = "_")
  bbb <- paste(SpLIST[, 2], 2, sep = "_")
  ccc <- paste(SpLIST[, 2], 1, sep = "_")
  eee <- paste(SpLIST[, 2], "J", sep = "_")
  fff <- paste(SpLIST[, 2], 0, sep = "_")
  ddd <- c(aaa, bbb, ccc, eee, fff)
  SpLIST <- rbind(SpLIST, SpLIST, SpLIST, SpLIST, SpLIST)
  SpLIST[, 2] <- ddd
  SpLIST1 <- read.delim("DANIHELKA2012rko.txt", sep = "\t")

  for (i in 1:nrow(tvrel)) {
  if (substr(tvrel[i,2],1,1)=="h"){
    abc = 1
  } else if (substr(tvrel[i,2],1,1)=="s") {
    abc = 2
  } else if (substr(tvrel[i,2],1,1)=="t") {
    abc = 3
  } else if (substr(tvrel[i,2],1,1)=="m") {
    abc = 0
  }
  tvrel[i,2] <- abc
}

tvrel[tvrel==""] = 0

while(T){
  cla <- readline("Data in Bran blanquet or percentage? (B/%)")
  if (cla == "B") {

    for (i in 1:ncol(tvrel[,c(-1,-2)])) {
      for (l in 1:nrow(tvrel[,c(-1,-2)])) {
        if (tvrel[,c(-1,-2)][l,i]==1) {
          tvrel[,c(-1,-2)][l,i]<-3
        } else if (tvrel[,c(-1,-2)][l,i]==2) {
          tvrel[,c(-1,-2)][l,i] <- 15
        } else if (tvrel[,c(-1,-2)][l,i]==3) {
          tvrel[,c(-1,-2)][l,i] <- 38
        } else if (tvrel[,c(-1,-2)][l,i]==4) {
          tvrel[,c(-1,-2)][l,i] <- 63
        } else if (tvrel[,c(-1,-2)][l,i]==5) {
          tvrel[,c(-1,-2)][l,i] <- 88
        } else if (tvrel[,c(-1,-2)][l,i]=="+") {
          tvrel[,c(-1,-2)][l,i] <- 2
        } else if (tvrel[,c(-1,-2)][l,i]=="r") {
          tvrel[,c(-1,-2)][l,i] <- 1
        } else if (tvrel[,c(-1,-2)][l,i]=="a") {
          tvrel[,c(-1,-2)][l,i] <- 8
        } else if (tvrel[,c(-1,-2)][l,i]=="b") {
          tvrel[,c(-1,-2)][l,i] <- 18
        }
      }

    }
    break
  } else if (cla == "%") {
    break
  }
  }

for (s in tvrel[,1]) {
  if (length((s==tvrel[,1])[(s==tvrel[,1])==TRUE])>1) {

    #l1 <- as.character(as.vector(tvrel[s == tvrel[,1],][1,]))
    l1 <- as.numeric(tvrel[s == tvrel[,1],][1,c(-1,-2)])
    l1[is.na(l1)] <- 0

    l2 <- as.numeric(tvrel[s == tvrel[,1],][2,c(-1,-2)]) ### tady konec, nutne tvorit tabulku po radku nebo spojit predtim v TVREL
    l2[is.na(l2)] <- 0
    r1 <- rownames(tvrel[s == tvrel[,1],][2,c(-1,-2)])

    l3 <- round(l1 + (l2*(1-(l1/100))))



    tvrel[tvrel[,1]==s,][1,c(-1,-2)] = l3
    tvrel = tvrel[!(rownames(tvrel) %in% r1),]
    #tvrel[tvrel$V1==s,] = subset(tvrel[rownames(tvrel),],!rownames(tvrel)==r1)


  }
}

for (i in 1:nrow(tvrel)) {


  sp = SpLIST[ SpLIST$FullName==tvrel[i,1],2][substr(SpLIST[ SpLIST$FullName==tvrel[i,1],2],9,9)==tvrel[i,2]][1]
  rvrel[i,1] = sp
  rrn = rownames(SpLIST[ SpLIST$ShortName==sp,])
  if (!is.na(sp)) {
    rownames(rvrel)[i] = rownames(SpLIST[ SpLIST$ShortName==sp,])
  }

  print(tvrel[i,1])
  print(rvrel[i,1])

  #nana <- paste(sp, abc, sep = "_")
  #specie_check<-SpLIST[SpLIST[,2]==substr(nana,1,9),]

  while (T) {
    check <- toupper(readline("correct name?(blank/N) "))
    if (check == "" & !is.na(rvrel[i,1])) {
      break
    }
    if (check == "N") {
      while (TRUE){
        k <- readline("AddSpeciesFirst3letters(eg.Che)? ")
        tt <- toupper(readline("Correct 3 letters? Y/N  "))

        if (tt == "Y" ) {
          break
        }
      }

      k <- paste0(toupper(substr(k, 1, 1)), tolower(substr(k, 2, 3)))
      print(SpLIST1[grep(paste0("^", k), SpLIST1[, 3]), ])
      while (TRUE){
        while (TRUE) {
          s <- toupper(readline("SpeciesName?(7lettersGenuSpe) "))
          if (nchar(s)==7) {
            nana <- paste(s, abc, sep = "_")
            specie_check<-SpLIST[SpLIST[,2]==substr(nana,1,9),]
            print(specie_check[3])
            break
          }
        }
        tt <- toupper(readline("Correct 7lettersGenuSpe Y/N  "))
        if ((tt == "Y") & !is.na(specie_check[1,3])) {
          rvrel[i,1] = nana
          rownames(rvrel)[i] = rownames(specie_check)
          break
        }
        break

      }
      break
    }

  }

  }

DATA <- cbind(rvrel,tvrel[,c(-1,-2)])
colnames(DATA) <- c("",paste0("X",c(1:(ncol(DATA)-1))))

write.csv(DATA, paste0(export, "REL.csv"))
write.csv(rvhead, paste0(export, "HEAD.csv"))
  }
#options("encoding" = "windows-1252")


