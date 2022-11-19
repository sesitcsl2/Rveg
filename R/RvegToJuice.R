#'
#' Export database Juice software compatible format
#'
#' @param Data name of your database without extension
#' @returns csv file
#'
#' @examples
#'
#' RvegToJuice(Database)
#'
#' @export
#'
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
  ttz <- paste0("Number of relevés:",ncol(tt))

  write(x = paste0(tty,"\n",ttz,"\n"),file = paste0(export, ".csv")) # possible encoding problem?
  write.table(tttt,file = paste0(export, ".csv"),row.names = F,col.names =F,na = "",sep = ",",quote = F,append = T) # fileEncoding = "Windows-1252"

}

#options("encoding" = "windows-1252")


