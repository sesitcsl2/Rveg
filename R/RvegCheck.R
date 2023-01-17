#'
#' RvegCheck
#'
#' Checking your DATABASE for duplicity and allowing to export table with full species name (not editable anymore).
#'
#' @param DATABASE name of csv files for releve table and header - database
#' @param fullnames logical value if you want to add fullnames to the database
#' @param export name of exporting database
#'
#'
#' @returns Export csv file releve table
#'
#' @export
#'

RvegCheck <- function(DATABASE, fullnames = FALSE,export = "export"){


  DATA <- read.csv(paste0(DATABASE, "REL.csv"),row.names =1)
  #HeaderDATA <- read.csv(paste0(DATABASE, "HEAD.csv"), row.names = 1)

  if (checklist == "default"){
    checklist<- paste0(path.package("Rveg"),"/extdata/DANIHELKA2012rko.txt")}

  SpLIST <- read.delim(checklist, sep = "\t")
  fullName = c(rep("",nrow(DATA)))
  DATA <- cbind(fullName,DATA)
  for (i in 1:length(DATA$ShortName)) {
    DATA$fullName[i] <- SpLIST$FullName[SpLIST$ShortName==substr(DATA$ShortName[i],1,7)]
  }

  for (i in 1:length(DATA$fullName)) {
    if (length(unique(substr(DATA$ShortName[DATA$fullName==DATA$fullName[i]],1,7))) > 1) {
      cat(paste0("found duplicate codes for ",DATA$fullName[i],"\n"))
      cat(unique(substr(DATA$ShortName[DATA$fullName==DATA$fullName[i]],1,7)))
      cat("\n")
      print(DATA[DATA$fullName==DATA$fullName[i],])
      while (TRUE) {
        met <- toupper(readline("select merging method?(M - merge, N - none) "))
        if (met == "M") {
          cat("This merging method will keep the higher value")
          while (TRUE) {
            l1 <- as.numeric(readline("select first row (with correct code) "))
            l2 <- as.numeric(readline("select second row "))
            if (is.na(l1) != TRUE & is.na(l2) != TRUE  & isTRUE(c(l1,l2) %in% as.numeric(row.names(DATA)))) {
              break
            }
          }


          for (i in 1:length(DATA[row.names(DATA)==l1,])) {
            if (is.numeric(DATA[l1,i])) {
              DATA[row.names(DATA)==l1,i] <- max(c(DATA[row.names(DATA)==l1,i],DATA[row.names(DATA)==l2,i]))
            }

          }
          DATA<- DATA[!(row.names(DATA)==l2),]


          break
        }

        if (met == "N") {
          break
        }
      }




    }
  }

  if (fullnames) {
    write.csv(DATA, paste0(export,"REL.csv"))
    #write.csv(HeaderDATA, paste0(export,"REL.csv"))
  } else  {
    write.csv(DATA[-1,], paste0(export,"REL.csv"))
    }
  }