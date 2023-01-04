#'
#' Writing and editing your releves
#' @param DATABASE name of csv files for releve table and header
#' @param SAVE name of exporting database
#' @param checklist custom checklist
#' @param extrahead extra rows in header
#'
#'
#' @returns Export two csv files, one for releve and one for header
#'
#' @examples
#'
#' addRELEVE(SAVE = "test_database")
#'
#' @export
#'
#'

addRELEVE <- function(DATABASE = "NEW", SAVE, checklist = "default", extrahead = NULL) {
  # LOAD DATA ------------------------------------------------------------------------------------------

  if (checklist == "default"){
    checklist<- paste0(path.package("Rveg"),"/extdata/DANIHELKA2012rko.txt")}

  warn = getOption("warn")
  options(warn=-1)

  if (DATABASE == "NEW") {
    DATA <- data.frame(ShortName = character(), stringsAsFactors = FALSE)
    HeaderDATA <- data.frame(ShortName = c("ID","DATE","SpringDATE","LOCALITY","FieldCODE","Authors",
                                           "PlotSize","Latitude","Longitude","Accuracy",
                                            "E3","E2","E1","Ejuv","E0","Note",extrahead),
                                          stringsAsFactors = FALSE
      )
    write.csv(DATA, paste0(SAVE, "REL.csv"))
    write.csv(HeaderDATA, paste0(SAVE, "HEAD.csv"))
  } else {
    DATARELE <- paste0(DATABASE, "REL.csv")
    DATAhead <- paste0(DATABASE, "HEAD.csv")
    DATA <-read.csv(DATARELE, row.names = 1)
    HeaderDATA <-read.csv(DATAhead, row.names = 1)

    write.csv(DATA, paste0(SAVE, "REL.csv"))
    write.csv(HeaderDATA, paste0(SAVE, "HEAD.csv"))
  }

  SpLIST <- read.delim(checklist, sep = "\t")
  aaa <- paste(SpLIST[, 2], 3, sep = "_")
  bbb <- paste(SpLIST[, 2], 2, sep = "_")
  ccc <- paste(SpLIST[, 2], 1, sep = "_")
  eee <- paste(SpLIST[, 2], "J", sep = "_")
  fff <- paste(SpLIST[, 2], 0, sep = "_")

  ddd <- c(aaa, bbb, ccc, eee, fff)
  SpLIST <- rbind(SpLIST, SpLIST, SpLIST, SpLIST, SpLIST)
  SpLIST[, 2] <- ddd

  SpLIST1 <- read.delim(checklist, sep = "\t")

  # DATA SETUP  ------------------------------------------------------------------------------------------

  while (TRUE) {
    DATA2 <- read.csv(paste0(SAVE, "REL.csv"),row.names =1) # rownames
    HeaderDATA2 <- read.csv(paste0(SAVE, "HEAD.csv"), row.names = 1) # rownames
    if (DATABASE == "NEW") {
      Header <-
        data.frame(
          ShortName = c("ID","DATE","SpringDATE","LOCALITY","FieldCODE","Authors","PlotSize",
            "Latitude","Longitude","Accuracy","E3","E2","E1","Ejuv","E0","Note",extrahead),
          Value = 0
        )
      RelNew <- data.frame(ShortName = SpLIST[, 2], Value = 0)
      ID <- ncol(DATA2)
      colnames(RelNew)[2] <- (ID - 1)
      Header[1, 2] <- (1)
      ab <- readline("DATE(Y/M/D-2009/07/05)? ")
      bb <- readline("SPRINGDATE(Y/M/D-2009/07/05)? ")
      bc <- readline("LOCALITY? ")
      a <- readline("FieldCODE? ")
      b <- readline("Authors? ")
      c <- readline("PlotSize? ")
      d <- readline("Latitude? ")
      e <- readline("Longitude? ")
      f <- readline("Accuracy? ")
      g <- readline("E3? ")
      h <- readline("E2? ")
      i <- readline("E1? ")
      j <- readline("Ejuv? ")
      k <- readline("E0? ")
      l <- readline("Note? ")

      extraval<-NULL
      if (!is.null(extrahead)) {
        for (val in extrahead) {
          assign(val, readline(paste0(val,"? ")))
          extraval<-c(extraval,eval(as.symbol(val)))
        }
      }
      hh <- c(ab, bb, bc, a, b, c, d, e, f, g, h, i, j, k, l, extraval)
      Header[2:(length(hh)+1), 2] <- hh
      colnames(Header)[2] <- (1)
      HeaderDATA2 <- data.frame(HeaderDATA2, Header[, 2])
      colnames(HeaderDATA2)[2] <- c(1)
      write.csv(HeaderDATA2, paste0(SAVE, "HEAD.csv"))

    aa = "NEW"
    DATABASE = ""
    } else {
    aa <-
      toupper(readline(
        "AddReleve?(Y/N/RREL/RHEAD/REMOVEREL/PRINTREL/PRINTHEAD) "
      ))
    }

    if (aa == "NEW" | aa == "RREL" | aa == "Y") {


      if (aa == "Y") {

      Header <-
        data.frame(
          ShortName = c("ID","DATE","SpringDATE","LOCALITY","FieldCODE","Authors","PlotSize","Latitude","Longitude","Accuracy",
            "E3","E2","E1","Ejuv","E0","Note",extrahead),
          Value = 0
        )
      RelNew <- data.frame(ShortName = SpLIST[, 2], Value = 0)
      ID <- ncol(DATA2)
      colnames(RelNew)[2] <- (ID - 1)
      Header[1, 2] <- (length(colnames(HeaderDATA2)))
      ab <- readline("DATE(Y/M/D-2009/07/05)? ")
      bb <- readline("SPRINGDATE(Y/M/D-2009/07/05)? ")
      bc <- readline("LOCALITY? ")
      a <- readline("FieldCODE? ")
      b <- readline("Authors? ")
      c <- readline("PlotSize? ")
      d <- readline("Latitude? ")
      e <- readline("Longitude? ")
      f <- readline("Accuracy? ")
      g <- readline("E3? ")
      h <- readline("E2? ")
      i <- readline("E1? ")
      j <- readline("Ejuv? ")
      k <- readline("E0? ")
      l <- readline("Note? ")

      extraval<-NULL
      if (!is.null(extrahead)) {
        for (val in extrahead) {
          assign(val, readline(paste0(val,"? ")))
          extraval<-c(extraval,eval(as.symbol(val)))
        }
      }
      hh <- c(ab, bb, bc, a, b, c, d, e, f, g, h, i, j, k, l, extraval)
      Header[2:(length(hh)+1), 2] <- hh
      colnames(Header)[2] <- (ID - 1)
      HeaderDATA2 <- data.frame(HeaderDATA2, Header[, 2])
      colnames(HeaderDATA2)[2:length(colnames(HeaderDATA2))] <- c(1:(length(colnames(HeaderDATA2)) - 1))
      write.csv(HeaderDATA2, paste0(SAVE, "HEAD.csv"))


    }
      if (aa == "RREL") {
      DATA2 <- read.csv(paste0(SAVE, "REL.csv"), row.names = 1) # rownames
      while (TRUE){

        n <- as.numeric(readline("ReleveNumber? "))
        if (is.na(n)!=TRUE) {
          HeaderDATA3 <- read.csv(paste0(SAVE, "HEAD.csv"), row.names = 1) # rownames
          print(HeaderDATA3[,n+1])
          tt <- toupper(readline("CorrectNumber?Y/N "))
          if (tt == "Y") {
          break
          }
        }
      }

      ID <- n + 1
      RelNew <- data.frame(ShortName = SpLIST[, 2], Value = 0)

      RelNew[rownames(DATA2), ][, 2] <- DATA2[, ID]
      TABLEexp <- RelNew[RelNew[, 2] > 0, ]
      colnames(TABLEexp) <- c("ShortNames", "Cover")
      print(TABLEexp)
    }
      while (TRUE) {
        rlist::list.save(RelNew, "RELnew.rds")
        m <- toupper(readline("AddNewLayer?(Y/N) "))
        if (m == "Y" |m == "N") {
          m <- m
        }

        if (m == "Y") {
          while (TRUE) {
            most <- toupper(readline("Add layer - 3,2,1,J,0 "))
            if (most == 3 | most == 2 |most == 1 |most == "J" |most == 0) {
              break
            }
          }
          while (TRUE) {
            oo <- toupper(readline("P - percentage, BB - Braun B. scale? "))

            if (oo == "P" |oo == "BB"| oo == "B" ) {
              break
            }
          }
          while (TRUE) {
            m <- toupper(readline("AddSpecies?(GenuSpe/N) "))

            if (nchar(m) == 7) {
              n <- m

              if (oo == "P") {
                o <- readline("Abundance(%)? ")
              } else if (oo == "BB"| oo == "B") {
                while (TRUE){
                  o <- toupper(readline("Abundance(0,R,+,1,2,M,A,B,3,4,5)? "))
                  if (o == "R" |o == "+" |o== "0"|o == "1" |o == "2" |o == "M" |o == "A" |o == "B" |
                    o == "3" |o == "4" |o == "5") {
                    break
                  }
                }
                if (o == "R") {
                  o <- 1
                } else if (o == "+") {
                  o <- 2
                } else if (o == "1") {
                  o <- 3
                } else if (o == "2") {
                  o <- 15
                } else if (o == "M") {
                  o <- 4
                } else if (o == "A") {
                  o <- 8
                } else if (o == "B") {
                  o <- 18
                } else if (o == "3") {
                  o <- 38
                } else if (o == "4") {
                  o <- 63
                } else if (o == "5") {
                  o <- 88
                } else if (o == "0") {
                  o <- 0
                }
              }
              nana <- paste(n, most, sep = "_")
              specie_check<-SpLIST[SpLIST[,2]==substr(nana,1,9),]
              print(specie_check[3])
              while(TRUE){
                tt <- toupper(readline("CorrectName?Y/F(search for name) "))
                if (tt == "Y" && !is.na(specie_check[1,3])| tt == "F") {
                  break
                }
              }

              if (tt == "F") {
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
                    nana <- paste(s, most, sep = "_")
                    specie_check<-SpLIST[SpLIST[,2]==substr(nana,1,9),]
                    print(specie_check[3])
                    break
                    }
                  }

                  tt <- toupper(readline("Correct 7lettersGenuSpe Y/N  "))
                  if ((tt == "Y") & !is.na(specie_check[1,3])) {
                    break
                  }
                }

                nanas <- paste(s, most, sep = "_")

                RelNew[which(RelNew$ShortName == substr(nanas, 1, 9)), ][, 2] <- o
                print(RelNew[(RelNew[, 2] > 0), ])
                rlist::list.save(RelNew, "RELnew.rds")
              } else {
                RelNew[which(RelNew$ShortName == substr(nana, 1, 9)), ][, 2] <- o
                print(RelNew[(RelNew[, 2] > 0), ])
                rlist::list.save(RelNew, "RELnew.rds")
              }
            } else if (m == "N"|m == "n") {
              break
            }
          }
        } else if (m == "N" | m == "n") {

          print("Species_richness")
          print(nrow(RelNew[(RelNew[, 2] > 0), ]))
          if (aa == "Y") {
          DATA2<-read.csv(paste0(SAVE, "REL.csv"),row.names = 1) # rownames
          DATA2<-createTABLE(SpLIST,RelNew,DATA2)
          write.csv(DATA2, paste0(SAVE,"REL.csv"))
          break
          }
          if (aa == "RREL") {
          DATA2 <- read.csv(paste0(SAVE, "REL.csv"), row.names = 1) # rownames
          DATA2 <- createTABLE(SpLIST, RelNew, DATA2)
          DATA2 <- DATA2[, -c(ID)]
          colnames(DATA2)[length(colnames(DATA2))] <- ID - 1
          DATA2 <-DATA2[, c(colnames(DATA2)[1], 1:(length(colnames(DATA2)) - 1))]
          write.csv(DATA2, paste0(SAVE, "REL.csv"))
          break
          }
          if (aa == "NEW") {
          DATA2 <- RelNew[(RelNew[, 2] > 0), ]
          colnames(DATA2)[2] <- 1
          write.csv(DATA2, paste0(SAVE, "REL.csv"))
          break
          }
        }
      }
    }

    if (aa == "PRINTHEAD") {
      HeaderDATA3 <- read.csv(paste0(SAVE, "HEAD.csv"), row.names = 1) # rownames
      print(HeaderDATA3)
    }

    if (aa == "PRINTREL") {
      DATA3 <- read.csv(paste0(SAVE, "REL.csv"), row.names = 1) # rownames
      print(DATA3)
    }

    if (aa == "RHEAD") {
    HeaderDATA2 <-read.csv(paste0(SAVE, "HEAD.csv"),row.names = 1) # pro nacteni databaze snimku je treba napsat aktualni jmeno databaze snimku - po praci vzdy ulozit pod novym datem
    while (TRUE){
      n <- as.numeric(readline("ReleveNumber? "))
      if(is.numeric(n)&is.na(n)==FALSE){
        if (n<=(ncol(HeaderDATA2)-1)& n>0) {
          print(HeaderDATA2[, colnames(HeaderDATA2)==paste0("X",n)])

          tt <- toupper(readline("CorrectColumn?Y/F "))
          if (tt == "Y") {
            break
          }
        }
      }

    }
    while (TRUE) {
      m <- toupper(readline("RepairHeader?(Y/N) "))
      if (m == "Y") {
        print(HeaderDATA2[, 1])
        while (TRUE) {
          l <- readline("HedearCharacteristic? ")

          if (l == "ID" |l == "DATE" |l == "SpringDATE" |l == "LOCALITY" |l == "FieldCODE" |
            l == "Authors" |l == "PlotSize" |l == "Latitude" |l == "4" |l == "5" |l == "Longitude" |
            l == "Accuracy" |l == "E3" |l == "E2" |l == "E1" |l == "Ejuv" |l == "E0" |l == "Note"|any(l == extrahead)) {
            break
          }
        }
        o <- readline("NewValue? ")
        HeaderDATA2[which(HeaderDATA2[, 1] == l), ][, n +1] <- o
      } else if (m == "N"|m == "n") {
        break
      }
    }
    write.csv(HeaderDATA2, paste0(SAVE, "HEAD.csv"))
    }

    if (aa == "REMOVEREL") {
      DATA2 <-
        read.csv(paste0(SAVE, "REL.csv"),row.names = 1) # rownames
      while(TRUE) {
        n <- readline("Removesetofnumber? (e.g. 3) ")
        n <- as.numeric(n)
        print(HeaderDATA2[,colnames(HeaderDATA2)==paste0("X",n)])
        tt <- toupper(readline("CorrectNumber?Y/N "))
        if (tt == "Y") {
          DATA2 <- DATA2[,colnames(DATA2)!=paste0("X",n)]
          HeaderDATA2 <- HeaderDATA2[,HeaderDATA2[1,]!=n]
          write.csv(DATA2, paste0(SAVE, "REL.csv"))
          write.csv(HeaderDATA2, paste0(SAVE, "HEAD.csv"))
          break
        }

      }

    }

    if (aa == "N") {
      write.csv(DATA2, paste0(SAVE, "REL.csv"))
      write.csv(HeaderDATA2, paste0(SAVE, "HEAD.csv"))
      break
    }
  }
 options(warn=warn)
}


