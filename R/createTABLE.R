createTABLE <- function(SpLIST, RelNew, DATA2, variation = 1) {

  if (variation == 1) {
    ## Create empty TABLE and zz Relevé ID counter
    TABLE <- data.frame(number = row.names(SpLIST), ShortName = SpLIST[, 2], Value = 0)
    TABLE <- TABLE[order(TABLE[,2]), ] #ordering the table based on ShortNames (only for new ordering)
    zz <- c()

    ## speed increase by pre ordering
    DATA2 <- DATA2[order(DATA2[,1]),]
    RelNewKrs <- data.frame(ShortName = SpLIST[, 2], Value = 0) # Create empty checklist
    RelNewKrs <- RelNewKrs[order(RelNewKrs[,1]), ]

    for (i in 2:length(colnames(DATA2))) {

      zzz <- DATA2[, c(1, i)] # Copy i releve
      #zzz <- zzz[order(as.numeric(row.names(zzz))), ] # previous order based on IDs
      zzz <- zzz[order(zzz[,1]), ] # new ordering base od ShortNames
      #RelNewKrs <- RelNewKrs[order(as.numeric(row.names(RelNewKrs))), ]
      RNK <- RelNewKrs
      RNK[RNK[, 1] %in% zzz[, 1], ][, 2] <- zzz[, 2] # Match the Relevé with SPlist

      ## check if the function works properly
      a <- RNK[RNK[, 1] %in% zzz[, 1], ][, 1]
      b <- zzz[, 1]
      if (!all(a == b)) {
        warning("Mismatch while matching the table, please make backup of your current database and
                           check your original data and used checklist.")
      }

      TABLE <- data.frame(TABLE, RNK[, 2])
      zz <- c(zz, i)
    }

    TABLE <- TABLE[, -c(3)] # removing empty collumn
    RelNew <- RelNew[order(RelNew[,1]), ] # ordering the new relevé by ShortNames
    TABLE <- data.frame(TABLE, RelNew[, 2]) # merging the Tables
    TABLE[] <- lapply(TABLE, as.character)
    TABLEexp <- TABLE[apply(TABLE[, c(-1, -2)], 1, function(x) sum(x != "0")) > 0, ] # Remove non present species
    TABLEexp <- TABLEexp[, -c(1)]
    colnames(TABLEexp)[2:(length(zz) + 2)] <- c(1:(length(zz) + 1)) # naming the collumns
    TABLEexp <- TABLEexp[order(as.numeric(rownames(TABLEexp))),] # Sorting back by previous ID <- by layers
    return(TABLEexp)
    print(TABLEexp)
  }

  if (variation == 2) {
    ## Create empty TABLE and zz Relevé ID counter
    TABLE <- data.frame(number = row.names(SpLIST), ShortName = SpLIST[, 2])
    # for (i in 1:length(RelNew)) {
    #   TABLE[,paste0("r",i)] <- 0
    # }
    TABLE <- TABLE[order(TABLE[,2]), ] #ordering the table based on ShortNames (only for new ordering)
    zz <- c()

    ## speed increase by pre ordering
    if (ncol(DATA2)!=1) {
      DATA2 <- DATA2[order(DATA2[,1]),]

      RelNewKrs <- data.frame(ShortName = SpLIST[, 2], Value = 0) # Create empty checklist
      RelNewKrs <- RelNewKrs[order(RelNewKrs[,1]), ]

      for (i in 2:length(colnames(DATA2))) {

        zzz <- DATA2[, c(1, i)] # Copy i releve
        #zzz <- zzz[order(as.numeric(row.names(zzz))), ] # previous order based on IDs
        zzz <- zzz[order(zzz[,1]), ] # new ordering base od ShortNames
        #RelNewKrs <- RelNewKrs[order(as.numeric(row.names(RelNewKrs))), ]
        RNK <- RelNewKrs
        RNK[RNK[, 1] %in% zzz[, 1], ][, 2] <- zzz[, 2] # Match the Relevé with SPlist

        ## check if the function works properly
        a <- RNK[RNK[, 1] %in% zzz[, 1], ][, 1]
        b <- zzz[, 1]
        if (!all(a == b)) {
          warning("Mismatch while matching the table, please make backup of your current database and
                           check your original data and used checklist.")
        }

        TABLE <- data.frame(TABLE, RNK[, 2])
        zz <- c(zz, i)
      }
    }




    #TABLE <- TABLE[, -c(3)] # removing empty collumn # now I have previous table sorted?! Tak hodne prace za malo muziky
    if (T) {
        RelSort <- RelNew[["r1"]]
        if (length(RelNew)>1) {
          for (i in 2:length(RelNew)) {
            RelSort <- cbind(RelSort,RelNew[[paste0("r",i)]][,-1])
          }
        }


      RelNew <- RelSort[order(RelSort[,1]), ] # ordering the new relevé by ShortNames
      TABLE <- data.frame(TABLE, RelNew[,-1]) # merging the Tables
      TABLE[] <- lapply(TABLE, as.character)
      TABLEexp <- TABLE[apply(TABLE[, c(-1, -2)], 1, function(x) sum(x != "0")) > 0, ] # Remove non present species
      TABLEexp <- TABLEexp[, -c(1)]
      colnames(TABLEexp)[-1] <- c(1:(length(zz) + length(RelNew)-1)) # naming the collumns
      TABLEexp <- TABLEexp[order(as.numeric(rownames(TABLEexp))),] # Sorting back by previous ID <- by layers
      return(TABLEexp)
      print(TABLEexp)
    }
  }

}

makeSpLIST <- function(checklist) {
  SpLIST <- read.delim(checklist, sep = "\t")
  aaa <- paste(SpLIST[, 2], 3, sep = "_")
  bbb <- paste(SpLIST[, 2], 2, sep = "_")
  ccc <- paste(SpLIST[, 2], 1, sep = "_")
  eee <- paste(SpLIST[, 2], "J", sep = "_")
  fff <- paste(SpLIST[, 2], 0, sep = "_")

  ddd <- c(aaa, bbb, ccc, eee, fff)
  SpLIST <- rbind(SpLIST, SpLIST, SpLIST, SpLIST, SpLIST) # combining species and layers
  SpLIST[, 2] <- ddd

  return(SpLIST)
}

createHEADER <- function(DATA) {


  Header <- data.frame(ShortName = rownames(DATA),
                       Value = 0)
  #Header[1, 2] <- (length(colnames(DATA)))
  Header[1, 2] <- as.numeric(DATA[1,ncol(DATA)])+1


  if (is.na(Header[1, 2])) {
    Header[1, 2] <- 1
  }

  ## hinting previous prompts
  ht <- list()# header return
  hhp <- DATA[-1,ncol(DATA)]

  message(paste0("RE <- ",hhp[1]))
  ab <- readline("DATE?(YYYY/MM/DD) ")
  message(paste0("RE <- ",hhp[2]))
  bb <- readline("SPRINGDATE?(YYYY/MM/DD) ")
  message(paste0("RE <- ",hhp[3]))
  bc <- readline("LOCALITY? ")
  message(paste0("RE <- ",hhp[4]))
  a <- readline("FieldCODE? ")
  message(paste0("RE <- ",hhp[5]))
  b <- readline("Authors? ")
  message(paste0("RE <- ",hhp[6]))
  c <- readline("PlotSize?(m2) ")
  message(paste0("RE <- ",hhp[7]))
  d <- readline("Latitude? ")
  message(paste0("RE <- ",hhp[8]))
  e <- readline("Longitude? ")
  message(paste0("RE <- ",hhp[9]))
  f <- readline("Accuracy?(m) ")
  message(paste0("RE <- ",hhp[10]))
  crs <- readline("Coordinta Reference System? ")
  message(paste0("RE <- ",hhp[11]))
  ff <- readline("Slope?(degrees) ")
  message(paste0("RE <- ",hhp[12]))
  fk <- readline("Exposure? ")
  message(paste0("RE <- ",hhp[13]))
  g <- readline("E3?(%) ")
  message(paste0("RE <- ",hhp[14]))
  h <- readline("E2?(%) ")
  message(paste0("RE <- ",hhp[15]))
  i <- readline("E1?(%) ")
  message(paste0("RE <- ",hhp[16]))
  j <- readline("Ejuv?(%) ")
  message(paste0("RE <- ",hhp[17]))
  k <- readline("E0?(%) ")
  message(paste0("RE <- ",hhp[18]))
  l <- readline("Note? ")

  if (length(hhp)>18) {
    extrahead <- c()
    for (ext in 1:(length(hhp)-18)) {
      extrahead[ext] <- DATA[ext+19,1]
    }
  } else {
    extrahead <- NULL
  }

  extraval <- NULL
  if (!is.null(extrahead)) {
    ehcounter <- 1 # extrahead counter
    for (val in extrahead) {
      message(paste0("RE <- ",hhp[18+ehcounter]))
      ehcounter <- ehcounter + 1
      assign(val, readline(paste0(val, "? ")))
      extraval <- c(extraval, eval(as.symbol(val)))
    }
  }

  hh <- c(ab, bb, bc, a, b, c, d, e, f, crs, ff, fk, g, h, i, j, k, l, extraval)

  for (hval in 1:length(hh)) {
    if (toupper(hh[hval]) == "RE") {
      hh[hval] <- hhp[hval]
    }
  }

  Header[2:(length(hh) + 1), 2] <- hh
  colnames(Header)[2] <- (ncol(DATA) - 1)
  return(Header)
  #HeaderDATA2 <- data.frame(HeaderDATA2, Header[, 2])
  #colnames(HeaderDATA2)[2:length(colnames(HeaderDATA2))] <- paste0("X",c(1:(length(colnames(HeaderDATA2)) - 1)))
  #write.csv(HeaderDATA2, paste0(SAVE, "HEAD.csv"))



  }
