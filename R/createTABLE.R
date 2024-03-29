createTABLE <- function(SpLIST, RelNew, DATA2) {

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
