#'
#' RvegLoad
#'
#' Reading your Rveg database
#' @param DATABASE name of Rveg database
#' @param CustomScale logical values if different than predefined scale was used during the database creation
#' @param checklist used checklist
#'
#' @returns read the database in one object
#'
#' @examples
#'
#'   RvegLoad()
#'
#' @export
#'
#'

RvegLoad <- function(DATABASE = "default",CustomScale = FALSE,checklist = "default") {

  if (DATABASE == "default") {
    DATABASE = paste0(path.package("Rveg"), "/extdata/example_db")
  }

  DATArele <- read.csv(paste0(DATABASE, "REL.csv"), row.names = 1)
  DATAhead <- read.csv(paste0(DATABASE, "HEAD.csv"), row.names = 1)

  if (CustomScale == TRUE) {

    rele <- DATArele[,-1]
    releorig <- rele
    uniq <- unique(unlist(rele))

    for (val in uniq) {
      replace <- readline(paste0("Percentage value for (",val,") "))
      rele[releorig==val] <- replace
    }

    DATArele[,-1] <- rele
  }

  if (checklist == "default") {
    checklist <- paste0(path.package("Rveg"), "/extdata/DANIHELKA2012rko.txt")
  }

  SpLIST <- read.delim(checklist, sep = "\t")
  SpLIST1 <- makeSpLIST(checklist) # navic pridane ODSTRANIT


  fullName <- c(rep("", nrow(DATArele)))
  layer <- c(rep("", nrow(DATArele)))
  layer <- cbind(fullName, layer) # fullname a layer
  DATArele <- cbind(layer, DATArele) # pro vrstvu
  for (i in 1:length(DATArele$ShortName)) {
    DATArele$fullName[i] <- SpLIST$FullName[SpLIST$ShortName == substr(DATArele$ShortName[i], 1, 7)]
    DATArele$layer[i] <- substr(DATArele$ShortName[i], 9, 9)
    DATArele$ShortName[i] <- substr(DATArele$ShortName[i], 1, 7)
  }

  DATArele <- cbind(DATArele$ShortName,DATArele[,-3])
  colnames(DATArele)[1] <- "ShortName"
  DATAhead <- cbind(rep("",times=nrow(DATAhead)),DATAhead)
  DATAhead <- cbind(rep("",times=nrow(DATAhead)),DATAhead)
  DATAhead <- cbind(DATAhead$ShortName,DATAhead[,-3])
  colnames(DATAhead) <- colnames(DATArele)


  output <- rbind(DATAhead,DATArele)

  return(output)

  }
