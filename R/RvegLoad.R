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

RvegLoad <- function(DATABASE = "default",checklist = "default", CustomScale = FALSE, variation = 1) {

  if (DATABASE == "default") {
    #DATABASE = paste0(path.package("Rveg"), "/extdata/example_db")
    DATABASE <- system.file("extdata", package = "Rveg", mustWork = TRUE)
    DATABASE <- paste0(DATABASE,"/example")
  }

  db <- read_db(DATABASE)

  if (CustomScale == TRUE) {

    rele <- db$RelDATA[,-1]
    releorig <- rele
    uniq <- unique(unlist(rele))

    for (val in uniq) {
      replace <- readline(paste0("Percentage value for (",val,") "))
      rele[releorig==val] <- replace
    }

    db$RelDATA[,-1] <- rele
  }

  if (checklist == "default") {
    checklist <- db$meta$checklist #
    if (checklist %in% c("wcvp_por","wcvp_que","cz_dh2012")) {
      checklist <- system.file("extdata",paste0(checklist,".txt"),package="Rveg",mustWork = TRUE)
    }
  }

  SpLIST1 <- read.delim(checklist, sep = "\t")
  SpLIST <- makeSpLIST(checklist, db$meta) # creating Species checklist

  fullName <- c(rep("", nrow(db$RelDATA)))
  layer <- c(rep("", nrow(db$RelDATA)))
  layer <- cbind(fullName, layer) # fullname a layer

  DATArele <- cbind(layer, db$RelDATA) # pro vrstvu
  for (i in 1:length(DATArele$ShortName)) {
    DATArele$fullName[i] <- SpLIST$FullName[SpLIST$ShortName == substr(DATArele$ShortName[i], 1, 9)]
    DATArele$layer[i] <- substr(DATArele$ShortName[i], 9, 9)
    DATArele$ShortName[i] <- substr(DATArele$ShortName[i], 1, 7)
  }

  DATArele <- cbind(DATArele$ShortName,DATArele[,-3])
  colnames(DATArele)[1] <- "ShortName"

  DATAhead <- cbind(rep("",times=nrow(db$HeaderDATA)),db$HeaderDATA)
  DATAhead <- cbind(rep("",times=nrow(DATAhead)),DATAhead)
  DATAhead <- cbind(DATAhead$ShortName,DATAhead[,-3])
  colnames(DATAhead) <- colnames(DATArele)

  if (variation == 1) {
    output <- rbind(DATAhead,DATArele)
  } ## one table cbind header and releve + fullnames

  if (variation == 2) {
    output <- db
  } ## raw output

  if (variation == 3) {
    db2<- db
    db2$RelDATA <- DATArele
    output <- db2
    } ## separate output with fullnames.

  return(output)

  }
