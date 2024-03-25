#'
#' RvegCombine
#'
#' Merging species or layers in the database
#'
#' @param database name of the loading database
#' @param export name of the exported database
#' @param checklist checklist to be used
#'
#' @returns export two csv files, one for releve and one for header
#'
#' @examples
#' ## NOT RUN
#' if (interactive()) {
#'   RvegCombine(database = paste0(
#'     path.package("Rveg"),
#'     "/extdata/example_db"
#'   ))
#'   read.csv("exportREL.csv", row.names = 1)
#' }
#'
#' @export
#'




RvegCombine <- function(database, export = "export", checklist = "default") {
  if (export == "export") {
    export <- file.path(tempdir(), "export")
  }
  if (checklist == "default") {
    checklist <- paste0(path.package("Rveg"), "/extdata/DANIHELKA2012rko.txt")
  }

  DATA <- read.csv(paste0(database, "REL.csv"), row.names = 1)
  HeaderDATA <- read.csv(paste0(database, "HEAD.csv"), row.names = 1)

  while (TRUE) {
    a <- toupper(readline("Combine?(LAYER/SPEC/PRINTREL/N) ")) # layer selection
    if (a == "LAYER") {
      while (TRUE) {
        b <- toupper(readline("Which layer?(3/2/1/0/J) "))
        c <- toupper(readline("To which layer?(3/2/1/0/J) "))
        if (any(c(b, c) %in% c(1, 2, 3, "J", 0))) {
          for (i in DATA$ShortName) {
            if (i == paste0(substr(i, 1, 7), "_", b) && any(DATA$ShortName == paste0(substr(i, 1, 7), "_", c))) {
              l1 <- DATA[DATA$ShortName == i, -1]
              l2 <- DATA[DATA$ShortName == paste0(substr(i, 1, 7), "_", c), -1]
              l3 <- round(l1 + (l2 * (1 - (l1 / 100))))
              DATA <- DATA[DATA$ShortName != paste0(substr(i, 1, 7), "_", b), ]
              DATA[DATA$ShortName == paste0(substr(i, 1, 7), "_", c), -1] <- l3

              write.csv(DATA, paste0(export, "REL.csv"))
              write.csv(HeaderDATA, paste0(export, "HEAD.csv")) # merge Two existing layerss
            } else if (i == paste0(substr(i, 1, 7), "_", b) && !any(DATA$ShortName == paste0(substr(i, 1, 7), "_", c))) {
              DATA[DATA$ShortName == i, 1] <- paste0(substr(i,1,7),"_", c) # changing single layer
              SpLIST <- makeSpLIST(checklist)
              for (i in 2:nrow(DATA)) {
                rownames(DATA)[i] <- row.names(SpLIST)[SpLIST$ShortName==DATA$ShortName[i]]
              }


              }
          }

          break
        } else {
          warning("wrong layers input")
        }
      }
    } else if (a == "SPEC") {
      while (TRUE) {
        b <- toupper(readline("Which specie?(GenuSpe_L) "))
        c <- toupper(readline("To which layer?(GenuSpe_L) "))
        if (nchar(b) == 9 & nchar(c) == 9) {
          l1 <- DATA[DATA$ShortName == b, -1]
          l2 <- DATA[DATA$ShortName == c, -1]
          l3 <- round(l1 + (l2 * (1 - (l1 / 100)))) # propability shared cover

          DATA <- DATA[DATA$ShortName != b, ]
          DATA[DATA$ShortName == c, -1] <- l3

          write.csv(DATA, paste0(export, "REL.csv"))
          write.csv(HeaderDATA, paste0(export, "HEAD.csv"))


          break
        } else {
          warning("wrong species input")
        }
      }
    } else if (a == "PRINTREL") {
      print(DATA)
    } else if (a == "N") {
      break
    }
  }
}
