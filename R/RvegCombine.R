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

  db <- read_db(database)
  print(db$HeaderDATA); print(db$RelDATA)

  while (TRUE) {
    a <- toupper(readline("Combine?(LAYER/SPEC/PRINTREL/N) ")) # layer selection
    if (a == "LAYER") {
      while (TRUE) {
        b <- toupper(readline("Which layer?(3/2/1/0/J) "))
        c <- toupper(readline("To which layer?(3/2/1/0/J) "))
        if (any(c(b, c) %in% c(1, 2, 3, "J", 0))) {
          for (i in db$RelDATA$ShortName) {
            if (i == paste0(substr(i, 1, 7), "_", b) && any(db$RelDATA$ShortName == paste0(substr(i, 1, 7), "_", c))) {
              l1 <- as.numeric(db$RelDATA[db$RelDATA$ShortName == i, -1])
              l2 <- as.numeric(db$RelDATA[db$RelDATA$ShortName == paste0(substr(i, 1, 7), "_", c), -1])
              l3 <- round(l1 + (l2 * (1 - (l1 / 100))))

              db$RelDATA <- db$RelDATA[db$RelDATA$ShortName != paste0(substr(i, 1, 7), "_", b), ]
              db$RelDATA[db$RelDATA$ShortName == paste0(substr(i, 1, 7), "_", c), -1] <- l3

              write_db(db$RelDATA, db$HeaderDATA, SAVE = export, meta = db$meta)

            } else if (i == paste0(substr(i, 1, 7), "_", b) && !any(db$RelDATA$ShortName == paste0(substr(i, 1, 7), "_", c))) {

              db$RelDATA <- db$RelDATA
              db$RelDATA[db$RelDATA$ShortName == i, 1] <- paste0(substr(i,1,7),"_", c) # changing single layer

              SpLIST <- makeSpLIST(checklist,metadata = db$meta)
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
          l1 <- as.numeric(db$RelDATA[db$RelDATA$ShortName == b, -1])
          l2 <- as.numeric(db$RelDATA[db$RelDATA$ShortName == c, -1])
          l3 <- round(l1 + (l2 * (1 - (l1 / 100)))) # propability shared cover

          db$RelDATA <- db$RelDATA[db$RelDATA$ShortName != b, ]
          db$RelDATA[db$RelDATA$ShortName == c, -1] <- l3

          write_db(db$RelDATA, db$HeaderDATA, SAVE = export, meta = db$meta)


          break
        } else {
          warning("wrong species input")
        }
      }
    } else if (a == "PRINTREL") {
      print(db$RelDATA)
    } else if (a == "N") {
      break
    }
  }
}
