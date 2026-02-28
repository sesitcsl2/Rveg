#' RvegCombine: Merge Species or Vegetation Layers in an Rveg Database
#'
#' @description
#' An interactive utility that allows users to merge the abundance covers of
#' specific species or entire vegetation layers within an existing `Rveg` database.
#'
#' @details
#' `RvegCombine()` operates via a console menu with two primary modes:
#' * **Layer Merging (`LAYER`):** Moves all species recorded in one specific layer (e.g., shrub layer `2`) into another layer (e.g., tree layer `3`).
#' * **Species Merging (`SPEC`):** Merges the records of one specific taxon into another across the entire database, which is highly useful for resolving taxonomic aggregates or correcting identification errors after data entry.
#'
#' **Mathematical Consolidation:** #' When merging entities that both have non-zero percentage covers in the same relevé,
#' the function does not simply add them together (which could exceed 100%). Instead,
#' it uses a probabilistic sum formula to estimate the combined cover:
#' \deqn{Combined = C_1 + C_2 \times (1 - \frac{C_1}{100})}{Combined = C1 + C2 * (1 - C1/100)}
#'
#' @param database Character. The path and name of the existing `Rveg` database
#' to be modified (e.g., `"path/to/my_db"`).
#' @param export Character. The output path and name where the modified database
#' files (`*HEAD.csv` and `*REL.csv`) will be saved. Defaults to a temporary directory.
#' @param checklist Character. The species checklist to be used. By default, it uses
#' the checklist defined in the database's metadata.
#'
#' @return Writes two linked CSV files to the location specified by `export`,
#' representing the modified Rveg database.
#'
#' @seealso \code{\link{addReleve}} for data entry, \code{\link{RvegMerge}} for merging entire databases.
#'
#' @examples
#' if (interactive()) {
#'   RvegCombine(
#'     database = file.path(path.package("Rveg"), "extdata/ExampleDB", "example_1")
#'   )
#' }
#'
#' @export

RvegCombine <- function(database, export = "export", checklist = "default") {
  message(rv_col("Only call this function on databases with percentage cover, if you have different scales, call RvegCheck first.","warn"))
  if (export == "export") {
    export <- file.path(tempdir(), "export")
  }


  db <- rv_read_db(database)
  DATA <- db$RelDATA; HeaderDATA <- db$HeaderDATA; metadata <- db$meta

  meta_checklist <- db$meta$checklist # ignore checklists prompt on existing
  if (file.exists(rv_get_checklist(meta_checklist))) {
    checklist <- rv_get_checklist(meta_checklist)
  }


  while (TRUE) {
    a <- toupper(readline("Combine?(LAYER/SPEC/PRINTREL/N) ")) # layer selection
    if (a == "LAYER") {
      while (TRUE) {
        b <- toupper(readline("Which layer?(3/2/1/0/J) "))
        c <- toupper(readline("To which layer?(3/2/1/0/J) "))
        if (all(c(b, c) %in% c(1, 2, 3, "J", 0))) {
          for (i in DATA$ShortName) {
            if (i == paste0(substr(i, 1, 7), "_", b) && any(DATA$ShortName == paste0(substr(i, 1, 7), "_", c))) {
              l1 <- as.numeric(DATA[DATA$ShortName == i, -1])
              l2 <- as.numeric(DATA[DATA$ShortName == paste0(substr(i, 1, 7), "_", c), -1])
              l3 <- round(l1 + (l2 * (1 - (l1 / 100))))

              DATA <- DATA[DATA$ShortName != paste0(substr(i, 1, 7), "_", b), ]
              DATA[DATA$ShortName == paste0(substr(i, 1, 7), "_", c), -1] <- l3

            } else if (i == paste0(substr(i, 1, 7), "_", b) && !any(DATA$ShortName == paste0(substr(i, 1, 7), "_", c))) {

              DATA[DATA$ShortName == i, 1] <- paste0(substr(i,1,7),"_", c) # changing single layer
              }
          }

          rv_write_db(DATA, HeaderDATA, save = export, meta = db$meta)
          break

        } else {
          message(rv_col("wrong layers input","err"))
        }
      }
    } else if (a == "SPEC") {
      while (TRUE) {
        b <- toupper(readline("Which specie?(GenuSpe_L) "))
        c <- toupper(readline("With which specie?(GenuSpe_L) "))
        if (nchar(b) == 9 & nchar(c) == 9) {

          if (!(b %in% DATA$ShortName) || !(c %in% DATA$ShortName)) {
            message(rv_col("One or both species not found in data. Please try again.","warn"))
            next
          }

          l1 <- as.numeric(DATA[DATA$ShortName == b, -1])
          l2 <- as.numeric(DATA[DATA$ShortName == c, -1])
          l3 <- round(l1 + (l2 * (1 - (l1 / 100)))) # propability shared cover

          DATA <- DATA[DATA$ShortName != b, ]
          DATA[DATA$ShortName == c, -1] <- l3

          rv_write_db(DATA, HeaderDATA, save = export, meta = db$meta)


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
