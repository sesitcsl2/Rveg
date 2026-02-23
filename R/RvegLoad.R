#' RvegLoad: Load an Rveg Database into the R Environment
#'
#' @description
#' Reads an existing `Rveg` database (containing relevé data, header data, and metadata)
#' into R for analysis. By default, it automatically translates the internal 7-character
#' `ShortName` codes back into full botanical names and extracts the layer information.
#'
#' @details
#' The structure of the imported data is controlled by the `variation` parameter:
#' * **`variation = 1` (Default):** Returns a single, large data frame. The header data (HEAD) is bound to the top of the species composition data (REL), with new columns added for `FullName` and `layer`.
#' * **`variation = 2`:** Returns the raw database as a list of three elements: `$HeaderDATA`, `$RelDATA`, and `$meta` (metadata) without any taxonomic translation.
#' * **`variation = 3`:** Returns the database as a list of three elements, but `$RelDATA` is processed to include the translated `FullName` and `layer` columns.
#'
#' **Custom Scales:** If `customscale = TRUE`, the function operates interactively. It will pause and prompt the user to manually define percentage replacements for any custom abundance symbols found in the data.
#'
#' @param database Character. The path and name of the `Rveg` database to load (e.g., `"path/to/my_db"`).
#' Defaults to `"default"`, which loads the package's built-in example database.
#' @param checklist Character. The species checklist to use for taxonomy translation.
#' By default, the function uses the checklist specified in the database's metadata.
#' @param customscale Logical. If `TRUE`, launches an interactive prompt to convert custom abundance scales into numeric percentages. Defaults to `FALSE`.
#' @param variation Numeric. Determines the format of the returned object (`1`, `2`, or `3`). Defaults to `1`.
#'
#' @return A data frame (if `variation = 1`) or a list containing data frames and metadata (if `variation = 2` or `3`).
#'
#' @examples
#' # Example 1: Load the built-in Rveg database into a single data frame
#' my_data <- RvegLoad()
#'
#' # Example 2: Load the database as a list with separated Header and Releve tables
#' my_list <- RvegLoad(variation = 3)
#'
#' @export

RvegLoad <- function(database = "default",checklist = "default", customscale = FALSE, variation = 1) {

  if (database == "default") {
    #database = paste0(path.package("Rveg"), "/extdata/example_db")
    database <- system.file("extdata", package = "Rveg", mustWork = TRUE)
    database <- paste0(database,"/example")
  }

  db <- rv_read_db(database)

  if (customscale) {

    rele <- db$RelDATA[,-1]
    releorig <- rele
    uniq <- unique(unlist(rele))

    for (val in uniq) {
      replace <- readline(paste0("Percentage value for (",val,") "))
      rele[releorig==val] <- replace
    }

    db$RelDATA[,-1] <- rele
  }

  checklist <- db$meta$checklist
  checklist <- rv_get_checklist(checklist)

  SpLIST <- rv_make_sp_list(checklist, db$meta) # creating Species checklist

  #fullName <- c(rep("", nrow(db$RelDATA)))
  #layer <- c(rep("", nrow(db$RelDATA)))

  layer <- data.frame(fullName = character(nrow(db$RelDATA)),
                      layer = character(nrow(db$RelDATA)),
                      stringsAsFactors = FALSE)

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
