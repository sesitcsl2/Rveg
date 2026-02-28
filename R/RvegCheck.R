#' RvegCheck: Inspect, Validate, Edit and Repair an Rveg Database
#'
#' @description
#' A diagnostic and managing utility to verify the health and integrity of an `Rveg` database.
#' `RvegCheck()` reads your database, checks for missing or corrupted data,
#' and outputs a summary of the database's current state.
#'
#' @details
#' *Note: This function is currently under active development.* #'
#' At present, the function serves primarily as a diagnostic and metadata-repair tool.
#' When run, it checks the database for a `project_name` and `project_description`.
#' If these are missing, it will interactively prompt the user to supply them. It also
#' prints vital database statistics, including the internal ID, creation date, last
#' modification date, and any custom species added to the checklist.
#'
#' In future updates, this function will also allow users to modify core database
#' parameters, such as swapping the underlying species checklist or altering the
#' header schema.
#'
#' @param database Character. The path and name of the existing `Rveg` database
#' to inspect (e.g., `"path/to/my_db"`).
#' @param export Character. The output path and name where the modified
#' database will be saved. Defaults to a temporary directory.
#' @param checklist Character. The species checklist to validate the database against.
#' By default, it uses the checklist defined in the database's metadata.
#'
#' @return Currently prints diagnostic information directly to the R console.
#'   If modifications are made, it exports the updated database files (`*HEAD.csv`
#'   and `*REL.csv`) to the path specified by `export`.
#'
#' @seealso \code{\link{RvegMerge}} for combining databases, \code{\link{RvegCombine}} for manipulating data within a database.
#'
#' @examples
#' if (interactive()) {
#'   # Inspect the built-in example database
#'   RvegCheck(
#'     database = file.path(path.package("Rveg"), "extdata/ExampleDB", "example_1")
#'   )
#' }
#'
#' @export

RvegCheck <- function(database, export = "export", checklist = "default") {

  if (export == "export") {
    export <- file.path(tempdir(), "export")
  }

  db <- rv_read_db(database)
  DATA <- db$RelDATA; HeaderDATA <- db$HeaderDATA; metadata <- db$meta

  meta_checklist <- db$meta$checklist # ignore checklists prompt on existing
  if (file.exists(rv_get_checklist(meta_checklist))) {
    checklist <- rv_get_checklist(meta_checklist)
  }
  SpLIST <- rv_make_sp_list(checklist,db$meta)

  # custom scale
  customscale <- rv_ask_choice("Do you want transfer cover to percentage values? (Y/N)",c("Y","N"))
  if (customscale == "Y") {
    DATA <- rv_cs_to_pct(db$RelDATA)
  }

  # project name
  if (nzchar(db$meta$project_name)) {
    message(paste0("Project: ", db$meta$project_name))
  } else {
    v <- readline("No project name found. Enter? (Y/N) ")
    if(toupper(v) %in% c("Y","YES"))
    metadata$project_name <- readline("Project name: ")
    }

  # project info
  if (nzchar(db$meta$project_description)) {
    message(paste0("Project info: ", metadata$project_description))
  } else {
    v <- readline("No project info found. Enter? (Y/N) ")
    if(toupper(v) %in% c("Y","YES"))
      metadata$project_description <- readline("Project info: ")
    }

  # project id
  message(paste0("id: ",metadata$db_id))

  # Rveg version
  message(paste0("Rveg built version: ", metadata$rveg_version))

  # Extra species
  if (nzchar(metadata$extra)) {
    message("No Custom species")
  } else {

    entries <- strsplit(metadata$extra, "\\|")[[1]]
    entries <- entries[nzchar(entries)]
    parts <- strsplit(entries, "::", fixed = TRUE)

    for (p in parts) {
      cat(sprintf("%s - %s\n", p[1], p[2]))
    }


    }

  # Creation
  message(paste0("Database created: ", metadata$created))
  message(paste0("Last change: ",metadata$last_change))

  # export
  rv_write_db(rel = DATA,head = HeaderDATA,meta = metadata,save = export)

}

