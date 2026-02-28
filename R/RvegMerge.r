
#' RvegMerge: Merge Two Rveg Databases
#'
#' @description
#' Combines two separate `Rveg` databases into a single, unified database.
#' This function cleanly merges both the species composition data (REL) and the
#' environmental plot header data (HEAD), and seamlessly combines their metadata.
#'
#' @details
#' During the merge process:
#' * **Relevé Re-indexing:** The relevé columns from both databases are sequentially re-indexed (e.g., `X1`, `X2`, ..., `Xn`) to prevent column name collisions.
#' * **Checklist Validation:** The function checks the metadata of both databases to ensure they were built using the same species checklist. If the checklists differ, it will issue a warning, though the merge will still proceed.
#' * **Missing Species:** If a species exists in one database but not the other, the function automatically fills the absences with `0`.
#' * **Metadata:** The total number of relevés is updated, and any custom species (`extra_spec`) from both databases are concatenated together.
#'
#' @param database_1 Character. The path and name of the first `Rveg` database (e.g., `"path/to/db1"`).
#' @param database_2 Character. The path and name of the second `Rveg` database (e.g., `"path/to/db2"`).
#' @param export Character. The output path and name where the merged database files
#'   (`*HEAD.csv` and `*REL.csv`) will be saved. Defaults to a temporary directory.
#'
#' @return Writes two linked CSV files to the location specified by `export`, representing
#'   the combined Rveg database.
#'
#' @seealso \code{\link{RvegCombine}} for manipulating data within a single database, \code{\link{addReleve}} for adding individual relevés.
#'
#' @examples
#' # Example: Merging the built-in database with itself
#' db_path <- file.path(path.package("Rveg"), "extdata/ExampleDB", "example_1")
#'
#' RvegMerge(
#'   database_1 = db_path,
#'   database_2 = db_path
#' )
#' @export

RvegMerge <- function(database_1, database_2, export = "export") {
  if (export == "export") {
    export <- file.path(tempdir(), "export_merge")
  }

  db1 <- rv_read_db(database_1); db2 <- rv_read_db(database_2)
  rel1 <- db1$RelDATA; head1 <- db1$HeaderDATA; meta1 <- db1$meta
  rel2 <- db2$RelDATA; head2 <- db2$HeaderDATA; meta2 <- db2$meta

  if (db1$meta$checklist != db2$meta$checklist) {
    warning(paste0("Each database was built with different checklist: ",db1$meta$checklist ,", and ",db2$meta$checklist))
  }

  # rel
  jointab <- merge(rel1, rel2, by = c("ShortName"),all = TRUE)
  jointab[is.na(jointab)] <- "0"
  jointab <- jointab[order(jointab$ShortName), ]


  n <- 2:ncol(jointab) - 1
  for (i in 2:ncol(jointab)) {
    colnames(jointab)[i] <- paste0("X", n[i - 1])
  }

  # head
  joinhead <- merge(head1, head2, by = c("ShortName"), all = TRUE, sort = FALSE)
  colnames(joinhead) <- colnames(jointab)
  joinhead[is.na(joinhead)] <- ""

  # metadata
  joinmeta <- meta1
  joinmeta$n_releves <- as.numeric(meta1$n_releves) + as.numeric(meta2$n_releves)
  #joinmeta$checklist <- selection?
  #joinmeta$extra_spec <- paste0(meta1$extra_spec, meta2$extra_spec)

  joinmeta$extra_spec <- mapply(function(a, b) {
    v <- Filter(nzchar, unlist(strsplit(paste0(ifelse(is.na(a), "", a),
                                               ifelse(is.na(b), "", b)), "\\|")))
    sc <- tolower(trimws(ifelse(grepl("::", v, fixed = TRUE), sub("^.*::", "", v), v)))
    v <- v[!duplicated(sc)]
    if (length(v)) paste0(paste(v, collapse = "|"), "|") else ""
  }, meta1$extra_spec, meta2$extra_spec, USE.NAMES = FALSE)

  # new id?

  # save
  rv_write_db(jointab, joinhead, export, joinmeta)

  }
