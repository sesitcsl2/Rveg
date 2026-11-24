
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
#' * **Checklist Validation:** The result uses the first database's checklist.
#'   The function warns when the checklists differ and reports code changes
#'   and matches based on normalized names.
#' * **Missing Species:** If a species exists in one database but not the other, the function automatically fills the absences with `0`.
#' * **Metadata:** Taxa without a unique target code are kept as separate
#'   entries in `extra_spec`. The total number of relevés is updated.
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
#' db_path <- file.path(path.package("Rveg"), "extdata/ExampleDB", "example_2")
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

  resolve_checklist <- function(database, meta) {
    fallback <- if (isTRUE(nzchar(meta$checklist, keepNA = TRUE))) NULL else "default"
    rv_get_checklist(checklist = fallback, meta = meta, wd = dirname(database))
  }
  checklist1 <- resolve_checklist(database_1, meta1)
  checklist2 <- resolve_checklist(database_2, meta2)
  different <- !identical(checklist1, checklist2)
  if (different) {
    warning("Databases use different checklists: ", checklist1, " and ", checklist2,
      ". The merged database will use the first checklist.", call. = FALSE)
  }

  target <- rv_make_sp_list(checklist1, meta1)
  source <- rv_make_sp_list(checklist2, meta2)
  mapping <- rv_merge_taxon_codes(rel1$ShortName, rel2$ShortName, target, source,
    rv_extra_species(meta2)$ShortName)
  base2 <- substr(rel2$ShortName, 1L, 7L)
  rel2$ShortName <- paste0(mapping$code[match(base2, mapping$source)], substring(rel2$ShortName, 8L))

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
  recorded_checklist <- tryCatch(rv_get_checklist(checklist = NULL, meta = meta1),
    error = function(e) NULL)
  if (!identical(recorded_checklist, checklist1)) joinmeta$checklist <- checklist1

  extras <- mapping$extra_species
  existing <- if (isTRUE(nzchar(meta1$extra_spec, keepNA = TRUE))) meta1$extra_spec else ""
  if (nrow(extras) && nzchar(existing) && !endsWith(existing, "|")) existing <- paste0(existing, "|")
  joinmeta$extra_spec <- paste0(existing, paste(sprintf("%s::%s|", extras$FullName, extras$ShortName),
    collapse = ""))

  # new id?

  # save
  rv_write_db(jointab, joinhead, export, joinmeta)

  if (length(mapping$changes)) {
    heading <- rv_col("Taxon changes (database 2 -> merged database)", "title")
    message(heading, "\n", paste(mapping$changes, collapse = "\n"))
  }
  }

# Map the second database's codes before joining rows, preserving its separate taxa.
rv_merge_taxon_codes <- function(first_rows, second_rows, target, source, extra_codes) {
  if (anyDuplicated(first_rows) || anyDuplicated(second_rows))
    stop("Duplicate species and layer codes in an input database.", call. = FALSE)

  first_codes <- unique(substr(first_rows, 1L, 7L))
  second_codes <- unique(substr(second_rows, 1L, 7L))
  needed <- unique(c(second_codes, extra_codes))
  source <- source[match(needed, source$ShortName), , drop = FALSE]
  source_key <- rv_taxon_key(source$FullName)
  unknown <- is.na(source$ShortName) | is.na(source_key) | !nzchar(source_key)
  if (any(unknown))
    stop("Second database has codes without a name in its checklist or metadata: ",
      paste(needed[unknown], collapse = ", "), call. = FALSE)

  candidates <- rv_checklist_matches(source$FullName, target)
  code <- vapply(candidates, function(codes) {
    if (length(codes) == 1L) codes else NA_character_
  }, character(1L), USE.NAMES = FALSE)

  # Keep a shared code when both databases use it for the same name.
  previous_names <- target$FullName[match(needed, target$ShortName)]
  target_key <- rv_taxon_key(previous_names)
  same_code <- !is.na(target_key) & source_key == target_key
  code[same_code] <- needed[same_code]

  # Prefer codes in use, then unchanged codes. Keep additional rows for later review.
  priority <- order(!needed %in% second_codes, !same_code, needed, method = "radix")
  repeated <- priority[!is.na(code[priority]) & duplicated(code[priority])]
  code[repeated] <- NA_character_

  added <- which(is.na(code))
  reason <- rep("", length(code))
  reason[added] <- "name absent from the first checklist"
  ambiguous <- added[lengths(candidates)[added] > 1L]
  reason[ambiguous] <- paste0("multiple candidate codes ",
    vapply(candidates[ambiguous], paste, character(1L), collapse = ", "))
  reason[repeated] <- "another source code already maps to this target"

  if (any(grepl("[|\r\n]|::", source$FullName[added])))
    stop("A taxon name contains a delimiter reserved for extra_spec metadata.", call. = FALSE)

  reserved <- toupper(c(target$ShortName, first_codes))
  available <- nchar(needed[added]) == 7L & !toupper(needed[added]) %in% reserved
  retained <- added[available & !duplicated(toupper(needed[added]))]
  code[retained] <- needed[retained]
  generated <- setdiff(added, retained)
  if (length(generated)) {
    reference <- rbind(target, source[retained, , drop = FALSE])
    taxon_names <- source$FullName[generated]
    new <- rv_checklist_table(taxon_names, reference = reference, reserved = first_codes,
      add_genus = FALSE)
    if (!identical(new$FullName, taxon_names))
      stop("Could not assign checklist codes to all new taxa.", call. = FALSE)
    code[generated] <- new$ShortName
  }

  if (anyNA(code) || any(nchar(code) != 7L) || anyDuplicated(toupper(code)) > 0L) {
    stop("Invalid or duplicate taxon codes after reconciliation.", call. = FALSE)
  }

  collisions <- !is.na(target_key) & !same_code
  new_names <- source$FullName
  mapped <- setdiff(seq_along(code), added)
  new_names[mapped] <- target$FullName[match(code[mapped], target$ShortName)]
  normalized_same_code <- same_code & code == needed & source$FullName != new_names
  changed <- which(code != needed | seq_along(code) %in% added | normalized_same_code)

  action <- ifelse(code %in% first_codes, "Merged", "Mapped")
  match_type <- ifelse(source$FullName == new_names, "exact name", "normalized name")
  action <- paste0(action, " (", match_type, ")")
  action[normalized_same_code] <- "Normalized name (code unchanged)"
  action[added] <- "Added to metadata"
  action[generated] <- "New code in metadata"
  action[collisions] <- paste("Code collision /", action[collisions])
  reason[collisions] <- paste0("first checklist uses ", needed[collisions], " for ",
    previous_names[collisions], ifelse(nzchar(reason[collisions]), "; ", ""), reason[collisions])

  original <- sprintf("%s [%s]", source$FullName[changed], needed[changed])
  result <- sprintf("%s [%s]", new_names[changed], code[changed])
  detail <- ifelse(original == result, result, paste(original, "->", result))
  detail <- paste0(detail, ifelse(nzchar(reason[changed]), paste0(" (", reason[changed], ")"), ""))
  styles <- rep("ok", length(code))
  styles[added] <- "info"
  styles[collisions | seq_along(code) %in% c(repeated, ambiguous)] <- "warn"
  changes <- mapply(rv_col, paste0("  ", action[changed], ": ", detail), styles[changed],
    USE.NAMES = FALSE)
  list(source = needed, code = code, changes = changes,
    extra_species = data.frame(ShortName = code[added], FullName = source$FullName[added]))
}
