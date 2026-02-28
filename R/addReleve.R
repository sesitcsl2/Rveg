#'
#' addReleve:
#' Digitize and Edit Relevés in an Rveg Database
#'
#' @description
#' The core interactive function of the `Rveg` package. `addReleve()` launches a
#' console-based menu system that allows users to create new vegetation databases,
#' digitize new relevés, and edit existing ones. It seamlessly manages both the
#' species composition data (REL) and the environmental plot header data (HEAD).
#'
#' @param database Character. The path and name of an existing Rveg database
#' (e.g., `"path/to/my_db"`). Defaults to `"NEW"` which creates a fresh database.
#' @param save Character. The output path and name where the resulting database
#' files (`*HEAD.csv` and `*REL.csv`) will be exported. Defaults to a temporary directory.
#' @param checklist Character. The species checklist to use. Can be a built-in
#' checklist (atm: `cz_dh2012`, `Czechia_slovakia_2015`, `cz_kaplan2019`, `wcvp_que`,
#' `wcvp_por`) or a file path to a custom `txt` checklist. Default use `cz_dh2012`.
#' see function \code{\link{CreateChecklist}}.
#' @param customhead Character vector. A vector of strings defining completely
#' custom header fields. Overrides the default schema.
#' @param extrahead Character vector. Additional header fields to append to the
#' end of the default or existing header schema.
#' @param metadata Character vector of length 2. Used to store the Project Title
#' and Project Description (e.g., `c("Alpine Flora", "Summer 2024 survey")`).
#' @param start Logical. If `TRUE`, the function skips the main menu upon creating
#' a `"NEW"` database and immediately prompts the user to digitize the first relevé.
#'
#' @return Writes two linked CSV files (Rveg database) to the location specified by `save`: one
#' containing the relevé species data (`*REL.csv`) and one containing the header data (`*HEAD.csv`).
#'
#' @examples
#' if (interactive()) {
#'   # Launch the interactive menu for a new database
#'   addReleve(
#'     database = "NEW",
#'     save = "my_new_project",
#'     metadata = c("Project Title", "Project Description")
#'   )
#' }
#'
#' @export

addReleve <- function(database = "NEW", save = "default", checklist = "default",
                      customhead = NULL, extrahead = NULL, metadata = NULL, start = TRUE) {

  # LOAD DATA ------------------------------------------------------------------------------------------

  if (save == "default") {
    save <- file.path(tempdir(), "default") # tempdir
  }

  if (database == "NEW") {

    checklist <- rv_get_checklist(checklist)

    # extra | custom fields
    labs <- if (is.null(customhead)) rv_default_header_fields()
    else rv_ensure_id_first(customhead)

    rv_create_new_db(save,labs,checklist,metadata)
    db <- rv_read_db(save)
    DATA <- db$RelDATA; HeaderDATA <- db$HeaderDATA; metadata <- db$meta

  } else {

    db <- rv_read_db(database)
    DATA <- db$RelDATA; HeaderDATA <- db$HeaderDATA; metadata <- db$meta

    meta_checklist <- db$meta$checklist # ignore checklists prompt on existing
    if (file.exists(rv_get_checklist(meta_checklist))) {
      checklist <- rv_get_checklist(meta_checklist)
    }


    # ensure ID row exists & is first
    if (!any(tolower(HeaderDATA$ShortName) == "id")) {
      idrow <- data.frame(ShortName = "ID", stringsAsFactors = FALSE)
      for (xc in setdiff(names(HeaderDATA), "ShortName")) idrow[[xc]] <- NA
      HeaderDATA <- rbind(idrow, HeaderDATA)
    } else {
      idx <- which(tolower(HeaderDATA$ShortName) == "id")[1]
      if (idx != 1) HeaderDATA <- HeaderDATA[c(idx, setdiff(seq_len(nrow(HeaderDATA)), idx)), , drop = FALSE]
    }

    # expand with extrahead if provided
    HeaderDATA <- rv_expand_head_with_fields(HeaderDATA, extrahead)

    rv_write_db(DATA, HeaderDATA, save, metadata)

  }

  FIELD_LABELS <- rv_schema_from_head(HeaderDATA) # includes "ID" first

  SpLIST <- rv_make_sp_list(checklist,db$meta)


  # WORKFLOW  ------------------------------------------------------------------------------------------

  while (TRUE) {

    db <- rv_read_db(save)
    DATA2 <- db$RelDATA; HeaderDATA2 <- db$HeaderDATA; metadata <- db$meta

    if (database == "NEW"  & isTRUE(start)) {

      Header <- data.frame(ShortName = FIELD_LABELS, stringsAsFactors = FALSE)
      #RelNew <- data.frame(ShortName = SpLIST$ShortName, stringsAsFactors = FALSE)
      #RelNew$value <- 0
      RelNew <- data.frame(ShortName = character(0),
                           value = character(0), stringsAsFactors = FALSE)


      lastcol <- NULL     # NULL for brand-new
      non_id  <- rv_prompt_header_values(FIELD_LABELS, HeaderDATA2, prev_col = lastcol, skip = "ID")
      id_val  <- "1"

      # assemble in stored order
      vals <- vapply(FIELD_LABELS, function(lab) {
        if (tolower(lab) == "id") id_val else non_id[[lab]]
      }, FUN.VALUE = character(1))

      HeaderDATA2$X1 <- vals

      rv_write_db(head = HeaderDATA2, save = save, meta = metadata)

      aa <- "NEW" # menu options
      database <- "" # leave NEW node

    } else {

      rel_count  <- max(0L, ncol(DATA2)       - 1L)
      head_count <- max(0L, ncol(HeaderDATA2) - 1L)


      if(get0("aa",ifnotfound = NA_character_) %in% c("Y","ADDREL","ADDHEAD","RREL","RHEAD","YSP","INFO",
                                                      "REMOVEHEAD","REMOVEREL","NEW")) {
        rv_status_banner(checklist, database, save, rel_count, head_count, metadata)
      }

      aa <- toupper(readline("$Rveg: "))
      if (aa %in% c("H", "HELP", "?")) {
        rv_print_help()
        next
      }

    }

    if ((aa == "Y" | aa == "YSP") & ncol(DATA2) != ncol(HeaderDATA2)) {
      message("Number of relev\u00e9s:s and headers must match!!!")
    } ## warning for mismatch

    if (aa == "NEW" | aa == "RREL" | (aa == "Y" & ncol(DATA2) == ncol(HeaderDATA2)) | aa == "ADDREL") {
      if (aa == "Y") {

        k <- rv_existing_k(HeaderDATA2)
        nextcol <- paste0("X", k + 1L)
        lastcol <- rv_last_x_col(HeaderDATA2)

        non_id <- rv_prompt_header_values(FIELD_LABELS, HeaderDATA2, prev_col = lastcol, skip = "ID")

        if (is.null(lastcol)) {
          last_id <- 0
        } else {
          last_id <- as.numeric(HeaderDATA2[[lastcol]][1])
        }

        id_val <- as.character(last_id + 1L)

        vals <- vapply(FIELD_LABELS, function(lab) {
          if (tolower(lab) == "id") id_val else non_id[[lab]]
        }, FUN.VALUE = character(1))


        HeaderDATA2[[nextcol]] <- as.character(vals)
        rv_write_db(head = HeaderDATA2, save = save, meta = metadata)
        RelNew <- data.frame(ShortName = character(0),
                             value = character(0), stringsAsFactors = FALSE)
      }

      if (aa == "RREL") {

        #DATA2 <- read_rel(save)
        DATA2 <- rv_read_db(save)$RelDATA
        k <- rv_existing_k(DATA2)

        while (TRUE) {
          n <- as.numeric(readline("ReleveNumber? ")) # releve to repair
          if (is.na(n) != TRUE) {
            HeaderDATA3 <- rv_read_db(save)$HeaderDATA
            print(HeaderDATA3[,c(1, n + 1)])
            tt <- toupper(readline("CorrectNumber?(Y/N) ")) # double check
            if (tt == "Y") {
              break
            }
          }
        }

        ID <- n + 1 # identifikace snimku
        RelNew <- DATA2[, c(1,ID)]
        TABLEexp <- RelNew[RelNew[, 2] != 0, ]
        colnames(RelNew) <- c("ShortName", "value")
        colnames(TABLEexp) <- c("ShortName", "Cover")
        print(TABLEexp)
      }

      if (aa == "ADDREL") {
        #RelNew <- data.frame(ShortName = SpLIST$ShortName, value = 0, stringsAsFactors = FALSE)
        RelNew <- data.frame(ShortName = character(0),
                             value = character(0), stringsAsFactors = FALSE)
      }

      Rel_list <- rv_releve_dialogue(SpLIST,RelNew,metadata)
      RelNew <- Rel_list$RelNew; SpLIST <- Rel_list$SpLIST; metadata <- Rel_list$meta

      if (aa == "NEW" | !isTRUE(start)) {
        start = TRUE
        database = ""
      }

      if (aa == "Y" | aa == "ADDREL" | aa == "NEW") {
        DATA2 <- rv_read_db(save)$RelDATA
        DATA2 <- rv_create_table(RelNew, DATA2)
        rv_write_db(head = HeaderDATA2, rel = DATA2, save = save, meta = metadata)
      }

      if (aa == "RREL") {
        DATA2 <- rv_read_db(save)$RelDATA

        DATA2 <- rv_create_table(RelNew, DATA2)
        target <- paste0("X", n)                 # n is the relevé number
        newvals <- DATA2[[ncol(DATA2)]]
        DATA2[[target]] <- newvals
        DATA2[[ncol(DATA2)]] <- NULL
        rv_write_db(rel = DATA2, head = HeaderDATA2, save = save, meta = metadata)  # see issue #1
      }

    }

    if (aa == "PRINTHEAD") {
      HeaderDATA3 <- rv_read_db(save)$HeaderDATA
      print(HeaderDATA3)
    }

    if (aa == "PRINTREL") {
      DATA3 <- rv_read_db(save)$RelDATA
      print(DATA3)
    }

    if (aa == "PRINTMETA") {
      metaDATA3 <- rv_read_db(save)$meta
      print(metaDATA3)
    }

    if (aa == "RHEAD") {
      HeaderDATA2 <- rv_read_db(save)$HeaderDATA
      releve_cols <- colnames(HeaderDATA2)


      # choose relevé
      repeat {
        n <- suppressWarnings(as.integer(readline("ReleveNumber? ")))
        if (is.na(n) || n < 1) { cat("Please enter a positive integer.\n"); next }
        target <- paste0("X", n)
        if (!target %in% releve_cols) { cat("No such column: ", target, "\n", sep = ""); next }
        print(HeaderDATA2[, c("ShortName",target), drop = FALSE])
        if (toupper(readline("CorrectColumn?(Y/N) ")) == "Y") break
      }

      mode <- toupper(readline("Repair mode: (S)ingle field / (R)efill all / (C)ancel ? "))
      if (mode == "C") {
        cat("Canceled.\n")
      }

      if (mode == "S") {
        fields <- HeaderDATA2[["ShortName"]]
        repeat {
          cat("Available fields:\n"); print(fields)
          l <- readline("HeaderCharacteristic to repair? ")
          if (!nzchar(l)) next
          idx <- pmatch(l, fields, duplicates.ok = TRUE)
          idx <- idx[!is.na(idx)]
          if (!length(idx)) { cat("No match. Try again.\n"); next }
          if (length(idx) > 1) { cat("Multiple matches:\n"); print(fields[idx]); next }
          break
        }
        cur <- HeaderDATA2[idx, target, drop = TRUE]
        prompt <- paste0(fields[idx], "? ")
        HeaderDATA2[idx, target] <- rv_read_or_re(prompt, cur)

      }

      if (mode == "R") {
        fields <- HeaderDATA2[["ShortName"]][-1]
        for (i in seq_along(fields)) {
          cur <- HeaderDATA2[i+1, target, drop = TRUE]
          prompt <- paste0(fields[i], "? ")
          HeaderDATA2[i+1, target] <- rv_read_or_re(prompt, cur)
        }
      }

      print(HeaderDATA2[, c("ShortName", target), drop = FALSE])
      if (toupper(readline("save changes? (Y/N) ")) == "Y") {
        rv_write_db(head = HeaderDATA2,save = save, meta = metadata)
        cat("Header updated.\n")
      } else {
        cat("Discarded changes.\n")
      }


    }

    if (aa == "REMOVEREL") {
      DATA2 <- rv_read_db(save)$RelDATA
      HeaderDATA2 <- rv_read_db(save)$HeaderDATA
      while (TRUE) {
        n <- suppressWarnings(as.integer(readline("ReleveNumber? ")))
        if (is.na(n) || n < 1) { cat("Please enter a positive integer.\n"); next }

        target <- paste0("X", n)
        if (!target %in% names(HeaderDATA2)) {
          cat("No such column: ", target, "\n", sep = ""); next
        }

        print(HeaderDATA2[, target, drop = FALSE]) # selection or releve

        tt <- toupper(readline("CorrectNumber?(Y/N) "))
        if (tt != "Y") next

        # Remove from both REL and HEAD (keep order of remaining columns)
        DATA2 <- DATA2[, setdiff(names(DATA2), target), drop = FALSE]
        HeaderDATA2 <- HeaderDATA2[, setdiff(names(HeaderDATA2), target), drop = FALSE]

        # Reindex ONLY the X columns to X1..Xm
        DATA2 <- rv_reindex_releves(DATA2, keep = c("ShortName"))
        HeaderDATA2 <- rv_reindex_releves(HeaderDATA2, keep = c("ShortName"))

        rv_write_db(rel = DATA2, head = HeaderDATA2,save = save,meta = metadata)
        break
      }
    }

    if (aa == "YSP" & ncol(DATA2) == ncol(HeaderDATA2)) {

      res <- rv_releve_dialogue(SpLIST, RelNew, metadata, save, HeaderDATA2, variation = 2)
      SpLIST <- res$SpLIST
      metadata <- res$meta
      HeaderDATA2 <- res$HeaderDATA2
    }

    if (aa == "ADDHEAD") {

      k <- rv_existing_k(HeaderDATA2)
      nextcol <- paste0("X", k + 1L)
      lastcol <- rv_last_x_col(HeaderDATA2)

      non_id <- rv_prompt_header_values(FIELD_LABELS, HeaderDATA2, prev_col = lastcol, skip = "ID")
      if (!is.null(lastcol)) {
        last_id <- as.numeric(HeaderDATA2[[lastcol]][1])
      } else {
        last_id <- 0
      }

      id_val <- as.character(last_id + 1L)

      vals <- vapply(FIELD_LABELS, function(lab) {
        if (tolower(lab) == "id") id_val else non_id[[lab]]
      }, FUN.VALUE = character(1))

      HeaderDATA2[[nextcol]] <- vals
      rv_write_db(head = HeaderDATA2, save = save, meta = metadata)

    }

    if (aa == "N" | aa == "Q") {
      # the script ends, database is saved
      rv_write_db(DATA2, HeaderDATA2, save, metadata)
      break
    }

  }
}

