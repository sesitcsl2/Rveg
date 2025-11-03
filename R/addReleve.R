#'
#' addReleve
#'
#' Digitizing and editing your releves - Rveg database
#' @param DATABASE name of csv files for releve table and header - database
#' @param SAVE name of exporting database
#' @param checklist custom checklist
#' @param extrahead extra rows in header
#' @param start Boolean to start immediately digitizing first releve
#'
#' @returns export two csv files, one for releve and one for header (Rveg database)
#'
#' @examples
#' ## NOT RUN
#' if (interactive()) {
#'   addReleve()
#' }
#'
#' @export
#'
#'

addReleve <- function(DATABASE = "NEW", SAVE = "default", checklist = "default",
                      customhead = NULL, extrahead = NULL, metadata = NULL, start = TRUE) {
  # LOAD DATA ------------------------------------------------------------------------------------------

  if (SAVE == "default") {
    SAVE <- file.path(tempdir(), "default") # tempdir
  }

  if (DATABASE == "NEW") {

    # Built in checklists
    if (checklist == "default") {
      checklist <- system.file("extdata","cz_dh2012.txt", package="Rveg",mustWork = TRUE)
    } else if (checklist %in% c("wcvp_por","wcvp_que")) {
      checklist <- system.file("extdata",paste0(checklist,".txt"),package="Rveg",mustWork = TRUE)
    }

    # extra | custom fields
    labs <- if (is.null(customhead)) default_header_fields()
    else ensure_id_first(customhead)

    create_new_db(SAVE,labs,checklist,metadata)
    db <- read_db(SAVE)
    DATA <- db$RelDATA; HeaderDATA <- db$HeaderDATA; metadata <- db$meta

  } else {

    db <- read_db(DATABASE)
    DATA <- db$RelDATA; HeaderDATA <- db$HeaderDATA; metadata <- db$meta

    checklist <- db$meta$checklist # ignore checklists prompt on existing
    if (checklist == "default") {
      checklist <- system.file("extdata","cz_dh2012.txt", package="Rveg",mustWork = TRUE)
    } else if (checklist %in% c("wcvp_por","wcvp_que","cz_dh2012")) {
      checklist <- system.file("extdata",paste0(checklist,".txt"),package="Rveg",mustWork = TRUE)
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
    HeaderDATA <- expand_head_with_fields(HeaderDATA, extrahead)

    write_db(DATA, HeaderDATA, SAVE, metadata)

    }

  FIELD_LABELS <- schema_from_head(HeaderDATA)   # includes "ID" first

  SpLIST1 <- read.delim(checklist, sep = "\t")
  SpLIST <- makeSpLIST(checklist, metadata) # creating Species checklist

  # WORKFLOW  ------------------------------------------------------------------------------------------

  while (TRUE) {

    db <- read_db(SAVE); DATA2 <- db$RelDATA; HeaderDATA2 <- db$HeaderDATA

    if (DATABASE == "NEW"  & isTRUE(start)) {

      Header <- data.frame(ShortName = FIELD_LABELS, stringsAsFactors = FALSE)
      RelNew <- data.frame(ShortName = SpLIST$ShortName, stringsAsFactors = FALSE)
      RelNew$value <- 0

      lastcol <- NULL     # NULL for brand-new
      non_id  <- prompt_header_values(FIELD_LABELS, HeaderDATA2, prev_col = lastcol, skip = "ID")
      id_val  <- "1"

      # assemble in stored order
      vals <- vapply(FIELD_LABELS, function(lab) {
        if (tolower(lab) == "id") id_val else non_id[[lab]]
      }, FUN.VALUE = character(1))

      HeaderDATA2$X1 <- vals

      write_db(HEAD = HeaderDATA2, SAVE = SAVE, meta = metadata)

      aa <- "NEW" # menu options
      DATABASE <- "" # leave NEW node

    } else {

      rel_count  <- max(0L, ncol(DATA2)       - 1L)
      head_count <- max(0L, ncol(HeaderDATA2) - 1L)


      if(get0("aa",ifnotfound = NA_character_) %in% c("Y","ADDREL","ADDHEAD","RREL","RHEAD","YSP","INFO",
                                                      "REMOVEHEAD","REMOVEREL","NEW")) {
        rv_status_banner(checklist, DATABASE, SAVE, rel_count, head_count, metadata)
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

        k <- existing_k(HeaderDATA2)
        lastcol <- last_x(HeaderDATA2)
        nextcol <- paste0("X", k + 1L)
        lastcol <- last_x_col(HeaderDATA2)

        non_id <- prompt_header_values(FIELD_LABELS, HeaderDATA2, prev_col = lastcol, skip = "ID")
        last_id <- as.numeric(HeaderDATA2[[lastcol]][1])
        id_val <- as.character(last_id + 1L)

        vals <- vapply(FIELD_LABELS, function(lab) {
          if (tolower(lab) == "id") id_val else non_id[[lab]]
        }, FUN.VALUE = character(1))


        HeaderDATA2[[nextcol]] <- vals
        write_db(HEAD = HeaderDATA2, SAVE = SAVE, meta = metadata)

        RelNew <- data.frame(ShortName = SpLIST$ShortName, value = 0, stringsAsFactors = FALSE)

      }

      if (aa == "RREL") {

        #DATA2 <- read_rel(SAVE)
        DATA2 <- read_db(SAVE)$RelDATA
        k <- existing_k(DATA2)

        while (TRUE) {
          n <- as.numeric(readline("ReleveNumber? ")) # releve to repair
          if (is.na(n) != TRUE) {
            HeaderDATA3 <- read_db(SAVE)$HeaderDATA
            print(HeaderDATA3[, n + 1])
            tt <- toupper(readline("CorrectNumber?(Y/N) ")) # double check
            if (tt == "Y") {
              break
            }
          }
        }

        ID <- n + 1
        RelNew <- data.frame(ShortName = SpLIST$ShortName, value = 0)

        RelNew <- RelNew[order(RelNew[, 1]), ] #
        DATA2 <- DATA2[order(DATA2[, 1]), ] # testing the shortname ordering
        RelNew[RelNew[, 1] %in% DATA2[, 1], ][, 2] <- DATA2[, ID]
        TABLEexp <- RelNew[RelNew[, 2] > 0, ]
        colnames(TABLEexp) <- c("ShortNames", "Cover")
        print(TABLEexp)
      }

      if (aa == "ADDREL") {
        RelNew <- data.frame(ShortName = SpLIST$ShortName, value = 0, stringsAsFactors = FALSE)
      }

      Rel_list <- ReleveDialogue(SpLIST,SpLIST1,RelNew,metadata)
      RelNew <- Rel_list$RelNew; SpLIST <- Rel_list$SpLIST; SpLIST1 <- Rel_list$SpLIST1; metadata <- Rel_list$meta

      if (aa == "NEW" | !isTRUE(start)) {
        start = TRUE
        DATABASE = ""
      }

      if (aa == "Y" | aa == "ADDREL" | aa == "NEW") {
        DATA2 <- read_db(SAVE)$RelDATA
        DATA2 <- createTABLE(SpLIST, RelNew, DATA2)
        write_db(REL = DATA2, SAVE = SAVE, meta = metadata)
      }

      if (aa == "RREL") {
        DATA2 <- read_db(SAVE)$RelDATA
        DATA2 <- createTABLE(SpLIST, RelNew, DATA2)

        colnames(DATA2)[length(colnames(DATA2))] <- paste0("X",ID - 1) #rename new col
        DATA2[, c(ID)] <- DATA2[,ncol(DATA2)] #replace old col
        DATA2 <- DATA2[, -ncol(DATA2)] #remove new col
        #colnames(DATA2)[length(colnames(DATA2))] <- ID - 1
        #DATA2 <- DATA2[, c(colnames(DATA2)[1], 1:(length(colnames(DATA2)) - 1))]

        write_db(REL = DATA2,SAVE = SAVE,meta = metadata)
      }

    }

    if (aa == "PRINTHEAD") {
      HeaderDATA3 <- read_db(SAVE)$HeaderDATA
      print(HeaderDATA3)
    }

    if (aa == "PRINTREL") {
      DATA3 <- read_db(SAVE)$RelDATA
      print(DATA3)
    }

    if (aa == "PRINTMETA") {
      metaDATA3 <- read_db(SAVE)$meta
      print(metaDATA3)
    }

    if (aa == "RHEAD") {
      HeaderDATA2 <- read_db(SAVE)$HeaderDATA
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
        HeaderDATA2[idx, target] <- read_or_re(prompt, cur)

      }

      if (mode == "R") {
        fields <- HeaderDATA2[["ShortName"]][-1]
        for (i in seq_along(fields)) {
          cur <- HeaderDATA2[i+1, target, drop = TRUE]
          prompt <- paste0(fields[i], "? ")
          HeaderDATA2[i+1, target] <- read_or_re(prompt, cur)
        }
      }

      print(HeaderDATA2[, c("ShortName", target), drop = FALSE])
      if (toupper(readline("Save changes? (Y/N) ")) == "Y") {
        write_db(HEAD = HeaderDATA2,SAVE = SAVE, meta = metadata)
        cat("Header updated.\n")
      } else {
        cat("Discarded changes.\n")
      }


    }

    if (aa == "REMOVEREL") {
      DATA2 <- read_db(SAVE)$RelDATA
      HeaderDATA2 <- read_db(SAVE)$HeaderDATA
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
        DATA2 <- reindex_releves(DATA2, keep = c("ShortName"))
        HeaderDATA2 <- reindex_releves(HeaderDATA2, keep = c("ShortName"))

        write_db(REL = DATA2, HEAD = HeaderDATA2,SAVE = SAVE,meta = metadata)
        break
      }
    }

    if (aa == "YSP" & ncol(DATA2) == ncol(HeaderDATA2)) {

      ReleveDialogue(SpLIST, SpLIST1, RelNew, metadata, SAVE, HeaderDATA2, variation = 2)

    }

    if (aa == "ADDHEAD") {

      k <- existing_k(HeaderDATA2)
      lastcol <- last_x(HeaderDATA2)
      nextcol <- paste0("X", k + 1L)
      lastcol <- last_x_col(HeaderDATA2)

      non_id <- prompt_header_values(FIELD_LABELS, HeaderDATA2, prev_col = lastcol, skip = "ID")
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
      write_db(HEAD = HeaderDATA2, SAVE = SAVE, meta = metadata)

      }

    if (aa == "N" | aa == "Q") {
      # the script ends, database is saved
      write_db(DATA2, HeaderDATA2, SAVE, metadata)
      break
    }

  }
}

