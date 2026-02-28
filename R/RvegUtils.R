#-----------------------------------#
#---- Rveg internal functions ------#
#-----------------------------------#

#' @keywords internal
#' @noRd
rv_cs_to_pct <- function(releve) {

  rele <- releve[,-1]
  releorig <- rele
  uniq <- unique(unlist(rele))

  for (val in uniq) {
    replace <- rv_ask_text(paste0("Percentage value for (",val,") "))
    rele[releorig==val] <- replace
  }

  releve[,-1] <- rele
  return(releve)

  }


#' @keywords internal
#' @noRd
rv_species_not_found <- function(bad_name, SpList, metadata, orig_SpList = NULL) {

  message(rv_col(paste0("\nUNKNOWN SPECIES: '", bad_name, "'"),"warn"))

  while (TRUE) {

    if (!is.null(orig_SpList)) {

      bad_code <-  orig_SpList[which(orig_SpList$name == bad_name),]$code
      code_name <- SpList[which(SpList$ShortName == bad_code),]$FullName
      if (length(code_name)>0) {

        while(TRUE) {



          cat(paste0("suggest from the original list:", bad_name, " -> ", code_name, " ? (Y/N)" ))
          m <- toupper(readline("Accept? (Y/N)" ))
          if (m == "N") {
            break
          }
          if (m == "Y" ) {
            new_code <- SpList[which(SpList$FullName == code_name),]$ShortName
            return(list(code = new_code, meta = metadata))
          }
        }


      }
    }

    m <- toupper(readline("Seach with 3 letters + or (I)nsert new code:"))

    if (nchar(m) > 2) {
      # Case-insensitive grep
      hits <- SpList[grep(paste0("^",m), SpList$FullName, ignore.case = TRUE), ]
      if (nrow(hits) == 0) {
        message(rv_col("No matches found.","warn"))

      } else {
        # Show hits with index numbers
        print(hits[, c("ShortName", "FullName")])

        # Ask user to pick one
        sel <- toupper(readline("Enter species shortcode to select: "))
        if (sel %in% hits$ShortName) {
          base_code <- sel
          conf <- toupper(readline(paste0("Map '", bad_name, "' -> '", hits$FullName[which(hits$ShortName == sel)], "'? (Y/N): ")))
          if (conf == "Y") {
            return(list(code = base_code, meta = metadata))
          }
        }
      }



    } else if (m == "I") {
      new_code <- toupper(readline("Enter new 7-letter ShortCode (e.g. AlnuGlu): "))

      if (nchar(new_code) != 7) {
        message(rv_col("Error: Code must be exactly 7 characters.","err"))
      } else if (new_code %in% SpList$ShortName) {
        message(rv_col(paste0("The code '", new_code,
                       "' is already reserved in the checklist!"),"err"))
      } else {
        confirm <- toupper(readline(paste0("Map '", bad_name, "' -> '", new_code, "'? (Y/N): ")))
        if (confirm == "Y") {

          # Update Metadata (Add "FullName::ShortCode|")
          if (is.null(metadata$extra_spec)) metadata$extra_spec <- ""
          new_entry <- paste0(bad_name, "::", new_code, "|")
          metadata$extra_spec <- paste0(metadata$extra_spec, new_entry)

          message(rv_col("Metadata updated.","ok"))
          return(list(code = new_code, meta = metadata))
        }
      }

    }
  }
}


#' @keywords internal
#' @noRd
rv_add_rel_new <- function(RelNew,nana,o) {
  if (nana %in% RelNew$ShortName) {
    RelNew[RelNew$ShortName == nana, 2] <- o
  } else {
    new_row <- RelNew[1, ]
    is.na(new_row) <- TRUE

    new_row$ShortName <- nana
    new_row[, 2] <- o

    RelNew <- rbind(RelNew, new_row)
    RelNew <- RelNew[order(RelNew$ShortName), ]
  }
  rownames(RelNew) <- NULL
  return(RelNew)
}

#' @keywords internal
#' @noRd
rv_ask_abundance <- function(scale) {
  scale <- toupper(trimws(scale))

  if (scale == "P") {
    repeat {
      x <- trimws(readline("Abundance?(%) "))
      if (x == "") next
      o <- suppressWarnings(as.numeric(x))
      if (!is.na(o) && o >= 0 && o <= 100) return(o)
    }
  }

  if (scale == "CS") {
    repeat {
      x <- trimws(readline("Abundance? "))
      if (x == "") next
      return(as.character(x))
    }
  }

  if (scale %in% c("BB", "B")) {
    bb <- rv_ask_choice(
      "Abundance?(0,R,+,1,2,M,A,B,3,4,5) ",
      c("0","R","+","1","2","M","A","B","3","4","5")
    )
    return(rv_bb_to_pct(bb))
  }
}

#' @keywords internal
#' @noRd
rv_resolve_species_code <- function(code, SpLIST, metadata, RelNew = NULL) {

  if (is.null(metadata$extra_spec) || is.na(metadata$extra_spec)) {
    metadata$extra_spec <- ""
  }

  code <- toupper(trimws(code))

  repeat {
    chk <- SpLIST[SpLIST$ShortName == code, , drop = FALSE]

    if (nrow(chk) == 0 || is.na(chk$FullName[1])) {
      message(rv_col("Species not found in the checklist","err"))
    } else {
      print(chk$FullName[1])
    }

    ynf <- rv_ask_choice("CorrectName? (Y=accept / F=find) ", c("Y", "F"))

    if (ynf == "Y") {
      if (nrow(chk) > 0 && !is.na(chk$FullName[1])) {
        return(list(code = code, SpLIST = SpLIST, metadata = metadata, RelNew = RelNew))
      }
      next
    }

    # -------- FIND ----------
    repeat {
      k <- rv_ask_text("SpeciesFirst3letters? (eg.Che) ", min_nchar = 3)

      searchlist <- SpLIST[grep(paste0("^", k), SpLIST$FullName, ignore.case = TRUE), , drop = FALSE]
      if (nrow(searchlist) == 0) {
        message(rv_col("No matches","warn"))
      } else {
        print(searchlist[order(searchlist$FullName), ])
      }

      repeat {
        s <- rv_ask_text("SpeciesCode? (GenuSpe / SEARCH / INSERT) ",
                         min_nchar = 1, allow_cmd = c("SEARCH", "INSERT"))
        sU <- toupper(trimws(s))

        if (sU == "SEARCH") break

        if (sU == "INSERT") {

          repeat {
            sn <- rv_ask_text("Full species name?: ", min_nchar = 1)
            if (rv_ask_choice(paste0(sn, ": correct? (Y/N) "), c("Y", "N")) == "Y") break
          }

          repeat {
            sc <- toupper(rv_ask_text("ShortCode? (GenuSpe): ", min_nchar = 1))
            if (nchar(sc) != 7) next
            if (!rv_check_short_name(sc, SpLIST, metadata)) next

            if (rv_ask_choice(paste0(sn, "; ", sc, "? (Y/N) "), c("Y", "N")) != "Y") next

            SpLIST <- rbind(SpLIST, data.frame(ShortName = sc, FullName = sn, stringsAsFactors = FALSE))
            metadata$extra_spec <- paste0(metadata$extra_spec, sn, "::", sc, "|")

            if (!is.null(RelNew)) {
              if (!any(RelNew$ShortName == sc)) {
                RelNew <- rbind(RelNew, data.frame(ShortName = sc, value = 0, stringsAsFactors = FALSE))
              }
            }

            print(sn)
            return(list(code = sc, SpLIST = SpLIST, metadata = metadata, RelNew = RelNew))
          }
        }

        if (nchar(sU) == 7) {
          chk2 <- SpLIST[SpLIST$ShortName == sU, , drop = FALSE]
          if (nrow(chk2) == 0 || is.na(chk2$FullName[1])) {
            message(rv_col("Species not found in the checklist","err"))
            next
          }
          print(chk2$FullName[1])

          if (rv_ask_choice("Use this species? (Y/N) ", c("Y", "N")) == "Y") {
            return(list(code = sU, SpLIST = SpLIST, metadata = metadata, RelNew = RelNew))
          }
          next
        }

        # otherwise invalid -> re-prompt
      }
    }
  }
}

#' @keywords internal
#' @noRd
rv_ask_choice <- function(prompt, allowed) {
  allowed <- toupper(allowed)
  repeat {
    x <- toupper(trimws(readline(prompt)))
    if (x == "") next
    if (x %in% allowed) return(x)
  }
}

#' @keywords internal
#' @noRd
rv_ask_text <- function(prompt, min_nchar = 1, allow_cmd = character()) {
  allow_cmd <- toupper(allow_cmd)
  repeat {
    x <- trimws(readline(prompt))
    if (x == "") next
    xU <- toupper(x)
    if (xU %in% allow_cmd) return(xU)     # return command token (uppercase)
    if (nchar(x) >= min_nchar) return(x)  # return trimmed text
  }
}

#----- releve making --------

#' Dialogue loop for writing releves, var 1 for classic, var 2 for batch
#' @keywords internal
#' @noRd
rv_releve_dialogue <- function(SpLIST, RelNew, metadata, SAVE = NULL, HeaderDATA2 = NULL, variation = 1) {

  out <- list()

  if (variation == 1) {

    repeat {
      add_layer <- rv_ask_choice("AddNewLayer?(Y/N) ", c("Y","N"))

      if (add_layer == "N") {

        message(rv_col("Species_richness","info"))
        print(nrow(RelNew[(RelNew[, 2] != 0), ]))
        break
      }

      most <- rv_ask_choice("Select Layer (3,2,1,J,0) ", c("3","2","1","J","0"))
      oo   <- rv_ask_choice("P - percentage, BB - Braun B. scale, CS - custom scale ",
                            c("P","BB","B","CS"))

      repeat {
        m <- rv_ask_text("AddSpecies?(GenuSpe/N) ", allow_cmd = c("N"))
        if (m == "N") break

        n <- toupper(trimws(m))
        if (nchar(n) != 7) next

        o <- rv_ask_abundance(oo)
        res <- rv_resolve_species_code(n, SpLIST, metadata, RelNew = RelNew)
        n <- res$code
        SpLIST <- res$SpLIST
        metadata <- res$metadata
        RelNew <- res$RelNew



        nana <- paste(n, most, sep = "_")
        RelNew <- rv_add_rel_new(RelNew, nana, o)
        print(RelNew[(RelNew[, 2] > 0), ])
      }
    }

    out$RelNew <- RelNew
    out$SpLIST <- SpLIST
    out$meta <- metadata
    return(out)
  }

  if (variation == 2) {

    repeat {
      x <- rv_ask_text("How many releves? ")
      m <- suppressWarnings(as.numeric(x))
      if (!is.na(m) && m > 0) break
    }

    Rels <- list()
    DATAtemp <- rv_read_db(SAVE)$RelDATA
    for (i in 1:m) {
      Rels[[paste0("r", i)]] <- data.frame(ShortName = character(0),
                                           value = character(0),
                                           stringsAsFactors = FALSE)
    }

    oo <- rv_ask_choice("P - percentage, BB - Braun B. scale, CS - custom scale ",
                        c("P","BB","B","CS"))

    repeat {
      x <- rv_ask_text("Add new species (GenuSpe)/N?  ", min_nchar = 1, allow_cmd = c("N"))

      if (x == "N") {
        rs <- rv_ask_choice("Add headers?(Y/N) ", c("Y","N"))
        if (rs == "Y") {
          for (i in 1:m) {
            cat(rv_col(paste0("Relev\u00e9 ", i),"info"))

            k <- rv_existing_k(HeaderDATA2)
            nextcol <- paste0("X", k + 1L)
            lastcol <- rv_last_x_col(HeaderDATA2)

            FIELD_LABELS <- rv_schema_from_head(HeaderDATA2)

            non_id <- rv_prompt_header_values(FIELD_LABELS, HeaderDATA2, prev_col = lastcol, skip = "ID")
            id_val <- as.character(k + 1L)

            vals <- vapply(FIELD_LABELS, function(lab) {
              if (tolower(lab) == "id") id_val else non_id[[lab]]
            }, FUN.VALUE = character(1))

            HeaderDATA2[[nextcol]] <- vals
            rv_write_db(head = HeaderDATA2, save = SAVE, meta = metadata)
          }
        }
        break
      }

      n <- toupper(trimws(x))
      if (nchar(n) != 7) next

      l <- rv_ask_choice("Select Layer (3,2,1,J,0) ", c("3","2","1","J","0"))

      res <- rv_resolve_species_code(n, SpLIST, metadata, RelNew = NULL)
      n <- res$code
      SpLIST <- res$SpLIST
      metadata <- res$metadata

      nana <- paste(n, l, sep = "_")

      for (i in 1:m) {
        cat(rv_col(paste0("Relev\u00e9 ", i),"info"))
        o <- rv_ask_abundance(oo)

        Rels[[paste0("r", i)]] <- rv_add_rel_new(Rels[[paste0("r", i)]], nana, o)
        print(Rels[[paste0("r", i)]][(Rels[[paste0("r", i)]][, 2] > 0), ])
      }

      DATA2 <- rv_create_table(Rels, DATAtemp, variation = 2)
      print(DATA2)
      colnames(DATA2)[-1] <- paste0("X", 1:(length(colnames(DATA2)) - 1))

      # persist rel + updated meta (incl. inserts)
      rv_write_db(rel = DATA2, save = SAVE, meta = metadata)
    }

    out$SpLIST <- SpLIST
    out$meta <- metadata
    out$HeaderDATA2 <- HeaderDATA2
    return(out)
  }

  stop("Unknown variation.")
}


#----- Reading & Writing ------------

#' Creates metadata at creation of database
#' @keywords internal
#' @noRd
rv_create_metadata <- function(checklist, meta = NULL) {

  if(is.null(meta)) {meta <- c("","")}

  md <- list()
  md$project_name <- meta[1]
  md$project_description <- meta[2]
  md$n_releves <- 0
  md$checklist <- tools::file_path_sans_ext(basename(checklist))
  md$rveg_version <- utils::packageVersion("Rveg")
  md$extra_spec <- ""
  md$created <- round(Sys.time(),"secs")
  md$last_change <- round(Sys.time(),"secs")
  md$db_id <- rv_make_id()

  invisible(md)

}

#' Saving actual metadata
#' @keywords internal
#' @noRd
rv_write_metadata <- function(meta,con) {

  for (k in names(meta)) {
    v <- if (is.null(meta[[k]])) "" else as.character(meta[[k]])
    writeLines(paste0("# ", k, ": ", v), con, useBytes = TRUE)
  }

}

#' Reading & using checklist
#' @keywords internal
#' @noRd
rv_get_checklist <- function(checklist) {

  if (checklist %in% c("default","cz_dh2012")) {
    checklist <- system.file("extdata",paste0("/RvChecklist/","cz_dh2012.txt"), package="Rveg",mustWork = TRUE)
  } else if (checklist %in% c("wcvp_por","wcvp_que", "cz_kaplan2019")) {
    checklist <- system.file("extdata",paste0("/RvChecklist/",checklist,".txt"),package="Rveg",mustWork = TRUE)
  } else if (checklist %in% c("Czechia_slovakia_2015")) {
    checklist <- system.file("extdata",paste0("/TvChecklist/",checklist,".txt"),package="Rveg",mustWork = TRUE)
    }
  return(checklist)
  }


#' Creates new database
#' @keywords internal
#' @noRd
rv_create_new_db <- function(save, labs, checklist, meta) {

  DATA <- data.frame(ShortName = character(), stringsAsFactors = FALSE)
  HeaderDATA <- data.frame(ShortName = labs, stringsAsFactors = FALSE)
  meta <- rv_create_metadata(checklist, meta)

  con <- file(paste0(save, "HEAD.csv"), open = "wt", encoding = "UTF-8")

  for (k in names(meta)) {
    v <- if (is.null(meta[[k]])) "" else as.character(meta[[k]])
    writeLines(paste0("# ", k, ": ", v), con, useBytes = TRUE)
  }

  utils::write.table(
    HeaderDATA, file = con, sep = ",", row.names = FALSE, col.names = TRUE,
    na = "", qmethod = "double"
  )

  close(con)

  con <- file(paste0(save, "REL.csv"), open = "wt", encoding = "UTF-8")

  for (k in names(meta)) {
    v <- if (is.null(meta[[k]])) "" else as.character(meta[[k]])
    writeLines(paste0("# ", k, ": ", v), con, useBytes = TRUE)
  }

  utils::write.table(
    DATA, file = con, sep = ",", row.names = FALSE, col.names = TRUE,
    na = "", qmethod = "double"
  )
  close(con)

}

#' Read existing database
#' @keywords internal
#' @noRd
rv_read_db <- function(save) {

  lines <- readLines(paste0(save,"HEAD.csv"))
  meta  <- list()

  # count ALL leading comment lines that start with '#'
  is_comment <- grepl("^#", lines)
  lead_n <- if (length(is_comment) && is_comment[1]) {
    rle(is_comment)$lengths[1]
  } else 0

  if (lead_n > 0) {
    block <- sub("^#\\s*", "", lines[seq_len(lead_n)])
    # parse "key: value" pairs
    kv <- strsplit(block, ":", fixed = TRUE)
    for (pair in kv) {
      if (length(pair) >= 2) {
        key <- trimws(pair[1])
        val <- trimws(paste(pair[-1], collapse=":"))
        if (nzchar(key)) meta[[key]] <- val
      }
    }
  }

  HeaderDATA <- utils::read.table(
    paste0(save,"HEAD.csv"), sep = ",", header = TRUE, quote = "\"", comment.char = "#",
    stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character"
  )

  RelDATA <- utils::read.table(
    paste0(save,"REL.csv"), sep = ",", header = TRUE, quote = "\"", comment.char = "#",
    stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character"
  )

  abc <- list(HeaderDATA = HeaderDATA, RelDATA = RelDATA, meta = meta)
  invisible(abc)

}

#' Save existing database
#' @keywords internal
#' @noRd
rv_write_db <- function(rel = NULL, head = NULL, save = NULL, meta = NULL) {

  meta$last_change <- round(Sys.time(),"secs")

  if (!is.null(rel)) {
    con <- file(paste0(save, "REL.csv"), open = "wt", encoding = "UTF-8")

    meta$n_releves <- ncol(rel)-1
    rv_write_metadata(meta,con)

    utils::write.table(
      rel, file = con, sep = ",", row.names = FALSE, col.names = TRUE,
      na = "", qmethod = "double"
    )

    close(con)
  }

  if (!is.null(head)) {
    con <- file(paste0(save, "HEAD.csv"), open = "wt", encoding = "UTF-8")

    meta$n_releves <- ncol(head)-1
    rv_write_metadata(meta,con)

    utils::write.table(
      head, file = con, sep = ",", row.names = FALSE, col.names = TRUE,
      na = "", qmethod = "double"
    )

    close(con)
  }

}

#' NEED DESCRIPTION
#' @keywords internal
#' @noRd
rv_read_or_re <- function(prompt, default) {
  def <- if (length(default) == 0 || is.na(default) || identical(default, "NA")) "" else as.character(default)
  # only show a hint if we actually have a previous value
  if (nzchar(def)) cat(rv_col(paste0("RE <- ", def),"info"))
  ans <- readline(prompt)
  ans_trim <- toupper(trimws(ans))
  if (ans_trim == "RE" && nzchar(def)) def else ans
}

#' Random pseudo code to link DB files
#' @keywords internal
#' @noRd
rv_make_id <- function() {
  # pseudo-UUID using base R
  hex <- c(0:9, letters[1:6])
  paste0(
    paste(sample(hex, 4,  TRUE), collapse=""), "-",
    paste(sample(hex, 4,  TRUE), collapse=""), "-",
    paste(sample(hex, 4,  TRUE), collapse="")
  )
}

#----- User interface  --------------



#' Text styles
#' @keywords internal
#' @noRd
rv_col <- function(txt, style = NULL) {
  if (is.null(style)) return(txt)
  # Minimal ANSI coloring (works in most R consoles / RStudio)
  start <- switch(style,
                  title = "\033[1;36m",  # bold cyan
                  ok    = "\033[32m",    # green
                  warn  = "\033[33m",    # yellow
                  err   = "\033[31m",    # red
                  info  = "\033[34m",    # blue
                  "")
  if (start == "") return(txt)
  paste0(start, txt, "\033[0m")
}

#' Prints available functions for menu
#' @keywords internal
#' @noRd
rv_print_help <- function() {
  cmds <- rv_menu_commands()
  cat(rv_col("\nCommands \n", "title"))
  for (nm in names(cmds)) {
    cat(rv_col(sprintf("  %-12s", nm), "title"), as.character(cmds[[nm]]), "\n", sep = "")
  }
  cat("\n")
}

#' Available menu commands for help command
#' @keywords internal
#' @noRd
rv_menu_commands <- function() {
  c(
    Y         = "Add new relev\u00e9 (with header)",
    ADDREL    = "Add relev\u00e9 body only",
    ADDHEAD   = "Add header only",
    RREL      = "Repair an existing relev\u00e9",
    RHEAD     = "Repair an existing header",
    YSP       = "Batch: add one species to many relev\u00e9s",
    PRINTREL  = "Print REL table",
    PRINTHEAD = "Print HEAD table",
    REMOVEREL = "Remove a relev\u00e9 (REL+HEAD) and reindex",
    INFO = "Print sessions informations",
    "?, H, HELP"       = "Show this help",
    "Q, N"         = "Save & quit"
  )
}

#' Project information banner (during menu)
#' @keywords internal
#' @noRd
rv_status_banner <- function(checklist_path, database_label, save_dir, rel_count, head_count, meta) {
  cat("", rv_col(paste0("Rveg ",meta$rveg_version,"::"), "title"), sep = "")
  cat(rv_col(paste0("Project: ", meta$project_name, "; "), "info"))
  cat(rv_col(paste0("Checklist: ", meta$checklist, "; \n"), "info"))
  cat(rv_col(paste0("Database: ", normalizePath(save_dir, winslash = "/", mustWork = FALSE), "; "), "info"))

  same <- isTRUE(rel_count == head_count)
  cat(rv_col(sprintf("Relev\u00e9s: %d; Headers: %d", rel_count, head_count), if (same) "ok" else "err"))
}

#' Prompt for all labels except ID; reuse previous if user types "RE".
#' Returns a named character vector for the provided labels.
#' @keywords internal
#' @noRd
rv_prompt_header_values <- function(labels, HeaderDATA, prev_col, skip = "ID") {
  labs <- labels[!(tolower(labels) %in% tolower(skip))]
  get_prev <- function(lab) {
    if (is.null(prev_col) || !prev_col %in% names(HeaderDATA)) return("")
    v <- HeaderDATA[HeaderDATA$ShortName == lab, prev_col, drop = TRUE]
    if (length(v) == 0 || is.na(v) || identical(v, "NA")) "" else as.character(v)
  }
  vals <- vapply(labs, function(lab) {
    prev <- get_prev(lab)
    rv_read_or_re(paste0(lab, "? "), prev)  # your helper; only hints if prev != ""
  }, FUN.VALUE = character(1))
  stats::setNames(vals, labs)
}



#----- Data manipulation  -----------

#' Creates tables from all releves var 1 for classic, var 2 for batch
#' @keywords internal
#' @noRd
rv_create_table <- function(RelNew, DATA2 = NULL, variation = 1) {

  # --- 1. Data Prep & Zero Filtering ---

  # Normalize RelNew into a list
  if (variation == 1) {
    RelNew_list <- list(RelNew)
  } else if (variation == 2) {
    RelNew_list <- RelNew
  }

  # Filter out 0 values immediately
  RelNew_list <- lapply(RelNew_list, function(df) {
    df[df$value != "0", , drop = FALSE]
  })

  # --- 2. Build Master Species List ---

  observed_names <- character(0)

  # Get names from DATA2
  if (!is.null(DATA2) && ncol(DATA2) > 1) {
    observed_names <- c(observed_names, DATA2$ShortName)
  }

  # Get names from RelNew
  new_names <- unlist(lapply(RelNew_list, function(x) x$ShortName))
  observed_names <- c(observed_names, new_names)

  # Create the unique backbone
  unique_observed <- unique(observed_names)
  base <- data.frame(ShortName = unique_observed, stringsAsFactors = FALSE)

  # --- 3. Populate Data (Using Match to avoid merge-sorting issues) ---

  # Fill from DATA2
  if (!is.null(DATA2) && ncol(DATA2) > 1) {
    # Identify columns in DATA2 that aren't ShortName
    d2_cols <- setdiff(names(DATA2), "ShortName")

    # Match indices
    idx <- match(base$ShortName, DATA2$ShortName)

    # Bring over columns
    for(col in d2_cols) {
      base[[col]] <- DATA2[[col]][idx]
    }
  }

  # Fill from RelNew
  for (i in seq_along(RelNew_list)) {
    df <- RelNew_list[[i]]
    idx <- match(base$ShortName, df$ShortName)

    # Create new column
    new_col_name <- paste0("new_temp_", i)
    base[[new_col_name]] <- df$value[idx]
  }

  # Fill all NAs with "0" (doing this once for the whole DF is faster)
  base[is.na(base)] <- "0"

  # --- 4. Final Cleanup ---

  # Rename columns sequentially 1, 2, 3...
  # Excluding the first column (ShortName)
  total_data_cols <- ncol(base) - 1
  #names(base)[-1] <- as.character(seq_len(total_data_cols))
  names(base)[-1] <- paste0("X", seq_len(total_data_cols))

  # --- 5. SORTING (Happens LAST to guarantee order) ---

  # Split "SpeCode_Layer"
  pattern <- "^(.+)_([^_]+)$"
  parts <- strcapture(pattern, base$ShortName, proto = data.frame(code="", layer=""))

  # Add temporary sorting columns
  # We use the parts directly for ordering the 'base' dataframe
  # Order: 1. Layer (Alphabetical), 2. Code (Alphabetical)
  ord_idx <- order(parts$layer, parts$code)

  # Apply the sort
  base <- base[ord_idx, ]

  # Reset row names for cleanliness
  rownames(base) <- NULL

  return(base)
}

#' @keywords internal
#' @noRd
rv_make_sp_list <- function(checklist, metadata) {
  Sp <- read.delim(checklist, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
  Sp <- Sp[, c("ShortName", "FullName")]
  Sp <- Sp[!is.na(Sp$ShortName) & nzchar(Sp$ShortName), , drop = FALSE]

  out <- Sp

  # parse "LONG::SHORT" securely
  md <- metadata$extra_spec

  # Only attempt to parse if md is a valid, non-empty string
  if (!is.null(md) && nzchar(trimws(md))) {
    items <- trimws(strsplit(md, "|", fixed = TRUE)[[1]])
    items <- items[nzchar(items)]

    # Check if there are actually items after trimming
    if (length(items) > 0) {
      parts <- strsplit(items, "::", fixed = TRUE)

      long  <- vapply(parts, function(p) trimws(p[1]), "", USE.NAMES = FALSE)
      short <- vapply(parts, function(p) if (length(p) >= 2) trimws(p[2]) else NA_character_,
                      "", USE.NAMES = FALSE)

      meta_out <- data.frame(FullName = long,
                             ShortName = short)

      out <- rbind(out, meta_out)
    }
  }

  # Ensure keys are unique (better to stop than silently mangle)
  dups <- duplicated(out$ShortName)
  if (any(dups)) {
    stop(
      "Duplicate ShortName keys after layering: ",
      paste(unique(out$ShortName[dups]), collapse = ", ")
    )
  }

  rownames(out) <- NULL
  out
}

#' Transform Braun-Blanquet scale to percentage
#' @keywords internal
#' @noRd
rv_bb_to_pct <- function(x) {
  switch(toupper(x),
         "R"=1, "+"=2, "1"=3, "2"=15, "2M"=4, "M"=4, "2A"=8, "A"=8,
         "2B"= 18, "B"=18, "3"=38, "4"=63, "5"=88, "0"=0, NA_real_
  )
}

#' Reindexing after rel removal
#' @keywords internal
#' @noRd
rv_reindex_releves <- function(df, keep = "ShortName") {
  keep <- intersect(keep, names(df))
  rel <- grep("^X\\d+$", names(df), value = TRUE)
  if (!length(rel)) rel <- setdiff(names(df), keep)
  rel <- setdiff(rel, keep)
  names(df)[match(rel, names(df))] <- paste0("X", seq_along(rel))
  df
}


#' Default header fields for creating DB
#' @keywords internal
#' @noRd
rv_default_header_fields <- function() {
  c("ID","DATE","SpringDATE","LOCALITY","FieldCODE","Authors",
    "PlotSize","Latitude","Longitude","Accuracy","CRS","Slope","Exposure",
    "E3","E2","E1","Ejuv","E0","Note")
}

#' Normalize labels, check for duplicates
#' @keywords internal
#' @noRd
rv_normalize_fields <- function(x) {
  if (is.null(x)) return(NULL)
  y <- gsub("\\s+", " ", trimws(as.character(x)))
  y <- y[nzchar(y)]
  y[!duplicated(tolower(y))]
}

# Merging layers, Logic: l1 + l2 * (1 - l1/100)
#' @keywords internal
#' @noRd
Merge_layers <- function(x) {

  x <- x[!is.na(x) & x > 0]

  # If empty, return 0; if only one layer, return it
  if (length(x) == 0) return(0)
  if (length(x) == 1) return(x)

  # Apply the formula iteratively to all numbers in the vector
  result <- Reduce(function(l1, l2) {
    l1 + (l2 * (1 - (l1 / 100)))
  }, x)

  return(round(result))
}

#----- Not sorted atm ---------------

#' ID always first part of the header
#' @keywords internal
#' @noRd
rv_ensure_id_first <- function(labels) {
  labs <- rv_normalize_fields(labels)
  labs_wo_id <- labs[tolower(labs) != "id"]
  c("ID", labs_wo_id)
}

#' Extract schema (ShortName) from HEAD table.
#' @keywords internal
#' @noRd
rv_schema_from_head <- function(head_df) {
  if (is.null(head_df) || !"ShortName" %in% names(head_df)) return(character(0))
  as.character(head_df$ShortName)
}

#' Append new header rows (never ID). Fill NA across existing X* columns.
#' @keywords internal
#' @noRd
rv_expand_head_with_fields <- function(HeaderDATA, new_fields) {
  nf <- setdiff(rv_normalize_fields(new_fields), rv_schema_from_head(HeaderDATA))
  nf <- nf[tolower(nf) != "id"]
  if (!length(nf)) return(HeaderDATA)
  add <- data.frame(ShortName = nf, stringsAsFactors = FALSE)
  for (xc in setdiff(names(HeaderDATA), "ShortName")) add[[xc]] <- NA
  rbind(HeaderDATA, add)
}

#' Last X* column name or NULL.
#' @keywords internal
#' @noRd
rv_last_x_col <- function(df) {
  xs <- grep("^X\\d+$", names(df), value = TRUE)
  if (!length(xs)) return(NULL)
  xs <- xs[order(as.integer(sub("^X", "", xs)))]
  lc <- tail(xs, 1)

  # If last X* is effectively empty, treat as no previous column
  vals <- df[[lc]]
  # coerce to character, remove NA, test for any non-empty
  ch <- ifelse(is.na(vals), "", as.character(vals))
  if (!any(nzchar(ch))) return(NULL)

  lc
}

#' Check if shortname code already exist, if not, passes TRUE (ok)
#' @keywords internal
#' @noRd
rv_check_short_name <- function(code, checklist, meta) {
  code <- toupper(trimws(code))
  in_chk <- code %in% toupper(checklist$ShortName)

  extra_codes <- character(0)
  md <- meta$extra_spec
  if (!is.null(md) && nzchar(trimws(md))) {
    items <- strsplit(md, "|", fixed = TRUE)[[1]]
    items <- items[nzchar(items)]
    parts <- strsplit(items, "::", fixed = TRUE)
    extra_codes <- toupper(vapply(parts, function(p) if (length(p) >= 2) trimws(p[2]) else "", ""))
    extra_codes <- extra_codes[nzchar(extra_codes)]
  }

  !(in_chk || code %in% extra_codes)
}

# pochybne funkce?

#' @keywords internal
#' @noRd
rv_existing_k <- function(df) max(0L, ncol(df) - 1L)

#' Robustly bind a list of dataframes by rows (Base R alternative to dplyr::bind_rows)
#' @keywords internal
#' @noRd
rv_bind_rows <- function(df_list) {
  # Drop empty elements
  df_list <- df_list[vapply(df_list, function(x) length(x) > 0 && nrow(x) > 0, logical(1))]
  if (length(df_list) == 0) return(data.frame())

  # Find all unique column names across all dataframes
  all_cols <- unique(unlist(lapply(df_list, names)))

  # Pad missing columns with NA and ensure consistent column order
  padded_list <- lapply(df_list, function(df) {
    missing_cols <- setdiff(all_cols, names(df))
    if (length(missing_cols) > 0) {
      df[missing_cols] <- NA
    }
    df[, all_cols, drop = FALSE]
  })

  # Bind them together
  do.call(rbind, padded_list)
}


# on attach message
.onAttach <- function(libname, pkgname) {
  # Get the current version dynamically
  pkg_version <- utils::packageVersion("Rveg")
  packageStartupMessage(rv_col(paste0("Welcome to Rveg (version ", pkg_version, ")!"),"ok"))
  packageStartupMessage("Detailed guide (v0.1.6): `vignette('Rveg')`, Updates: `news(package = 'Rveg')`")
}
