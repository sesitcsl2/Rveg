#-----------------------------------#
#---- Rveg internal functions ------#
#-----------------------------------#

#---- Console UI, messages and printing ----

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

#' split df to several columns based on the console width
#' @keywords internal
#' @noRd
rv_print_dynamic <- function(df) {
  n <- nrow(df)
  if (n == 0) return(invisible())

  # Add formatted row numbers (1 to n) to each string
  comb <- paste(format(1:n), format(df[, 1]), format(df[, 2]), sep = "  ")

  # Define the gap between column groups (8 spaces)
  sep_spaces <- 8
  sep_str <- "        "

  # Calculate how many columns fit based on console width
  item_width <- max(nchar(comb))
  n_cols <- max(1, floor((getOption("width") + sep_spaces) / (item_width + sep_spaces)))

  # Determine required rows to maintain the fixed column structure
  n_rows <- ceiling(n / n_cols)

  # Pad to make a perfect rectangle (n_rows * n_cols)
  pad <- (n_rows * n_cols) - n

  # Fill top-to-bottom (byrow = FALSE)
  out_mat <- matrix(c(comb, rep("", pad)), nrow = n_rows, ncol = n_cols, byrow = FALSE)

  # Print without matrix row/col names
  write.table(out_mat, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = sep_str)
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
    PRINTMETA = "Print metadata",
    REMOVEREL = "Remove a relev\u00e9 (REL+HEAD) and reindex",
    INFO = "Print session information",
    "?, H, HELP"       = "Show this help",
    "Q, N"         = "Save & quit"
  )
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

#---- User input helpers ----

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

#' Read a value or reuse the previous value
#' If the user enters "RE", the previous non-empty value is returned.
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

#' @keywords internal
#' @noRd
rv_ask_abundance <- function(scale) {
  scale <- toupper(trimws(scale))

  if (scale == "P") {
    repeat {
      x <- rv_ask_text("Abundance? (%) ")
      o <- suppressWarnings(as.numeric(x))
      if (!is.na(o) && o >= 0 && o <= 100) return(o)
      message(rv_col("Enter number in range 0-100", "warn"))
    }
  }

  if (scale == "CS") {
    return(rv_ask_text("Abundance? "))
  }

  if (scale %in% c("BB", "B")) {
    bb <- rv_ask_choice(
      "Abundance? (0,R,+,1,2,M,A,B,3,4,5) ",
      c("0","R","+","1","2","M","A","B","3","4","5")
    )
    return(rv_bb_to_pct(bb))
  }
}

#---- Header schema helpers ----

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

#' @keywords internal
#' @noRd
rv_existing_k <- function(df) max(0L, ncol(df) - 1L)

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



#---- Species and checklist helpers ----

#' Resolve checklist path
#' 1. If metadata contains a checklist, try that first.
#' 2. First search installed Rveg checklist folders.
#' 3. If not found, search the working directory / direct path.
#' 4. If metadata checklist fails, fall back to the function argument.
#' @keywords internal
#' @noRd
rv_get_checklist <- function(checklist = "default",
                             meta = NULL,
                             prefer_meta = TRUE,
                             wd = getwd()) {

  has_text <- function(x) {
    if (is.null(x) || length(x) == 0L) return(FALSE)
    x <- as.character(x[1])
    !is.na(x) && nzchar(trimws(x))
  }

  # Candidate order
  meta_checklist <- NULL
  if (!is.null(meta) && has_text(meta$checklist)) {
    meta_checklist <- meta$checklist
  }

  candidates <- if (prefer_meta) {
    c(meta_checklist, checklist)
  } else {
    c(checklist, meta_checklist)
  }

  candidates <- candidates[vapply(candidates, has_text, logical(1))]
  candidates <- unique(trimws(as.character(candidates)))

  for (x in candidates) {

    # Default alias
    if (identical(x, "default")) {
      x <- "cz_dh2012"
    }

    # Try checklist distributed with Rveg
    checklist_name <- tools::file_path_sans_ext(basename(x))

    package_paths <- c(
      system.file(
        "extdata", "RvChecklist", paste0(checklist_name, ".txt"),
        package = "Rveg"
      ),
      system.file(
        "extdata", "TvChecklist", paste0(checklist_name, ".txt"),
        package = "Rveg"
      )
    )

    package_paths <- package_paths[nzchar(package_paths)]

    package_paths <- package_paths[file.exists(package_paths)]

    if (length(package_paths) > 0L) {
      return(normalizePath(package_paths[1], winslash = "/", mustWork = TRUE))
    }


    # 2. Try custom checklist path / working directory
    custom_paths <- x

    if (!nzchar(tools::file_ext(x))) {
      custom_paths <- c(custom_paths, paste0(x, ".txt"))
    }

    custom_paths <- unique(c(
      custom_paths,
      file.path(wd, custom_paths)
    ))

    custom_paths <- custom_paths[file.exists(custom_paths)]

    if (length(custom_paths) > 0L) {
      return(normalizePath(custom_paths[1], winslash = "/", mustWork = TRUE))
    }
  }

  stop(
    "Checklist could not be found. Tried: ",
    paste(candidates, collapse = ", "),
    call. = FALSE
  )
}

#' @keywords internal
#' @noRd
rv_extra_species <- function(metadata) {
  md <- metadata$extra_spec
  if (!isTRUE(nzchar(trimws(md), keepNA = TRUE)))
    return(data.frame(ShortName = character(), FullName = character()))

  items <- trimws(strsplit(md, "|", fixed = TRUE)[[1L]])
  parts <- strsplit(items[nzchar(items)], "::", fixed = TRUE)
  data.frame(
    ShortName = vapply(parts, function(p) trimws(p[2L]), "", USE.NAMES = FALSE),
    FullName = vapply(parts, function(p) trimws(p[1L]), "", USE.NAMES = FALSE)
  )
}

rv_make_sp_list <- function(checklist, metadata) {
  Sp <- read.delim(checklist, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
  Sp <- Sp[, c("ShortName", "FullName")]
  Sp <- Sp[!is.na(Sp$ShortName) & nzchar(Sp$ShortName), , drop = FALSE]

  out <- rbind(Sp, rv_extra_species(metadata))

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

rv_check_long_name <- function(name, checklist, meta) {
  name <- toupper(trimws(name))
  in_chk <- name %in% toupper(checklist$FullName)

  extra_names <- character(0)
  md <- meta$extra_spec
  if (!is.null(md) && nzchar(trimws(md))) {
    items <- strsplit(md, "|", fixed = TRUE)[[1]]
    items <- items[nzchar(items)]
    parts <- strsplit(items, "::", fixed = TRUE)
    extra_names <- toupper(vapply(parts, function(p) if (length(p) >= 2) trimws(p[1]) else "", ""))
    extra_names <- extra_names[nzchar(extra_names)]
  }

  !(in_chk || name %in% extra_names)
}

rv_search_species <- function(SpLIST) {
  k <- rv_ask_text("SpeciesFirst3letters? (eg.Che / F=cancel) ",allow_cmd = "F", min_nchar = 2)

  if (k == "F") return(invisible(NULL))

  searchlist <- SpLIST[grep(paste0("^", k), SpLIST$FullName, ignore.case = TRUE), , drop = FALSE]

  if (nrow(searchlist) == 0) {
    message(rv_col("No matches found.", "warn"))
  } else {
    print(searchlist[order(searchlist$FullName), ])
  }
  return(invisible(NULL))
}

rv_insert_species <- function(SpLIST, metadata) {
  if (is.null(metadata$extra_spec) || is.na(metadata$extra_spec)) metadata$extra_spec <- ""

  repeat {
    sn <- rv_ask_text("Full species name? (N to cancel): ", min_nchar = 1)
    if (toupper(trimws(sn)) == "N") return(NULL)

    if (!rv_check_long_name(sn, SpLIST, metadata)) {
      message(rv_col("Species already present.", "warn"))
      next
    }

    if (rv_ask_choice(paste0(sn, ": correct? (Y/N) "), c("Y", "N")) == "Y") break
  }

  repeat {
    sc <- toupper(rv_ask_text("ShortCode? (GenuSpe / N to cancel): ", min_nchar = 1))
    if (toupper(trimws(sc)) == "N") return(NULL)

    if (nchar(sc) != 7) next

    if (!rv_check_short_name(sc, SpLIST, metadata)) {
      message(rv_col("Code already reserved.", "warn"))
      next
    }

    if (rv_ask_choice(paste0(sn, "; ", sc, "? (Y/N) "), c("Y", "N")) == "Y") {
      SpLIST <- rbind(SpLIST, data.frame(ShortName = sc, FullName = sn, stringsAsFactors = FALSE))
      metadata$extra_spec <- paste0(metadata$extra_spec, sn, "::", sc, "|")
      #print(sn)

      # Return the updated lists AND the new code
      return(list(code = sc, SpLIST = SpLIST, metadata = metadata))
    }
  }
}

#' @keywords internal
#' @noRd
rv_resolve_species_code <- function(code, SpLIST, metadata, RelNew = NULL) {

  if (is.null(metadata$extra_spec) || is.na(metadata$extra_spec)) {
    metadata$extra_spec <- ""
  }
  code <- toupper(trimws(code))

  # -------- 1. STANDARD 7-CHAR CODE CHECK -------- #

  if (code != "SEARCH" && code != "INSERT") {
    chk <- SpLIST[SpLIST$ShortName == code, , drop = FALSE]

    if (nrow(chk) == 0 || is.na(chk$FullName[1])) {
      message(rv_col(paste("Species code", code, "not found."), "err"))
      return(NULL) # Kicks back to parent loop for immediate typo correction
    }

    print(chk$FullName[1])
    if (rv_ask_choice("CorrectName? (Y=accept / N=reject) ", c("Y", "N")) == "Y") {
      return(list(code = code, SpLIST = SpLIST, metadata = metadata, RelNew = RelNew))
    } else {
      return(NULL)
    }
  }


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
rv_species_not_found <- function(bad_name, SpList, metadata, orig_SpList = NULL) {

  message(rv_col(paste0("\nUNKNOWN SPECIES: '", bad_name, "'"),"warn"))

  while (TRUE) {

    if (!is.null(orig_SpList)) {

      bad_code <-  orig_SpList[which(orig_SpList$name == bad_name),]$code
      code_name <- SpList[which(SpList$ShortName == bad_code),]$FullName
      if (length(code_name)>0) {

        while(TRUE) {



          cat(paste0("suggest from the original list:", bad_name, " -> ", code_name, " ? (Y/N)" ))
          m <- rv_ask_choice("Accept? (Y/N): ", c("Y", "N"))
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

    m <- rv_ask_text("Search with 3 letters + or (I)nsert new code: ", min_nchar = 3, allow_cmd = "I")

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

#---- Relevé entry helpers ----

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

    most_sort <- factor(sub(".*_", "", RelNew$ShortName), levels = c("3", "2", "1", "J", "0"))
    RelNew <- RelNew[order(most_sort, RelNew$ShortName), ]
    # zde prostor pro smazani prazdnych druhu
    #RelNew <- RelNew[order(RelNew$ShortName), ]
  }
  rownames(RelNew) <- NULL
  return(RelNew)
}

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
  # remove empty rows / species
  base <- base[rowSums(base[, -1, drop = FALSE] != "0") > 0, ]

  # --- 5. SORTING (Happens LAST to guarantee order) ---

  # Split "SpeCode_Layer"
  pattern <- "^(.+)_([^_]+)$"
  parts <- strcapture(pattern, base$ShortName, proto = data.frame(code="", layer=""))

  # Add temporary sorting columns
  # We use the parts directly for ordering the 'base' dataframe
  # Order: 1. Layer (Alphabetical), 2. Code (Alphabetical)
  #ord_idx <- order(parts$layer, parts$code)
  layer_factor <- factor(parts$layer, levels = c("3", "2", "1", "J", "0"))
  ord_idx <- order(layer_factor, parts$code)

  # Apply the sort
  base <- base[ord_idx, ]

  # Reset row names for cleanliness
  rownames(base) <- NULL

  return(base)
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

#---- Relevé dialogue ----

#' Dialogue loop for writing releves, var 1 for classic, var 2 for batch
#' @keywords internal
#' @noRd
rv_releve_dialogue <- function(SpLIST, RelNew, metadata, SAVE = NULL, HeaderDATA2 = NULL, variation = 1) {

  out <- list()

  if (variation == 1) {

    repeat {
      add_layer <- rv_ask_choice("AddNewLayer? (Y/N) ", c("Y","N"))

      if (add_layer == "N") {

        message(rv_col("Species richness","info"))
        #print(nrow(RelNew[(RelNew[, 2] != 0), ]))

        valid_data <- RelNew[RelNew[, 2] != 0, ]
        layers <- factor(sub(".*_", "", valid_data[, 1]), levels = c("3", "2", "1", "J", "0"))
        counts <- table(layers)
        print(counts[counts != 0])

        break
      }

      most <- rv_ask_choice("Select Layer (3,2,1,J,0) ", c("3","2","1","J","0"))
      oo   <- rv_ask_choice("P - percentage, BB - Braun-Blanquet scale, CS - custom scale ",
                            c("P","BB","B","CS"))

      repeat {
        # First Entry
        m <- rv_ask_text("AddSpecies? (GenuSpe / SEARCH / INSERT / N) ",
                         allow_cmd = c("N", "SEARCH", "INSERT"))
        if (m == "N") break

        n <- toupper(trimws(m))

        # SEARCH
        if (n == "SEARCH") {
          rv_search_species(SpLIST)
          next
        }

        # INSERT
        if (n == "INSERT") {
          res <- rv_insert_species(SpLIST, metadata)
          if (!is.null(res)) {
            SpLIST <- res$SpLIST
            metadata <- res$metadata
            message(paste("Added", res$code, "to checklist."))
          }
          next
        }

        # STANDARD
        if (nchar(n) != 7) next

        o <- rv_ask_abundance(oo)
        valid_code <- FALSE

        # Validation
        # Validation
        repeat {
          # 1. Master Guard: Catch 'N' immediately to safely cancel
          if (n == "N") break

          # 2. Length Guard: Prevent bad inputs from querying the database
          if (nchar(n) == 7) {
            # 3. Database Check (Only runs if exactly 7 characters)
            chk <- SpLIST[SpLIST$ShortName == n, , drop = FALSE]

            if (nrow(chk) > 0 && !is.na(chk$FullName[1])) {
              print(chk$FullName[1])
              if (rv_ask_choice("CorrectName? (Y=accept / N=reject) ", c("Y", "N")) == "Y") {
                valid_code <- TRUE
                break
              }
            } else {
              message(rv_col(paste("Code", n, "not found."), "err"))
            }
          }

          # 4. The Fix Prompt (Triggered by bad length, bad code, or user rejection)
          m_fix <- rv_ask_text("SpeciesCode? (GenuSpe / SEARCH / INSERT / N) ",
                               allow_cmd = c("N", "SEARCH", "INSERT"))

          if (m_fix == "N") break

          n_fix <- toupper(trimws(m_fix))

          if (n_fix == "SEARCH") {
            rv_search_species(SpLIST)
            # Update prompt to remind them they can still cancel here
            n <- toupper(trimws(rv_ask_text("Enter SpeciesCode (or N to cancel): ")))
            next
          }

          if (n_fix == "INSERT") {
            res <- rv_insert_species(SpLIST, metadata)
            if (!is.null(res)) {
              SpLIST <- res$SpLIST
              metadata <- res$metadata
              n <- res$code
              next
            } else {
              break # If they cancelled the insert process, cancel the entry
            }
          }

          # Standard manual typo fix
          n <- n_fix
        }

        # If user cancelled out of the typo loop, skip saving
        if (!valid_code) next

        # Save the record
        nana <- paste(n, most, sep = "_")
        RelNew <- rv_add_rel_new(RelNew, nana, o)

        RelNew_filter <- RelNew[(RelNew[, 2] != 0) & endsWith(as.character(RelNew[, 1]), most), ]
        rv_print_dynamic(RelNew_filter)
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

    oo <- rv_ask_choice("P - percentage, BB - Braun-Blanquet scale, CS - custom scale ",
                        c("P","BB","B","CS"))

    repeat {
      x <- rv_ask_text("Add new species (GenuSpe)/N?  ", min_nchar = 1, allow_cmd = c("N"))

      if (x == "N") {
        rs <- rv_ask_choice("Add headers? (Y/N) ", c("Y","N"))
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
      if (is.null(res)) {
        message(rv_col("Species entry canceled.", "warn"))
        next
      }
      n <- res$code
      SpLIST <- res$SpLIST
      metadata <- res$metadata

      nana <- paste(n, l, sep = "_")

      for (i in 1:m) {
        cat(rv_col(paste0("Relev\u00e9 ", i),"info"))
        o <- rv_ask_abundance(oo)

        Rels[[paste0("r", i)]] <- rv_add_rel_new(Rels[[paste0("r", i)]], nana, o)
        print(Rels[[paste0("r", i)]][(Rels[[paste0("r", i)]][, 2] != 0), ])
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



#---- Database metadata helpers ----

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


#---- Database reading and writing ----

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

#---- Cover scale and cover merging helpers ----

#' Transform Braun-Blanquet scale to percentage
#' @keywords internal
#' @noRd
rv_bb_to_pct <- function(x) {
  switch(toupper(x),
         "R"=1, "+"=2, "1"=3, "2"=15, "2M"=4, "M"=4, "2A"=8, "A"=8,
         "2B"= 18, "B"=18, "3"=38, "4"=63, "5"=88, "0"=0, NA_real_
  )
}

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

# Merging layers, Logic: l1 + l2 * (1 - l1/100)
#' @keywords internal
#' @noRd
rv_merge_layers <- function(x) {

  x <- suppressWarnings(as.numeric(x))
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

#' Remap layer codes using a named vector
#' @keywords internal
#' @noRd
rv_layer_map <- function(layers, map, na_value = NULL) {
  if (!is.null(na_value)) layers[is.na(layers)] <- na_value
  layers <- as.character(layers)
  mapped <- map[layers]
  unname(ifelse(is.na(mapped), layers, mapped))
}

# Convert vegetation layers between the conventions used by Rveg and its file formats.
rv_juice_layers <- function(layers, direction, unmapped = c("na", "keep", "empty")) {
  layers <- as.character(layers)
  unmapped <- match.arg(unmapped)

  # JUICE manual, section 1.7.1. Layer 0 means undefined.
  from_juice <- c("0" = NA_character_, "1" = "3", "2" = "3", "3" = "3",
    "4" = "2", "5" = "2", "6" = "1", "7" = "J", "8" = "J", "9" = "0")

  # Use one representative per Rveg layer on export.
  to_juice <- c("3" = "2", "2" = "4", "1" = "6", "0" = "9", "J" = "7")
  to_tv <- c("3" = "t1", "2" = "s1", "1" = "hl", "0" = "ml", "J" = "jl")

  if (direction == "tv_to_rveg") {
    # Preserve the legacy Turboveg import convention for numeric layer 0.
    from_juice["0"] <- "0"
    map <- c(setNames(names(to_tv), substr(to_tv, 1L, 1L)), from_juice)
    keys <- substr(layers, 1L, 1L)
  } else {
    map <- switch(direction, rveg_to_juice = to_juice, juice_to_rveg = from_juice,
      rveg_to_tv = to_tv)
    keys <- layers
  }

  matches <- match(keys, names(map))
  result <- unname(map[matches])
  missing <- is.na(matches)
  if (unmapped == "keep") result[missing] <- layers[missing]
  if (unmapped == "empty") result[missing] <- ""
  result
}

#' Merge species rows by ShortName + Layer, with optional layer/species remapping
#' @keywords internal
#' @noRd
rv_merge_species <- function(species, codes, layers, covers,
                             merge_layers = NULL, merge_species = NULL) {
  df <- data.frame(FullName = species, ShortName = codes, Layer = layers,
                   stringsAsFactors = FALSE)
  df <- cbind(df, covers)
  plot_cols <- names(covers)

  # 1. User-specified layer remapping
  if (!is.null(merge_layers)) {
    df$Layer <- rv_layer_map(df$Layer, merge_layers)
  }
  # 2. User-specified species remapping
  if (!is.null(merge_species)) {
    df$ShortName <- rv_layer_map(df$ShortName, merge_species)
  }

  # 3. Merge rows with identical ShortName + Layer
  groups <- paste(df$ShortName, df$Layer, sep = "||")
  unique_groups <- unique(groups)

  merged <- do.call(rbind, lapply(unique_groups, function(g) {
    rows <- df[groups == g, , drop = FALSE]
    out  <- rows[1, 1:3, drop = FALSE]

    if (nrow(rows) == 1) {
      out[plot_cols] <- rows[1, plot_cols, drop = FALSE]
      return(out)
    }

    names_involved <- unique(rows$FullName)
    cover_mat      <- as.matrix(rows[, plot_cols, drop = FALSE])
    has_overlap     <- any(colSums(cover_mat != 0) > 1)

    if (!has_overlap && length(names_involved) == 1) {
      # Fast path: no overlap, same species — just sum columns
      for (pc in plot_cols) out[[pc]] <- sum(rows[[pc]])
    } else {
      # Real merge — use Merge_layers for overlapping plots
      for (pc in plot_cols) out[[pc]] <- rv_merge_layers(rows[[pc]])

      if (length(names_involved) > 1) {
        message(rv_col(paste0(
          'Merged "', paste(names_involved[-1], collapse = '", "'),
          '" to "', names_involved[1], '" [', out$Layer, ']'), "ok"))
      } else {
        message(rv_col(paste0(
          'Merged ', nrow(rows), ' rows of "', names_involved[1],
          '" [', out$Layer, ']'), "ok"))
      }
    }
    out
  }))

  rownames(merged) <- NULL
  merged
}

#---- Table and data-frame manipulation ----

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


#---- Optional standalone helpers ----

#' Remap cover codes in a data.frame using a named vector
#' @keywords internal
#' @noRd
rv_cover_map <- function(df, map) {
  df[] <- lapply(df, function(col) {
    mapped <- map[as.character(col)]
    unname(ifelse(is.na(mapped), col, mapped))
  })
  df
}

#' Match species names to short codes from a checklist
#' @keywords internal
#' @noRd
rv_species_code_map <- function(species, code_map) {

  if (!"note" %in% names(code_map)) code_map$note <- NA_character_

  code_map$preset <- code_map$FullName %in% species
  missing <- species[!species %in% code_map$FullName]

  if (length(missing) > 0) {
    message(rv_col(paste0(length(missing), " species not found in checklist:"), "warn"))
    message(paste(" ", missing, collapse = "\n"))
    unmatched <- data.frame(ShortName = NA_character_, FullName = missing,
                            preset = TRUE, note = NA_character_,
                            stringsAsFactors = FALSE)
    code_map <- rbind(code_map, unmatched)
  }

  rownames(code_map) <- NULL
  code_map
}

#' Fill unmatched ShortName codes via interactive dialogue
#' @keywords internal
#' @noRd
rv_fill_shortnames <- function(map, code_map) {

  if (!"note" %in% names(map)) map$note <- NA_character_

  missing_idx <- which(is.na(map$ShortName))
  if (length(missing_idx) == 0) {
    message(rv_col("All species already matched!", "ok"))
    return(map)
  }

  n <- length(missing_idx)
  remove_idx <- integer(0)

  cat(sprintf("\n%d unmatched species to resolve:\n", n))

  for (pos in seq_along(missing_idx)) {
    i <- missing_idx[pos]
    sp <- map$FullName[i]

    # --- suggest code (4+3 rule / GENU-SP) ---
    parts   <- strsplit(trimws(sp), "\\s+")[[1]]
    genus   <- parts[1]
    epithet <- if (length(parts) > 1) parts[2] else ""
    suggested <- NULL

    if (grepl("^(sp\\.|sp|species|spp\\.|spp)$", epithet, ignore.case = TRUE)) {
      suggested <- paste0(toupper(substr(genus, 1, 4)), "-SP")
    } else if (nchar(genus) >= 4 && nchar(epithet) >= 3) {
      suggested <- paste0(toupper(substr(genus, 1, 4)), toupper(substr(epithet, 1, 3)))
    }

    # --- build suggestion hint with merge/unique info ---
    suggest_hint <- NULL
    suggest_existing <- NULL
    if (!is.null(suggested)) {
      suggest_existing <- code_map$FullName[code_map$ShortName == suggested]
      if (length(suggest_existing) > 0) {
        suggest_hint <- sprintf("%s (merge with %s)", suggested, suggest_existing[1])
      } else {
        suggest_hint <- sprintf("%s (unique)", suggested)
      }
    }

    cat(sprintf("\n[%d/%d] '%s'\n", pos, n, sp))
    if (!is.null(suggest_hint)) cat(sprintf("  Suggested: %s\n", suggest_hint))

    assigned <- FALSE
    while (!assigned) {
      cat("  (R)emove | (A)uto | (M)anual | (F)ind existing | (S)kip | (E)xit\n")
      action <- rv_ask_choice("  > ", c("R", "A", "M", "F", "S", "E"))

      # ---- Exit ----
      if (action == "E") {
        if (length(remove_idx) > 0) map <- map[-remove_idx, ]
        still_na <- sum(is.na(map$ShortName))
        message(rv_col(paste0("\n  Exiting. Remaining NA: ", still_na), "warn"))
        rownames(map) <- NULL
        return(map)

        # ---- Remove ----
      } else if (action == "R") {
        remove_idx <- c(remove_idx, i)
        map$note[i] <- "removed"
        message(rv_col(paste0("  Removed: '", sp, "'"), "warn"))
        assigned <- TRUE

        # ---- Auto-create ----
      } else if (action == "A") {
        if (is.null(suggested)) {
          message(rv_col("  Cannot auto-generate. Try (M)anual or (F)ind.", "warn"))
          next
        }

        if (length(suggest_existing) > 0) {
          map$note[i] <- paste0("auto: merged with ", suggest_existing[1], " (", suggested, ")")
        } else {
          map$note[i] <- paste0("auto: ", suggested)
        }

        map$ShortName[i] <- suggested
        message(rv_col(paste0("  ", sp, " --> ", suggested), "ok"))
        assigned <- TRUE

        # ---- Manual code ----
      } else if (action == "M") {
        code <- rv_ask_text("Enter code: ", min_nchar = 7)
        if (nchar(code) == 0) next

        existing <- code_map$FullName[code_map$ShortName == code]
        if (length(existing) > 0) {
          cat(sprintf("  '%s' exists as: %s\n", code, existing[1]))
          cat(sprintf("  This will merge '%s' with '%s'\n", sp, existing[1]))
          if (rv_ask_choice("  Proceed? (Y/N): ", c("Y", "N")) == "N") next
          map$note[i] <- paste0("manual: merged with ", existing[1], " (", code, ")")
        } else {
          map$note[i] <- paste0("manual: ", code)
        }

        map$ShortName[i] <- code
        message(rv_col(paste0("  ", sp, " --> ", code), "ok"))
        assigned <- TRUE

        # ---- Find existing ----
      } else if (action == "F") {
        repeat {
          query <- rv_ask_text("Search (min 2 letters):", min_nchar = 2)
          if (nchar(query) < 2) next

          hits <- code_map[grep(toupper(query), toupper(code_map$FullName), fixed = TRUE), ]
          if (nrow(hits) == 0) {
            message(rv_col("  No matches.", "warn"))
            next
          }

          print(hits[order(hits$FullName), c("ShortName", "FullName")])
          pick <- rv_ask_text("TypeCode (or BACK):", allow_cmd = "BACK")

          if (pick == "BACK") break

          if (pick %in% toupper(hits$ShortName)) {
            code <- hits$ShortName[toupper(hits$ShortName) == pick][1]
            full <- hits$FullName[toupper(hits$ShortName) == pick][1]
            cat(sprintf("  '%s' --> %s (%s)\n", sp, code, full))
            if (rv_ask_choice("  Confirm? (Y/N): ", c("Y", "N")) == "Y") {
              map$ShortName[i] <- code
              map$note[i] <- paste0("find: matched to ", full, " (", code, ")")
              assigned <- TRUE
              break
            }
          } else {
            cat("  Code not in results.\n")
          }
        }

        # ---- Skip ----
      } else if (action == "S") {
        map$note[i] <- "skipped"
        cat("  Skipped.\n")
        assigned <- TRUE
      }
    }
  }

  # Remove marked rows
  if (length(remove_idx) > 0) map <- map[-remove_idx, ]

  # Summary
  still_na <- sum(is.na(map$ShortName))
  cat(sprintf("\n  Resolved: %d | Removed: %d | Remaining NA: %d\n",
              n - length(remove_idx) - still_na, length(remove_idx), still_na))

  rownames(map) <- NULL
  map
}

#---- Rveg Classify ----

# Rveg relevés in long format for classification
rv_releves <- function(rel) {
  plots <- grep("^X[0-9]+$", names(rel), value = TRUE)
  cover <- as.matrix(rel[plots])
  storage.mode(cover) <- "double"
  presence <- which(cover > 0, arr.ind = TRUE)

  species <- data.frame(
    plot_id = plots[presence[, 2]],
    taxon_name = rel$fullName[presence[, 1]],
    layer = rv_juice_layers(rel$layer[presence[, 1]], "rveg_to_juice"),
    cover_pct = cover[presence],
    taxon_id = rel$ShortName[presence[, 1]],
    stringsAsFactors = FALSE
  )
  list(plot_id = plots, species = species)
}

# Taxon name normalization
rv_taxon_key <- function(x) {
  x <- tolower(sub("^$", NA_character_, trimws(x)))
  x <- gsub("[[:space:]]*\u00d7[[:space:]]*", " x ", x)
  x <- gsub("\\b(ssp\\.?|subsp\\.?|subspecies)(?=\\s|$)", "subsp.", x, perl = TRUE)
  x <- gsub('"', "", x, fixed = TRUE)
  x <- gsub("\\b(sensu|s\\.?)[[:space:]]*lat(o)?\\.?(?=\\s|\\*|$)", "s.lat.", x, perl = TRUE)
  x <- gsub("\\b(sensu|s\\.?)[[:space:]]*str(icto)?\\.?(?=\\s|\\*|$)", "s.str.", x, perl = TRUE)
  x <- gsub("\\baggr?\\.?(?=\\s|$)", "agg.", x, perl = TRUE)
  x <- trimws(gsub("[[:space:]]+", " ", x))
  sub("^([^ ]+) sp\\.?$", "\\1 species", x)
}

# Find checklist codes by exact name, with normalized spelling as a fallback.
rv_checklist_matches <- function(names, checklist) {
  checklist <- checklist[!is.na(checklist$ShortName) & nzchar(checklist$ShortName), , drop = FALSE]
  by_name <- split(checklist$ShortName, checklist$FullName)
  candidates <- by_name[names]
  missing <- lengths(candidates) == 0L
  if (any(missing)) {
    by_key <- split(checklist$ShortName, rv_taxon_key(checklist$FullName))
    candidates[missing] <- by_key[rv_taxon_key(names[missing])]
  }
  lapply(candidates, unique)
}

# Taxon matching and synonyms
rv_resolve_synonym_file <- function(synonym_file) {
  if (is.null(synonym_file)) return(NULL)

  if (identical(synonym_file, "WCVP-Global")) {
    filename <- "WCVP-v16-synonyms.tsv.gz"
    bundled <- system.file("extdata", "Classification", filename, package = "Rveg")
    if (nzchar(bundled)) return(bundled)

    cache <- if (getRversion() >= "4.0.0") tools::R_user_dir("Rveg", "cache") else tempdir()
    cached <- file.path(cache, "Classification", filename)
    if (file.exists(cached)) return(cached)
    if (!interactive()) {
      stop("Global WCVP synonyms are missing. Run once in an interactive R session to download them, ",
        "or pass a local synonym file.", call. = FALSE)
    }

    choice <- rv_ask_choice(paste0("Download global WCVP v16 synonyms from GitHub (about 7 MiB)?\n",
      "Y = download; N = use the bundled Czech table: "), c("Y", "N"))
    if (choice == "Y") return(rv_download_wcvp_synonyms(cached))
    synonym_file <- "WCVP-Czech"
  }

  if (identical(synonym_file, "WCVP-Czech")) {
    return(system.file("extdata", "Classification", "WCVP-v16-Czech-synonyms.tsv.gz",
      package = "Rveg", mustWork = TRUE))
  }
  if (file.exists(synonym_file)) synonym_file else NULL
}

# World Checklist of Vascular Plants v16, Royal Botanic Gardens, Kew, CC BY 3.0.
# https://doi.org/10.34885/egs6-cp24
rv_download_wcvp_synonyms <- function(file) {
  folder <- dirname(file)
  if (!dir.exists(folder)) dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  pending <- tempfile(tmpdir = folder)
  on.exit(unlink(pending), add = TRUE)

  settings <- options(timeout = max(300L, getOption("timeout")))
  on.exit(options(settings), add = TRUE)
  url <- paste0("https://github.com/sesitcsl2/Rveg/releases/download/wcvp-v16/",
    "WCVP-v16-synonyms.tsv.gz")
  status <- utils::download.file(url, pending, mode = "wb")
  if (status != 0L) stop("Could not download the global WCVP synonym table.", call. = FALSE)
  if (!identical(unname(tools::md5sum(pending)), "28489a2869602da56e2b9613855c529e"))
    stop("The downloaded WCVP synonym table does not match the expected file.", call. = FALSE)
  if (!file.rename(pending, file) && !file.exists(file))
    stop("Could not save the global WCVP synonym table.", call. = FALSE)
  file
}

rv_match_taxa <- function(source, target, synonym_file = NULL, aggregate_targets = target) {
  target <- unique(target[!is.na(target)])
  source_key <- rv_taxon_key(source)
  target_key <- rv_taxon_key(target)
  by_name <- split(target, target_key)
  candidates <- by_name[source_key]

  # Exact spelling takes precedence over normalized spellings.
  exact <- !is.na(source) & source %in% target
  candidates[exact] <- as.list(source[exact])

  # Look for a same-name aggregate when there is no name match.
  aggregate <- grepl(" (agg\\.|s\\.lat\\.)$", target_key) & target %in% aggregate_targets
  base <- sub(" (agg\\.|s\\.lat\\.)$", "", target_key[aggregate])
  binomial <- grepl("^[^ ]+ [^ ]+$", base) & !grepl(" (species|spp?\\.?)$", base)
  by_aggregate <- split(target[aggregate][binomial], base[binomial])
  missing <- lengths(candidates) == 0L & !is.na(source_key)
  candidates[missing] <- by_aggregate[source_key[missing]]

  # Look up synonyms for names still missing from the rules or model.
  missing <- which(lengths(candidates) == 0L & !is.na(source_key))
  if (length(missing) && !is.null(synonym_file)) {
    synonym_table <- utils::read.delim(synonym_file, quote = "", colClasses = "character",
      fileEncoding = "UTF-8")
    synonym_list <- strsplit(synonym_table$synonyms, " | ", fixed = TRUE)
    row_id <- seq_len(nrow(synonym_table))
    name_rows <- c(row_id, rep(row_id, lengths(synonym_list)))
    names_all <- c(synonym_table$accepted_name, unlist(synonym_list, use.names = FALSE))

    # Search the genera used by either the source or the target names.
    needed <- unique(c(source_key[missing], target_key))
    first_word <- tolower(sub('[[:space:]"\u00d7].*$', "", trimws(names_all)))
    possible <- first_word %in% sub(" .*", "", needed) | first_word == ""
    keys <- rv_taxon_key(names_all[possible])
    use <- !is.na(keys) & keys %in% needed
    synonym_rows <- lapply(split(name_rows[possible][use], keys[use]), unique)

    target_rows <- synonym_rows[target_key]
    names_by_row <- split(rep(target, lengths(target_rows)),
      unlist(target_rows, use.names = FALSE))
    source_rows <- synonym_rows[source_key[missing]]

    for (i in seq_along(missing)) {
      source_ids <- source_rows[[i]]
      matches <- unique(unlist(names_by_row[as.character(source_ids)], use.names = FALSE))
      if (length(matches) != 1L || length(source_ids) != 1L) next
      target_ids <- synonym_rows[[rv_taxon_key(matches)]]
      if (length(target_ids) != 1L) next

      # Avoid inferring a subspecies or variety from a species name alone.
      infra <- " (subsp\\.|var\\.|subvar\\.|f\\.|nothosubsp\\.|nothovar\\.|nothof\\.) "
      if (grepl(infra, rv_taxon_key(matches)) &&
          !grepl(infra, source_key[missing[i]])) next
      candidates[[missing[i]]] <- matches
    }
  }

  result <- rep(NA_character_, length(source))
  single <- lengths(candidates) == 1L
  result[single] <- unlist(candidates[single], use.names = FALSE)
  result
}

# Apply the ESy aggregation rules to each record using its name and layer.
rv_esy_aggregate_names <- function(species, mappings) {
  if (!nrow(species) || is.null(mappings) || !nrow(mappings)) return(as.list(species$taxon_name))
  by_layer <- split(mappings$target[!is.na(mappings$layer)],
    paste(mappings$source[!is.na(mappings$layer)],
      mappings$layer[!is.na(mappings$layer)], sep = "\r"))
  any_layer <- split(mappings$target[is.na(mappings$layer)],
    mappings$source[is.na(mappings$layer)])
  key <- paste(species$taxon_name, ifelse(is.na(species$layer), "0", species$layer),
    sep = "\r")
  Map(function(name, specific, general) {
    goal <- unique(c(specific, general))
    if (length(goal)) goal else name
  }, species$taxon_name, by_layer[key], any_layer[species$taxon_name])
}

# Combine the records that contribute to each plot and taxon.
rv_cover_matrix <- function(species, plots, taxa, mappings = NULL,
  names_by_row = rv_esy_aggregate_names(species, mappings)) {
  cover <- matrix(0, length(plots), length(taxa), dimnames = list(plots, taxa))
  if (!nrow(species)) return(cover)

  rows <- rep(seq_len(nrow(species)), lengths(names_by_row))
  columns <- match(unlist(names_by_row, use.names = FALSE), taxa)
  use <- !is.na(columns)
  cells <- match(species$plot_id[rows[use]], plots) +
    length(plots) * (columns[use] - 1L)
  if (length(cells)) {
    merged <- rowsum(log1p(-species$cover_pct[rows[use]] / 100), cells,
      reorder = FALSE)
    cover[as.integer(rownames(merged))] <- round(-100 * expm1(merged[, 1]), 10L)
  }
  cover
}

### ESy
# Read and understand ESy rules
rv_read_esy_rules <- function(file) {
  lines <- readLines(file, encoding = "UTF-8", warn = FALSE)
  lines <- sub("^\ufeff", "", lines)

  # Read the rows between the start and end markers of a section.
  section <- function(number, title, trim = TRUE) {
    start <- match(paste0("SECTION ", number, ": ", title), lines)
    end <- match(paste0("SECTION ", number, ": End"), lines)
    if (is.na(start)) return(character())
    out <- head(lines[seq.int(start + 1L, end)], -1L)
    out <- out[nzchar(trimws(out))]
    if (trim) trimws(out) else out
  }

  ## Section 1 - Species aggregation
  aggregation <- section(1, "Species aggregation", trim = FALSE)
  target_line <- !grepl("^\\s", aggregation)
  layer_pattern <- "\\s+(?:-\\s+)?([0-9])$"
  has_layer <- grepl(layer_pattern, trimws(aggregation), perl = TRUE)
  layers <- rep(NA_character_, length(aggregation))
  layers[has_layer] <- sub(".*([0-9])$", "\\1", trimws(aggregation[has_layer]))
  taxa <- trimws(sub(layer_pattern, "", trimws(aggregation), perl = TRUE))
  mappings <- unique(data.frame(
    source = taxa[!target_line],
    layer = layers[!target_line],
    target = taxa[target_line][cumsum(target_line)[!target_line]]
  ))

  ## Section 2 - Species groups
  members <- section(2, "Species groups")
  prefix <- "^(###|#[0-9]{2}|#TC|#SC|##[DCQ])[[:space:]]+"
  group_line <- grepl("^(###|#TC|#SC|##[DCQ])[[:space:]]+", members)
  group_names <- sub(prefix, "", members[group_line])
  groups <- split(members[!group_line], group_names[cumsum(group_line)[!group_line]])
  groups <- lapply(groups[group_names], unique)
  discriminating <- group_names[grepl("^##[DCQ][[:space:]]+", members[group_line])]

  ## Section 3 - Group definitions
  definitions <- section(3, "Group definitions")
  definitions <- definitions[!startsWith(definitions, "---")]
  tail_lines <- lines[seq_along(lines) > match("SECTION 3: End", lines)]
  legacy_header <- grepl("^[0-9A-Za-z][[:space:]]+\\S", definitions)
  legacy_names <- sub("^[0-9A-Za-z][[:space:]]+", "", definitions[legacy_header])
  legacy_codes <- sub("[[:space:]].*$", "", legacy_names)
  legacy_priority <- substr(definitions[legacy_header], 1L, 1L)

  # Older files may store names in a legend or use the names as headings.
  legend_start <- match("Syntaxon codes", trimws(tail_lines))
  if (!is.na(legend_start)) {
    legend <- trimws(tail_lines[seq_along(tail_lines) > legend_start])
    legend <- legend[grepl("^\\S+ - .+$", legend)]
    name_only <- legacy_codes == legacy_names
    legend_codes <- sub(" - .*$", "", legend)
    labels <- sub("^\\S+ - ", "", legend)[match(tolower(legacy_codes[name_only]), tolower(legend_codes))]
    legacy_names[name_only] <- paste(legacy_codes[name_only],
      ifelse(is.na(labels), legacy_codes[name_only], labels))
  } else if (anyDuplicated(legacy_codes) && any(legacy_names == legacy_codes) &&
      all(grepl("^[A-Z][a-z]+", legacy_names))) {
    legacy_names <- paste(gsub("[^[:alnum:]-]+", "_", legacy_names), legacy_names)
  }
  definitions[legacy_header] <- paste(legacy_priority,
    ifelse(legacy_names == legacy_codes, paste(legacy_codes, legacy_codes), legacy_names))

  # Normalize compact thresholds and brackets found in legacy JUICE files.
  threshold <- "(UP|GR|GE|EQ)[[:space:]]*([+-]?[0-9]+(?:\\.[0-9]+)?)"
  definitions[!legacy_header] <- gsub(paste0("<([^<>]+)>[[:space:]]+", threshold, ">"),
    "<\\1 \\2 \\3>", definitions[!legacy_header], perl = TRUE)
  label_pattern <- "^([0-9A-Za-z])[[:space:]]+(\\S+)[[:space:]]+(.+)$"
  is_label <- grepl(label_pattern, definitions)
  header <- definitions[is_label]
  priority <- match(sub(label_pattern, "\\1", header), c(0:9, LETTERS, letters))
  codes <- sub(label_pattern, "\\2", header)
  labels <- sub(label_pattern, "\\3", header)
  formulas <- split(definitions[!is_label], factor(cumsum(is_label)[!is_label],
    levels = seq_along(codes)))
  formulas <- vapply(formulas, paste, collapse = " ", FUN.VALUE = character(1))
  tokens <- regmatches(formulas, gregexpr("<[^<>]+>|AND|OR|NOT|[(){}]|\\[|\\]", formulas))

  # Convert each <...> expression into one predicate.
  atoms <- unique(unlist(lapply(tokens, function(x) x[startsWith(x, "<")]),
    use.names = FALSE))
  terms <- trimws(substr(atoms, 2L, nchar(atoms) - 1L))
  terms <- sub("UP([0-9]{2})$", " UP \\1", terms)
  terms <- sub(" (UP|GR|GE|EQ)([+-]?[0-9][0-9.]*)$", " \\1 \\2", terms)
  terms <- sub(paste0(threshold, "$"), " \\1 \\2", terms, perl = TRUE)
  terms <- gsub("[[:space:]]+", " ", terms)

  # A condition can refer to taxa or species groups and include exclusions.
  read_term <- function(text) {
    pieces <- strsplit(text, "[[:space:]]+EXCEPT[[:space:]]+")[[1]]
    sets <- lapply(strsplit(pieces, "|", fixed = TRUE), trimws)
    items <- sets[[1]]
    kind <- if (grepl(prefix, items[1])) sub("^([^ ]+) .*", "\\1", items[1]) else "species"
    if (items[1] %in% c("#T$", "#$$")) kind <- items[1]
    name <- sub(prefix, "", items[1])

    expand <- function(x) {
      key <- sub(prefix, "", x)
      grouped <- grepl(prefix, x) | (startsWith(key, "+") & key %in% names(groups))
      unique(c(key[!grouped], unlist(groups[key[grouped]], use.names = FALSE)))
    }
    excluded <- expand(unlist(sets[-1], use.names = FALSE))
    taxa <- if (kind %in% c("#T$", "#$$")) character() else expand(items)
    if (kind == "species" && length(items) > 1L) kind <- "#TC"
    list(kind = kind, taxa = setdiff(taxa, excluded), excluded = excluded,
      group = name, label = text)
  }

  # Read UP/GR (>), GE (>=), EQ (==) and relative thresholds such as $xx. -> split to parts
  numeric_pattern <- "^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)$"
  predicates <- lapply(terms, function(text) {
    parts <- regmatches(text, regexec("^(.*?) (UP|GR|GE|EQ) (.+)$", text, perl = TRUE))[[1]]

    if (startsWith(text, "$$N ") || startsWith(text, "$$C ")) {
      value <- parts[4]
      if (startsWith(text, "$$N ")) value <- as.numeric(value)
      return(list(field = substring(parts[2], 5L), op = parts[3], value = value))
    }

    if (!length(parts)) {
      left <- read_term(text)
      if (left$kind %in% c("##D", "##C", "##Q")) {
        set <- if (grepl("^\\+[0-9]{2} ", left$group)) substr(left$group, 1, 3) else ""
        sets <- ifelse(grepl("^\\+[0-9]{2} ", discriminating),
          substr(discriminating, 1, 3), "")
        peers <- setdiff(discriminating[sets == set], left$group)
        return(list(left = left, op = "GR", peers = peers))
      }
      minimum <- if (left$kind == "###") {
        if (length(left$taxa) == 2L) 2L else ceiling(length(left$taxa) / 2)
      } else as.integer(substring(left$kind, 2L))
      return(list(left = left, op = "GE", value = minimum))
    }

    left <- read_term(parts[2])
    right <- parts[4]
    if (grepl(numeric_pattern, right, perl = TRUE))
      return(list(left = left, op = parts[3], value = as.numeric(right)))
    if (startsWith(right, "$") && grepl(numeric_pattern, substring(right, 2L), perl = TRUE))
      return(list(left = left, op = parts[3], relative = as.numeric(substring(right, 2L))))

    right <- read_term(right)
    if (right$kind %in% c("#T$", "#$$")) {
      right$excluded <- union(right$excluded, left$taxa)
      right$label <- paste(right$label, left$label, sep = "\r")
    }
    list(left = left, op = parts[3], right = right)
  })

  # Rules combining
  # Build R calls directly so deeply nested ESy rules fit without parsing R text.
  # ESy 'A NOT B' means 'A & !B' and has the same precedence as AND.
  precedence <- c(OR = 1L, AND = 2L, NOT = 2L, "(" = 0L)
  expressions <- lapply(tokens, function(terms) {
    operators <- character()
    operands <- list()
    reduce <- function() {
      last <- length(operators)
      op <- operators[last]
      operators <<- operators[-last]
      n <- length(operands)
      right <- operands[[n]]
      if (op == "NOT") right <- call("!", right)
      operands[[n - 1L]] <<- call(if (op == "OR") "|" else "&", operands[[n - 1L]], right)
      operands[[n]] <<- NULL
    }
    for (token in terms) {
      if (startsWith(token, "<")) {
        operands[[length(operands) + 1L]] <- call("[[", quote(predicates), match(token, atoms))
      } else if (token %in% c("(", "[", "{")) {
        operators <- c(operators, "(")
      } else if (token %in% c(")", "]", "}")) {
        while (length(operators) && tail(operators, 1L) != "(") reduce()
        operators <- head(operators, -1L)
      } else {
        while (length(operators) && precedence[tail(operators, 1L)] >= precedence[token]) reduce()
        operators <- c(operators, token)
      }
    }
    while (length(operators)) reduce()
    operands[[1L]]
  })

  # What taxons to be taken from Rveg Database
  required_taxa <- unique(unlist(lapply(predicates, function(p) {
    c(p$left$taxa, p$right$taxa, unlist(groups[p$peers], use.names = FALSE))
  }), use.names = FALSE))
  needs_all_taxa <- any(vapply(predicates, function(p) {
    !is.null(p$relative) || any(c(p$left$kind, p$right$kind) %in% c("#T$", "#$$"))
  }, logical(1)))
  header_fields <- unique(unlist(lapply(predicates, `[[`, "field"), use.names = FALSE))

  ## Section 4 - Similarity (FPFI)
  profile <- section(4, "Similarity")
  # Some older files append a numbered frequency table outside section 4.
  profile_start <- grep("^Frequency table[[:space:]]+-{3,}", trimws(tail_lines))
  profile_end <- grep("^Frequency table - END", trimws(tail_lines))
  if (!length(profile) && length(profile_start) == 1L && length(profile_end) == 1L) {
    legacy_profile <- trimws(tail_lines[seq.int(profile_start, profile_end)])
    profile_groups <- grep("^Group[0-9]+ name:[[:space:]]*$", legacy_profile)
    numbers <- suppressWarnings(as.integer(sub("^.*-", "", codes)))
    group_numbers <- as.integer(sub("^Group([0-9]+) name:.*$", "\\1", legacy_profile[profile_groups]))
    if (identical(numbers, seq_along(codes)) && identical(group_numbers, numbers)) {
      legacy_profile[profile_groups] <- paste0("Group", numbers, " name: ", codes)
      profile <- legacy_profile[nzchar(legacy_profile)]
    }
  }
  similarity <- NULL
  if (length(profile)) {
    # Read the total number of plots, the plot count per association and the taxon frequencies.
    last_number <- function(x) as.numeric(sub("^.*[[:space:]]", "", x))
    total <- last_number(profile[2])
    n_groups <- as.integer(last_number(profile[3]))
    start <- seq.int(4L, 2L + 2L * n_groups, by = 2L)
    profile_codes <- sub("\\s.*$", "", trimws(sub("^Group[0-9]+ name:", "", profile[start])))
    group_n <- last_number(profile[start + 1L])

    payload <- profile[seq.int(5L + 2L * n_groups, length(profile) - 1L)]
    payload <- payload[!grepl("^-{3,}$", payload)]
    rows <- matrix(payload, nrow = n_groups + 2L)
    profile_taxa <- rows[1, ]
    counts <- t(matrix(last_number(rows[-c(1, 2), , drop = FALSE]), nrow = n_groups))

    # For repeated taxa, keep the highest frequency recorded in each unit.
    for (name in unique(profile_taxa[duplicated(profile_taxa)])) {
      same <- which(profile_taxa == name)
      counts[same[1], ] <- apply(counts[same, , drop = FALSE], 2, max)
    }
    keep <- !duplicated(profile_taxa)
    counts <- counts[keep, , drop = FALSE]
    profile_taxa <- profile_taxa[keep]

    taxon_n <- rowSums(counts)
    phi <- (total * counts - outer(taxon_n, group_n)) /
      sqrt(outer(taxon_n * (total - taxon_n), group_n * (total - group_n)))
    phi[!is.finite(phi) | phi < 0] <- 0
    denom_count <- colSums(counts)
    denom_phi <- colSums(phi)
    denom_count[denom_count == 0] <- 1
    denom_phi[denom_phi == 0] <- 1
    weights <- 50 * (sweep(counts, 2, denom_count, "/") +
        sweep(phi, 2, denom_phi, "/"))
    similarity <- list(taxa = profile_taxa, codes = profile_codes, weights = weights)
  }

  list(
    associations = data.frame(code = codes, name = labels, priority = priority),
    formulas = expressions, predicates = predicates,
    taxa = required_taxa, groups = groups, mappings = mappings,
    header_fields = header_fields, needs_all_taxa = needs_all_taxa,
    similarity = similarity, source = basename(file)
  )
}

# Combine parsed ESy sections for a joint evaluation of the rules.
rv_merge_esy_rules <- function(definitions) {
  # Offsets keep each formula connected to its original predicates.
  shift <- function(expr, offset) {
    if (identical(expr[[1L]], quote(`[[`))) {
      expr[[3L]] <- expr[[3L]] + offset
      return(expr)
    }
    as.call(c(list(expr[[1L]]), lapply(as.list(expr)[-1L], shift, offset = offset)))
  }
  associations <- profiles <- formulas <- predicates <- groups <- list()
  for (i in seq_along(definitions)) {
    rule <- definitions[[i]]
    units <- rule$associations
    units$original_code <- units$code
    units$code <- paste(i, units$code, sep = "/")[seq_len(nrow(units))]
    units$rule_source <- rep(rule$source, nrow(units))
    associations[[i]] <- units
    formulas <- c(formulas, lapply(rule$formulas, shift, offset = length(predicates)))

    # Qualify group names and codes by file, including competing groups.
    group_names <- names(rule$groups)
    keys <- setNames(paste(i, group_names, sep = "/")[seq_along(group_names)], group_names)
    groups <- c(groups, setNames(rule$groups, unname(keys)))
    predicates <- c(predicates, lapply(rule$predicates, function(p) {
      if (length(p$peers)) p$peers <- unname(keys[p$peers])
      p
    }))
    if (!is.null(rule$similarity)) {
      profile <- rule$similarity
      profile$codes <- paste(i, profile$codes, sep = "/")
      profiles[[length(profiles) + 1L]] <- profile
    }
  }

  # Align profile rows while keeping the weights from each reference dataset.
  similarity <- NULL
  if (length(profiles)) {
    taxa <- unique(unlist(lapply(profiles, `[[`, "taxa"), use.names = FALSE))
    weights <- lapply(profiles, function(profile) {
      aligned <- matrix(0, length(taxa), length(profile$codes))
      aligned[match(profile$taxa, taxa), ] <- profile$weights
      aligned
    })
    similarity <- list(taxa = taxa, codes = unlist(lapply(profiles, `[[`, "codes"), use.names = FALSE),
      weights = do.call(cbind, weights))
  }

  list(associations = do.call(rbind, associations), formulas = formulas, predicates = predicates,
    taxa = unique(unlist(lapply(definitions, `[[`, "taxa"), use.names = FALSE)),
    groups = groups, mappings = unique(do.call(rbind, lapply(definitions, `[[`, "mappings"))),
    header_fields = unique(unlist(lapply(definitions, `[[`, "header_fields"), use.names = FALSE)),
    needs_all_taxa = any(vapply(definitions, `[[`, logical(1L), "needs_all_taxa")),
    similarity = similarity,
    source = paste(vapply(definitions, `[[`, character(1L), "source"), collapse = " + "))
}

# Evaluate the formal ESy definitions for each plot.
rv_classify_esy <- function(data, rules, top_n = 3L, exclude_layers = c("7", "8"),
  min_fpfi = 0, synonym_file = NULL, include_fpfi_taxa = FALSE) {
  ids <- data$plot_id
  species <- data$species
  species <- species[!species$layer %in% exclude_layers, , drop = FALSE]

  # Keep unmatched taxa when the rules use all recorded taxa.
  if (rules$needs_all_taxa) {
    use <- is.na(species$taxon_name)
    species$taxon_name[use] <- species$source_taxon_name[use]
  }
  species <- species[!is.na(species$taxon_name) & species$cover_pct > 0, , drop = FALSE]
  taxa <- rules$taxa
  if (rules$needs_all_taxa) taxa <- union(taxa, species$taxon_name)

  ## evaluate individual predicates
  # Each predicate returns one logical value per plot.
  hits_by_batch <- list()
  uncertain_ids <- character()
  batch_size <- max(1L, min(4096L, floor(4e6 / max(1L, length(taxa)))))
  formal_ids <- ids[ids %in% species$plot_id]
  batches <- split(formal_ids, ceiling(seq_along(formal_ids) / batch_size))

  for (batch in batches) {
    records <- species[species$plot_id %in% batch, , drop = FALSE]
    cover <- rv_cover_matrix(records, batch, taxa, rules$mappings)

    # Calculate the cover or species count requested by the ESy term.
    term_value <- function(term) {
      selected <- term$taxa
      if (term$kind %in% c("#T$", "#$$")) selected <- setdiff(colnames(cover), term$excluded)
      values <- cover[, selected, drop = FALSE]
      kind <- term$kind
      if (kind %in% c("#SC", "#$$"))
        return(if (length(selected)) do.call(pmax, as.data.frame(values)) else rep(0, length(batch)))
      if (kind %in% c("#TC", "#T$"))
        return(round(-100 * expm1(rowSums(log1p(-values / 100))), 10L))
      if (kind == "##Q") return(round(rowSums(sqrt(values)), 5L))
      if (kind %in% c("##C", "species")) return(rowSums(values))
      rowSums(values > 0)
    }

    predicates <- lapply(rules$predicates, function(p) {
      if (!is.null(p$field)) {
        left <- data$headers[[p$field]][match(batch, ids)]
        if (is.numeric(p$value)) {
          left <- as.numeric(left)
        } else {
          left <- trimws(left)
          left[left == ""] <- NA_character_
        }
        right <- p$value
      } else {
        left <- term_value(p$left)
        right <- p$value
        if (!is.null(p$right)) right <- term_value(p$right)
        if (!is.null(p$relative))
          right <- p$relative / 100 * term_value(list(kind = "#T$", excluded = character()))
        if (!is.null(p$peers)) {
          peer_values <- lapply(p$peers, function(name)
            term_value(list(kind = p$left$kind, taxa = rules$groups[[name]])))
          right <- if (length(peer_values)) do.call(pmax, peer_values) else 0
        }
      }
      switch(p$op, UP = left > right, GR = left > right,
        GE = left >= right, EQ = left == right)
    })

    ## evaluate definitions and apply priorities
    truth <- vapply(rules$formulas,
      function(expr) eval(expr, envir = list(predicates = predicates)),
      logical(length(batch)))
    if (is.null(dim(truth))) truth <- matrix(truth, ncol = length(rules$formulas))
    hits <- which(truth, arr.ind = TRUE)
    priority <- rules$associations$priority

    best_priority <- integer(length(batch))
    if (nrow(hits)) {
      best <- tapply(priority[hits[, 2]], hits[, 1], max)
      best_priority[as.integer(names(best))] <- best
    }
    unknown <- which(is.na(truth), arr.ind = TRUE)
    uncertain <- unique(unknown[priority[unknown[, 2]] >=
        best_priority[unknown[, 1]], 1])
    uncertain_ids <- c(uncertain_ids, batch[uncertain])

    selected <- hits[priority[hits[, 2]] == best_priority[hits[, 1]] &
        !hits[, 1] %in% uncertain, , drop = FALSE]
    hits_by_batch[[length(hits_by_batch) + 1L]] <- data.frame(
      plot_id = batch[selected[, 1]],
      association_code = rules$associations$code[selected[, 2]],
      association_name = rules$associations$name[selected[, 2]]
    )
  }

  all_matches <- if (length(hits_by_batch)) do.call(rbind, hits_by_batch) else
    data.frame(plot_id = character(), association_code = character(),
      association_name = character())
  all_matches <- unique(all_matches)
  all_matches$rule_source <- rep(rules$source, nrow(all_matches))

  ## summarize formal matches ---
  n_matches <- tabulate(match(all_matches$plot_id, ids), length(ids))
  n_matches[ids %in% uncertain_ids] <- NA_integer_
  single <- match(ids, all_matches$plot_id)
  single[n_matches != 1L | is.na(n_matches)] <- NA_integer_
  summary <- data.frame(
    plot_id = ids,
    esy_n_matches = n_matches,
    esy_code = all_matches$association_code[single],
    esy_name = all_matches$association_name[single]
  )

  ## FPFI (optional Similarity section)
  fpfi_ranked <- data.frame(
    plot_id = character(), rank = integer(), association_code = character(),
    association_name = character(), fpfi = numeric(), n_used_taxa = integer()
  )
  fpfi_taxon_map <- NULL

  if (!is.null(rules$similarity)) {
    profile <- rules$similarity
    valid_ids <- setdiff(ids, uncertain_ids)
    fpfi_species <- data$species
    fpfi_species <- fpfi_species[fpfi_species$plot_id %in% valid_ids &
        !fpfi_species$layer %in% exclude_layers &
        fpfi_species$cover_pct > 0, , drop = FALSE]
    # Match the original names again because the profile may use different names.
    original_names <- unique(fpfi_species$source_taxon_name)
    profile_names <- rv_match_taxa(original_names, profile$taxa, synonym_file)
    alternative <- profile_names[match(fpfi_species$source_taxon_name, original_names)]
    replace <- !fpfi_species$taxon_name %in% profile$taxa & !is.na(alternative)
    fpfi_species$taxon_name[replace] <- alternative[replace]
    named <- !is.na(fpfi_species$taxon_name)
    fpfi_records <- fpfi_species[named, , drop = FALSE]
    fpfi_names <- rv_esy_aggregate_names(fpfi_records, rules$mappings)

    if (include_fpfi_taxa) {
      # Record the profile names used after applying the layer-specific ESy rules.
      targets <- vector("list", nrow(fpfi_species))
      targets[named] <- lapply(fpfi_names, intersect, y = profile$taxa)
      rows <- rep(seq_len(nrow(fpfi_species)), lengths(targets))
      fpfi_taxon_map <- unique(data.frame(taxon_id = fpfi_species$taxon_id[rows],
        taxon_name = fpfi_species$source_taxon_name[rows],
        matched_name = as.character(unlist(targets, use.names = FALSE))))
      evaluated <- unique(data.frame(taxon_id = fpfi_species$taxon_id,
        taxon_name = fpfi_species$source_taxon_name))
      key <- paste(fpfi_taxon_map$taxon_id, fpfi_taxon_map$taxon_name, sep = "\r")
      unmatched <- evaluated[!paste(evaluated$taxon_id, evaluated$taxon_name, sep = "\r") %in% key,
        , drop = FALSE]
      unmatched$matched_name <- rep(NA_character_, nrow(unmatched))
      fpfi_taxon_map <- rbind(fpfi_taxon_map, unmatched)
    }

    present <- rv_cover_matrix(fpfi_records, valid_ids, profile$taxa, names_by_row = fpfi_names) > 0
    formal <- split(all_matches$association_code, all_matches$plot_id)
    by_code <- setNames(rules$associations$name, rules$associations$code)

    ranked <- lapply(seq_along(valid_ids), function(i) {
      taxon_rows <- which(present[i, ])
      if (!length(taxon_rows)) return(NULL)
      score <- colSums(profile$weights[taxon_rows, , drop = FALSE])
      possible <- if (valid_ids[i] %in% names(formal)) {
        profile$codes %in% formal[[valid_ids[i]]]
      } else rep(TRUE, length(profile$codes))
      choices <- which(possible & score > 0 & score >= min_fpfi)
      if (!length(choices)) return(NULL)
      best <- head(choices[order(-score[choices], profile$codes[choices])], top_n)
      if (!length(best)) return(NULL)
      data.frame(
        plot_id = valid_ids[i], rank = seq_along(best),
        association_code = profile$codes[best],
        association_name = unname(by_code[profile$codes[best]]),
        fpfi = unname(score[best]), n_used_taxa = length(taxon_rows)
      )
    })
    fpfi_ranked <- do.call(rbind, ranked)
    if (is.null(fpfi_ranked)) fpfi_ranked <- data.frame(
      plot_id = character(), rank = integer(), association_code = character(),
      association_name = character(), fpfi = numeric(), n_used_taxa = integer())
  }

  result <- list(summary = summary, all_matches = all_matches, fpfi_ranked = fpfi_ranked)
  result$fpfi_taxon_map <- fpfi_taxon_map
  result
}

### Veg Key
# Combine cover across layers to keep one record per taxon and plot.
# Classification and model creation use the same calculation.
rv_vegkey_records <- function(species, plots, exclude_layers) {
  species <- species[species$plot_id %in% plots &
      !is.na(species$taxon_name) &
      !species$layer %in% exclude_layers &
      (is.na(species$cover_pct) | species$cover_pct > 0),
    c("plot_id", "taxon_name", "cover_pct"), drop = FALSE]
  if (!nrow(species)) return(species)
  taxa <- unique(species$taxon_name)
  key <- match(species$plot_id, plots) +
    length(plots) * (match(species$taxon_name, taxa) - 1L)
  merged <- rowsum(log1p(-species$cover_pct / 100), key, reorder = FALSE)
  out <- species[match(as.integer(rownames(merged)), key), , drop = FALSE]
  out$cover_pct <- round(-100 * expm1(merged[, 1]), 10L)
  rownames(out) <- NULL
  out
}

# Multiply the constancies of recorded taxa by summing their logarithms.
rv_classify_vegkey <- function(data, model, top_n = 3L, candidate_units = NULL,
  use_dominance = TRUE) {
  ids <- data$plot_id
  units <- if (is.null(candidate_units)) model$units$code else unique(candidate_units)
  unit_info <- model$units[match(units, model$units$code), , drop = FALSE]
  presence <- model$presence[, units, drop = FALSE]
  occurrence <- log(pmax(presence, model$min_constancy))
  dominant <- if (use_dominance && !is.null(model$dominant)) {
    log(pmax(model$dominant[, units, drop = FALSE], model$min_constancy))
  } else NULL

  ## select plot taxa
  species <- rv_vegkey_records(data$species, ids, model$exclude_layers)
  species <- species[species$taxon_name %in% rownames(presence)[rowSums(presence) > 0], , drop = FALSE]
  by_plot <- split(seq_len(nrow(species)), species$plot_id)

  ## score and rank vegetation units
  ranked <- lapply(ids, function(id) {
    records <- species[by_plot[[id]], , drop = FALSE]
    if (!nrow(records)) return(NULL)
    weights <- occurrence[records$taxon_name, , drop = FALSE]
    high <- !is.na(records$cover_pct) & records$cover_pct > 25
    if (!is.null(dominant))
      weights[high, ] <- dominant[records$taxon_name[high], , drop = FALSE]

    score <- colSums(weights)
    score <- exp(score - max(score))
    score <- 100 * score / sum(score)
    best <- head(order(-score, units), top_n)
    if (!length(best)) return(NULL)
    data.frame(
      plot_id = id, rank = seq_along(best), unit_code = units[best],
      unit_name = unit_info$name[best], relative_score = unname(score[best]),
      n_training = unit_info$n_plots[best]
    )
  })
  ranked <- do.call(rbind, ranked)
  if (is.null(ranked)) ranked <- data.frame(
    plot_id = character(), rank = integer(), unit_code = character(),
    unit_name = character(), relative_score = numeric(), n_training = integer())

  ## VegKey - summarize scores
  best <- match(ids, ranked$plot_id)
  summary <- data.frame(
    plot_id = ids,
    n_used_taxa = unname(lengths(by_plot[ids])),
    vegkey_code = ranked$unit_code[best],
    vegkey_name = ranked$unit_name[best],
    relative_score = ranked$relative_score[best]
  )
  list(summary = summary, vegkey_ranked = ranked)
}


#---- Package startup ----

# on attach message
.onAttach <- function(libname, pkgname) {
  # Get the current version dynamically
  pkg_version <- utils::packageVersion("Rveg")
  packageStartupMessage(rv_col(paste0("Welcome to Rveg (version ", pkg_version, ")!"),"ok"))
  packageStartupMessage("User guide: `vignette('Rveg')`, Updates: `news(package = 'Rveg')`")
}
