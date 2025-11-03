#' Creates tables from all releves var 1 for classic, var 2 for batch
#' @keywords internal
#' @noRd
createTABLE <- function(SpLIST, RelNew, DATA2, variation = 1) {
  stopifnot(ncol(SpLIST) >= 2)
  # base species list and a stable order index (replaces use of rownames)
  base <- data.frame(
    ShortName = SpLIST$ShortName,
    .ord = seq_len(nrow(SpLIST)),
    stringsAsFactors = FALSE
  )

  # bring in existing REL columns (if any); assume first col is ShortName
  if (!is.null(DATA2) && ncol(DATA2) > 1L) {
    D2 <- DATA2[, c("ShortName", setdiff(names(DATA2), "ShortName")), drop = FALSE]
    base <- merge(base, D2, by = "ShortName", all.x = TRUE, sort = FALSE)

    rel_idx <- which(!(names(base) %in% c("ShortName", ".ord")))
    if (length(rel_idx)) {
      names(base)[rel_idx] <- paste0("X", seq_along(rel_idx))
    }
  } else {
    rel_idx <- integer(0)  # no existing relevés
  }

  if (variation == 1) {
    # RelNew is a data.frame: ShortName + value (single new relevé)
    stopifnot(is.data.frame(RelNew), all(c("ShortName", "value") %in% names(RelNew)))

    # align values by ShortName
    vals <- RelNew$value[match(base$ShortName, RelNew$ShortName)]
    vals[is.na(vals)] <- 0

    out <- base
    next_col <- paste0("X", length(rel_idx) + 1L)
    out[[next_col]] <- vals

    rel_cols <- setdiff(names(out), c("ShortName", ".ord"))
    for (nm in rel_cols) {
      if (is.character(out[[nm]])) {
        #tmp <- suppressWarnings(as.numeric(out[[nm]]))
        tmp <- out[[nm]]
        tmp[is.na(tmp)] <- 0
        out[[nm]] <- tmp
      } else {
        out[[nm]][is.na(out[[nm]])] <- 0
      }
    }

    # drop species with all zeros across all relevé columns
    #nz <- rowSums(out[, setdiff(names(out), c("ShortName", ".ord"))] != 0, na.rm = TRUE) > 0
    nz <- rowSums(out[, rel_cols, drop = FALSE] != 0) > 0
    out <- out[nz, , drop = FALSE]
    # restore original checklist order (by .ord), then drop helper col
    out <- out[order(out$.ord), , drop = FALSE]
    out$.ord <- NULL

    # make types uniform like your original
    out[] <- lapply(out, as.character)
    return(out)
  }

  if (variation == 2) {
    # RelNew is a list: r1, r2, ... each is data.frame ShortName + value
    stopifnot(is.list(RelNew), length(RelNew) >= 1L)

    # build a data.frame with one column per new relevé, aligned by ShortName
    newcols <- lapply(RelNew, function(df) {
      stopifnot(is.data.frame(df), all(c("ShortName", "value") %in% names(df)))
      v <- df$value[match(base$ShortName, df$ShortName)]
      v[is.na(v)] <- 0
      v
    })
    new_df <- as.data.frame(newcols, stringsAsFactors = FALSE)
    names(new_df) <- paste0("new", seq_along(newcols))  # temporary names

    out <- cbind(base, new_df)
    out[is.na(out)] <- 0
    nz <- rowSums(out[, setdiff(names(out), c("ShortName", ".ord"))] != 0, na.rm = TRUE) > 0
    out <- out[nz, , drop = FALSE]
    out <- out[order(out$.ord), , drop = FALSE]
    out$.ord <- NULL

    # optional numeric names to match your later renaming logic
    # k_exist = existing columns excluding ShortName
    k_exist <- max(0L, ncol(DATA2) - 1L)
    colnames(out)[-1] <- as.character(seq_len(k_exist + length(newcols)))

    out[] <- lapply(out, as.character)
    return(out)
  }

  stop("variation must be 1 or 2")
}

#' Make SP list from checklist & metadata extra species
#' @keywords internal
#' @noRd
makeSpLIST <- function(checklist, metadata, layers = c("3","2","1","J","0")) {
  Sp <- read.delim(checklist, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)

  # Require canonical column names
  stopifnot(all(c("ShortName", "FullName") %in% names(Sp)))

  # Keep only what we need; ignore any 'Number' (or anything else)
  Sp <- Sp[, c("ShortName", "FullName")]

  # Drop blank/NA short names
  Sp <- Sp[!is.na(Sp$ShortName) & nzchar(Sp$ShortName), , drop = FALSE]

  # Build layered keys: ShortName_<layer>
  out <- do.call(
    rbind,
    lapply(layers, function(L) {
      data.frame(
        ShortName = paste0(Sp$ShortName, "_", L),
        FullName  = Sp$FullName,
        stringsAsFactors = FALSE
      )
    })
  )


  # parse "LONG::SHORT"
  md <- metadata$extra_spec
  items <- trimws(strsplit(md, "|", fixed = TRUE)[[1]])
  items <- items[nzchar(items)]
  parts <- strsplit(items, "::", fixed = TRUE)

  long  <- vapply(parts, function(p) trimws(p[1]), "", USE.NAMES = FALSE)
  short <- vapply(parts, function(p) if (length(p) >= 2) trimws(p[2]) else NA_character_,
                  "", USE.NAMES = FALSE)

  # expand to ShortName_base + "_" + layer, replicate FullName
  meta_out <- do.call(
    rbind,
    lapply(seq_along(long), function(i)
      data.frame(
        ShortName = paste0(short[i], "_", layers),
        FullName  = rep(long[i], length(layers)),
        stringsAsFactors = FALSE
      )
    )
  )

  out <- rbind(out, meta_out)

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


#' Dialogue loop for writing releves, var 1 for classic, var 2 for batch
#' @keywords internal
#' @noRd
ReleveDialogue <- function(SpLIST,SpLIST1,RelNew,metadata,
                           SAVE=NULL,HeaderDATA2=NULL,variation = 1) {
  out <- list()
  if (variation == 1) {

    while(TRUE) {

      m <- toupper(readline("AddNewLayer?(Y/N) "))

      if (m == "Y") {
        while (TRUE) {
          most <- toupper(readline("Select Layer (3,2,1,J,0) "))
          if (most %in% c("3","2","1","J","0")) {
            break
          }
        } # Select layer
        while (TRUE) {
          oo <- toupper(readline("P - percentage, BB - Braun B. scale, CS - custom scale "))

          if (oo %in% c("P","BB","B","CS")) {
            break
          }
        } # Select scale
        while (TRUE) {
          m <- toupper(readline("AddSpecies?(GenuSpe/N) "))

          if (nchar(m) == 7) {
            n <- m

            if (oo == "P") {
              while (TRUE) {
                o <- suppressWarnings(as.numeric(readline("Abundance?(%) ")))
                if (!is.na(o) && o >= 0 && o <= 100) {break}
              }
            }
            if (oo == "CS") {
              o <-
                as.character(readline("Abundance? "))
            }
            if (oo == "BB" | oo == "B") {
              while (TRUE) {
                o <- toupper(readline("Abundance?(0,R,+,1,2,M,A,B,3,4,5) "))
                if (o %in% c("R", "+", "0", "1", "2", "M", "A", "B", "3", "4", "5")) {
                  break
                }
              }
              o <- bb_to_pct(o)
            }

            nana <- paste(n, most, sep = "_") # creating the species + layer
            specie_check <- SpLIST[SpLIST$ShortName==nana,] # double check

            if (length(specie_check$FullName)==0) {
              message("Species not found in the checklist")
            } else {
              print(specie_check$FullName)
              }


            while (TRUE) {
              tt <- toupper(readline("CorrectName?(Y/F(search for name)) ")) # double check
              if (tt == "Y" && !is.na(specie_check$FullName[1]) | tt == "F") {
                break
              }
            }

            if (tt == "F") {
              while (TRUE) {
                k <- readline("SpeciesFirst3letters?(eg.Che) ") # search correct name in original list
                if (nchar(k) >= 3) {
                  k <- paste0(toupper(substr(k, 1, 1)), tolower(substr(k, 2, 3)))
                  searchlist <- SpLIST1[grep(paste0("^", k), SpLIST1$FullName),]
                  print(searchlist[order(searchlist$FullName), ])
                  while (TRUE) {
                    while (TRUE) {
                      s <- toupper(readline("SpeciesName?(GenuSpe / search / insert) "))
                      if (nchar(s) == 7) {
                        nana <- paste(s, most, sep = "_")
                        specie_check <- SpLIST[SpLIST$ShortName == nana, ]
                        print(specie_check$FullName)
                        break
                      }
                      if (s == "SEARCH") {
                        break
                      }

                      if (s == "INSERT") {
                        while(TRUE) {
                          sn <- (readline("Full species name?: "))
                          snc <- toupper(readline(paste0(sn, ": correct?(y/f) ")))
                          if (snc == "Y") {
                            while(TRUE) {
                              s <- toupper(readline("ShortCode?: "))
                              if (CheckShortname(s,SpLIST1,metadata)) {
                                okay <- toupper(readline((paste0(sn,"; ",s,"? (y/f) "))))
                                if (okay == "Y") {
                                  # pridat do newrel
                                  RelNewNs <- data.frame(ShortName = paste0(s,"_",c("J",0:3)),
                                                         value = 0)
                                  RelNew <- rbind(RelNew,RelNewNs)

                                  SpLISTNs <- data.frame(ShortName = paste0(s,"_",c("J",0:3)),
                                                         FullName = sn)

                                  SpLIST <- rbind(SpLIST,SpLISTNs)

                                  SpLIST1Ns <- data.frame(ShortName = s,
                                                         FullName = sn)

                                  SpLIST1 <- rbind(SpLIST1,SpLIST1Ns)

                                  # pridat do metadata
                                  metadata$extra_spec <- paste0(metadata$extra_spec,paste0(sn,"::",s,"|"))
                                  break
                                }
                              }
                              }
                            break
                          }
                        }
                        break
                      } # Importing a new species code
                    }

                    if (nchar(s) == 7){
                      tt <- toupper(readline("CorrectSpecies?(Y/N)  "))
                      if ((tt == "Y") & !is.na(specie_check$FullName[1])) {
                        break
                      }
                    }
                    break
                  }
                  if(nchar(s) == 7){
                    break
                  }
                }
              }

              nanas <- paste(s, most, sep = "_")

              RelNew[which(RelNew$ShortName == substr(nanas, 1, 9)), ][, 2] <- o
              print(RelNew[(RelNew[, 2] > 0), ])
            } else {
              RelNew[which(RelNew$ShortName == substr(nana, 1, 9)), ][, 2] <- o
              print(RelNew[(RelNew[, 2] > 0), ])
            }
          } else if (m == "N") {
            break
          }

        } # Add species
      } else if (m == "N") {
        message("Species_richness")
        print(nrow(RelNew[(RelNew[, 2] > 0), ]))
        break
      }

    }
    out$RelNew <- RelNew; out$SpLIST <- SpLIST; out$SpLIST1 <- SpLIST1; out$meta <- metadata
    return(out)
  }


  if (variation == 2) {

    while (TRUE) {
      m <- suppressWarnings(as.numeric(readline("How many releves? ")))
      if (!is.na(m) & m>0) {

        Rels <- list()
        DATAtemp <- read_db(SAVE)$RelDATA
        for (i in 1:m) {
          Rels[[paste0("r",i)]] <- data.frame(ShortName = SpLIST[, 1],value = 0)
        } # new releves

        break
      }

    } ## how many releves

    while (TRUE) {
      oo <- toupper(readline("P - percentage, BB - Braun B. scale, CS - custom scale "))

      if (oo == "P" | oo == "BB" | oo == "B" | oo == "CS") {
        break
      }
    } ## Scale

    while (TRUE) {
      n <-toupper(readline("Add new species (GenuSpe)/N?  "))
      if (nchar(n) == 7) {

        while (TRUE){
          l <- toupper(readline("Select Layer (3,2,1,J,0) " ))
          if (l %in% c("1","2","3","J","0")) {
            break
          }

        } ## what layer

        nana <- paste(n, l, sep = "_") # creating the species name
        specie_check <- SpLIST[SpLIST[, "ShortName"] == substr(nana, 1, 9), ] # double check
        print(specie_check[2])

        while (TRUE) {
          tt <- toupper(readline("CorrectName?(Y/F(search for name)) ")) # double check
          if (tt == "Y" && !is.na(specie_check[1, 2])) {
            break
          } else if (tt == "F") {
            while (TRUE) {
              k <- readline("SpeciesFirst3letters?(eg.Che) ") # search correct name in original list
              if (nchar(k) >= 3) {
                k <- paste0(toupper(substr(k, 1, 1)), tolower(substr(k, 2, 3)))
                searchlist <- SpLIST1[grep(paste0("^", k), SpLIST1[, 2]), ]
                print(searchlist[order(searchlist$FullName), ])
                while (TRUE) {
                  while (TRUE) {
                    s <- toupper(readline("SpeciesName?(GenuSpe) "))
                    if (nchar(s) == 7) {
                      nana <- paste(s, l, sep = "_")
                      specie_check <- SpLIST[SpLIST[, "ShortName"] == substr(nana, 1, 9), ]
                      print(specie_check[2])
                      break
                    }
                  }

                  tt <- toupper(readline("CorrectSpecies?(Y/N)  "))
                  if ((tt == "Y") & !is.na(specie_check[1, 2])) {
                    break
                  }
                }
                break
              }
            }
          }
        } # correct name

        RelFake <- data.frame(ShortName = SpLIST[, 1])
        for (i in 1:m) {
          RelFake[[paste0("r",i)]] <- 0
        }

        for (i in 1:m) {
          message(paste0("Relev\u00e9 ",i))

          if (oo == "P") {
            while (TRUE) {
              o <- suppressWarnings(as.numeric(readline("Abundance?(%) ")))
              if (!is.na(o) && o >= 0 && o <= 100) {break}
            }
          } else if (oo == "CS") {
            o <- as.character(readline("Abundance? "))
          } else if (oo == "BB" | oo == "B") {
            while (TRUE) {
              o <- toupper(readline("Abundance?(0,R,+,1,2,M,A,B,3,4,5) "))
              if (o %in% c("R", "+", "0", "1", "2", "M", "A", "B", "3", "4", "5")) {
                break
              }
            }
            o <- bb_to_pct(o)
          }

          Rels[[paste0("r",i)]][which(Rels[[paste0("r",i)]]$ShortName == substr(nana, 1, 9)), ][, 2] <- o
          RelFake[which(RelFake$ShortName == substr(nana, 1, 9)), ][, i+1] <- o
          print(RelFake[(RelFake[,i+1] > 0), ])

        }
        DATA2 <- DATAtemp
        DATA2 <- createTABLE(SpLIST, Rels, DATA2,variation = 2)
        print(DATA2)
        colnames(DATA2)[-1] <- paste0("X",1:(length(colnames(DATA2))-1))
        write_db(REL = DATA2,SAVE = SAVE,meta = metadata)
        start = TRUE
        DATABASE = ""


      } else if (n == "N") {
        while (TRUE) {
          rs <- toupper(readline("Add headers?(Y/N) "))
          if (rs == "Y") {
            for (i in 1:m) {
              message(paste0("Relev\u00e9 ",i))

              k <- existing_k(HeaderDATA2)
              lastcol <- last_x(HeaderDATA2)
              nextcol <- paste0("X", k + 1L)
              lastcol <- last_x_col(HeaderDATA2)

              FIELD_LABELS <- schema_from_head(HeaderDATA2)

              non_id <- prompt_header_values(FIELD_LABELS, HeaderDATA2, prev_col = lastcol, skip = "ID")
              id_val <- as.character(k + 1L)

              vals <- vapply(FIELD_LABELS, function(lab) {
                if (tolower(lab) == "id") id_val else non_id[[lab]]
              }, FUN.VALUE = character(1))


              HeaderDATA2[[nextcol]] <- vals
              write_db(HEAD = HeaderDATA2, SAVE = SAVE, meta = metadata)

            }


            break
          } else if (rs == "N") {

            break
          }

        }
        break

      }

    } ## add new species


  }

}

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
    Y         = "Add new relevé (with header)",
    ADDREL    = "Add relevé body only",
    ADDHEAD   = "Add header only",
    RREL      = "Repair an existing relevé",
    RHEAD     = "Repair an existing header",
    YSP       = "Batch: add one species to many relevés",
    PRINTREL  = "Print REL table",
    PRINTHEAD = "Print HEAD table",
    REMOVEREL = "Remove a relevé (REL+HEAD) and reindex",
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
  cat(rv_col(sprintf("Relevés: %d; Headers: %d", rel_count, head_count), if (same) "ok" else "error"))
}


## helper functions / repeated steps

#' Transform Braun-Blanquet scale to percentage
#' @keywords internal
#' @noRd
bb_to_pct <- function(x) {
  switch(toupper(x),
         "R"=1, "+"=2, "1"=3, "2"=15, "M"=4, "A"=8, "B"=18, "3"=38, "4"=63, "5"=88, "0"=0, NA_real_
  )
}

#' Reindexing after rel removal
#' @keywords internal
#' @noRd
reindex_releves <- function(df, keep = "ShortName") {
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
default_header_fields <- function() {
  c("ID","DATE","SpringDATE","LOCALITY","FieldCODE","Authors",
    "PlotSize","Latitude","Longitude","Accuracy","CRS","Slope","Exposure",
    "E3","E2","E1","Ejuv","E0","Note")
}

#' Normalize labels, check for duplicates
#' @keywords internal
#' @noRd
normalize_fields <- function(x) {
  if (is.null(x)) return(NULL)
  y <- gsub("\\s+", " ", trimws(as.character(x)))
  y <- y[nzchar(y)]
  y[!duplicated(tolower(y))]
}

#' ID always first part of the header
#' @keywords internal
#' @noRd
ensure_id_first <- function(labels) {
  labs <- normalize_fields(labels)
  labs_wo_id <- labs[tolower(labs) != "id"]
  c("ID", labs_wo_id)
}

#' Extract schema (ShortName) from HEAD table.
#' @keywords internal
#' @noRd
schema_from_head <- function(head_df) {
  if (is.null(head_df) || !"ShortName" %in% names(head_df)) return(character(0))
  as.character(head_df$ShortName)
}

#' Append new header rows (never ID). Fill NA across existing X* columns.
#' @keywords internal
#' @noRd
expand_head_with_fields <- function(HeaderDATA, new_fields) {
  nf <- setdiff(normalize_fields(new_fields), schema_from_head(HeaderDATA))
  nf <- nf[tolower(nf) != "id"]
  if (!length(nf)) return(HeaderDATA)
  add <- data.frame(ShortName = nf, stringsAsFactors = FALSE)
  for (xc in setdiff(names(HeaderDATA), "ShortName")) add[[xc]] <- NA
  rbind(HeaderDATA, add)
}

#' Last X* column name or NULL.
#' @keywords internal
#' @noRd
last_x_col <- function(df) {
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

#' Prompt for all labels except ID; reuse previous if user types "RE".
#' Returns a named character vector for the provided labels.
#' @keywords internal
#' @noRd
prompt_header_values <- function(labels, HeaderDATA, prev_col, skip = "ID") {
  labs <- labels[!(tolower(labels) %in% tolower(skip))]
  get_prev <- function(lab) {
    if (is.null(prev_col) || !prev_col %in% names(HeaderDATA)) return("")
    v <- HeaderDATA[HeaderDATA$ShortName == lab, prev_col, drop = TRUE]
    if (length(v) == 0 || is.na(v) || identical(v, "NA")) "" else as.character(v)
  }
  vals <- vapply(labs, function(lab) {
    prev <- get_prev(lab)
    read_or_re(paste0(lab, "? "), prev)  # your helper; only hints if prev != ""
  }, FUN.VALUE = character(1))
  stats::setNames(vals, labs)
}


#' NEED DESCRIPTION
#' @keywords internal
#' @noRd
read_or_re <- function(prompt, default) {
  def <- if (length(default) == 0 || is.na(default) || identical(default, "NA")) "" else as.character(default)
  # only show a hint if we actually have a previous value
  if (nzchar(def)) message(paste0("RE <- ", def))
  ans <- readline(prompt)
  ans_trim <- toupper(trimws(ans))
  if (ans_trim == "RE" && nzchar(def)) def else ans
}

#' Random pseudo code to link DB files
#' @keywords internal
#' @noRd
make_id <- function() {
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
  md$db_id <- make_id()

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


#' Creates new database
#' @keywords internal
#' @noRd
create_new_db <- function(SAVE, labs, checklist, meta) {

  DATA <- data.frame(ShortName = character(), stringsAsFactors = FALSE)
  HeaderDATA <- data.frame(ShortName = labs, stringsAsFactors = FALSE)
  meta <- rv_create_metadata(checklist, meta)

  con <- file(paste0(SAVE, "HEAD.csv"), open = "wt")

  for (k in names(meta)) {
    v <- if (is.null(meta[[k]])) "" else as.character(meta[[k]])
    writeLines(paste0("# ", k, ": ", v), con, useBytes = TRUE)
  }

  utils::write.table(
    HeaderDATA, file = con, sep = ",", row.names = FALSE, col.names = TRUE,
    na = "", qmethod = "double"
  )

  close(con)

  con <- file(paste0(SAVE, "REL.csv"), open = "wt")

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
read_db <- function(SAVE) {

  lines <- readLines(paste0(SAVE,"HEAD.csv"))
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
    paste0(SAVE,"HEAD.csv"), sep = ",", header = TRUE, quote = "\"", comment.char = "#",
    stringsAsFactors = FALSE, check.names = FALSE
  )

  RelDATA <- utils::read.table(
    paste0(SAVE,"REL.csv"), sep = ",", header = TRUE, quote = "\"", comment.char = "#",
    stringsAsFactors = FALSE, check.names = FALSE
  )

  abc <- list(HeaderDATA = HeaderDATA, RelDATA = RelDATA, meta = meta)
  invisible(abc)

}

#' Save existing database
#' @keywords internal
#' @noRd
write_db <- function(REL = NULL, HEAD = NULL, SAVE = NULL, meta = NULL) {

  meta$last_change <- round(Sys.time(),"secs")

  if (!is.null(REL)) {
    con <- file(paste0(SAVE, "REL.csv"), open = "wt")

    meta$n_releves <- ncol(REL)-1
    rv_write_metadata(meta,con)

    utils::write.table(
      REL, file = con, sep = ",", row.names = FALSE, col.names = TRUE,
      na = "", qmethod = "double"
    )

    close(con)
  }

  if (!is.null(HEAD)) {
    con <- file(paste0(SAVE, "HEAD.csv"), open = "wt")

    meta$n_releves <- ncol(HEAD)-1
    rv_write_metadata(meta,con)

    utils::write.table(
      HEAD, file = con, sep = ",", row.names = FALSE, col.names = TRUE,
      na = "", qmethod = "double"
    )

    close(con)
  }

}

#' Check if shortname code already exist, if not, passes TRUE (ok)
#' @keywords internal
#' @noRd
CheckShortname <- function(code, checklist, meta) {
  if (code %in% checklist$ShortName | code %in% meta$extra_spec) {
    FALSE
  } else {
    TRUE
    }
}




# pochybne funkce?

#' @keywords internal
#' @noRd
next_x <- function(df) paste0("X", ncol(df))

#' @keywords internal
#' @noRd
last_x <- function(df) if (ncol(df) > 1) colnames(df)[ncol(df)] else NULL

#' @keywords internal
#' @noRd
existing_k <- function(df) max(0L, ncol(df) - 1L)


