#-----------------------------------#
#---- Rveg internal functions ------#
#-----------------------------------#

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

#----- releve making --------

#' Dialogue loop for writing releves, var 1 for classic, var 2 for batch
#' @keywords internal
#' @noRd
rv_releve_dialogue <- function(SpLIST,RelNew,metadata, SAVE=NULL,HeaderDATA2=NULL,variation = 1) {
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
              o <- rv_bb_to_pct(o)
            }

            nana <- paste(n, most, sep = "_") # creating the species + layer
            #specie_check <- SpLIST[SpLIST$ShortName==nana,] # double check
            specie_check <- SpLIST[SpLIST$ShortName==n,]
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
                  #searchlist <- SpLIST1[grep(paste0("^", k), SpLIST1$FullName),]
                  searchlist <- SpLIST[grep(paste0("^", k), SpLIST$FullName),]
                  print(searchlist[order(searchlist$FullName), ])
                  while (TRUE) {
                    while (TRUE) {
                      s <- toupper(readline("SpeciesName?(GenuSpe / search / insert) "))
                      if (nchar(s) == 7) {
                        nana <- paste(s, most, sep = "_")
                        #specie_check <- SpLIST[SpLIST$ShortName == nana, ]
                        specie_check <- SpLIST[SpLIST$ShortName == s, ]
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
                              if (rv_check_short_name(s,SpLIST,metadata)) { #SpLIST1
                                okay <- toupper(readline((paste0(sn,"; ",s,"? (y/f) "))))
                                if (okay == "Y") {
                                  # pridat do newrel
                                  # RelNewNs <- data.frame(ShortName = paste0(s,"_",c("J",0:3)),
                                                         # value = 0)
                                  RelNewNs <- data.frame(ShortName = s,value = 0)
                                  RelNew <- rbind(RelNew,RelNewNs)

                                  #SpLISTNs <- data.frame(ShortName = paste0(s,"_",c("J",0:3)),
                                  SpLISTNs <- data.frame(ShortName = s,
                                                         FullName = sn)

                                  SpLIST <- rbind(SpLIST,SpLISTNs)

                                  #SpLIST1Ns <- data.frame(ShortName = s,
                                  #                        FullName = sn)

                                  #SpLIST1 <- rbind(SpLIST1,SpLIST1Ns)

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

              #RelNew[which(RelNew$ShortName == substr(nanas, 1, 9)), ][, 2] <- o
              #RelNew[which(RelNew$ShortName == nanas), ][, 2] <- o
              RelNew <- rv_add_rel_new(RelNew,nanas,o)

              print(RelNew[(RelNew[, 2] > 0), ])
            } else {
              #RelNew[which(RelNew$ShortName == substr(nana, 1, 9)), ][, 2] <- o
              #RelNew[which(RelNew$ShortName == nana), ][, 2] <- o
              RelNew <- rv_add_rel_new(RelNew,nana,o)
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
    out$RelNew <- RelNew; out$SpLIST <- SpLIST; out$meta <- metadata #out$SpLIST1 <- SpLIST1;
    return(out)
  }


  if (variation == 2) {

    while (TRUE) {
      m <- suppressWarnings(as.numeric(readline("How many releves? ")))
      if (!is.na(m) & m>0) {

        Rels <- list()
        DATAtemp <- rv_read_db(SAVE)$RelDATA
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
                searchlist <- SpLIST[grep(paste0("^", k), SpLIST[, 2]), ] # SpLIST1 prior
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
            o <- rv_bb_to_pct(o)
          }

          Rels[[paste0("r",i)]][which(Rels[[paste0("r",i)]]$ShortName == substr(nana, 1, 9)), ][, 2] <- o
          RelFake[which(RelFake$ShortName == substr(nana, 1, 9)), ][, i+1] <- o
          print(RelFake[(RelFake[,i+1] > 0), ])

        }
        DATA2 <- DATAtemp
        DATA2 <- rv_create_table(Rels, DATA2,variation = 2)
        print(DATA2)
        colnames(DATA2)[-1] <- paste0("X",1:(length(colnames(DATA2))-1))
        rv_write_db(rel = DATA2,save = SAVE,meta = metadata)
        # outside function set start = TRUE, database = ""


      } else if (n == "N") {
        while (TRUE) {
          rs <- toupper(readline("Add headers?(Y/N) "))
          if (rs == "Y") {
            for (i in 1:m) {
              message(paste0("Relev\u00e9 ",i))

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

#' @keywords internal
#' @noRd
rv_species_not_found <- function(bad_name, SpList, metadata, orig_SpList = NULL) {

  message(paste0("\nUNKNOWN SPECIES: '", bad_name, "'"))

  while (TRUE) {

    if (!is.null(orig_SpList)) {

      bad_code <-  orig_SpList[which(orig_SpList$name == bad_name),]$code
      code_name <- SpList[which(SpList$ShortName == bad_code),]$FullName
      if (length(code_name)>0) {

      while(TRUE) {



        message(paste0("suggest from the original list:", bad_name, " -> ", code_name, " ? (Y/N)" ))
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
          message("No matches found.")
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
        message("Error: Code must be exactly 7 characters.")
      } else if (new_code %in% SpList$ShortName) {
        message(paste0("Error: The code '", new_code, "' is already reserved in the checklist!"))
      } else {
        confirm <- toupper(readline(paste0("Map '", bad_name, "' -> '", new_code, "'? (Y/N): ")))
        if (confirm == "Y") {

          # Update Metadata (Add "FullName::ShortCode|")
          if (is.null(metadata$extra_spec)) metadata$extra_spec <- ""
          new_entry <- paste0(bad_name, "::", new_code, "|")
          metadata$extra_spec <- paste0(metadata$extra_spec, new_entry)

          message("Metadata updated.")
          return(list(code = new_code, meta = metadata))
        }
      }

    }
  }
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
    checklist <- system.file("extdata","cz_dh2012.txt", package="Rveg",mustWork = TRUE)
  } else if (checklist %in% c("wcvp_por","wcvp_que")) {
    checklist <- system.file("extdata",paste0(checklist,".txt"),package="Rveg",mustWork = TRUE)
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

  con <- file(paste0(save, "REL.csv"), open = "wt", , encoding = "UTF-8")

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
    stringsAsFactors = FALSE, check.names = FALSE
  )

  RelDATA <- utils::read.table(
    paste0(save,"REL.csv"), sep = ",", header = TRUE, quote = "\"", comment.char = "#",
    stringsAsFactors = FALSE, check.names = FALSE
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
  if (nzchar(def)) message(paste0("RE <- ", def))
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
  cat(rv_col(sprintf("Relev\u00e9s: %d; Headers: %d", rel_count, head_count), if (same) "ok" else "error"))
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
  } else {
    stop("Variation must be 1 or 2")
  }

  # Filter out 0 values immediately
  RelNew_list <- lapply(RelNew_list, function(df) {
    df[df$value != 0, , drop = FALSE]
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
  names(base)[-1] <- as.character(seq_len(total_data_cols))

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


#' Make SP list from checklist & metadata extra species
#' @keywords internal
#' @noRd
rv_make_sp_list <- function(checklist, metadata, layers = c("3","2","1","J","0")) {
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
  if (code %in% checklist$ShortName | code %in% meta$extra_spec) {
    FALSE
  } else {
    TRUE
  }
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

  packageStartupMessage(paste0("Welcome to Rveg (version ", pkg_version, ")!"))
  packageStartupMessage("Detailed guide (v0.1.6): `vignette('Rveg')`, Updates: `news(package = 'Rveg')`")
}
