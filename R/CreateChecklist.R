#'
#' CreateChecklist:
#' Create a custom checklist for Rveg
#'
#' @description
#' Generates a custom species checklist formatted for use within the `Rveg` package.
#' This function takes a list of full botanical names and deterministically converts
#' them into unique, 7-character `ShortName` codes.
#'
#' `Rveg` includes the following
#' built-in checklists:
#' * `cz_dh2012` (Default: Checklist of vascular plants of the Czech Republic)
#' * `Czechia_slovakia_2015` (Turboveg compatible checklist)
#' * `wcvp_que` (example WCVP subset for Quebec)
#' * `wcvp_por` (example WCVP subset for Portugal)
#'
#' @details
#' **ShortName Generation Rules:**
#' The function guarantees a unique 7-character code for every species. The first
#' 4 characters always represent a unique Genus code. The remaining 3 characters
#' depend on the taxonomic rank:
#' * **Standard Species:** Genus (4) + first 3 letters of the specific epithet (e.g., `GENUEPI`).
#' * **Species (sp.):** Genus (4) + `-SP` (e.g., `GENU-SP`).
#' * **Hybrids (x):** Genus (4) + `*` + first 2 letters of hybrid epithet (e.g., `GENU*HY`).
#' * **Aggregates (agg.):** Genus (4) + `#` + first 2 letters of main epithet (e.g., `GENU#EP`).
#' * **Subspecies (ssp.):** Genus (4) + first letter of main epithet + `-` + first letter of infra epithet (e.g., `GENUE-I`).
#' * **Varieties/Forms (var./f.):** Genus (4) + first letter of main epithet + `;` + first letter of infra epithet (e.g., `GENUE;I`).
#' * **Sections (sect.):** Genus (4) + `SE` + first letter of section epithet (e.g., `GENUSEE`).
#'
#' *Note: If a 7-character code clash occurs, the 7th character is automatically adjusted using trailing letters or alphanumeric fallbacks to ensure absolute uniqueness.*
#'
#' @param specieslist Character vector OR Character. Either a vector
#' of full botanical names in your R environment, or a path to a `.txt` file
#' containing a single column named "FullName".
#' @param export Character. The output path and name for the exported checklist.
#' Defaults to a temporary directory.
#'
#' @return Writes a `.txt` file containing two columns (`ShortName` and `FullName`)
#' to the location specified by `export`.  The output can be then used
#' inside \code{\link{addReleve}} and other functions.
#'
#' @examples
#'   # Example 1: Creating a checklist from a local txt file
#'   CreateChecklist(
#'     specieslist = paste0(path.package("Rveg"), "/extdata/SpeciesList"),
#'   )
#'
#'   # Example 2: Creating a checklist dynamically using the rWCVP package
#'   if (requireNamespace("rWCVP", quietly = TRUE)) {
#'     wcvp_data <- rWCVP::wcvp_checklist(area_codes = "QUE")
#'     unique_taxa <- unique(wcvp_data$taxon_name)
#'     CreateChecklist(specieslist = unique_taxa, export = "wcvp_que")
#'   }
#'
#' @export

CreateChecklist <- function(specieslist, export = "export") {

  ## ---- Input ----- ####

  if (length(specieslist) > 1 && is.character(specieslist)) {
    FullName <- specieslist
  } else {
    in_file <- paste0(specieslist, ".txt")
    if (!file.exists(in_file)) stop("File not found: ", in_file)

    # Read and Clean
    FullName <- read.table(
      in_file, sep = "\t", quote = "", comment.char = "",
      col.names = "FullName", fileEncoding = "UTF-8",
      stringsAsFactors = FALSE
    )[["FullName"]]
  }

  FullName <- unique(trimws(FullName))
  FullName <- FullName[nzchar(FullName)]

  ## Replace "× Genus" -> "XGenus" (line start only)
  FullName_parse <- sub("^\\s*\u00D7\\s*", "X", FullName)

  ## Drop genus-only rows (require >= 2 tokens)
  tok_counts <- vapply(strsplit(FullName_parse, "\\s+"), length, integer(1))
  keep <- tok_counts >= 2L
  FullName       <- FullName[keep]
  FullName_parse <- FullName_parse[keep]

  ## ---- Token helpers ----- ####

  strip_trailing_dot <- function(x) sub("\\.+$", "", x)
  clean_alnum        <- function(x) gsub("[^A-Z0-9]", "", x)

  tokenize_spec <- function(spec_str) {
    # normalize Unicode × to standalone X and uppercase
    s <- toupper(gsub("\u00D7", " X ", spec_str))
    w <- strsplit(s, "\\s+")[[1]]
    w <- w[nzchar(w)]
    # accept optional dots uniformly by stripping trailing dots
    w <- strip_trailing_dot(w)
    w
  }

  # token groups
  agg_tokens   <- c("AGG", "AGGR")
  sect_tokens  <- c("SECT")
  ssp_tokens   <- c("NSSP","SSP","SUBSP","NOTHOSUBSP")
  var_tokens   <- c("NOTHOVAR","NVAR","VAR", "F", "FORMA")


  # tokens that are not epithets (for "first epithet" extraction)
  non_epithet_tokens <- c("X", agg_tokens, "SPECIES", ssp_tokens, var_tokens, sect_tokens, "SP")


  first_epithet <- function(tokens) {
    # return first token that can plausibly be an epithet
    ok <- !(tokens %in% non_epithet_tokens)
    if (!any(ok)) return("")
    tokens[which(ok)[1]]
  }

  # hybrid epithet: token after the *last* X, skipping rank tokens and genus repeats/abbrev
  hybrid_epithet <- function(tokens, genus_up) {
    k <- which(tokens == "X")
    if (!length(k)) return("")
    pos <- k[length(k)]
    j <- pos + 1

    skip_after_x <- c(
      ssp_tokens,
      var_tokens,
      sect_tokens,"SP","SPECIES",agg_tokens
    )

    is_genus_abbrev <- function(tok, genus_full) {
      grepl("^[A-Z]$", tok) && substr(tok, 1, 1) == substr(genus_full, 1, 1)
    }

    while (j <= length(tokens) &&
           (tokens[j] %in% skip_after_x ||
            tokens[j] == genus_up ||
            is_genus_abbrev(tokens[j], genus_up))) {
      j <- j + 1
    }
    if (j <= length(tokens)) tokens[j] else ""
  }

  # tail letters used for deterministic 7th-character disambiguation
  tail_letters <- function(ep, from_pos) {
    epc <- clean_alnum(ep)
    if (!nzchar(epc)) return(character(0))
    ch <- strsplit(epc, "")[[1]]
    if (length(ch) < from_pos) return(character(0))
    ch[from_pos:length(ch)]
  }


  ## ----- Parse genus/spec ------- ####

  tokens_full <- strsplit(FullName, "\\s+")
  genus_orig_from_full <- vapply(tokens_full, function(x) if (length(x)) x[1] else "", "", USE.NAMES = FALSE)

  tokens_parse <- strsplit(FullName_parse, "\\s+")
  genus_orig   <- vapply(tokens_parse, function(x) if (length(x)) x[1] else "", "", USE.NAMES = FALSE)
  genus_up     <- toupper(genus_orig)

  spec_str_up <- toupper(vapply(tokens_parse, function(x) {
    if (length(x) > 1) paste(x[-1], collapse = " ") else ""
  }, "", USE.NAMES = FALSE))

  spec_tokens <- lapply(spec_str_up, tokenize_spec)

  ## ---- Genus code map (order-independent) ----- ####

  make_gen_codes_unique <- function(ug) {
    ug <- toupper(ug)
    uniq_g <- sort(unique(ug))  # stable regardless of input order
    codes <- character(length(uniq_g))
    used  <- character(0)

    for (i in seq_along(uniq_g)) {
      gi <- uniq_g[i]
      base <- sprintf("%-4s", substring(gi, 1, 4))
      cand <- base

      if (!(cand %in% used)) { codes[i] <- cand; used <- c(used, cand); next }

      j <- 5
      repeat {
        if (j <= nchar(gi)) {
          cand <- paste0(substring(gi, 1, 3), substring(gi, j, j))
          cand <- substring(sprintf("%-4s", cand), 1, 4)
          if (!(cand %in% used)) { codes[i] <- cand; used <- c(used, cand); break }
          j <- j + 1
        } else {
          for (p in c(LETTERS, 0:9)) {
            cand <- paste0(substring(gi, 1, 3), p)
            if (!(cand %in% used)) { codes[i] <- cand; used <- c(used, cand); break }
          }
          if (!nzchar(codes[i])) codes[i] <- base
          break
        }
      }
    }
    setNames(codes, uniq_g)
  }

  gen_code_map <- make_gen_codes_unique(genus_up)
  shortgen <- unname(gen_code_map[genus_up])

  ## ---- Rule priority + initial codes ---- ####
  # Priority: SPECIES > HYBRID > AGG > SECT > SUBSP/SSP/... > VAR/... > DEFAULT

  pool <- c(LETTERS, 0:9)

  type <- character(length(FullName))
  ep_main   <- character(length(FullName))  # first epithet (where relevant)
  ep_hyb    <- character(length(FullName))  # epithet after X (hybrids)
  ep_infra  <- character(length(FullName))  # infra epithet (subsp/var)
  ep_sect   <- character(length(FullName))  # sect epithet

  shortname <- character(length(FullName))

  for (i in seq_along(FullName)) {
    tok <- spec_tokens[[i]]

    has_species <- any(tok == "SPECIES")
    has_x       <- any(tok == "X")
    has_agg     <- any(tok %in% agg_tokens)
    pos_sect    <- match(TRUE, tok %in% sect_tokens)
    pos_ssp     <- match(TRUE, tok %in% ssp_tokens)
    pos_var     <- match(TRUE, tok %in% var_tokens)

    ep_main[i] <- first_epithet(tok)

    if (has_species) {
      type[i] <- "SPECIES"
      shortname[i] <- paste0(shortgen[i], "-SP")
      next
    }

    if (has_x) {
      type[i] <- "HYBRID"
      ep_hyb[i] <- hybrid_epithet(tok, genus_up[i])
      suf2 <- substr(paste0(clean_alnum(ep_hyb[i]), "XX"), 1, 2)
      shortname[i] <- paste0(shortgen[i], "*", suf2)
      next
    }

    if (has_agg) {
      type[i] <- "AGG"
      suf2 <- substr(paste0(clean_alnum(ep_main[i]), "XX"), 1, 2)
      shortname[i] <- paste0(shortgen[i], "#", suf2)
      next
    }

    if (!is.na(pos_sect) && pos_sect < length(tok)) {
      type[i] <- "SECT"
      ep_sect[i] <- tok[pos_sect + 1]
      after1 <- substr(paste0(clean_alnum(ep_sect[i]), "X"), 1, 1)
      shortname[i] <- paste0(shortgen[i], "SE", after1)
      next
    }

    if (!is.na(pos_ssp) && pos_ssp < length(tok)) {
      type[i] <- "SSP"
      ep_infra[i] <- tok[pos_ssp + 1]
      sp1 <- substr(paste0(clean_alnum(ep_main[i]), "X"), 1, 1)
      in1 <- substr(paste0(clean_alnum(ep_infra[i]), "X"), 1, 1)
      shortname[i] <- paste0(shortgen[i], sp1, "-", in1)
      next
    }

    if (!is.na(pos_var) && pos_var < length(tok)) {
      type[i] <- "VAR"
      ep_infra[i] <- tok[pos_var + 1]
      sp1 <- substr(paste0(clean_alnum(ep_main[i]), "X"), 1, 1)
      in1 <- substr(paste0(clean_alnum(ep_infra[i]), "X"), 1, 1)
      shortname[i] <- paste0(shortgen[i], sp1, ";", in1)
      next
    }

    type[i] <- "DEFAULT"
    suf3 <- substr(paste0(clean_alnum(ep_main[i]), "XXX"), 1, 3)
    shortname[i] <- paste0(shortgen[i], suf3)
  }


  ## ---- Deterministic deduplication ---- ####
  # base6 fixed; vary only the 7th character.
  # Allocation is deterministic by sorting within each base6 group by FullName.

  base6   <- substring(shortname, 1, 6)
  cur7    <- substring(shortname, 7, 7)

  # per-row candidate 7th chars derived from relevant epithet tails
  cand7 <- vector("list", length(shortname))
  for (i in seq_along(shortname)) {
    if (type[i] == "DEFAULT") {
      cand7[[i]] <- tail_letters(ep_main[i], 4)     # beyond 3rd letter
    } else if (type[i] == "AGG") {
      cand7[[i]] <- tail_letters(ep_main[i], 3)     # beyond 2nd letter
    } else if (type[i] == "HYBRID") {
      cand7[[i]] <- tail_letters(ep_hyb[i], 3)      # beyond 2nd letter
    } else if (type[i] %in% c("SSP", "VAR")) {
      cand7[[i]] <- tail_letters(ep_infra[i], 2)    # beyond 1st letter
    } else if (type[i] == "SECT") {
      cand7[[i]] <- tail_letters(ep_sect[i], 2)     # beyond 1st letter
    } else {
      cand7[[i]] <- character(0)                    # SPECIES or unknown
    }
  }

  # allocate per base6
  for (b in unique(base6)) {
    idx <- which(base6 == b)
    if (length(idx) <= 1L) next

    ord <- idx[order(FullName[idx])]  # stable ordering independent of input order
    used <- character(0)

    for (i in ord) {
      s7 <- cur7[i]
      if (nzchar(s7) && !(s7 %in% used)) {
        used <- c(used, s7)
        next
      }

      # try epithet-derived letters first, then fallback pool
      picked <- ""
      for (x in c(cand7[[i]], pool)) {
        if (!(x %in% used)) { picked <- x; break }
      }
      if (!nzchar(picked)) picked <- "X"

      cur7[i] <- picked
      shortname[i] <- paste0(base6[i], picked)
      used <- c(used, picked)
    }
  }


  ## ---- Build table ---- ####

  out <- data.frame(
    ShortName = shortname,
    FullName  = FullName,
    stringsAsFactors = FALSE
  )

  ## Add one "-SP" row per unique genus if missing (stable casing based on sorted FullName)
  uniq_gen <- sort(unique(genus_up))
  sp_shortnames <- paste0(unname(gen_code_map[uniq_gen]), "-SP")
  missing_sp <- !(sp_shortnames %in% out$ShortName)

  if (any(missing_sp)) {
    # stable "original" casing: pick genus as it appears in the alphabetically sorted FullName list
    ord_full <- order(FullName)
    genus_sorted_up   <- genus_up[ord_full]
    genus_sorted_orig <- genus_orig_from_full[ord_full]

    genus_for_name <- vapply(uniq_gen, function(g) {
      j <- which(genus_sorted_up == g)[1]
      if (length(j) && !is.na(j)) genus_sorted_orig[j] else g
    }, "", USE.NAMES = FALSE)

    sp_rows <- data.frame(
      ShortName = sp_shortnames[missing_sp],
      FullName  = paste0(genus_for_name[missing_sp], " SPECIES"),
      stringsAsFactors = FALSE
    )
    out <- rbind(out, sp_rows)
    out <- out[order(out$FullName), ]
  }


  ## ---- sanity checks ---- ####

  bad_len <- which(nchar(out$ShortName) != 7L)
  if (length(bad_len)) {
    stop(
      "Invariant failed: some ShortName are not 7 characters. Examples:\n",
      paste0("  ", out$ShortName[bad_len][1:min(10, length(bad_len))], "\t", out$FullName[bad_len][1:min(10, length(bad_len))], collapse = "\n")
    )
  }

  dup <- out$ShortName[duplicated(out$ShortName)]
  if (length(dup)) {
    d <- unique(dup)
    ex <- out[out$ShortName %in% d, , drop = FALSE]
    ex <- ex[order(ex$ShortName, ex$FullName), ]
    stop(
      "Invariant failed: duplicate ShortName remain after deduplication. Examples:\n",
      paste0("  ", ex$ShortName, "\t", ex$FullName, collapse = "\n")
    )
  }


  ## ---- Write file ---- ####

  if (identical(export, "export")) export <- file.path(tempdir(), "export")
  dir.create(dirname(export), recursive = TRUE, showWarnings = FALSE)
  out_file <- paste0(export, ".txt")

  utils::write.table(
    out, file = out_file, sep = "\t",
    quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8"
  )

  message("Checklist written: ", out_file)
  invisible(out)
}
