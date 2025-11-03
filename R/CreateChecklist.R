#'
#' CreateChecklist
#'
#' Create a custom checklist with species ShortNames
#'
#' @param specieslist path to list of species
#' @param export name of your exported checklist file
#'
#' @returns txt file used as checklist in Rveg functions
#'
#' @examples
#' ## NOT RUN
#' if (interactive()) {
#' # using txt file
#' CreateChecklist(specieslist = paste0(path.package("Rveg"),
#' "/extdata/SpeciesList"))
#'
#' # creating from wcvp using rWCVP package
#' stopifnot(requireNamespace("rWCVP"))
#' a <- wcvp_checklist(area_codes = "QUE")
#' a <- unique(a$taxon_name)
#' CreateChecklist(a,export = "wcvp_que")
#'}
#'
#' @export

CreateChecklist <- function(specieslist, export = "export") {

  if (length(specieslist)>1 & is.character(specieslist)) {
    FullName <- specieslist
  } else {
    in_file  <- paste0(specieslist, ".txt")
    if (!file.exists(in_file)) stop("File not found: ", in_file)

    # read and clean
    FullName <- read.table(in_file, sep = "\t", quote = "", comment.char = "",
                           col.names = "FullName", fileEncoding = "UTF-8",
                           stringsAsFactors = FALSE)[["FullName"]]
  }


  FullName <- unique(trimws(FullName))
  FullName <- FullName[nzchar(FullName)]


  ## Replace  "× Genus" -> XGenus
  FullName_parse <- sub("^\\s*\u00D7\\s*", "X", FullName)  # only at start of the line

  ## Drop genus-only rows: keep rows with at least 2 tokens (Genus-species)
  tok_counts <- vapply(strsplit(FullName_parse, "\\s+"), length, integer(1))
  keep <- tok_counts >= 2L
  FullName       <- FullName[keep]
  FullName_parse <- FullName_parse[keep]

  ## REPLACE your current tokens/genus/spec:
  tokens_orig <- strsplit(FullName, "\\s+")
  genus_orig_from_full <- vapply(tokens_orig, function(x) if (length(x)) x[1] else "", "", USE.NAMES = FALSE)

  tokens     <- strsplit(FullName_parse, "\\s+")
  genus_orig <- vapply(tokens, function(x) if (length(x)) x[1] else "", "", USE.NAMES = FALSE)
  genus_up   <- toupper(genus_orig)
  spec_up    <- toupper(vapply(tokens, function(x) if (length(x) > 1) paste(x[-1], collapse = " ") else "", "", USE.NAMES = FALSE))

  ## Make consistent 4-char genus codes (padded right; one code per unique genus)
  make_gen_codes_unique <- function(ug) {
    ug <- toupper(ug) # genus
    uniq_g <- unique(ug) # unique genus
    codes <- character(length(uniq_g)) # placeholder for code
    used  <- character(0)
    for (i in seq_along(uniq_g)) {
      gi <- uniq_g[i] # select genus
      # first 4 chars, right-padded with spaces to 4 (e.g., "POA ")
      base <- sprintf("%-4s", substring(gi, 1, 4))
      cand <- base # first candidate to genus name
      # not used yet, write to used and go to next genus
      if (!(cand %in% used)) { codes[i] <- cand; used <- c(used, cand); next }
      # resolve collisions using 3 + next letter(s) or a pool
      j <- 5
      repeat {
        if (j <= nchar(gi)) {
          cand <- paste0(substring(gi, 1, 3), substring(gi, j, j)) # try next letter
          cand <- substring(sprintf("%-4s", cand), 1, 4) # pad to 4
          if (!(cand %in% used)) { codes[i] <- cand; used <- c(used, cand); break }
          j <- j + 1
          # if available code not found
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
    setNames(codes, uniq_g) # output
  }
  gen_code_map <- make_gen_codes_unique(genus_up)# genus names + codes
  shortgen <- unname(gen_code_map[genus_up]) # genus codes

  ## initial short spec + full shortname
  shortspec <- substring(spec_up, 1, 3) # 3 letters
  shortname <- paste0(shortgen, shortspec) # shortname

  ## normalize hybrids: also treat Unicode × as X
  spec_norm <- gsub("\u00D7", " X ", spec_up, fixed = FALSE)

  ## Create -SP from SPECIES tag (Poa species)
  is_species <- grepl("\\bSPECIES\\b", spec_norm)
  shortname[is_species] <- paste0(shortgen[is_species], "-SP")

  ## Create `#` aggregate tag ()
  is_agg <- grepl("\\bAGG\\.", spec_norm)
  shortname[is_agg] <- paste0(shortgen[is_agg], "#", substring(spec_norm[is_agg], 1, 2))

  ## Subspecies (nssp, ssp, subsp, nothosubsp.)
  m_ssp <- regexpr("\\b((?:N?SSP\\.|SUBSP\\.|NOTHOSUBSP\\.))\\s+(\\S+)", spec_norm, perl = TRUE)
  if (any(m_ssp > 0)) {
    mat <- regmatches(spec_norm, m_ssp)
    before1 <- substring(spec_norm, 1, 1) # first letter
    after1  <- vapply(strsplit(mat, "\\s+"), function(z) substring(z[length(z)], 1, 1), "")
    idx     <- which(m_ssp > 0)
    shortname[idx] <- paste0(shortgen[idx], before1[idx], "-", after1)
  }

  ## Variets (nothovar, nvar, var)
  m_var <- regexpr("\\b((?:NOTHOVAR\\.|N?VAR\\.))\\s+(\\S+)", spec_norm, perl = TRUE)
  if (any(m_var > 0)) {
    mat <- regmatches(spec_norm, m_var)
    after1  <- vapply(strsplit(mat, "\\s+"), function(z) substring(z[length(z)], 1, 1), "")
    idx     <- which(m_var > 0)
    shortname[idx] <- paste0(shortgen[idx], substring(spec_norm[idx], 1, 1), ";", after1)
  }

  ## Sections (sect)
  m_sect <- regexpr("\\bSECT\\.\\s+(\\S+)", spec_norm, perl = TRUE)
  if (any(m_sect > 0)) {
    mat   <- regmatches(spec_norm, m_sect)
    after <- vapply(strsplit(mat, "\\s+"), function(z) substring(z[length(z)], 1, 1), "")
    idx <- which(m_sect > 0)
    shortname[idx] <- paste0(shortgen[idx], "SE", after)
  }

  ## Hybrids: one or more standalone X (Unicode × already normalized to " X ")
  m_hyb <- grepl("(^|\\s)X(\\s|$)", spec_norm, perl = TRUE)

  # tokens to skip after X (for suffix)
  skip_tokens <- c("SUBSP.","SUBSP","SSP.","SSP","NSSP.","NSSP",
                   "VAR.","VAR","NVAR.","NVAR",
                   "SECT.","SECT","SP.","SP","SPECIES","AGG.")

  if (any(m_hyb)) {
    h_idx <- which(m_hyb)
    next2 <- character(length(h_idx)) # empty placeholder

    is_genus_abbrev <- function(tok, genus_full) {
      # single-letter genus (e.g., "A" or "A.") that matches Genus name (Poa annua x P. pratensis)
      grepl("^[A-Z]\\.?$", tok) && substring(tok, 1, 1) == substring(genus_full, 1, 1)
    }

    for (t in seq_along(h_idx)) {
      i <- h_idx[t]
      w <- strsplit(spec_norm[i], "\\s+")[[1]]
      k <- which(w == "X")
      if (length(k) > 0) {
        pos <- k[length(k)]                # use the *last* X
        j <- pos + 1
        # skip rank tokens, the full genus, and genus abbreviations like "A."
        while (j <= length(w) &&
               (w[j] %in% skip_tokens ||
                w[j] == genus_up[i] ||
                is_genus_abbrev(w[j], genus_up[i]))) {
          j <- j + 1
        }
        if (j <= length(w) && nzchar(w[j])) {
          # take the next *epithet* token after the skips
          next2[t] <- substr(w[j], 1, 2)
        } else {
          # fallback: strip and take first two; if still empty, use "HY"
          cleaned <- gsub("[^A-Z0-9]", "", spec_norm[i])
          next2[t] <- if (nzchar(cleaned)) substr(cleaned, 1, 2) else "HY"
        }
      } else {
        cleaned <- gsub("[^A-Z0-9]", "", spec_norm[i])
        next2[t] <- if (nzchar(cleaned)) substr(cleaned, 1, 2) else "HY"
      }
    }
    shortname[h_idx] <- paste0(shortgen[h_idx], "*", next2)
  }


  ## Deterministic deduplication: keep first 6 chars fixed (4 genus + first 2 species); vary only the 7th
  pool <- c(LETTERS, 0:9)
  seen <- character(0)
  for (i in seq_along(shortname)) {
    cand <- shortname[i]
    if (!(cand %in% seen)) {
      seen <- c(seen, cand); next
    }
    base6 <- substring(cand, 1, 6)          # genus (4) + first 2 of species (fixed)
    # try letters from SPEC, then fallback pool, if special case use after.

    # special cases (var, hybrids, ssp. etc)
    w <- strsplit(spec_norm[i], "\\s+")[[1]]
    k <- which(w %in% c(skip_tokens,"X"))
    if (length(k) > 0) {
      wk <- w[(1+k):length(w)]
      spec_letters <- unlist(strsplit(gsub("[^A-Z0-9]", "", wk), ""))
    } else {
      spec_letters <- unlist(strsplit(gsub("[^A-Z0-9]", "", spec_norm[i]), ""))[-c(1:3)]
      }
    for (x in c(spec_letters, pool)) {
      alt <- paste0(base6, x)
      if (!(alt %in% seen)) { cand <- alt; break }
    }
    shortname[i] <- cand
    seen <- c(seen, cand)
  }

  ## Build main table
  out <- data.frame(
    #Number    = seq_len(length(FullName)),
    ShortName = shortname,
    FullName  = FullName,
    stringsAsFactors = FALSE
  )

  ## Add one "-SP" row per unique genus if missing
  uniq_gen <- unique(genus_up)
  sp_shortnames <- paste0(unname(gen_code_map[uniq_gen]), "-SP")
  missing_sp <- !(sp_shortnames %in% out$ShortName)


  if (any(missing_sp)) {
    # use the first-seen original-cased genus for the FullName SPECIES
    first_seen_idx <- match(uniq_gen, genus_up)
    # genus_for_name <- genus_orig[first_seen_idx]
    genus_for_name <- genus_orig_from_full[first_seen_idx]

    sp_rows <- data.frame(
      #Number    = seq_len(sum(missing_sp)) + nrow(out),
      ShortName = sp_shortnames[missing_sp],
      FullName  = paste0(genus_for_name[missing_sp], " SPECIES"),
      stringsAsFactors = FALSE
    )
    out <- rbind(out, sp_rows)
    out <- out[order(out[[2]]), ]
    #out$Number <- 1:nrow(out)
  }

  ## write file
  if (identical(export, "export")) export <- file.path(tempdir(), "export")
  dir.create(dirname(export), recursive = TRUE, showWarnings = FALSE)
  out_file <- paste0(export, ".txt")
  utils::write.table(out, file = out_file, sep = "\t", quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")

  message("Checklist written: ", out_file)
  invisible(out)
}
