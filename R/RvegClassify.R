#' RvegClassify:
#' Classify Rveg relevés into vegetation units
#'
#' @description
#' Classifies relevés from an Rveg database using formal Expert System (ESy)
#' definitions, a VegKey constancy model, or both.
#'
#' @details
#' ESy retains all formal matches at the highest matching priority. A missing
#' header value can make the classification uncertain if it affects a rule at
#' or above that priority. When the rule file contains a similarity profile,
#' FPFI ranks the formal matches or, for plots with no formal match, all units
#' represented in that profile.
#'
#' VegKey ranks units by the product of the constancies of the recorded taxa.
#' Relative scores sum to 100 across the candidate units before truncating
#' the ranking to `top_n`.
#'
#' @param database Character. Path and name of an Rveg database, or
#' `"default"` for the example database in [RvegLoad()].
#' @param method Character vector. One or both of `"esy"` (the default)
#' and `"vegkey"`.
#' @param rules Character vector. Use `"CzechVeg-ESy-basic"` (the default),
#' `"CzechVeg-ESy-full"`, or paths to custom ESy rule files. Bundled names and
#' custom paths can be combined in the same vector.
#' @param model Character or list. Use `"CzechVeg-VegKey"` (the default),
#' a path to a VegKey `.rds` file, or a loaded `RvegVegKey` model from
#' [CreateVegKey()].
#' @param synonym_file Character. Use `"WCVP-Czech"` (the default) for the bundled
#' Czech table or `"WCVP-Global"` for the global WCVP v16 table. If the global
#' table is missing, an interactive session asks before downloading it from the Rveg GitHub release.
#' Declining uses the Czech table. In a non-interactive session, the global table
#' must already be available. You can also pass a path to a UTF-8 TSV (optionally
#' gzip-compressed) with `accepted_name` and `synonyms` columns. Separate multiple
#' synonyms with `" | "`. Use `NULL` to disable synonym matching.
#' @param plot_id Vector of plot IDs to classify, or `NULL` to use all plots.
#' @param checklist Character. Species checklist passed to [RvegLoad()].
#' @param customscale Logical. Passed to [RvegLoad()] to convert custom
#' abundance scales interactively.
#' @param exclude_layers Character vector. JUICE layer codes omitted from ESy
#' and FPFI. VegKey uses the exclusions stored in the model.
#' @param top_n Integer. Maximum number of ranked FPFI or VegKey results per
#' plot and classification run.
#' @param min_fpfi Numeric. Minimum FPFI score to retain.
#' @param candidate_units Character vector. VegKey unit codes to consider,
#' or `NULL` to consider all units in the model.
#' @param use_dominance Logical. If `TRUE`, VegKey uses dominance constancy
#' for taxa with cover above 25% when the model includes it.
#' @param output Character. Directory for `classification.csv` and
#' `taxa.csv`, or `NULL` to skip export.
#' @param esy_mode Character. Use `"separate"` to evaluate each ESy file
#' independently or `"merge"` to evaluate rules jointly and apply priorities
#' across files.
#'
#' @return For one method, a list containing a plot-level `summary` and a
#' `taxon_map` of original and matched names. ESy also returns `all_matches`
#' and `fpfi_ranked`. VegKey also returns `vegkey_ranked`. If both methods
#' are requested, a named list contains one such result per method.
#'
#' @seealso [CreateESyRules()], [CreateVegKey()]
#'
#' @examples
#' \dontrun{
#' RvegClassify("path/to/my_db", rules = "my_rules.txt", synonym_file = NULL)
#' }
#'
#' @export
RvegClassify <- function(database, method = "esy",
  rules = "CzechVeg-ESy-basic",
  model = "CzechVeg-VegKey",
  synonym_file = "WCVP-Czech",
  plot_id = NULL, checklist = "default",
  customscale = FALSE, exclude_layers = c("7", "8"),
  top_n = 3L, min_fpfi = 0,
  candidate_units = NULL, use_dominance = TRUE,
  output = NULL, esy_mode = "separate") {
  if (!is.character(method) || !length(method))
    stop("Choose at least one method: esy or vegkey.", call. = FALSE)
  methods <- unique(vapply(method, match.arg, character(1L),
    choices = c("esy", "vegkey"), USE.NAMES = FALSE))

  ## load the Rveg database
  db <- RvegLoad(database, checklist = checklist,
    customscale = customscale, variation = 3)
  data <- rv_releves(db$RelDATA)
  if (!is.null(plot_id)) {
    data$plot_id <- data$plot_id[data$plot_id %in% plot_id]
    data$species <- data$species[data$species$plot_id %in% plot_id, , drop = FALSE]
  }

  ## prepare the original names and optional synonym file ----
  synonym_file <- rv_resolve_synonym_file(synonym_file)
  original <- data$species$taxon_name
  original_taxa <- unique(original)
  taxon_map <- unique(data$species[c("taxon_id", "taxon_name")])
  data$species$source_taxon_name <- original

  results <- export_runs <- list()
  multiple_esy <- FALSE
  for (selected_method in methods) {

    ## load the rules or model ----
    if (selected_method == "esy") {
      bundled_rules <- c(
        "CzechVeg-ESy-full" = "CzechVeg-ESy-full.txt.gz",
        "CzechVeg-ESy-basic" = "CzechVeg-ESy-basic-v1-2020-01-12.txt"
      )

      rule_files <- vapply(unique(rules), function(rule) {
        if (!rule %in% names(bundled_rules)) return(rule)
        system.file("extdata", "Classification", bundled_rules[[rule]],
          package = "Rveg", mustWork = TRUE)
      }, character(1L), USE.NAMES = FALSE)
      rule_files <- unique(normalizePath(rule_files, mustWork = FALSE))
      multiple_esy <- length(rule_files) > 1L
      definitions <- lapply(rule_files, rv_read_esy_rules)
      sources <- basename(rule_files)
      same_name <- duplicated(sources) | duplicated(sources, fromLast = TRUE)
      sources[same_name] <- rule_files[same_name]
      for (i in seq_along(definitions)) definitions[[i]]$source <- sources[i]
      if (multiple_esy && match.arg(esy_mode, c("separate", "merge")) == "merge")
        definitions <- list(rv_merge_esy_rules(definitions))
    } else if (selected_method == "vegkey") {
      if (identical(model, "CzechVeg-VegKey") ||
          (identical(model, "CzechVeg-VegKey.rds") && !file.exists(model))) {
        model <- system.file("extdata", "Classification", "CzechVeg-VegKey.rds",
          package = "Rveg", mustWork = TRUE)
      }
      definitions <- list(if (inherits(model, "RvegVegKey")) model else readRDS(model))
    }

    runs <- lapply(definitions, function(definition) {
      method_data <- data
      if (selected_method == "esy") {
        target_taxa <- unique(c(definition$mappings$source, definition$mappings$target,
          definition$taxa))
        aggregate_targets <- c(definition$mappings$target, definition$taxa)

        # Look up only the header fields used in these ESy definitions.
        if (length(definition$header_fields)) {
          head <- db$HeaderDATA
          fields <- rv_schema_from_head(head)
          rows <- match(tolower(definition$header_fields), tolower(fields))
          method_data$headers <- setNames(as.data.frame(t(head[rows, data$plot_id, drop = FALSE])),
            definition$header_fields)
        }
      } else if (selected_method == "vegkey") {
        target_taxa <- rownames(definition$presence)
        aggregate_targets <- target_taxa
      }

      ## match taxon names for this rule set or model ----
      matched <- rv_match_taxa(original_taxa, target_taxa, synonym_file, aggregate_targets)
      method_map <- taxon_map
      method_map$matched_name <- matched[match(method_map$taxon_name, original_taxa)]
      method_data$species$taxon_name <- matched[match(original, original_taxa)]

      ## classify plots ----
      result <- if (selected_method == "esy") {
        rv_classify_esy(method_data, definition, top_n = top_n,
          exclude_layers = exclude_layers, min_fpfi = min_fpfi,
          synonym_file = synonym_file, include_fpfi_taxa = !is.null(output))
      } else if (selected_method == "vegkey") {
        rv_classify_vegkey(method_data, definition, top_n = top_n,
          candidate_units = candidate_units, use_dominance = use_dominance)
      }
      fpfi_taxon_map <- result$fpfi_taxon_map
      result$fpfi_taxon_map <- NULL
      result$taxon_map <- method_map

      if (selected_method == "esy" && multiple_esy) {
        result$summary$rule_source <- rep(definition$source, nrow(result$summary))
        result$taxon_map$rule_source <- rep(definition$source, nrow(result$taxon_map))
        result$fpfi_ranked$rule_source <- rep(definition$source, nrow(result$fpfi_ranked))

        # Restore original codes and sources after a merged evaluation.
        units <- definition$associations
        if (!is.null(units$original_code)) {
          single <- match(result$summary$esy_code, units$code)
          result$summary$esy_code <- units$original_code[single]
          result$summary$rule_source[!is.na(single)] <- units$rule_source[single[!is.na(single)]]
          for (name in c("all_matches", "fpfi_ranked")) {
            rows <- match(result[[name]]$association_code, units$code)
            result[[name]]$association_code <- units$original_code[rows]
            result[[name]]$rule_source <- units$rule_source[rows]
          }
        }
      }
      list(result = result, fpfi_taxon_map = fpfi_taxon_map, source = definition$source)
    })
    result <- runs[[1L]]$result
    if (length(runs) > 1L) {
      for (name in names(result))
        result[[name]] <- do.call(rbind, lapply(runs, function(run) run$result[[name]]))
    }
    results[[selected_method]] <- result
    if (!is.null(output)) export_runs[[selected_method]] <- runs
  }

  ## export tables -------------------------------------------------
  if (!is.null(output)) {
    classification_tables <- taxon_tables <- list()
    columns <- c("plot_id", "method", "status", "rank", "association_code", "association_name",
      "score", "n_used_taxa", "n_training", "rule_source")
    for (selected_method in methods) {
      for (run in export_runs[[selected_method]]) {
        result <- run$result
        maps <- setNames(list(result$taxon_map), selected_method)
        if (selected_method == "esy") {
          tables <- list(esy = result$all_matches)
          if (!is.null(run$fpfi_taxon_map)) {
            tables$fpfi <- result$fpfi_ranked
            names(tables$fpfi)[names(tables$fpfi) == "fpfi"] <- "score"
            maps$fpfi <- run$fpfi_taxon_map
          }
        } else if (selected_method == "vegkey") {
          tables <- list(vegkey = result$vegkey_ranked)
          names(tables$vegkey)[match(c("unit_code", "unit_name", "relative_score"), names(tables$vegkey))] <-
            c("association_code", "association_name", "score")
        }

        # Keep a row for every plot and rule set, even without a match or score.
        uncertain <- result$summary$plot_id[is.na(result$summary$esy_n_matches)]
        for (name in names(tables)) {
          rows <- tables[[name]]
          missing_ids <- setdiff(data$plot_id, rows$plot_id)
          rows <- rows[c(seq_len(nrow(rows)), rep(NA_integer_, length(missing_ids))), , drop = FALSE]
          rows$plot_id[is.na(rows$plot_id)] <- missing_ids
          rows$method <- rep(name, nrow(rows))
          if (name == "esy") {
            rows$status <- ifelse(is.na(rows$association_code), "no_match", "matched")
          } else {
            rows$status <- ifelse(is.na(rows$rank), "no_score", "ranked")
          }
          if (name %in% c("esy", "fpfi")) rows$status[rows$plot_id %in% uncertain] <- "uncertain"
          if (name == "vegkey") {
            summary <- result$summary
            rows$n_used_taxa <- summary$n_used_taxa[match(rows$plot_id, summary$plot_id)]
          }
          for (column in setdiff(columns, names(rows))) rows[[column]] <- rep(NA, nrow(rows))
          if (multiple_esy && selected_method == "esy")
            rows$rule_source[is.na(rows$rule_source)] <- run$source
          classification_tables[[length(classification_tables) + 1L]] <- rows[columns]
        }
        for (name in names(maps)) {
          rows <- maps[[name]]
          rows$method <- rep(name, nrow(rows))
          rows$status <- ifelse(is.na(rows$matched_name), "unmatched", "matched")
          rows <- rows[c("taxon_id", "taxon_name", "method", "matched_name", "status")]
          if (multiple_esy)
            rows$rule_source <- rep(if (selected_method == "esy") run$source else NA_character_, nrow(rows))
          taxon_tables[[length(taxon_tables) + 1L]] <- rows
        }
      }
    }
    classification <- do.call(rbind, classification_tables)
    classification <- classification[order(match(classification$plot_id, data$plot_id),
      match(classification$method, unique(classification$method))), , drop = FALSE]
    taxa <- do.call(rbind, taxon_tables)

    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(classification, file.path(output, "classification.csv"),
      row.names = FALSE, na = "", fileEncoding = "UTF-8")
    utils::write.csv(taxa, file.path(output, "taxa.csv"),
      row.names = FALSE, na = "", fileEncoding = "UTF-8")
  }
  if (length(results) == 1L) results[[1L]] else results
}

#' CreateESyRules:
#' Write a custom Expert System rule file
#'
#' @description
#' Writes vegetation unit definitions and optional species groups to a UTF-8
#' ESy rule file for [RvegClassify()].
#'
#' @details
#' Write conditions in angle brackets and combine them with `AND`, `OR`
#' and `NOT`. For example, `<Alnus lusitanica GR 25>` requires cover above
#' 25%. Classification retains matches at the highest matching priority.
#'
#' @param definitions Data frame with one row per unit and columns `code`
#' (a code without spaces), `name` (the unit name) and `formula` (an ESy
#' expression). The optional `priority` column accepts a single digit or
#' letter per row and defaults to `"5"`.
#' @param file Character. Path to the output rule file.
#' @param groups Named list of character vectors, with one taxon name per
#' element of each vector. Group names can begin with an ESy type prefix
#' followed by a space. Unprefixed names use `###`.
#'
#' @return Invisibly returns `file`. Pass the path to the `rules` argument
#' of [RvegClassify()].
#'
#' @examples
#' definitions <- data.frame(
#'   code = "A1",
#'   name = "Alder woodland",
#'   formula = "<Alnus lusitanica GR 25>"
#' )
#' CreateESyRules(definitions, tempfile(fileext = ".txt"))
#'
#' @export
CreateESyRules <- function(definitions, file, groups = NULL) {
  fields <- c("code", "name", "formula")
  if (!is.data.frame(definitions) || !all(fields %in% names(definitions)) || !nrow(definitions))
    stop("Provide a non-empty table with code, name and formula columns.", call. = FALSE)

  ## Prepare the definition headers and formulas --------------------------------
  code <- trimws(as.character(definitions$code))
  name <- trimws(as.character(definitions$name))
  formula <- trimws(as.character(definitions$formula))
  priority <- definitions[["priority"]]
  priority <- if (is.null(priority)) rep("5", nrow(definitions)) else trimws(as.character(priority))
  if (anyNA(c(code, name, formula, priority)) || any(!nzchar(c(code, name, formula))) ||
      any(grepl("[[:space:]]", code)) || any(grepl("[\r\n]", name)) ||
      any(!grepl("^[0-9A-Za-z]$", priority)))
    stop("Each definition needs a code without spaces, a name, a formula and a valid priority.",
      call. = FALSE)
  labels <- paste(priority, code, name)

  ## Write species groups with the declared prefix or the default ### ------------
  group_lines <- character()
  if (length(groups)) {
    group_names <- trimws(names(groups))
    prefix <- "^(###|#TC|#SC|##[DCQ])[[:space:]]+"
    if (!is.list(groups) || length(group_names) != length(groups) || anyNA(group_names) ||
        any(!nzchar(group_names) | grepl("[\r\n]", group_names)) ||
        anyDuplicated(sub(prefix, "", group_names)))
      stop("Provide species groups as a list with distinct, non-empty names.", call. = FALSE)
    for (i in seq_along(groups)) {
      members <- trimws(as.character(groups[[i]]))
      if (!length(members) || anyNA(members) || any(!nzchar(members) | grepl("[\r\n]", members)))
        stop("Each species group needs at least one taxon name, with one name per entry.",
          call. = FALSE)
      header <- group_names[i]
      if (!grepl(prefix, header)) header <- paste("###", header)
      group_lines <- c(group_lines, header, paste0("     ", members))
    }
  }

  ## Assemble the ESy sections and write the final file -------------------------
  lines <- c(
    "SECTION 1: Species aggregation", "SECTION 1: End",
    "SECTION 2: Species groups", group_lines, "SECTION 2: End",
    "SECTION 3: Group definitions",
    as.vector(rbind(labels, formula)),
    "SECTION 3: End"
  )
  writeLines(enc2utf8(lines), file, useBytes = TRUE)
  invisible(file)
}

#' CreateVegKey:
#' Build a VegKey model for vegetation classification
#'
#' @description
#' Builds a VegKey model from classified reference relevés or a synoptic
#' table of taxon constancies and saves it as an RDS file.
#'
#' @details
#' For reference relevés, occurrence constancy is the proportion of usable
#' plots in a vegetation unit where the taxon occurs. When cover is known
#' for all usable species records, the function also estimates dominance
#' constancy as the proportion of plots where the taxon exceeds 25% cover.
#'
#' @param database Character or list. Path and name of a reference Rveg
#' database, or a list containing a `headers` data frame with `plot_id`
#' and the unit fields, and a `species` data frame with `plot_id`,
#' `taxon_name`, `layer` and `cover_pct`. Supply either `database`
#' or `synoptic`.
#' @param unit_column Character. Header field containing vegetation unit
#' codes. Required when using `database`.
#' @param synoptic Data frame or character path to a CSV file. Use one row
#' per taxon and unit with columns `unit_code`, `taxon_name` and
#' `constancy`. Optional columns are `unit_name`, `n_plots` and
#' `dominant_constancy`. Use the same scale for both constancy columns.
#' @param file Character. Path to the output `.rds` file.
#' @param unit_name_column Character. Optional header field containing unit
#' names for a reference database. Unit codes are used when names are missing.
#' @param checklist Character. Species checklist passed to [RvegLoad()] when
#' reading a reference database from disk.
#' @param customscale Logical. Passed to [RvegLoad()] to convert custom
#' abundance scales interactively.
#' @param exclude_layers Character vector. JUICE layer codes omitted from
#' reference records and stored in the model for later classification.
#' @param source Optional source information stored in the model.
#' @param min_constancy Numeric. Lower bound on constancy during scoring.
#' Must be greater than 0 and at most 1.
#' @param constancy_scale Character. Use `"percent"` (0 to 100, the default)
#' or `"proportion"` (0 to 1) for the constancy columns in `synoptic`.
#'
#' @return Invisibly returns `file`. Pass the path to the `model` argument
#' of [RvegClassify()] with `method = "vegkey"`.
#'
#' @examples
#' synoptic <- data.frame(
#'   unit_code = c("A1", "A2"),
#'   taxon_name = c("Alnus glutinosa", "Fagus sylvatica"),
#'   constancy = c(80, 65)
#' )
#' CreateVegKey(synoptic = synoptic, file = tempfile(fileext = ".rds"))
#'
#' @export
CreateVegKey <- function(database = NULL, unit_column = NULL, synoptic = NULL,
  file, unit_name_column = NULL,
  checklist = "default", customscale = FALSE,
  exclude_layers = c("7", "8"), source = NULL,
  min_constancy = 0.0001,
  constancy_scale = c("percent", "proportion")) {
  if (missing(file) || !is.character(file) || length(file) != 1L ||
      is.na(file) || tolower(tools::file_ext(file)) != "rds")
    stop("Set file to the output path of a .rds file.", call. = FALSE)
  if (is.null(database) == is.null(synoptic))
    stop("Provide either a reference database or a synoptic table.", call. = FALSE)
  if (!is.numeric(min_constancy) || length(min_constancy) != 1L ||
      !is.finite(min_constancy) || min_constancy <= 0 || min_constancy > 1)
    stop("min_constancy must be a single number greater than 0 and at most 1.", call. = FALSE)
  dominant <- NULL

  ## 1. Reference database with classified plots ------------------------------
  if (!is.null(database)) {
    if (is.null(unit_column))
      stop("Set unit_column to the header field with the reference classification.", call. = FALSE)
    fields <- unique(c(unit_column, unit_name_column))
    if (is.character(database)) {
      db <- RvegLoad(database, checklist = checklist,
        customscale = customscale, variation = 3)
      training <- rv_releves(db$RelDATA)
      rows <- match(fields, rv_schema_from_head(db$HeaderDATA))
      if (anyNA(rows)) stop("Reference unit fields are missing from the database header.", call. = FALSE)
      headers <- setNames(as.data.frame(t(db$HeaderDATA[rows, training$plot_id,
        drop = FALSE])), fields)
      headers$plot_id <- training$plot_id
    } else {
      training <- database
      headers <- training$headers
      if (!all(fields %in% names(headers)))
        stop("Reference unit fields are missing from the supplied headers.", call. = FALSE)
    }

    code <- trimws(as.character(headers[[unit_column]]))
    code[code == ""] <- NA_character_
    used <- !is.na(code)
    species <- rv_vegkey_records(training$species, headers$plot_id[used], exclude_layers)
    used <- used & headers$plot_id %in% species$plot_id
    if (!any(used)) stop("No classified reference plots contain usable taxa.", call. = FALSE)
    label_codes <- code[used]
    labels <- if (!is.null(unit_name_column)) headers[[unit_name_column]][used] else NULL
    units <- sort(unique(label_codes))
    n <- tabulate(match(label_codes, units), length(units))

    taxa <- sort(unique(species$taxon_name))
    unit_index <- match(code[match(species$plot_id, headers$plot_id)], units)
    cells <- match(species$taxon_name, taxa) + length(taxa) * (unit_index - 1L)
    presence <- matrix(tabulate(cells, length(taxa) * length(units)),
      length(taxa), dimnames = list(taxa, units))
    presence <- sweep(presence, 2, n, "/")

    # Estimate dominance constancy when cover is known for all training records.
    if (!anyNA(species$cover_pct)) {
      high <- species$cover_pct > 25
      cells_high <- cells[high]
      dominant <- matrix(tabulate(cells_high, length(taxa) * length(units)),
        length(taxa), dimnames = list(taxa, units))
      dominant <- sweep(dominant, 2, n, "/")
    }

  } else {
    ## 2. Synoptic table of taxon constancies ----------------------------------
    if (is.character(synoptic)) {
      synoptic <- utils::read.csv(synoptic, colClasses = "character",
        check.names = FALSE, fileEncoding = "UTF-8-BOM")
    }

    scale <- c(percent = 100, proportion = 1)[[constancy_scale[1]]]
    label_codes <- trimws(as.character(synoptic$unit_code))
    taxon_names <- trimws(as.character(synoptic$taxon_name))
    labels <- synoptic[["unit_name"]]
    units <- sort(unique(label_codes))
    taxa <- sort(unique(taxon_names))
    index <- cbind(match(taxon_names, taxa), match(label_codes, units))

    ## 2a. Occurrence constancy -----------------------------------------------
    presence <- matrix(0, length(taxa), length(units), dimnames = list(taxa, units))
    presence[index] <- as.numeric(as.character(synoptic$constancy)) / scale

    ## 2b. Dominance constancy (optional column) ------------------------------
    if ("dominant_constancy" %in% names(synoptic)) {
      dominant <- matrix(0, length(taxa), length(units), dimnames = list(taxa, units))
      dominant[index] <- as.numeric(as.character(synoptic$dominant_constancy)) / scale
    }

    n <- rep(NA_integer_, length(units))
    if ("n_plots" %in% names(synoptic)) {
      n <- as.integer(as.character(synoptic$n_plots[match(units, label_codes)]))
    }
  }

  ## 3. Unit names and model --------------------------------------------------
  unit_names <- units
  if (!is.null(labels)) {
    labels <- trimws(as.character(labels))
    named <- !is.na(labels) & nzchar(labels)
    matched <- match(units, label_codes[named])
    available <- !is.na(matched)
    unit_names[available] <- labels[named][matched[available]]
  }

  used_taxa <- rowSums(presence) > 0
  presence <- presence[used_taxa, , drop = FALSE]
  if (!is.null(dominant)) dominant <- dominant[used_taxa, , drop = FALSE]

  model <- structure(list(
    presence = presence,
    dominant = dominant,
    units = data.frame(code = units, name = unit_names, n_plots = n),
    min_constancy = min_constancy,
    exclude_layers = exclude_layers,
    source = source
  ), class = "RvegVegKey")

  ## 4. Save the RDS model ----------------------------------------------------
  saveRDS(model, file, version = 2)
  invisible(file)
}
