#' RvegToJuice:
#' Export an Rveg Database to JUICE compatible format
#'
#' @description
#' Exports an existing `Rveg` database into a format directly compatible with
#' JUICE, a comprehensive software for vegetation classification. This function
#' processes both the species composition data and the header data, formatting
#' them to meet JUICE's import requirements.
#'
#' @details
#' To ensure seamless compatibility with JUICE, this function performs several
#' background transformations:
#' * **Layer Mapping:** Rveg layers are automatically converted to JUICE's numeric layer representations (e.g., Tree layer "3" becomes "2", Shrub "2" becomes "4", Herb "1" becomes "6", etc.).
#' * **Absence Encoding:** Zero values (`0`) are converted to `.`.
#' * **Encoding:** Files are written using `ISO-8859-15` encoding, which is the standard expected by JUICE for proper character rendering.
#'
#' For JUICE import first import relevé data as Spreadsheet file and follow with Header data as Tab delimineted file.
#'
#'
#' @param database Character. The path and name of an existing Rveg database
#' to be exported (e.g., `"path/to/my_db"`).
#' @param export Character. The output path and name where the resulting JUICE-compatible
#' files will be saved. Defaults to a temporary directory.
#' @param export name of your exported csv file
#' @param checklist Character. The species checklist to use. By default, the function
#' attempts to read the checklist specified in the database's metadata. You can
#' override this by providing a custom file path or a built-in dictionary string.
#'
#'
#' @return Writes two text-based CSV files to the location specified by `export`:
#' one containing the header data (`*H.csv`) and one containing the relevé
#' species data formatted with JUICE headers (`*R.csv`).
#'
#' @examples
#' # Example: Exporting the built-in example Rveg database to JUICE format
#' RvegToJuice(
#'   database = file.path(path.package("Rveg"), "extdata/ExampleDB", "example_1")
#' )
#'
#' @export

RvegToJuice <- function(database,  export = "export", checklist = "default") {

  # --- export target ---- ####
  if (export == "export") {
    export <- file.path(tempdir(), "export")
  }

  # --- read DB ---- ####
  db <- rv_read_db(database)
  DATA <- db$RelDATA
  HeaderDATA <- db$HeaderDATA

  meta_checklist <- db$meta$checklist # ignore checklists prompt on existing
  if (file.exists(rv_get_checklist(meta_checklist))) {
    checklist <- rv_get_checklist(meta_checklist)
  }

  SpLIST <- rv_make_sp_list(checklist,db$meta)


  sp_map <- setNames(SpLIST$FullName, SpLIST$ShortName)

  # --- JUICE matrix ---- ####
  code <- as.character(DATA[[1]])
  short <- substr(code, 1, 7)
  layer_raw <- substr(code, 9, 9)

  layer_map <- c("3" = "2", "2" = "4", "1" = "6", "0" = "9", "J" = "7")
  layer <- unname(layer_map[layer_raw])
  layer[is.na(layer)] <- layer_raw[is.na(layer)]

  species <- unname(sp_map[short])
  species[is.na(species)] <- short

  cover <- as.matrix(DATA[, -1, drop = FALSE])
  cover[cover == 0] <- "."
  cover <- apply(cover, 2, as.character)

  rel_out <- rbind(
    c(NA, NA, seq_len(ncol(cover))),
    cbind(species, layer, cover)
  )

  # --- JUICE header ---- ####
  HeaderDATA <- as.data.frame(t(HeaderDATA), stringsAsFactors = FALSE)
  names(HeaderDATA) <- iconv(HeaderDATA[1, ], from = "UTF-8", to = "ASCII//TRANSLIT")
  HeaderDATA <- HeaderDATA[-1, , drop = FALSE]
  HeaderDATA <- cbind("Relev\u00e9 number" = seq_len(nrow(HeaderDATA)), HeaderDATA)

  # --- write files ---
  header_file <- paste0(export, "H.csv")
  rel_file <- paste0(export, "R.csv")

  write.table(
    HeaderDATA,
    file = header_file,
    sep = "\t",
    eol = "\r\n",
    row.names = FALSE,
    quote = FALSE,
    fileEncoding = "ISO-8859-15"
  )

  tty <- paste0("Export from ", database)
  ttz <- paste0("Number of relev\u00e9s:", ncol(cover))

  con <- file(rel_file, open = "w", encoding = "ISO-8859-15")
  writeLines(c(tty, ttz, ""), con = con, sep = "\r\n")
  close(con)

  write.table(
    rel_out,
    file = rel_file,
    sep = ",",
    eol = "\r\n",
    row.names = FALSE,
    col.names = FALSE,
    na = "",
    quote = FALSE,
    append = TRUE,
    fileEncoding = "ISO-8859-15"
  )

}

#' TvToRveg:
#' Import Turboveg Data into an Rveg Database
#'
#' @description
#' Converts a Turboveg export file into a fully functional `Rveg` database.
#' The function parses plot headers, standardizes species nomenclature against a
#' specified checklist, and maps vegetation layers.
#'
#' @details
#' This function natively supports both `.csv` and `.xml` Turboveg export formats.
#' In Turboveg, either selecet `Standard XML file` or `Spreadsheet table`. In the case
#' of spreadsheet table, select format `semicolon delimited and requested header data.
#'
#' During the import process, the function operates interactively:
#' * **Species Resolution:** If a species in the Turboveg file cannot be automatically matched to the provided checklist, the function will pause and prompt the user to manually resolve the unknown species using a search interface.
#' * **Scale Selection:** The user will be prompted to specify whether the imported abundance data uses percentages ("P") or the Braun-Blanquet scale ("BB").
#'
#'
#'
#' @param tv Character. The file path to the Turboveg export file (`.csv` or `.xml`).
#' @param export Character. The output path and name of the new `Rveg` database
#' Defaults to a temporary directory.
#' @param checklist Character. The species checklist used to match Turboveg full
#' names to Rveg's 7-character `ShortName` codes. Defaults to `"default"`. Select one you
#' want to use in your `Rveg` database or the one most similar to the one used in `Turboveg`.
#' @param Rveglayers Logical. If `TRUE` (the default), Turboveg layer codes
#' (e.g., 't', 's', 'h', 'm', 'j') are automatically translated into standard
#'  Rveg numeric/character layers (`3`, `2`, `1`, `0`, `J`).
#'
#' @return Writes two linked CSV files (`*REL.csv` and `*HEAD.csv` = `Rveg database`) to the location
#' specified by `export`.
#'
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_attrs xml_attr
#'
#' @examples
#' if (interactive()) {
#'   # Example: Importing a Turboveg CSV export (or use tvexport.xml)
#'   TvToRveg(
#'     tv = file.path(path.package("Rveg"), "extdata/ExampleDB", "tvexport.csv"),
#'     Rveglayers = TRUE
#'   )
#' }
#'
#' @export
TvToRveg <- function(tv, export = "export", checklist = "default", Rveglayers = TRUE) {

  if (export == "export") {
    export <- file.path(tempdir(), "export")
  }

  #SpLIST <- rv_make_sp_list(checklist)
  SpLIST <- read.delim(rv_get_checklist(checklist), sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
  file_end <- tolower(tools::file_ext(trimws(tv)))

  if (file_end == "xml" ) {
    tvdata <- read_xml(tv)

    plot_nodes <- xml_find_all(tvdata, "/Plot_package/Plots/Plot")

    header_list <- list()
    species_list <- list()

    releve_order <- character(length(plot_nodes))

    # Iterate through plot nodes
    for (i in seq_along(plot_nodes)) {

      plot_node <- plot_nodes[[i]]

      # --- Header Extraction ---

      # Standard Record
      header_std_node <- xml_find_first(plot_node, "./header_data/standard_record")
      if (!is.na(header_std_node)) {
        std_vec <- xml_attrs(header_std_node)
      } else {
        std_vec <- character()
      }

      # UDF Records (The Fix for multiple fields)
      udf_nodes <- xml_find_all(plot_node, "./header_data/udf_record")

      if (length(udf_nodes) > 0) {
        udf_names <- xml_attr(udf_nodes, "name")
        udf_values <- xml_attr(udf_nodes, "value")
        udf_vec <- setNames(udf_values, udf_names)
      } else {
        udf_vec <- character()
      }

      # Combine and Store Header
      combined_vec <- c(std_vec, udf_vec)
      header_list[[i]] <- as.data.frame(as.list(combined_vec), stringsAsFactors = FALSE)

#       if ("releve_nr" %in% names(std_vec)) {
#         #rel_id <- std_vec[["releve_nr"]]
#         rel_id <- combined_vec[["releve_nr"]]
#       } else {
#         rel_id <- NA_character_
#       }

      rel_id <- combined_vec[["releve_nr"]]
      if (is.null(rel_id) || is.na(rel_id) || !nzchar(rel_id)) rel_id <- as.character(i)

      # --- 2. Species Extraction ---

      combined_vec[["releve_nr"]] <- rel_id
      releve_order[i] <- rel_id

      sp_nodes <- xml_find_all(plot_node, "./species_data/species/standard_record")

      if (length(sp_nodes) > 0) {

        temp_sp_rows <- list()

        # Inner loop for species attributes
        for (j in seq_along(sp_nodes)) {
          attrs <- xml_attrs(sp_nodes[[j]])
          temp_sp_rows[[j]] <- as.data.frame(as.list(attrs), stringsAsFactors = FALSE)
        }

        # Combine species rows for this specific plot
        df_species <- rv_bind_rows(temp_sp_rows)

        # Add the Releve ID column (Using the variable we captured above)
        df_species$releve_nr <- rel_id

        # Store in the main species list
        species_list[[i]] <- df_species
      }
    }

    # Combine all lists into master Data Frames
    final_header <- rv_bind_rows(header_list)
    final_species_long <- rv_bind_rows(species_list)

    species_subset <- final_species_long[, c("nr","layer","releve_nr","cover")]
    species_subset$unique_id <- paste(species_subset$nr, species_subset$layer, sep = "_")

    # 1. Add an index to track the original order
    # Use seq_len() instead of 1:nrow() as it handles empty data frames safely
    species_subset$original_order <- seq_len(nrow(species_subset))


    # # 2. Group by your variables
    # grouped_species <- dplyr::group_by(species_subset, unique_id, releve_nr)
    #
    # # 3. Summarise
    # # We calculate 'sort_idx' by taking the minimum original_order in each group.
    # # We set .groups = "drop" to ensure the output is a standard tibble/data.frame, not grouped.
    # species_deduplicated <- dplyr::summarise(grouped_species,
    #                                          cover = paste(cover, collapse = ", "),
    #                                          nr = dplyr::first(nr),
    #                                          layer = dplyr::first(layer),
    #                                          sort_idx = min(original_order),
    #                                          .groups = "drop")

    # 2) Aggregate cover and sort_idx together
    species_deduplicated <- aggregate(
      cbind(cover, sort_idx = original_order) ~ unique_id + releve_nr + nr + layer,
      data = species_subset,
      FUN = function(x) {
        # This FUN is applied column-wise; handle each by type
        if (is.character(x)) paste(x, collapse = ", ")
        else min(x)
      }
    )

    # 4. Arrange back to the original order using the captured index
    species_deduplicated <- species_deduplicated[order(species_deduplicated$sort_idx), ]

    # 5. Remove the temporary index column
    species_deduplicated$sort_idx <- NULL



    df_ready <- species_deduplicated[, c("nr", "layer", "releve_nr", "cover"), drop = FALSE]

    # Reshape
    final_species_wide <- reshape(
      data = as.data.frame(df_ready),
      idvar = c("nr", "layer"),      # Use these two columns to identify rows
      timevar = "releve_nr",         # This column becomes the headers
      v.names = "cover",             # This is the value that goes in the cells
      direction = "wide"
    )

    # Clean up column names
    names(final_species_wide) <- gsub("^cover\\.", "", names(final_species_wide))

    # Enforce XML plot order for relevé columns
    releve_order <- releve_order[!is.na(releve_order) & nzchar(releve_order)]
    #releve_order <- unique(releve_order)
    if (rel_id %in% releve_order[seq_len(i - 1)]) rel_id <- paste0(rel_id, "_", i)

    missing_cols <- setdiff(releve_order, names(final_species_wide))
    if (length(missing_cols) > 0) {
      # create missing columns as "0" so alignment is preserved
      for (cc in missing_cols) final_species_wide[[cc]] <- "0"
    }

    final_species_wide <- final_species_wide[, c("nr", "layer", releve_order), drop = FALSE]

    # Fill NAs with "0"
    final_species_wide[is.na(final_species_wide)] <- "0"


    ### Lookup Dictionary
    lookup_nodes <- xml_find_all(tvdata, "/Plot_package/Lookup_tables/Species_list/species_record")
    dictionary_list <- list()

    if (length(lookup_nodes) > 0) {
      for (k in seq_along(lookup_nodes)) {
        l_attrs <- xml_attrs(lookup_nodes[[k]])
        dictionary_list[[k]] <- as.data.frame(as.list(l_attrs), stringsAsFactors = FALSE)
      }
      final_dictionary <- rv_bind_rows(dictionary_list)
    }

    # CSV table alike
    final_species_wide$nr <- as.character(final_species_wide$nr)
    final_dictionary$nr <- as.character(final_dictionary$nr)
    final_dictionary <- final_dictionary[!duplicated(final_dictionary$nr), ]

    result_df <- merge(
      x = final_species_wide,
      y = final_dictionary[, c("nr", "name")], # We only pick the 'nr' and 'name' columns
      by = "nr",
      all.x = TRUE,
      sort = FALSE
    )

    result_df$nr <- NULL

    all_cols <- names(result_df)

    data_cols <- setdiff(all_cols, c("name", "layer"))

    new_order <- c("name", "layer", data_cols)

    tvrel <- result_df[, new_order]
    tvhead <- as.data.frame(t(final_header))
    tvhead <- cbind(table=rownames(tvhead),Spacer = "",tvhead)
    tvsplist <- final_dictionary

  }

  if (file_end == "csv") {
    tvdata <- read.csv(tv,fileEncoding = "ISO-8859-2")

    lim <- as.numeric(rownames(tvdata[tvdata[, 1] == "", ])) # blank separator of header and releves

    tvhead <- tvdata[1:(lim - 1), -(ncol(tvdata))]
    tvrel <- tvdata[(lim + 2):nrow(tvdata), -(ncol(tvdata))]
    tvsplist <- NULL

  }

  colnames(tvhead)[3:ncol(tvhead)] <- paste0("X", 1:(ncol(tvhead)-2))
  colnames(tvrel)[3:ncol(tvrel)] <- paste0("X", 1:(ncol(tvrel)-2))

  rv_create_new_db(export,labs = tvhead[,1],checklist = checklist,meta = NULL)
  db <- rv_read_db(export)
  DATA <- db$RelDATA; HeaderDATA <- db$HeaderDATA; metadata <- db$meta

  HeaderDATA <- cbind(HeaderDATA,tvhead[,3:ncol(tvhead)]) # Vyresit cislovani sloupcu

  species_names <- tvrel[, 1]
  raw_layers <- tvrel[, 2]
  first_letter <- substr(raw_layers, 1, 1)

  unique_import_names <- unique(species_names)
  unknown_mask <- !(unique_import_names %in% SpLIST$FullName)
  unknown_species <- unique_import_names[unknown_mask]

  # Create a lookup vector (Name -> BaseCode)
  # Start by filling knowns
  name_map <- character(length(unique_import_names))
  names(name_map) <- unique_import_names

  # Fill knowns from SpList
  known_names <- unique_import_names[!unknown_mask]
  match_idx <- match(known_names, SpLIST$FullName)
  raw_codes <- SpLIST$ShortName[match_idx]
  name_map[known_names] <- raw_codes

  # --- Interactive Resolution Loop ---
  if (length(unknown_species) > 0) {
    message(paste("Found", length(unknown_species), "unknown species."))

    for (bad_name in unknown_species) {

      # Run the helper function
      res <- rv_species_not_found(bad_name, SpLIST, metadata, orig_SpList = tvsplist)

      # Store result in our map
      name_map[bad_name] <- res$code

      # Update metadata object for the next iteration/final return
      metadata <- res$meta
    }
  }


  base_codes_col <- name_map[species_names]

  raw_layers <- tvrel[, 2]
  first_letter <- substr(raw_layers, 1, 1)
  layer_code <- raw_layers

  if (Rveglayers) {
    # Map letters to your numeric codes
    layer_code <- character(length(first_letter))
    layer_code[first_letter == "h"] <- "1"
    layer_code[first_letter == "s"] <- "2"
    layer_code[first_letter == "t"] <- "3"
    layer_code[first_letter == "m"] <- "0"
    layer_code[first_letter == "j"] <- "J"

    layer_code[first_letter %in% c("6")] <- "1"
    layer_code[first_letter %in% c("4","5")] <- "2"
    layer_code[first_letter %in% c("1","2","3")] <- "3"
    layer_code[first_letter %in% c("9","0")] <- "0"
    layer_code[first_letter %in% c("7","8")] <- "J"

  }

  data_only <- tvrel[, 3:ncol(tvrel)]
  while(TRUE){
    m <- toupper(readline("Abundance in percentage or Braun-blanquet?"))
    if (m %in% c("P","BB","B")) {
      break
    }
  }

  if (m %in% c("B","BB") & Rveglayers) {
    transform <- Vectorize(rv_bb_to_pct)
    data_only[] <- lapply(data_only, function(col) transform(col))
  }

  # Aggregate using the Mapped Base Code + Layer Code
  condensed_data <- aggregate(
    x = data_only,
    by = list(BaseCode = base_codes_col, LayerCode = layer_code),
    FUN = Merge_layers
  )


  final_ids <- paste0(condensed_data$BaseCode,"_", condensed_data$LayerCode)

  result_df <- data.frame(ShortName = final_ids, stringsAsFactors = FALSE)
  result_df <- cbind(result_df, condensed_data[, -c(1, 2)])



  rv_write_db(rel = result_df, head = HeaderDATA, save = export, meta = metadata)


}

#' RvegToTv:
#' Export an Rveg Database to Turboveg Compatible Format
#'
#' @description
#' Exports an existing `Rveg` database into a CSV format compatible with the
#' Turboveg vegetation database management system. The function automatically
#' reconstructs full botanical names from the internal `ShortName` codes and
#' maps vegetation layers to standard Turboveg abbreviations.
#'
#' @details
#' During export, Rveg's alphanumeric layers are translated into Turboveg's
#' specific layer codes (e.g., `0` becomes `ml`, `1` becomes `hl`, `2` becomes `s1`,
#' `3` becomes `t1`, and `J` becomes `jl`).
#'
#' The output structure changes depending on the target Turboveg version specified
#' by the `ver` parameter:
#' * **Turboveg v3 (`ver = 3`):** Writes a single, combined `.csv` file containing both header and species data.
#' * **Turboveg v2 (`ver = 2`):** Writes two separate files: `*R.csv` for relevé species data and `*H.csv` for header data.
#'
#' @param database Character. The path and name of the existing `Rveg` database
#' to be exported (e.g., `"path/to/my_db"`).
#' @param export Character. The output path and name where the resulting Turboveg
#' CSV file(s) will be saved. Defaults to a temporary directory.
#' @param checklist Character. The species checklist used to match Rveg's 7-character
#' `ShortName` codes back to their full botanical names. Defaults to `"default"`.
#' @param ver Numeric. The target Turboveg version format to export to (either `2` or `3`).
#' Defaults to `3`.
#'
#' @return Writes one or two CSV files to the location specified by `export`,
#' depending on the chosen Turboveg version.
#'
#' @examples
#' # Example: Exporting the built-in Rveg database to Turboveg v3 format
#' RvegToTv(
#'   database = file.path(path.package("Rveg"), "extdata/ExampleDB", "example_1"),
#'   ver = 3
#' )
#'
#' @export
RvegToTv <- function(database, export = "export", checklist = "default", ver = 3) {

  if (export == "export") {
    export <- file.path(tempdir(), "export")
  }

  db <- rv_read_db(database)
  DATA <- db$RelDATA; HeaderDATA <- db$HeaderDATA; metadata <- db$meta

  meta_checklist <- db$meta$checklist # ignore checklists prompt on existing
  if (file.exists(rv_get_checklist(meta_checklist))) {
    checklist <- rv_get_checklist(meta_checklist)
  }

  SpLIST <- rv_make_sp_list(checklist,db$meta)


  ### Header
  HeaderDATA <- cbind(HeaderDATA[1], Layer = "", HeaderDATA[-1])

  ## fullnames
  DATA$Fullname <- substr(DATA$ShortName, 1, 7)
  key <- DATA$Fullname

  DATA$Fullname <- SpLIST$FullName[ match(DATA$Fullname, SpLIST$ShortName) ]

  miss <- is.na(match(key, SpLIST$ShortName))
  if (any(miss)) warning("Unmatched ShortName codes: ", paste(unique(key[miss]), collapse = ", "))

  ## layers
  DATA$Layer <- substr(DATA$ShortName, 9, 9)
  map <- c("0"="ml", "1"="hl", "2"="s1", "3"="t1", "J"="jl")
  x <- as.character(DATA$Layer)
  DATA$Layer <- ifelse(x %in% names(map), unname(map[x]), DATA$Layer)


  if (ver == 3) {
    DATA <- cbind(DATA$Fullname, DATA$Layer, DATA[, -c(1, ncol(DATA), ncol(DATA) - 1)])
    names(DATA) <- 1:ncol(DATA)
    names(HeaderDATA) <- 1:ncol(HeaderDATA)
    out <- rbind(HeaderDATA, c(rep("", ncol(HeaderDATA))), DATA)
    colnames(out)<-out[1,]
    write.csv(out[-1,], paste0(export,".csv"), row.names = F)

  } else if (ver == 2) {
    # rel
    DATA <- cbind(DATA$Fullname, DATA$Layer, DATA[, -c(1, ncol(DATA), ncol(DATA) - 1)])
    names(DATA) <- DATA[1,]
    write.csv(DATA[-1,], paste0(export,"R.csv"), row.names = F)
    # head
    HeaderDATA<-as.data.frame(t(HeaderDATA))
    colnames(HeaderDATA) <- HeaderDATA[1,]
    HeaderDATA <- cbind(table_nr = seq(1:(nrow(HeaderDATA)-2)),HeaderDATA[-c(1,2),])
    write.csv(HeaderDATA,paste0(export,"H.csv"), row.names = F)
  }

}
