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
#' CreateChecklist(specieslist = paste0(path.package("Rveg"),
#' "/extdata/SpeciesList"))
#'}
#'
#' @export
#'
#'
#'


CreateChecklist <- function(specieslist, export = "export") {
  set <- read.table(paste0(specieslist,".txt"),sep="\t")
  set <- as.data.frame(set[!duplicated(set),]) # Remove duplicates species
  colnames(set) <- "set"

   if (export == "export") {
    export <- file.path(tempdir(), "export") # tempdir
  }

  splitList <- strsplit(set$set, " ")
  df <- data.frame(Column1 = character(), Column2 = character(), stringsAsFactors = FALSE)

  for (i in seq_along(splitList)) {
    # Separate the first value from the rest
    first_value <- splitList[[i]][1]
    remaining_values <- splitList[[i]][-1]
    remaining_values_str <- paste(remaining_values, collapse = " ")

    # Add the new row to the dataframe
    df <- rbind(df, c(first_value, remaining_values_str))
  } # Separate genus and spec names

  df$genus <- toupper(df[,1])
  df$spec <- toupper(df[,2])

  genus <- data.frame(gen = unique(df$genus)) # extracting all genus
  for (i in seq_along(genus$gen)) {
    genus$shortgen <- substring(genus$gen, 1, 4)
  } # create the shortname for genus

  for (i in seq_along(genus$shortgen)) {
    ii <- 5
    while (duplicated(genus$shortgen)[i] | nchar(genus$shortgen[i]) != 4) {
      genus$shortgen[i] <- paste0(substring(genus$gen[i], 1, 3), substring(genus$gen[i], ii, ii))
      ii <- ii + 1
      if (ii > 20) {
        genus$shortgen[i] <- readline(paste0("please enter the genus short code for ", genus$gen[i], " :"))
      }
    } # in case duplicates genus shortnames user will select the desired code

  }

  for (i in seq_along(df$genus)) {
    df$shortgen[i] <- genus$shortgen[genus$gen == df$genus[i]]
  } # adding to the df

  df$shortspec <- substring(df$spec, 1, 3) # Species shortname
  df$shortname <- paste0(df$shortgen, df$shortspec) # Create full shortnames

  # Very often will exist the duplicates codes
  summary(duplicated(df$shortname))

  for (i in 1:nrow(df)) {
    if (grepl("SPECIES", df$spec[i])) {
      df$shortname[i] <- paste0(df$shortgen[i], "-SP")
    } # - SP variations

    if (grepl("AGG\\.", df$spec[i])) {
      df$shortname[i] <- paste0(df$shortgen[i], "#", substring(df$spec[i], 1, 2))
    } # Aggregate variations

    if (grepl("SSP\\.", df$spec[i])) {

      words <- strsplit(df$spec[i], " ")[[1]]
      ssp_index <- which(words == "SSP." | words == "NSSP.")
      word_after_ssp <- words[ssp_index + 1]

      df$shortname[i] <- paste0(df$shortgen[i], substring(df$spec[i], 1, 1), "-", substring(word_after_ssp, 1, 1))
    } # subspecies variations


    if (grepl("VAR\\.", df$spec[i])) {

      words <- strsplit(df$spec[i], " ")[[1]]
      ssp_index <- which(words == "VAR." | words == "NVAR.")
      word_after_ssp <- words[ssp_index + 1]

      df$shortname[i] <- paste0(df$shortgen[i], substring(df$spec[i], 1, 1), ";", substring(word_after_ssp, 1, 1))
    } # Variety variations

    if (grepl("X ", df$spec[i])) {

      words <- strsplit(df$spec[i], " ")[[1]]
      ssp_index <- which(words == "X")
      word_after_ssp <- words[ssp_index + 1]

      df$shortname[i] <- paste0(df$shortgen[i], "*", substring(word_after_ssp, 1, 2))
    } # Hybrides variations


    if (grepl("SECT\\.", df$spec[i])) {

      words <- strsplit(df$spec[i], " ")[[1]]
      ssp_index <- which(words == "SECT.")
      word_after_ssp <- words[ssp_index + 1]

      df$shortname[i] <- paste0(df$shortgen[i], "SE", substring(word_after_ssp, 1, 1))
    } # Section variations
    ii <- 4
    while (duplicated(df$shortname)[i]) {

        randlett <- unlist(strsplit(df$spec[i], ""))
        df$shortname[i] <- paste0(substring(df$shortname[i], 1, 6), randlett[ii])

      if (ii > 40) {
        randlett <- sample(LETTERS, size = 1)
        df$shortname[i] <- paste0(substring(df$shortname[i], 1, 6), randlett)
      }
      ii <- ii + 1
    } # Duplicates codes alternation, first following the letters, then random association

    df$nchar[i] <- nchar(df$shortname[i]) # check for 7 letters code
  }

  dfwrite <- cbind(data.frame(Number = c(1:nrow(df))), data.frame(ShortName = df$shortname), data.frame(FullName = set$set))
  write.table(x = dfwrite, file = paste0(export, ".txt"), sep = "\t", row.names = F) # writing the checklist
}
