#'
#' Checking your DATABASE for duplicity
#' @param DATABASE name of csv files for releve table and header
#' @param SAVE name of exporting database
#' @param checklist custom checklist
#' @param extrahead extra rows in header
#'
#'
#' @returns Export two csv files, one for releve and one for header
#'
#' @examples
#'
#' modify(SAVE = "test_database")
#'
#' @export
#'
#'

RvegCheck <- function(DATABASE){


  DATA2 <- read.csv(paste0(DATABASE, "REL.csv"),row.names =1) # rownames
  HeaderDATA2 <- read.csv(paste0(DATABASE, "HEAD.csv"), row.names = 1) # rownames



  }
