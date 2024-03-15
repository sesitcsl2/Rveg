#'
#' RvegMerge
#'
#' Merge two Rveg databases
#'
#' @param x name of first database
#' @param y name of second database
#' @param save name of exported databes
#' @param head logical value if want to merge header
#'
#' @returns export two csv files, one for releve and one for header
#'
#' @examples
#' ## NOT RUN
#' if (interactive()) {
#'   RvegMerge(x = paste0(
#'     path.package("Rveg"),
#'     "/extdata/example_db"
#'   ), y = paste0(
#'     path.package("Rveg"),
#'     "/extdata/example_db"
#'   ))
#'   read.csv("export_mergeREL.csv", row.names = 1)
#' }
#'
#' @export
#'

RvegMerge <- function(x, y, save = "export_merge", head = TRUE) {
  if (save == "export_merge") {
    save <- file.path(tempdir(), "export_merge")
  }

  tab1 <- read.csv(paste0(x, "REL.csv"))
  tab2 <- read.csv(paste0(y, "REL.csv"))
  head1 <- read.csv(paste0(x, "HEAD.csv"))
  head2 <- read.csv(paste0(y, "HEAD.csv"))

  # rel
  jointab <- dplyr::full_join(tab1, tab2[-1], by = c("ShortName"))
  jointab[is.na(jointab)] <- 0
  jointab <- jointab[order(jointab$ShortName), ]


  n <- 3:ncol(jointab) - 2
  for (i in 3:ncol(jointab)) {
    colnames(jointab)[i] <- paste0("X", n[i - 2])
  }

  n <- 1:nrow(jointab)
  jointab$X <- n

  write.csv(x = jointab, file = paste0(save, "REL.csv"), row.names = FALSE)

  # head
  if (head == TRUE) {
    joinhead <- cbind(head1, head2[, c(-1, -2)])
    colnames(joinhead)[(ncol(head1) + 1):(ncol(head1) + ncol(head2) - 2)] <- paste0("X", (ncol(head1) - 1):(ncol(head1) + ncol(head2) - 4))
    write.csv(x = joinhead, file = paste0(save, "HEAD.csv"), row.names = FALSE)
  }
}
