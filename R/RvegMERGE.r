#'
#' Merge two Rveg databases
#'
#'
#' @param x name of first database without extension
#' @param y name of second database without extension
#' @param save name of exported databes withou extension
#' @param head logical value if you want to export header data aswell
#'
#' @returns Rveg database containing two csv files, header data and releve data.
#'
#' @examples RvegMERGE(x, y)
#'
#'
#'
#' @export
#'
#'

RvegMERGE <- function(x, y, save="export_merge", head=T){

  tab1 <- read.csv(paste0(x, "REL.csv"))
  tab2 <- read.csv(paste0(y, "REL.csv"))
  head1 <- read.csv(paste0(x, "HEAD.csv"))
  head2 <- read.csv(paste0(y, "HEAD.csv"))

  # rel
  jointab<-full_join(tab1,tab2,by = c("X","ShortName"))
  jointab[is.na(jointab)] <- 0
  jointab<-jointab[order(jointab$X),]

  n <- 3:ncol(jointab)-2
  for (i in 3:ncol(jointab)) {
  colnames(jointab)[i] <- paste0("X",n[i-2])

  write.csv(x = jointab,file = paste0(save,"REL.csv"),row.names = F)

  # head
  if (head == T) {
    joinhead <- cbind(head1,head2[,c(-1,-2)])
    colnames(joinhead)[(ncol(head1)+1):(ncol(head1)+ncol(head2)-2)] <- paste0("X",(ncol(head1)-1):(ncol(head1)+ncol(head2)-4))
    write.csv(x = joinhead,file = paste0(save,"HEAD.csv"), row.names = F)
  }
}


} # require dplyr


