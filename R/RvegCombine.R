#'
#' RvegCombine
#'
#' Merging species or layers in the database
#'
#' @param database name of the loading database
#' @param export name of the exported database
#'
#' @returns export two csv files, one for releve and one for header
#'
#' @examples
#' ## NOT RUN
#' if (interactive()) {RvegCombine(database = paste0(path.package("Rveg"),
#' "/extdata/example_db"))
#' read.csv("exportREL.csv",row.names = 1)}
#'
#' @export
#'




RvegCombine <- function(database, export = "export"){

    #warning("This function will write files into your working directory")
    #write_check <- readline("do you want to continue?(Y/N) ")
    #if (toupper(write_check) == "N") {
    #  stop("access denied")
    #}

    if (export == "export") {
    export = file.path(tempdir(), "export")
    }

    DATA <-read.csv(paste0(database, "REL.csv"), row.names = 1)
    HeaderDATA <-read.csv(paste0(database, "HEAD.csv"), row.names = 1)

    while (TRUE) {
      a <- toupper(readline("Combine?(LAYER/SPEC/PRINTREL/N) "))
        if (a == "LAYER") {
          while (TRUE) {

            b <- toupper(readline("Which layer?(3/2/1/0/J) "))
            c <- toupper(readline("To which layer?(3/2/1/0/J) "))
            if (any(c(b,c) %in% c(1,2,3,"J",0))) {

              for (i in DATA$ShortName) {
                if (i == paste0(substr(i,1,7),"_",b)&&any(DATA$ShortName==paste0(substr(i,1,7),"_",c))) {
                  l1 <- DATA[DATA$ShortName==i,-1]
                  l2 <- DATA[DATA$ShortName==paste0(substr(i,1,7),"_",c),-1]
                  l3 <- round(l1 + (l2*(1-(l1/100))))
                  DATA <- DATA[DATA$ShortName!=paste0(substr(i,1,7),"_",b),]
                  DATA[DATA$ShortName==paste0(substr(i,1,7),"_",c),-1] <- l3

                  write.csv(DATA, paste0(export, "REL.csv"))
                  write.csv(HeaderDATA, paste0(export, "HEAD.csv"))

                }

              }

              break
            } else { warning("wrong layers input") }


          }


        } else if (a == "SPEC") {

          while (TRUE) {

            b <- toupper(readline("Which specie?(GenuSpe_L) "))
            c <- toupper(readline("To which layer?(GenuSpe_L) "))
            if (nchar(b) == 9 & nchar(c) == 9) {

              l1 <- DATA[DATA$ShortName==b,-1]
              l2 <- DATA[DATA$ShortName==c,-1]
              l3 <- round(l1 + (l2*(1-(l1/100))))

              DATA <- DATA[DATA$ShortName!=b,]
              DATA[DATA$ShortName==c,-1] <- l3

              write.csv(DATA, paste0(export, "REL.csv"))
              write.csv(HeaderDATA, paste0(export, "HEAD.csv"))


              break
            } else { warning("wrong species input") }


          }



        } else if (a == "PRINTREL") {
          print(DATA)
        } else if (a == "N") {
          break
        }


    }

  }
