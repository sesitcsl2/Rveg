#'
#' RvegToJuice
#'
#' Export Rveg database to Juice software compatible format
#'
#' @param Data name of your Rveg database
#' @param checklist path to your custom species checklist
#' @param export name of your exported csv file
#'
#' @returns csv file which is readible by Juice
#'
#' @examples
#' ## NOT RUN
#' if (interactive()) {
#'   RvegToJuice(Data = paste0(
#'     path.package("Rveg"),
#'     "/extdata/example_db"
#'   ))
#'   read.csv("export.csv", header = FALSE)
#' }
#'
#' @export
#'

RvegToJuice <- function(Data, checklist = "default", export = "export") {
  if (export == "export") {
    export <- file.path(tempdir(), "export")
  }

  Data_rel <- read.csv(paste0(Data, "REL.csv"), row.names = 1)
  Data_head <- read.csv(paste0(Data, "HEAD.csv"), row.names = 1)
  if (checklist == "default") {
    checklist <- paste0(path.package("Rveg"), "/extdata/DANIHELKA2012rko.txt")
  }
  Splist <- read.delim(checklist, sep = "\t")

  x <- NULL
  y <- NULL
  z <- NULL

  for (i in 1:nrow(Data_rel)) {
    n <- substr(Data_rel[i, 1], 9, 9)
    if (n == 3) {
      n <- 2
    } else if (n == 2) {
      n <- 4
    } else if (n == 1) {
      n <- 6
    } else if (n == 0) {
      n <- 9
    } else if (n == "J") {
      n <- 7
    } # Juice codes for layers
    y <- c(y, n) # layer column
    x <- c(x, Splist[Splist[, 2] == substr(Data_rel[i, 1], 1, 7), 3]) # species names
    z[[i]] <- as.vector(Data_rel[i, 2:ncol(Data_rel)]) # cover
  }

  tt <- data.frame(matrix(unlist(z), nrow = length(z), byrow = TRUE))
  ttt <- data.frame(x, y)
  tt[tt == 0] <- "."
  tzt <- c(NA, NA, 1:ncol(tt))
  tttt <- cbind(ttt, tt)
  tttt <- rbind(tzt, tttt)
  tty <- paste0("Export from ", Data)
  ttz <- paste0("Number of relev\u00e9s:", ncol(tt)) # way to export Juice format

  write(x = paste0(tty, "\n", ttz, "\n"), file = paste0(export, ".csv")) # possible encoding problem?
  write.table(tttt, file = paste0(export, ".csv"), row.names = FALSE, col.names = FALSE, na = "", sep = ",", quote = FALSE, append = TRUE) # fileEncoding = "Windows-1252"?
}


#'
#' TvToRveg
#'
#' Export Turboveg csv file to Rveg database compatible format
#'
#' @param tv path to Turboveg csv export
#' @param export name of your exported database
#' @param checklist checklist used to match shortnames with species name
#'
#' @returns csv file
#'
#' @examples
#' ## NOT RUN
#' if (interactive()) {
#'   tvToRveg(tv = paste0(
#'     path.package("Rveg"),
#'     "/extdata/tvexport.csv"
#'   ))
#'   read.csv("exportREL.csv", row.names = 1)
#' }
#'
#' @export
#'
#'

tvToRveg <- function(tv, export = "export", checklist = "default") {
  if (export == "export") {
    export <- file.path(tempdir(), "export")
  }

  data <- read.csv(tv)
  lim <- as.numeric(rownames(data[data[, 1] == "", ])) # blank separator of header and relevés

  tvhead <- data[1:(lim - 1), ]
  tvrel <- data[(lim + 1):nrow(data), ]

  rvrel <- data.frame(ShortName = character(), stringsAsFactors = FALSE)
  rvhead <- data.frame(
    ShortName = c(
      "ID", "DATE", "SpringDATE", "LOCALITY", "FieldCODE", "Authors",
      "PlotSize", "Latitude", "Longitude", "Accuracy",
      "E3", "E2", "E1", "Ejuv", "E0", "Note", data[22:lim - 1, 1]
    ),
    stringsAsFactors = FALSE
  )

  # header
  for (i in 3:ncol(data)) {
    rvhead[rvhead$ShortName == "ID", i - 1] <- tvhead[tvhead[, 1] == "Releve number", i]
    rvhead[rvhead$ShortName == "DATE", i - 1] <- tvhead[tvhead[, 1] == "Date (year/month/day)", i]
    rvhead[rvhead$ShortName == "LOCALITY", i - 1] <- tvhead[tvhead[, 1] == "Releve number", i]
    rvhead[rvhead$ShortName == "FieldCODE", i - 1] <- tvhead[tvhead[, 1] == "Releve number", i]
    rvhead[rvhead$ShortName == "Authors", i - 1] <- "unknown"
    rvhead[rvhead$ShortName == "PlotSize", i - 1] <- tvhead[tvhead[, 1] == "Releve area (m2)", i]
    rvhead[rvhead$ShortName == "Latitude", i - 1] <- "unknown"
    rvhead[rvhead$ShortName == "Longitude", i - 1] <- "unknown"
    rvhead[rvhead$ShortName == "E3", i - 1] <- tvhead[tvhead[, 1] == "Cover tree layer (%)", i]
    rvhead[rvhead$ShortName == "E2", i - 1] <- tvhead[tvhead[, 1] == "Cover shrub layer (%)", i]
    rvhead[rvhead$ShortName == "E1", i - 1] <- tvhead[tvhead[, 1] == "Cover herb layer (%)", i]
    rvhead[rvhead$ShortName == "Note", i - 1] <- tvhead[tvhead[, 1] == "Remarks", i]
    rvhead[17:(17 + lim - 22), i - 1] <- tvhead[22:lim - 1, i] # matching consistent header characteristics


  }
  colnames(rvhead) <- c("","ShortName", paste0("X", c(1:(ncol(rvhead) - 2))))
  # header end

  if (checklist == "default") {
    checklist <- paste0(path.package("Rveg"), "/extdata/DANIHELKA2012rko.txt")
  }

  SpLIST1 <- read.delim(checklist, sep = "\t")
  SpLIST <- makeSpLIST(checklist)

  for (i in 1:nrow(tvrel)) {
    if (substr(tvrel[i, 2], 1, 1) == "h") {
      abc <- 1
    } else if (substr(tvrel[i, 2], 1, 1) == "s") {
      abc <- 2
    } else if (substr(tvrel[i, 2], 1, 1) == "t") {
      abc <- 3
    } else if (substr(tvrel[i, 2], 1, 1) == "m") {
      abc <- 0
    }
    tvrel[i, 2] <- abc
  } # converting TV layer codes

  tvrel[tvrel == ""] <- 0

  while (TRUE) {
    cla <- readline("Data in Bran blanquet or percentage? (B/%)") # converting to percentage
    if (cla == "B") {
      for (i in 1:ncol(tvrel[, c(-1, -2)])) {
        for (l in 1:nrow(tvrel[, c(-1, -2)])) {
          if (tvrel[, c(-1, -2)][l, i] == 1) {
            tvrel[, c(-1, -2)][l, i] <- 3
          } else if (tvrel[, c(-1, -2)][l, i] == 2) {
            tvrel[, c(-1, -2)][l, i] <- 15
          } else if (tvrel[, c(-1, -2)][l, i] == 3) {
            tvrel[, c(-1, -2)][l, i] <- 38
          } else if (tvrel[, c(-1, -2)][l, i] == 4) {
            tvrel[, c(-1, -2)][l, i] <- 63
          } else if (tvrel[, c(-1, -2)][l, i] == 5) {
            tvrel[, c(-1, -2)][l, i] <- 88
          } else if (tvrel[, c(-1, -2)][l, i] == "+") {
            tvrel[, c(-1, -2)][l, i] <- 2
          } else if (tvrel[, c(-1, -2)][l, i] == "r") {
            tvrel[, c(-1, -2)][l, i] <- 1
          } else if (tvrel[, c(-1, -2)][l, i] == "2a") {
            tvrel[, c(-1, -2)][l, i] <- 8
          } else if (tvrel[, c(-1, -2)][l, i] == "2b") {
            tvrel[, c(-1, -2)][l, i] <- 18
          } else if (tvrel[, c(-1, -2)][l, i] == "2m") {
            tvrel[, c(-1, -2)][l, i] <- 4
          }
        }
      }
      break
    } else if (cla == "%") {
      break
    }
  }

  for (s in tvrel[, 1]) {
    if (length((s == tvrel[, 1])[(s == tvrel[, 1]) == TRUE]) > 1) {
      l1 <- as.numeric(tvrel[s == tvrel[, 1], ][1, c(-1, -2)])
      l1[is.na(l1)] <- 0

      l2 <- as.numeric(tvrel[s == tvrel[, 1], ][2, c(-1, -2)])
      l2[is.na(l2)] <- 0
      r1 <- rownames(tvrel[s == tvrel[, 1], ][2, c(-1, -2)])

      l3 <- round(l1 + (l2 * (1 - (l1 / 100))))

      tvrel[tvrel[, 1] == s, ][1, c(-1, -2)] <- l3
      tvrel <- tvrel[!(rownames(tvrel) %in% r1), ] # merginng more detailed TV layers (eg t1 with t2 etc.)
    }
  }

  for (i in 1:nrow(tvrel)) {
    sp <- SpLIST[SpLIST$FullName == tvrel[i, 1], 2][substr(SpLIST[SpLIST$FullName == tvrel[i, 1], 2], 9, 9) == tvrel[i, 2]][1]
    rvrel[i, 1] <- sp
    rrn <- rownames(SpLIST[SpLIST$ShortName == sp, ])
    if (!is.na(sp)) {
      rownames(rvrel)[i] <- rownames(SpLIST[SpLIST$ShortName == sp, ])
    } # writing fullnames

    print(tvrel[i, 1])
    print(rvrel[i, 1])

    while (TRUE) {
      check <- toupper(readline("correct name?(blank/N) ")) # doublecheck for correct code
      if (check == "" & !is.na(rvrel[i, 1])) {
        break
      }
      if (check == "N") {
        while (TRUE) {
          k <- readline("AddSpeciesFirst3letters(eg.Che)? ")
          tt <- toupper(readline("Correct 3 letters? Y/N  "))

          if (tt == "Y") {
            break
          }
        }

        k <- paste0(toupper(substr(k, 1, 1)), tolower(substr(k, 2, 3)))
        print(SpLIST1[grep(paste0("^", k), SpLIST1[, 3]), ])
        while (TRUE) {
          while (TRUE) {
            s <- toupper(readline("SpeciesName?(7lettersGenuSpe) "))
            if (nchar(s) == 7) {
              nana <- paste(s, abc, sep = "_")
              specie_check <- SpLIST[SpLIST[, 2] == substr(nana, 1, 9), ]
              print(specie_check[3])
              break
            }
          }
          tt <- toupper(readline("Correct 7lettersGenuSpe Y/N  "))
          if ((tt == "Y") & !is.na(specie_check[1, 3])) {
            rvrel[i, 1] <- nana
            rownames(rvrel)[i] <- rownames(specie_check)
            break
          }
          break
        }
        break
      }
    }
  }

  DATA <- cbind(rvrel, tvrel[, c(-1, -2)])
  colnames(DATA) <- c("ShortName", paste0("X", c(1:(ncol(DATA) - 1))))

  write.csv(DATA, paste0(export, "REL.csv"))
  write.csv(rvhead, paste0(export, "HEAD.csv"))
}

#'
#' RvegToTv
#'
#' Export Turboveg csv compatible file
#'
#' @param database path to Rveg database
#' @param export name of your exported Tv file
#' @param sep chosen separator
#' @param checklist checklist to match Fullnames
#'
#' @returns csv file
#'
#' @examples
#' ## NOT RUN
#' if (interactive()) {
#'   RvegToTv(database = paste0(
#'     path.package("Rveg"),
#'     "/extdata/example_db"
#'   ))
#' }
#'
#' @export
#'
#'
#'

RvegToTv <- function(database, export = "export", ,ver = 3,sep = ",", checklist = "default") {
  if (export == "export") {
    export <- file.path(tempdir(), "export")
  }

  if (checklist == "default") {
    checklist <- paste0(path.package("Rveg"), "/extdata/DANIHELKA2012rko.txt")
  }

  # splist

  SpLIST1 <- read.delim(checklist, sep = "\t")
  SpLIST <- makeSpLIST(checklist)

  ### Header
  header <- read.csv(paste0(database, "HEAD.csv"))
  header[, 1] <- header[, 2]
  header[, 2] <- ""
  ### Rel
  rel <- read.csv(paste0(database, "REL.csv"))

  ## fullnames
  rel$Fullname <- substr(rel$ShortName, 1, 7)
  for (i in 1:nrow(rel)) {
    rel$Fullname[i] <- SpLIST1[SpLIST1[, 2] == rel$Fullname[i], 3]
  }
  ## layers
  rel$Layer <- substr(rel$ShortName, 9, 9)
  for (i in 1:nrow(rel)) {
    if (rel$Layer[i] == 1) {
      rel$Layer[i] <- "hl"
    } else if (rel$Layer[i] == 2) {
      rel$Layer[i] <- "s1"
    } else if (rel$Layer[i] == 3) {
      rel$Layer[i] <- "t1"
    } else if (rel$Layer[i] == 0) {
      rel$Layer[i] <- "ml"
    } else if (rel$Layer[i] == "J") {
      rel$Layer[i] <- "jl"
    }
  }

  if (ver == 3) {
    rel <- cbind(rel$Fullname, rel$Layer, rel[, -c(1, 2, ncol(rel), ncol(rel) - 1)])
    names(rel) <- 1:ncol(rel)
    names(header) <- 1:ncol(header)
    out <- rbind(header, c(rep("", ncol(header))), rel)
    colnames(out)<-out[1,]
    write.csv(out[-1,], paste0(export,".csv"), row.names = F)
  } else if (ver == 2) {
    # rel
    rel <- cbind(rel$Fullname, rel$Layer, rel[, -c(1, 2, ncol(rel), ncol(rel) - 1)])
    names(rel) <- rel[1,]
    write.csv(rel[-1,], paste0(export,"R.csv"), row.names = F)
    # head
    header<-as.data.frame(t(header))
    colnames(header) <- header[1,]
    write.csv(header[-1,],paste0(export,"H.csv"), row.names = F)
  }

}
