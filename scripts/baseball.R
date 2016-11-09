# function to download and parse the retrosheet data
# requires folders download.folder/zipped and download.folder/unzipped
# requires that "cwevents.exe" from retrosheet be in the download.folder as well
parse.retrosheet.php  <- function(season){
     download.retrosheet <- function(season){
          download.file(
               url=paste("http://www.retrosheet.org/events/",
                         season,
                         "eve.zip",
                         sep=""),
               destfile = paste("download.folder",
                                "/zipped/",
                                season,
                                "eve.zip",
                                sep = "")
          )
     }
     unzip.retrosheet <- function(season){
          unzip(paste("download.folder",
                      "/zipped/",
                      season,
                      "eve.zip",
                      sep=""),
                exdir = paste("download.folder",
                              "/unzipped",
                              sep = ""))
     }
     create.csv.file <- function(year){
          wd <- getwd()
          setwd("download.folder/unzipped")
          shell(paste(paste("cwevent -y",
                            year,
                            "-f 0-96"),
                      paste(year,
                            "*.EV*",
                            sep=""),
                      paste("> all",
                            year,
                            ".csv",
                            sep="")))
          setwd(wd)
     }
     create.csv.roster <- function(year){
          filenames <- list.files(path = "download.folder/unzipped/")
          filenames.roster <- 
               subset(filenames,
                      substr(filenames, 4, 11) == paste(year,".ROS",sep = ""))
          read.csv2 <- function(file)
               read.csv(paste("download.folder/unzipped/",
                              file,
                              sep=""),
                        header=FALSE)
          R <- do.call("rbind",
                       lapply(filenames.roster,
                              read.csv2))
          names(R)[1:6] <- c("Player.ID",
                             "Last.Name",
                             "First.Name",
                             "Bats",
                             "Pitches",
                             "Team")
          wd <- getwd()
          setwd("download.folder/unzipped")
          write.csv(R,
                    file = paste("roster",
                                 year,
                                 ".csv",
                                 sep = ""))
          setwd(wd)
     }
     cleanup <- function(){
          wd <- getwd()
          setwd("download.folder/unzipped")
          shell("del *.EVN")
          shell("del *.EVA")
          shell("del *.ROS")
          shell("del TEAM*")
          setwd(wd)
     }
     season <- as.character(season)
     download.retrosheet(season)
     unzip.retrosheet(season)
     create.csv.file(season)
     create.csv.roster(season)
     cleanup()
}

#
#  Function for extracting id information from Lahman's data
#

# requires "Master.csv" to be load as "master"
getinfo <- function(firstname, lastname){
     playerline <- subset(master,
                          nameFirst == firstname & nameLast == lastname)
     name.code <- as.character(playerline$playerID)
     birthyear <- playerline$birthYear
     birthmonth <- playerline$birthMonth
     birthday <- playerline$birthDay
     byear <- ifelse(birthmonth <= 6, birthyear, birthyear + 1)
     list(name.code=name.code, byear=byear)
}

