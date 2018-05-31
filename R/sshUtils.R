#' @name parseRemoteCSVFile
#' @title parse csv file from server.
#' @description Function that parses a csv file from a remote server, and 
#'  and convert it to data.frame.
#' @import RCurl
#' @param fileFullPath file path and name.
#' @param userName user name on remote host.
#' @param password user password on remote host.
#' @param sshURL url to remote host.
#' @return a data.frame
#' @author Italo Garleni
readRemoteCSVFile <- function(fileFullPath, userName, password, sshURL)
{
  workingPath <- getwd()
  fileName <- basename(fileFullPath)
  scpQuery <- paste0("pscp -pw ", password," ", userName, "@", sshURL, ":",
                     fileFullPath, " ", workingPath)
  system(scpQuery)
  inputData <- read.csv(fileName, sep = ";", stringsAsFactors = FALSE)
  file.remove(fileName)
  return(inputData)
}

writeToRemoteCSVFile <- function(inputData, remoteFullPath, userName, password,
                                 sshURL)
{
  workingPath <- getwd()
  fileName <- basename(remoteFullPath)
  write.csv2(inputData, fileName, quote = FALSE)
  scpQuery <- paste0("pscp -pw ", password," ", workingPath, "/",fileName,
                     " ", userName, "@", sshURL, ":", remoteFullPath)
  success <- system(scpQuery)
  if(success)
    file.remove(fileName)
  else
    stop(paste("Fail on upload to remote server. You can find the file on",
               workingPath," and upload it manually and execute",
               "updateVersion()."), call. = FALSE)
}

