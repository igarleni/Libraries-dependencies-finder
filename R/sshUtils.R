#' @name readCSVFromRemote
#' @title parse csv file from remote server.
#' @description Function that parses a csv file from a remote server, and 
#'  and convert it to data.frame.
#' @keywords internal
#' @param fileFullPath remote file path and name.
#' @param userName user name on remote host.
#' @param password user password on remote host.
#' @param sshURL url to remote host.
#' @return a data.frame
#' @author Italo Garleni
readCSVFromRemote <- function(fileFullPath, userName, password, sshURL)
{
  tempPath <- tempdir()
  scpQuery <- paste0("pscp -pw ", password," ", userName, "@", sshURL, ":",
                     fileFullPath, " ", tempPath)
  system(scpQuery)
  
  fileName <- basename(fileFullPath)
  tempFileFullPath <- paste0(tempPath,"/", fileName)
  inputData <- read.csv(tempFileFullPath, sep = ";", stringsAsFactors = FALSE)
  file.remove(tempFileFullPath)
  return(inputData)
}

#' @name writeCSVToRemote
#' @title write csv file on server.
#' @description Function that upload data.frame as a csv file on a remote
#'  server.
#' @keywords internal
#' @param inputData data frame to upload.
#' @param remoteFullPath remote file path and name.
#' @param userName user name on remote host.
#' @param password user password on remote host.
#' @param sshURL url to remote host.
#' @author Italo Garleni
writeCSVToRemote <- function(inputData, remoteFullPath, userName, password, 
                             sshURL)
{
  tempPath <- tempdir()
  fileName <- basename(remoteFullPath)
  tempFileFullPath <- paste0(tempPath,"/", fileName)
  write.csv2(inputData, tempFileFullPath, quote = F, row.names = F)
  uploadToRemote(tempFileFullPath, remoteFullPath, userName, password, sshURL)
  file.remove(tempFileFullPath)
}

#' @name uploadToRemote
#' @title push file to server.
#' @description Function that upload file on a remote server.
#' @keywords internal
#' @param localFullPath local file to upload.
#' @param remoteFullPath remote file path and name.
#' @param userName user name on remote host.
#' @param password user password on remote host.
#' @param sshURL url to remote host.
#' @author Italo Garleni
uploadToRemote <- function(localFullPath, remoteFullPath, userName, 
                               password, sshURL)
{
  browser()
  scpQuery <- paste0("pscp -pw ", password," ", localFullPath, " ", userName,
                     "@", sshURL, ":", remoteFullPath)
  haveErrors <- system(scpQuery)
  if(!haveErrors)
    file.remove(fileName)
  else
    stop(paste("Fail on upload to remote server. You can find the file on",
               localFullPath), call. = FALSE)
}
