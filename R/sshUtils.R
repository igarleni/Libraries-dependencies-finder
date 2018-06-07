#' @name readCSVFromRemote
#' @title parse csv file from remote server.
#' @description Function that parses a csv file from a remote server, and 
#'  and convert it to data.frame.
#' @keywords internal
#' @param remoteFullPath remote file path and name.
#' @param userName user name on remote host.
#' @param password user password on remote host.
#' @param sshURL url to remote host (IP not permitted).
#' @return a data.frame
#' @author Italo Garleni
readCSVFromRemote <- function(remoteFullPath, userName, password, sshURL)
{
  tempPath <- tempdir()
  downloadFromRemote(tempPath, remoteFullPath, userName, password, sshURL)
  fileName <- basename(remoteFullPath)
  tempFileFullPath <- paste0(tempPath, "/", fileName)
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
#' @param remoteFullPath remote file path and name (without "~/" or "/" if you
#'  want to start from your home direcory).
#' @param userName user name on remote host.
#' @param password user password on remote host.
#' @param sshURL url to remote host (IP not permitted).
#' @author Italo Garleni
writeCSVToRemote <- function(inputData, remoteFullPath, userName, password, 
                             sshURL)
{
  tempPath <- tempdir()
  fileName <- basename(remoteFullPath)
  tempFileFullPath <- paste0(tempPath, "/", fileName)
  write.csv2(inputData, tempFileFullPath, quote = F, row.names = F)
  uploadToRemote(tempFileFullPath, remoteFullPath, userName, password, sshURL)
  file.remove(tempFileFullPath)
}

#' @name testServerConnection
#' @title test server connection.
#' @description Function that upload file on a remote server and then remove it.
#' @keywords internal
#' @param userName user name on remote host.
#' @param password user password on remote host.
#' @param sshURL url to remote host (IP not permitted).
#' @author Italo Garleni
testServerConnection <- function(userName, password, sshURL)
{
  tempPath <- tempdir()
  fileName <- "connectionTest"
  tempFileFullPath <- paste0(tempPath, "/", fileName)
  write("connectionTest", tempFileFullPath)
  tryCatch(
    {
      uploadToRemote(tempFileFullPath, "", userName, password,
                     sshURL)
      deleteFromRemote(fileName, userName, password, sshURL)
    },
    error = function(e)
    {
      stop("Can't connect to server. Check username/password/URL.", call. = F)
    },
    finally = 
    {
      file.remove(tempFileFullPath)
    }
  )
}

#' @name uploadToRemote
#' @title push file to server.
#' @description Function that upload file on a remote server.
#' @keywords internal
#' @param localFullPath local file to upload.
#' @param remoteFullPath remote file path and name (without "~/" or "/" if you
#'  want to start from your home direcory).
#' @param userName user name on remote host.
#' @param password user password on remote host.
#' @param sshURL url to remote host (IP not permitted).
#' @author Italo Garleni
uploadToRemote <- function(localFullPath, remoteFullPath, userName, 
                           password, sshURL)
{
  scpQuery <- paste0("pscp -pw ", password, " ", localFullPath, " ", userName,
                     "@", sshURL, ":", remoteFullPath)
  haveErrors <- suppressWarnings(system(scpQuery))
  if(haveErrors)
    stop(paste("Fail on upload to remote server. You can find the file on",
               localFullPath), call. = FALSE)
}


#' @name downloadFromRemote
#' @title pull file from server.
#' @description Function that download file on a remote server.
#' @keywords internal
#' @param localFullPath local path to downolad in.
#' @param remoteFullPath remote file path and name (without "~/" or "/" if you
#'  want to start from your home direcory).
#' @param userName user name on remote host.
#' @param password user password on remote host.
#' @param sshURL url to remote host (IP not permitted).
#' @author Italo Garleni
downloadFromRemote <- function(localFullPath, remoteFullPath, userName, 
                           password, sshURL)
{
  scpQuery <- paste0("pscp -pw ", password," ", userName, "@", sshURL, ":",
                     remoteFullPath, " ", localFullPath)
  haveErrors <- suppressWarnings(system(scpQuery))
  if(haveErrors)
    stop("Fail on download to remote server.", call. = FALSE)
}

#' @name deleteFromRemote
#' @title remove file from server.
#' @description Function that delete file from a remote server.
#' @keywords internal
#' @param remoteFullPath remote file path and name (without "~/" or "/" if you
#'  want to start from your home direcory).
#' @param userName user name on remote host.
#' @param password user password on remote host.
#' @param sshURL url to remote host (IP not permitted).
#' @author Italo Garleni
deleteFromRemote <- function(remoteFullPath, userName, password, sshURL)
{
  deleteQuery <- paste0("plink -ssh -pw ", password, " ", userName,
                        "@", sshURL, " rm ", remoteFullPath)
  haveErrors <- suppressWarnings(system(deleteQuery))
  if(haveErrors)
    stop("Fail on remove from remote server.", call. = FALSE)
}

#' @name runRCodeOnRemote
#' @title runs R code on server.
#' @description Function runs R code on remote server.
#' @keywords internal
#' @param rCode string with R code to evaluate.
#' @param userName user name on remote host.
#' @param password user password on remote host.
#' @param sshURL url to remote host (IP not permitted).
#' @author Italo Garleni
runRCodeOnRemote <- function(rCode, userName, password, sshURL)
{
  tempPath <- tempdir()
  fileName <- "tempScript.R"
  tempFileFullPath <- paste0(tempPath, "/", fileName)
  write(rCode, tempFileFullPath)
  tryCatch(
    {
      uploadToRemote(tempFileFullPath, "", userName, password, sshURL)
      rScriptQuery <- paste0("plink -ssh -pw ", password, " ", userName,
                             "@", sshURL, " Rscript ", fileName)
      haveErrors <- suppressWarnings(system(rScriptQuery))
      deleteFromRemote(fileName, userName, password, sshURL)
      if(haveErrors)
        stop(call. = FALSE)
    },
    error = function(e)
    {
      stop("Fail on running code on remote server.", call. = FALSE)
    },
    finally =
    {
      file.remove(tempFileFullPath)
    }
  )
  
}
