#' @name generateMiniCran
#' @title Add libraries to Libraries list or initiazlite it.
#' @description Function reads a csvFile with almost 1 column named "Package"
#'  (if provided), and a vector of libraries. Then, it executes getDependencies
#'  function with both libraries lists joined, and the result on a new csv file.
#'  It also test the results for better reliability. The csv file returned has 2
#'  columns: numeric id and "Package".
#' @export
#' @param newLibraries a list of new libraries the user wants to add to the
#'  previous one.
#' @param outputFileName name of the output csv file.
#' @param fileName (optional) the path to a csv with libraries.
#' @author Italo Garleni
generateMiniCran <- function(newLibraries, outputFileName, fileName = NULL)
{
  if(is.null(fileName))
  {
    libraries <- newLibraries
  }
  else
  {
    libraries <- read.csv(fileName, sep = ";")
    libraries <- as.character(libraries[["Package"]])
    libraries <- c(libraries, newLibraries)
  }
  librariesWithDependencies <- getDependencies(libraries)
  librariesData <- data.frame(Package = librariesWithDependencies)
  write.csv2(librariesData, outputFileName, quote = FALSE)
}

#' @name updateVersion
#' @title Read miniCran and updates its version.
#' @description Function reads a csvFile with almost 1 column named "Package".
#'  Then, it gets the version of each package on that column and generates a new
#'  column with this data. After this, saves this file on the path defined by 
#'  user.
#' @export
#' @param fileName the path to a csv with libraries.
#' @param outputFileName path of the output csv file.
#' @author Italo Garleni
updateVersion <- function(fileName, outputFileName)
{
  libraries <- read.csv(fileName, sep = ";", stringsAsFactors = FALSE)
  libraries$Version <- sapply(libraries$Package, getVersion)
  write.csv2(librariesData, outputFileName, quote = FALSE)
}

#' @name updateMiniCranOnServer
#' @title Add libraries to Libraries list or initiazlite it
#' @description Function reads a csvFile with almost 1 column named "Package"
#'  (if provided), and a vector of libraries. Then, it executes getDependencies
#'  function with both libraries lists joined, and save it on a new csv file. It
#'  also test the results for better reliability. The csv file returned has 2
#'  columns: numeric id and "Package".
#' @export
#' @param newLibraries a list of new libraries the user wants to add to the
#'  previous one.
#' @param outputRemoteFile name of the output csv file.
#' @param fileName (optional) the path to a csv with libraries.
#' @author Italo Garleni
updateMiniCranOnServer <- function(newLibraries, outputRemotePath,
                                   outputFileName, userName, password, sshURL,
                                   remoteOldFile = NULL)
{
  if(is.null(remoteOldFile))
  {
    libraries <- newLibraries
  }
  else
  {
    libraries <- parseRemoteCSVFile(remoteOldFile, userName, password, sshURL)
    libraries <- as.character(libraries[["Package"]])
    libraries <- c(libraries, newLibraries)
  }
  librariesWithDependencies <- getDependencies(libraries)
  librariesData <- data.frame(Package = librariesWithDependencies)
  writeToRemoteCSVFile(librariesData, outputRemotePath, outputFileName, 
                       userName, password, sshURL)
  #'@TODO run updateVersion on remote server
}
