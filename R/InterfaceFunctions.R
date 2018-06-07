#' @name updateDependencies
#' @title (main) Add libraries to Libraries list or initiazlite it.
#' @description Function reads a csvFile with almost 1 column named "Package"
#'  (if provided), and a vector of libraries. Then, it executes getDependencies
#'  function with both libraries lists joined, and the result on a new csv file.
#'  It also test the results for better reliability. The csv file returned has 2
#'  columns: numeric id and "Package".
#' @seealso getDependencies
#' @export
#' @param newLibraries a character vector of libraries.
#' @param outputFileName path of the output csv file.
#' @param fileName (optional) the path to a csv with libraries.
#' @author Italo Garleni
updateDependencies <- function(newLibraries, outputFileName, fileName = NULL)
{
  if(is.null(fileName))
  {
    libraries <- newLibraries
  }
  else
  {
    libraries <- read.csv(fileName, sep = ";", stringsAsFactors = F)
    libraries <- libraries[["Package"]]
    libraries <- c(libraries, newLibraries)
  }
  librariesWithDependencies <- getDependencies(libraries)
  librariesData <- data.frame(Package = librariesWithDependencies)
  write.csv2(librariesData, outputFileName, quote = F, row.names = F)
}

#' @name updateVersion
#' @title (main) Read csv file with libraries and updates its version.
#' @description Function reads a csv file with almost 1 column named "Package".
#'  Then, it gets the version of each package on that column and generates a new
#'  column with this data. After this, saves this file on the path defined by 
#'  user.
#' @keywords internal
#' @param fileName the path to a csv with libraries.
#' @param outputFileName path of the output csv file.
#' @author Italo Garleni
updateVersion <- function(fileName, outputFileName)
{
  libraries <- read.csv(fileName, sep = ";", stringsAsFactors = FALSE)
  libraries$Version <- sapply(libraries$Package, getVersion)
  write.csv2(libraries, outputFileName, quote = F, row.names = F)
}

#' @name remoteUpdateMiniCran
#' @title (main) Updates miniCran file on a remote server.
#' @description Function download a csvFile with almost 1 column named "Package"
#'  (if provided), and a vector of libraries. Then, it executes getDependencies
#'  function locally with both libraries lists joined, and uploadthe resulting
#'  csv file on the remote server. Finally, it updates the version column with
#'  libraries version on this server.
#' @export
#' @param newLibraries a list of new libraries the user wants to add to the
#'  previous one.
#' @param remoteOutputFullPath name of the remote output csv file (without "~/" 
#'  or "/" if you want to start from your home direcory).
#' @param userName user name on remote host.
#' @param password user password on remote host.
#' @param sshURL url to remote host (IP not permitted).
#' @param remoteOldFile (optional) path to a remote csv with libraries.
#' @author Italo Garleni
remoteUpdateMiniCran <- function(newLibraries, remoteOutputFullPath,
                                       userName, password, sshURL,
                                       remoteOldFile = NULL)
{
  testServerConnection(userName, password, sshURL)
  if(is.null(remoteOldFile))
  {
    libraries <- newLibraries
  }
  else
  {
    libraries <- readCSVFromRemote(remoteOldFile, userName, password, sshURL)
    libraries <- as.character(libraries[["Package"]])
    libraries <- c(libraries, newLibraries)
  }
  librariesWithDependencies <- getDependencies(libraries)
  librariesData <- data.frame(Package = librariesWithDependencies)
  writeCSVToRemote(librariesData, remoteOutputFullPath, userName, password,
                       sshURL)
  rExpression <- paste0("DependenciesFinder::updateVersion('",
                        remoteOutputFullPath, "', '", remoteOutputFullPath, "')")
  runRCodeOnRemote(rExpression, userName, password, sshURL)
}
