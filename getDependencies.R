#########################################################################
############################ getDependencies ############################ 
#########################################################################
library(miniCRAN)

#' @name addLibraries
#' @description Function reads a csvFile with almost 1 column named "Package" (if provided), and a vector of 
#' libraries. Then, it executes getDependencies function with both libraries list joined and save it on a new
#' csv file named "miniCran.csv". It also test the results for better reliability. The csv file returned has 3 
#' columns: numeric id, "Package".
#' @import miniCRAN
#' 
#' @param fileName: the path to the library csv (non mandatory)
#' @param newLibraries: a list of new libraries the user wants to add to the previous one.
#' @seealso minicran.csv, on working directory.
#' 
#' @author Italo Garleni
#' 
addLibraries = function(newLibraries, fileName = NULL)
{
  if(is.null(fileName))
  {
    libraries = newLibraries
  }
  else
  {
    libraries = read.csv(fileName, sep = ";")["Package"]
    libraries = as.character(unlist(libraries))
    libraries = c(libraries, newLibraries)
  }
  listDependencies = getDependencies(libraries)
  testDependencies(listDependencies)
  
  dfDependencies = as.data.frame(listDependencies)
  names(dfDependencies) = c("Package")
  write.csv2(dfDependencies,"miniCran.csv",quote = FALSE)
}


#' @name addVersion
#' @description Function reads a csvFile with almost 1 column named "Package". Then, it checks libraries
#' version on local machine and save it on a new csv file named "miniCranVersion.csv". The csv file
#' returned has 3 columns: numeric id, "Package", and "Version".
#' @import miniCRAN
#' 
#' @param fileName: the path to the library csv.
#' @seealso miniCranVersion.csv, on working directory. 
#' 
#' @author Italo Garleni
#' 
addVersion = function(fileName)
{
  libraries = read.csv(fileName, sep = ";")["Package"]
  libraries["Version"] = "NotFound"
  for(libraryId in 1:nrow(libraries))
    libraries[libraryId,2] = tryCatch(
      {
        packageDescription(libraries[libraryId,1])$Version
      },
      error = function(e) {
        print(paste0("library ", library, " not found!"))
        "NotFound"
      }
    )
  write.csv2(libraries,"miniCranVersion.csv",quote = FALSE)
}


#' @name getDependencies
#' @description Function that returns libraries' dependencies over a list of libraries, sorted by
#'  its dependency. Being ['n'=length(libraries)] and [1<='i'<='n'], library 'i' depends (or not)
#'  on the previous '1' to 'i' libraries, but never the other way around.
#' @import miniCRAN
#' 
#' @param libraries: a list of libraries the user wants to analyze.
#' @return vector with dependencies and library, sorted by its dependency.
#' 
#' @author Italo Garleni
#' 
getDependencies = function(libraries)
{
  # Obtener dependencias del primero
  firstLibrary = libraries[1]
  lastLibraries = libraries[-1]
  
  firstLibraryDependencies = tryCatch(
    {
      pkgDep(firstLibrary, suggests=F, includeBasePkgs=F, depends = T)
    },
    error = function(e) {
      firstLibrary
    }
  )
  
  if( length(firstLibraryDependencies) > 1)
  {
    print(paste0("------", firstLibrary, " have multiple dependencies..."))
    firstLibraryDependencies = firstLibraryDependencies[-1]
    firstLibraryDependencies = c(getDependencies(firstLibraryDependencies), firstLibrary)
  }
  else
  {
    print(paste0(firstLibraryDependencies, " doesnt have dependencies."))
  }
  
  if(length(libraries) > 1)
  {
    lastLibraryDependencies = getDependencies(libraries[-1])
    duplicatedDependencies = intersect(firstLibraryDependencies, lastLibraryDependencies)
    finalDependencies = firstLibraryDependencies[!firstLibraryDependencies %in% duplicatedDependencies]
    finalDependencies = c(lastLibraryDependencies, finalDependencies)
  }
  else
    finalDependencies = firstLibraryDependencies
  
  finalDependencies
}


#' @name testDependencies
#' @description Function reads a csvFile with almost 1 column named "Package", and a vector of libraries.
#' Then, it executes getDependencies function with both libraries list joined and save it on a new csv file
#' named "newMiniCran.csv". It also test the results for better reliability. The csv file returned has 3 
#' columns: numeric id, "Package".
#' @import miniCRAN
#' 
#' @param listDependencies: a list of libraries the user wants to check.
#' @seealso standard output (console)
#' 
#' @author Italo Garleni
#' 
testDependencies = function(listDependencies)
{
  for(i in 1:length(listDependencies))
  {
    dependencies = tryCatch(
      {
        pkgDep(listDependencies[i], suggests=F, includeBasePkgs = F, depends = T)
      },
      error = function(e) {
        listDependencies[i]
      })
    intersection = dependencies %in% listDependencies[1:i]
    if(!all(intersection))
    {
      print(paste0("ERROR: dependency ",dependencies[!intersection]," of library ",listDependencies[i] ," not found!"))
    }
    else
    {
      print(paste0(listDependencies[i]," validated."))
    }
  }
}
