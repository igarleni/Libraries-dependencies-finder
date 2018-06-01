#' @name getVersion
#' @title parse version of Library.
#' @description Function that parses the version of library installed on 
#'  local R.
#' @export
#' @import miniCRAN
#' @param library library name.
#' @return library version or "NotFound"
#' @author Italo Garleni
getVersion <- function(library)
{
  version <- tryCatch(
    {
      packageDescription(library)$Version
    },
    error = function(e)
    {
      print(paste("Library", library, "not found!"))
      "NotFound"
    }
  )
  return(version)
}

#' @name getDependencies
#' @title Search for libraries dependencies.
#' @description Function that returns libraries' dependencies over a list of
#'  libraries, sorted by its dependency. Being ['n'=length(libraries)] and
#'  [1<='i'<='n'], library 'i' depends (or not) on the previous '1' to 'i'
#'  libraries, but never the other way around.
#' @import miniCRAN
#' @export
#' @param libraries a list of libraries the user wants to analyze.
#' @return vector with dependencies and library, sorted by its dependency.
#' @author Italo Garleni
getDependencies <- function(libraries)
{
  # Obtener dependencias del primero
  firstLibrary <- libraries[1]
  lastLibraries <- libraries[-1]
  
  firstLibraryDependencies <- tryCatch(
    {
      pkgDep(firstLibrary, suggests = F, includeBasePkgs = F, depends = T)
    },
    error = function(e) {
      firstLibrary
    }
  )
  
  if( length(firstLibraryDependencies) > 1)
  {
    print(paste0("------", firstLibrary, " have multiple dependencies..."))
    firstLibraryDependencies <- firstLibraryDependencies[-1]
    firstLibraryDependencies <- c(getDependencies(firstLibraryDependencies),
                                  firstLibrary)
  }
  else
  {
    print(paste0(firstLibraryDependencies, " doesnt have dependencies."))
  }
  
  if(length(libraries) > 1)
  {
    lastLibraryDependencies <- getDependencies(libraries[-1])
    duplicatedDependencies <- intersect(firstLibraryDependencies, 
                                        lastLibraryDependencies)
    finalDependencies <- firstLibraryDependencies[
      !firstLibraryDependencies %in% duplicatedDependencies]
    finalDependencies <- c(lastLibraryDependencies, finalDependencies)
  }
  else
    finalDependencies <- firstLibraryDependencies
  
  finalDependencies
}

#' @name testDependencies
#' @title Check if libraries dependencies exists and are sorted
#' @description Function reads a list of dependencies and tests if they exists
#'  and are sorted by its dependency. Being ['n'=length(libraries)] and
#'  [1<='i'<='n'], library 'i' depends (or not) on the previous '1' to 'i'
#'  libraries, but never the other way around.
#' @import miniCRAN
#' @export
#' @param listLibraries a list of libraries the user wants to check.
#' @seealso standard output (console)
#' @author Italo Garleni
testDependencies <- function(listLibraries)
{
  for(i in 1:length(listLibraries))
  {
    dependencies <- tryCatch(
      {
        pkgDep(listLibraries[i], suggests=F,
               includeBasePkgs = F, depends = T)
      },
      error = function(e) {
        listLibraries[i]
      })
    intersection <- dependencies %in% listLibraries[1:i]
    if(!all(intersection))
    {
      print(paste0("ERROR: dependency ", dependencies[!intersection],
                   " of library ",listLibraries[i] , " not found!"))
    }
    else
    {
      print(paste0(listLibraries[i]," validated."))
    }
  }
}
