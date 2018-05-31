# Libraries-dependencies-finder

###### Get dependencies in an easy way

This helpful tool will **look for your libraries dependencies** and **sort them by its dependency** without efford. You may need to list your libraries and its dependencies on a csv file, so you can plot them on your future script report. If you need to do this, I am your script!


## Typical usages

**The initial use you should do** with this script may be **look for your first libraries and its dependencies** to generate an initial csv list. You can do this by creating an vector of libraries you want to add, and then get its versions (if you want to add this column):
```R
addLibraries(c("dplyr","xml2"), "miniCran.csv")  # Generates "miniCran.csv" file with this two libraries and its dependencies

addVersion("miniCran.csv", "miniCranVersion.csv")  # Generates "miniCranVersion.csv" with versions & libraries names

```


Now, imagine that **you are modifying your scripts** by using new libraries, for example _"ggplot2"_ and _"caret"_. You also have list of libraries on a csv called _"oldMiniCran.csv"_, and **you want to add** this **new libraries** you are currently using. This is how you should proceed to add them to your dependencies list:
```R
addLibraries(c("ggplot2","caret"), "miniCran.csv", "oldMiniCran.csv")  # Generates "miniCran.csv"

addVersion("miniCran.csv", "miniCranVersion.csv")  # Generates "miniCranVersion.csv" with versions & libraries names

```

The last typical usage is when you may just want a vector of dependencies, so you can directly use getDependencies function by passing it the libraries vector.
```R
dependenciesList = getDependencies(c("dplyr","xml2"))
```

## Function description

There are 4 functions you can use. **The main one is getDependencies**, but there are other 3 functions that will find your Libraries version, or add new Libraries to an old Libraries list you may have:

#### 1. getDependencies:
This main function reads a vector of libraries and returns libraries' dependencies of a list of libraries, sorted by its dependency. Being ['n'=length(libraries)] and [1<='i'<='n'], library 'i' depends (or not)  on the previous '1' to 'i' libraries, but never the other way around.

#### 2. addLibraries:
It reads reads a csvFile with almost 1 column named "Package" (if provided), and a vector of libraries. Then, it executes getDependencies function with both libraries list joined and save it on a new csv file named "miniCran.csv". It also test the results for better reliability. The csv file returned has 2  columns: numeric id, "Package".

#### 3. testDependencies:
This function reads a vector of libraries. Then, it executes look each dependency and looks if it is correctly sorted.

### 4. addVersion:
It reads a csvFile with almost 1 column named "Package". Then, it checks libraries version on local machine and save it on a new csv file named "miniCranVersion.csv". The csv file returned has 3 columns: numeric id, "Package", and "Version".

