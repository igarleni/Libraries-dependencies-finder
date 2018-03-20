# Libraries-dependencies-finder

###### Get dependencies in an easy way

This helpful tool will **look for your libraries dependencies** and **sort them by its dependency** without efford. You may need to list your dependencies on a csv file, so you can plot them on your future script report. If you need to do this, I am your script!

There are 4 functions you can use. **The main one is getDependencies**, but there are other 3 functions that will find your Libraries version, or add new Libraries to an old Libraries list you may have:

### 1. getDependencies:
This main function reads a vector of libraries and returns libraries' dependencies of a list of libraries, sorted by its dependency. Being ['n'=length(libraries)] and [1<='i'<='n'], library 'i' depends (or not)  on the previous '1' to 'i' libraries, but never the other way around.

### 2. addLibraries:
It reads a csvFile with almost 1 column named "Package", and a vector of libraries. Then, it executes getDependencies function with both libraries list joined and save it on a new csv file named "miniCran.csv". It also test the results for better reliability. The csv file returned has 3  columns: numeric id, "Package".

### 3. testDependencies:
This function reads a csvFile with almost 1 column named "Package", and a vector of libraries. Then, it executes getDependencies function with both libraries list joined and save it on a new csv file named "newMiniCran.csv". It also test the results for better reliability. The csv file returned has 3 columns: numeric id, "Package".

### 4. addVersion:
It reads a csvFile with almost 1 column named "Package". Then, it checks libraries version on local machine and save it on a new csv file named "miniCranVersion.csv". The csv file returned has 3 columns: numeric id, "Package", and "Version".