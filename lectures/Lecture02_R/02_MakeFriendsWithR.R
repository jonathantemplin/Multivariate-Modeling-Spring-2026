# MAKE FRIENDS WITH R
# BASED ON MAKE FRIENDS WITH SAS BY LESA HOFFMAN
# CREATED BY JONATHAN TEMPLIN

# R SYNTAX AND NAMING CONVENTIONS =============================================


# R comments begin with a # -- there are no multiline comments

# RStudio helps you build syntax
#   GREEN: Comments and character values in single or double quotes

# You can use the tab key to complete object names, functions, and arugments

# R is case sensitive. That means R and r are two different things.

# R Functions =================================================================

# In R, every statement is a function

# The print function prints the contents of what is inside to the console
print(x = 10)

# The terms inside the function are called the arguments; here print takes x
#   To find help with what the arguments are use:
?print

# Each function returns an object
print(x = 10)

# You can determine what type of object is returned by using the class function
class(print(x = 10))

# R Objects ===================================================================

# Each objects can be saved into the R environment (the workspace here)
#   You can save the results of a function call to a variable of any name
MyObject = print(x = 10)
class(MyObject)

# You can view the objects you have saved in the Environment tab in RStudio
# Or type their name
MyObject

# There are literally thousands of types of objects in R (you can create them),
#   but for our course we will mostly be working with data frames (more later)

# The process of saving the results of a function to a variable is called 
#   assignment. There are several ways you can assign function results to 
#   variables:

# The equals sign takes the result from the right-hand side and assigns it to
#   the variable name on the left-hand side:
MyObject = print(x = 10)

# The <- (Alt "-" in RStudio) functions like the equals (right to left)
MyObject2 <- print(x = 10)

identical(MyObject, MyObject2)

# The -> assigns from left to right:
print(x = 10) -> MyObject3

identical(MyObject, MyObject2, MyObject3)

# R Packages ==================================================================

# R allows users to install (or create) packages: Libraries of functions that
#   allow for all types of data analyses and other numerical things. Packages
#   also may include data files, help files, and examples.

# To download and install a package, use the install.packages() function. The 
#   case-sensitive name of the package must be enclosed in quotes. You must 
#   have an internet connection for the package to download. You only need 
#   to download the package once per computer you are using.

install.packages(pkgs = "psych")

# Importing and Exporting Data into R data frame objects ======================

# The data frame is an R object that is a rectangular array of data. The 
#   variables in the data frame can be any class (e.g., numeric, character)
#   and go across the columns. The observations are across the rows.

# We will start by importing data from a comma-separated values (csv) file.
#   We will use the read.csv() function. Here, the argument stringsAsFactors
#   keeps R from creating data strings
HeightsData = read.csv(file = "heights.csv", stringsAsFactors = FALSE)

# Note: The argument file is the path to the file. If you opened this script
#   directly in RStudio, then the current directory is the directory that
#   contains the script. If the data file is in that directory, you can omit
#   the full path. To find the current directory used in the environment, 
#   use the getwd() function. 
getwd()

# To show the files in that directory, use the dir() function. You can see if
#    the file you are opening is or is not in the current directory.
dir()

# You can also set the directory using setwd(). Here, I set my directory to 
#   my root folder:
setwd("~")

getwd()
dir()

# If I tried to re-load the data, I would get an error:
HeightsData = read.csv(file = "heights.csv", stringsAsFactors = FALSE)

# So, I can use the full path to the file:
HeightsData = 
  read.csv(
    file = "/Users/jonathantemplin/Library/CloudStorage/GoogleDrive-jtempli@g.clemson.edu/My Drive/02. Teaching/2026/EDF 9780 Multivariate/Lectures/02 Make Friends with R/heights.csv", 
    stringsAsFactors = FALSE)

# Or, I can reset the current directory and use the previous syntax:
setwd("/Users/jonathantemplin/Library/CloudStorage/GoogleDrive-jtempli@g.clemson.edu/My Drive/02. Teaching/2026/EDF 9780 Multivariate/Lectures/02 Make Friends with R/")

HeightsData = read.csv(file = "heights.csv", stringsAsFactors = FALSE)

# Note: Windows users will have to either change the direction of the slash
#   or put two slashes between folder levels.

# To show my data in RStudio, I can either double click it in the 
#   Environment tab or use the View() function
View(HeightsData)

# You can see the variable names and contents by using the $:
HeightsData$ID

# To read in SPSS files, we will need the foreign library. The foreign
#   library comes installed with R (so no need to use install.packages()).
library(foreign)

# The read.spss() function imports the SPSS file to an R data frame if the 
#   argument to.data.frame is TRUE
WideData = read.spss(file = "wide.sav", to.data.frame = TRUE)
WideData

# Merging R data frame objects ================================================

# The WideData and HeightsData have the same set of ID numbers. We can use the
#   merge() function to merge them into a single data frame. Here, x is the 
#   name of the left-side data frame and y is the name of the right-side data
#   frame. The arguments by.x and by.y are the name of the variable(s) by 
#   which we will merge:
AllData = merge(x = WideData, y = HeightsData, by.x = "ID", by.y = "ID")
AllData

# Transforming Wide to Long ===================================================

# Sometimes, certain packages require repeated measures data to be in a long
#   format. The below is a function that simplifies the process for
#   us. We first have to create a varlist object that will give directions
#   for which variables will be long-format. These are the time-invariant ones.
varlist = list()
varlist$DV = c("DVTime1", "DVTime2", "DVTime3", "DVTime4")
varlist$DV = paste0("DVTime", 1:4)
varlist$Age = paste0("AgeTime", 1:4)

MultiWide2Long = function(data, idvars, varlist, timevals = 1:length(varlist[[1]]), timevar = "time"){
  
  newData = NULL
  for (var in 1:length(varlist)){
    temp = stats::reshape(data = data[c(idvars, varlist[[var]])], idvar = idvars, varying = varlist[[var]],
                          v.names = names(varlist)[var], direction = "long")
    
    # for first reformat data frame to be correct type
    if (var == 1) {
      if (is.na(timevar)) {
        temp = temp[c(idvars, names(varlist)[var])]
      }
      
      newData = temp
    } else {
      
      newData = cbind(newData, temp[3])
    }
    
  }
  timeNameVar = which(names(newData) == "time")
  names(newData)[timeNameVar] = timevar
  return(newData)
  
}

# We then use the MultiWide2Long function:
AllDataLong = MultiWide2Long(data = AllData, idvars = "ID", varlist = varlist, 
                             timevals = 1:4, timevar = "time")
View(AllDataLong)
# Gathering Descriptive Statistics ============================================

# The psych package makes getting descriptive statistics very easy.
library(psych)

# We can use describe() to get descriptive statistics across all cases:
DescriptivesWide = describe(AllData)
DescriptivesWide

DescriptivesLong = describe(AllDataLong)
DescriptivesLong

# We can use describeBy() to get descriptive statistics by groups:
DescriptivesLongID = describeBy(AllDataLong, group = AllDataLong$ID)
DescriptivesLongID

# Transforming Data ===========================================================

# Transforming data is accomplished by the creation of new variables. The [4]
#  indicates the use of the 4th element of the $mean variable (atomic vector)
AllDataLong$AgeC = AllDataLong$Age - DescriptivesLong$mean[4]

# Here is another way to do the same thing, the term in the [ ] is logical
AllDataLong$AgeC = AllDataLong$Age - 
  DescriptivesLong$mean[which(rownames(DescriptivesLong) == "Age")]

# You can also use functions to create new variables. Here we create new terms
#   using the function for significant digits:
AllDataLong$AgeYear = signif(x = AllDataLong$Age, digits = 2)
AllDataLong$AgeDecade = signif(x = AllDataLong$Age, digits = 1)

