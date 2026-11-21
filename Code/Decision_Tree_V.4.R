####### Intialization

DataSet = read.csv("C:\\Users\\Cyrus\\Documents\\Math_180_project\\Latest Data as of 11.20.csv",
                   na.strings = "#N/A", stringsAsFactors = TRUE)

NoNADataSet = DataSet[complete.cases(DataSet),]

PlayerNames = DataSet$Player
DataSet = DataSet[,-1]

# The code commented out below ended up not being needed

# Changing the players with no draft year or number to factor "Undrafted"
#CHARVECYEAR = as.character(DataSet$Draft.Year)
#CHARVECNUM = as.character(DataSet$Draft..)
#CHARVECYEAR[is.na(CHARVECYEAR)] = "Undrafted"
#CHARVECNUM[is.na(CHARVECNUM)] = "Undrafted"
#DataSet$Draft.Year = as.factor(CHARVECYEAR)
#DataSet$Draft.. = as.factor(CHARVECNUM)


# Individual data set selected by position 
#Center = read.csv("C:\\Users\\Cyrus\\Documents\\Math_180_project\\Data Split by Position\\Center.csv")
Center = DataSet[DataSet$Position == "C",]
PointG = DataSet[DataSet$Position == "PG",]
PowerF = DataSet[DataSet$Position == "PF",]
ShootG = DataSet[DataSet$Position == "SG",] 
SmallF = DataSet[DataSet$Position == "SF",] 
#PointG = read.csv("C:\\Users\\Cyrus\\Documents\\Math_180_project\\Data Split by Position\\Point Gaurd.csv")
#PowerF = read.csv("C:\\Users\\Cyrus\\Documents\\Math_180_project\\Data Split by Position\\Power Forward.csv")
#ShootG = read.csv("C:\\Users\\Cyrus\\Documents\\Math_180_project\\Data Split by Position\\Shooting Gaurd.csv")
#SmallF = read.csv("C:\\Users\\Cyrus\\Documents\\Math_180_project\\Data Split by Position\\Small Forward.csv")


set.seed(1)
library(tree)
library(randomForest)
library(randomForestExplainer)
library(gbm3)

Checksums = vector()
PosDataSetYES = list()
PosDataSetNO = list()
YESIndexList = list()
#YESM = matrix()


# Combining the position data sets into a list (a list of data frames)
PosDataSet = list(Center, PointG, PowerF, ShootG, SmallF)


####### Splitting the data up into All Stars and Non-All Stars for train, test
####### validation splits

# This function loops through the list 'PosDataSet' and for each data frame in
# the list it loops through each row, checks if "YES" occurs in column 3 (All 
# Star "YES" or "NO"). If "YES" does appear, the row number is stored in 
# 'YESIndex', then the first row is removed from 'YESIndex' (as when initialized
# the first row is NA). Then all the rows with "YES" from PosDataSet[[f]] are 
# stored in 'PosDataSetYES[[f]]' by indexing by 'YESIndex. Finally, the 
# 'YESIndex' is stored in a list, 'YESIndexList' for use later.

# 'YESIndex' is a vector, 'YESIndexList' is a list of vectors, 'PosDataSetYES' 
# is a list of dataframes

for( f in 1:length(PosDataSet) ){
  YESIndex = vector()
  for( r in 1:nrow( PosDataSet[[f]] ) ){
    if( PosDataSet[[f]][r,2] == "Yes" ){
      YESIndex = append(YESIndex, r)
    }
  }
  YESIndex[-1] 
  PosDataSetYES[[f]] = PosDataSet[[f]][ YESIndex, ]
  YESIndexList[[f]] = YESIndex
}

# Gets every row in 'PosDataSet' that has "No" in column 3 and stores it in 
# 'PosDataSetNO'. Taken from 'YESIndexList'

for( f in 1:length(PosDataSet) ){
  PosDataSetNO[[f]] = PosDataSet[[f]][ -YESIndexList[[f]], ]
}

# Checking the sums of 'PosDataSetNo' and 'PosDataSetYes' to make sure they 
# match 'PosDataSet'


for( f in 1:length(PosDataSet) ){
  if( nrow(PosDataSetYES[[f]]) + nrow(PosDataSetNO[[f]]) == 
      nrow(PosDataSet[[f]])){
    Checksums[f] = TRUE
  } else
    Checksums[f] = FALSE
}

if(!all(Checksums)){
  stop("YES and NO datasets do not match orginal")
}

####### Splitting Position data set and whole data set into 1/3s for train, 
####### test, validation

# Split for the list of data frames Yes and No

for( f in 1:length(PosDataSet) ){
  
  SplitY = sample(1:nrow(PosDataSetYES[[f]]), nrow(PosDataSetYES[[f]]) / 3)
  SplitN = sample(1:nrow(PosDataSetNO[[f]]), nrow(PosDataSetNO[[f]]) / 3)
  
  SplitY2 = sample(1:nrow(PosDataSetYES[[f]][-SplitY,]), nrow(PosDataSetYES[[f]][-SplitY,]) / 2)
  SplitN2 = sample(1:nrow(PosDataSetNO[[f]][-SplitN,]), nrow(PosDataSetNO[[f]][-SplitN,]) / 2)
  
  
}

SplitW = sample(1:nrow(PosDataSet), nrow(PosDataSet) / 3)
SplitW2 = sample(1:nrow(PosDataSet[-SplitW,]), nrow(PosDataSet[-SplitW,]) / 2)

BTree = tree(AllStar ~. -Draft.Year - Draft.., DataSet)
plot(BTree)
text(BTree, pretty = 1, srt = 10, cex = .65, col = "red")
