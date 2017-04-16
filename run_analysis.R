
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only = TRUE, quietly = TRUE)
files <- 'C:/Users/WIN7/Documents/FIB/Coursera'

#download the file
  
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Data.zip"
if (!file.exists(files)) {
  dir.create(files)
}
download.file(url, file.path(files, f))

Input <- "C:/Users/WIN7/Documents/FIB/Coursera/Data/UCI HAR Dataset"
list.files(Input, recursive = TRUE)

#read the file and put the data into variables

Subject_Train <- fread(file.path(Input, "train", "subject_train.txt"))
Subject_Test <- fread(file.path(Input, "test", "subject_test.txt"))

Y_Train <- fread(file.path(Input, "train", "Y_train.txt"))
Y_Test <- fread(file.path(Input, "test", "Y_test.txt"))

fonction <- function(f) {
  df <- read.table(f)
  dt <- data.table(df)
}
X_Train <- fonction(file.path(Input, "train", "X_train.txt"))
X_Test <- fonction(file.path(Input, "test", "X_test.txt"))

#Merging the data and concatenate datatables

Subject <- rbind(Subject_Train, Subject_Test)
setnames(Subject, "V1", "subject")
Y <- rbind(Y_Train, Y_Test)
setnames(Y, "V1", "Num")
X <- rbind(X_Train, X_Test)

Subject <- cbind(Subject, Y)
X <- cbind(Subject, X)
#we set the key
setkey(X, subject, results)

Features <- fread(file.path(Input, "features.txt"))
setnames(Features, names(Features), c("Num", "Name"))

Features <- Features[grepl("mean\\(\\)|std\\(\\)", Name)]

# we Convert the column numbers 
Features$Code <- Features[, paste0("V", Num)]
head(Features)

Features$Code

Activity_labels <- fread(file.path(Input, "activity_labels.txt"))
setnames(Activity_labels, names(Activity_labels), c("Num", "Name"))

X <- merge(X, Activity_labels, by = "Num", all.x = TRUE)

# we add the num and the name as a key
setkey(X, subject, Num, Name)

X <- data.table(melt(X, key(X), variable.name = "Code"))

X <- merge(X, Features[, list(Num, Code, Name)], by = "Code",  all.x = TRUE)

# the goal of fonction 2 is to seperate the differents features.
fonction2 <- function(a) {
  grepl(a, X$Name)
}

n <- 2
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(fonction2("^t"), fonction2("^f")), ncol = nrow(y))
X$Domain <- factor(x %*% y, labels = c("Time"))
x <- matrix(c(fonction2("Acc"), fonction2("Gyro")), ncol = nrow(y))
X$Instrument <- factor(x %*% y, labels = c("Accelerometer"))
x <- matrix(c(fonction2("BodyAcc"), fonction2("GravityAcc")), ncol = nrow(y))
X$Acceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x <- matrix(c(fonction2("mean()"), fonction2("std()")), ncol = nrow(y))
X$Variable <- factor(x %*% y, labels = c( "SD"))

X$Jerk <- factor(fonction2("Jerk"), labels = c( "fJerk"))
X$Magnitude <- factor(fonction2("Mag"), labels = c(NA, "Magnitude"))

n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(fonction2("-X"), fonction2("-Y"), fonction2("-Z")), ncol = nrow(y))
X$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))


r1 <- nrow(X[, .N, by = c("Name")])
r2 <- nrow(X[, .N, by = c("Domain", "Acceleration", "Instrument", "fJerk", "Magnitude", "Variable", "Axis")])
r1 == r2
setkey(X, subject, Name, Domain, Acceleration, Instrument, Jerk,Magnitude, Variable)

# creation of the tidydataset
XTidy <- X[, list(count = .N, average = mean(value)), by = key(X)]




