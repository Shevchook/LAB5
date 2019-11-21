# LAB5
For lab 5 only

## Для виконання роботи необхідні бібліотеки:
library(bitops)
library(RCurl)
library(dplyr)

## Підготовка даних:
Зчитування тренувального і тестового датасетів у дата фрейм
train.x <- read.table(file = "./UCI HAR Dataset/train/X_train.txt")
test.x <- read.table(file = "./UCI HAR Dataset/test/X_test.txt")

Зчитування лейблів (від 1 до 6 - що вони там робили) у дата фрейм
train.y <- read.table(file = "./UCI HAR Dataset/train/Y_train.txt")
test.y <- read.table(file = "./UCI HAR Dataset/test/Y_test.txt")

Зчитування номерів волонтерів (від 1 до 30) у дата фрейм
train.sub <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt")
test.sub <- read.table(file = "./UCI HAR Dataset/test/subject_test.txt")

Зчитування назв 561 змінних
feature <- read.table("./UCI HAR Dataset/features.txt")

## Створення датафрейму
Об’єднання навчального і тестового наборів
x <- rbind(train.x, test.x) # складання докупи даних
y <- rbind(train.y, test.y) # складання 6 видів активності
sub <- rbind(train.sub, test.sub) # складання 30 волонтерів

Об’єднання волонтерів, активності і даних в один набір даних
data <- cbind(sub, y, x)

Приєднання назв змінних "SubjectID" - назва першого стовпчика, "activity" -
другого, feature$V2 - назви решти (береться зі зробленого раніше датафрейму)
names(data) <- c("SubjectID", "activity", as.character(feature$V2))

## Витягнути середнє та стандартне відхилення для кожного вимірювання

Вибрати назви стовпчиків, які містять "-mean" або "-std"
FindMeanSd <- feature$V2[grep("-mean\\(\\)|-std\\(\\)", feature[, 2])]

Сворити вектор назв цих стовпчиків + назви 2-х перших стовпчиків датафрейму
SelectColumns <- c("SubjectID", "activity", as.character(FindMeanSd))

Створити сабсет з середніми та стандартними відхиленнями
SelectData <- subset(data, select = SelectColumns)

## Замінити числа 1-6 на описові назви діяльності
SelectData$activity[SelectData$activity == 1] <- "WALKING"
SelectData$activity[SelectData$activity == 2] <- "WALKING_UPSTAIRS"
SelectData$activity[SelectData$activity == 3] <- "WALKING_DOWNSTAIRS"
SelectData$activity[SelectData$activity == 4] <- "SITTING"
SelectData$activity[SelectData$activity == 5] <- "STANDING"
SelectData$activity[SelectData$activity == 6] <- "LAYING"
Перетворення на фактор:
SelectData$activity <- factor(SelectData$activity)

## Дати змінним описові імена
names(SelectData) <- gsub("^t", "Time", names(SelectData))
names(SelectData) <- gsub("^f", "Frequency", names(SelectData))
names(SelectData) <- gsub("Acc", "Accelerometer", names(SelectData))
names(SelectData) <- gsub("Gyro", "Gyroscope", names(SelectData))
names(SelectData) <- gsub("Mag", "Magnitude", names(SelectData))

## Створити другий незалежний акуратний набір даних із середнім значенням для кожної змінної для кожної діяльності та кожного суб’єкту

Створення нового df схожого на старий, але з групувальними ознаками (feature)
по суб’єктам (SubjectID) і активності (activity):
GrpData <- group_by(SelectData, SubjectID, activity)

Створення tidy dataset із середніми величинами для кожної змінної для кожної
діяльності та кожного суб’єкту:
MyTidyDataset <- summarise_each(GrpData, funs(mean))
