# LAB 5
```{r}
# Необхідні бібліотеки:
library(bitops)
library(RCurl)
library(dplyr)
```

## Task 0
```{r}
# Підготовка даних
# Зчитування тренувального і тестового датасетів у дата фрейм
train.x <- read.table(file = "./UCI HAR Dataset/train/X_train.txt")
test.x <- read.table(file = "./UCI HAR Dataset/test/X_test.txt")

# Зчитування лейблів (від 1 до 6 - що вони там робили) у дата фрейм
train.y <- read.table(file = "./UCI HAR Dataset/train/Y_train.txt")
test.y <- read.table(file = "./UCI HAR Dataset/test/Y_test.txt")

# Зчитування номерів волонтерів (від 1 до 30) у дата фрейм
train.sub <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt")
test.sub <- read.table(file = "./UCI HAR Dataset/test/subject_test.txt")

# Зчитування назв 561 змінних
feature <- read.table("./UCI HAR Dataset/features.txt")
```


## Task 1
```{r}
# Об’єднання навчального і тестового наборів
x <- rbind(train.x, test.x) # складання докупи даних
y <- rbind(train.y, test.y) # складання 6 видів активності
sub <- rbind(train.sub, test.sub) # складання 30 волонтерів

# Об’єднання волонтерів, активності і даних в один набір даних
data <- cbind(sub, y, x)

# Приєднання назв змінних "SubjectID" - назва першого стовпчика, "activity" -
# другого, feature$V2 - назви решти (береться зі зробленого раніше датафрейму)
names(data) <- c("SubjectID", "activity", as.character(feature$V2))

ANSWER:
> head(data[1:5]) # Виведення частини df, щоб влізло в рядок
  SubjectID activity tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z
1         1        5         0.2885845       -0.02029417        -0.1329051
2         1        5         0.2784188       -0.01641057        -0.1235202
3         1        5         0.2796531       -0.01946716        -0.1134617
4         1        5         0.2791739       -0.02620065        -0.1232826
5         1        5         0.2766288       -0.01656965        -0.1153619
6         1        5         0.2771988       -0.01009785        -0.1051373

```


## Task 2
```{r}
# Витягнути середнє та стандартне відхилення для кожного вимірювання

# Вибрати назви стовпчиків, які містять "-mean" або "-std"
FindMeanSd <- feature$V2[grep("-mean\\(\\)|-std\\(\\)", feature[, 2])]
# Сворити вектор назв цих стовпчиків + назви 2-х перших стовпчиків датафрейму
SelectColumns <- c("SubjectID", "activity", as.character(FindMeanSd))

# Створити сабсет з середніми та стандартними відхиленнями
SelectData <- subset(data, select = SelectColumns)

ANSWER:
> head(SelectData[66:68]) # Виведення частини df, щоб влізло в рядок
  fBodyBodyGyroMag-std() fBodyBodyGyroJerkMag-mean() fBodyBodyGyroJerkMag-std()
1             -0.9613094                  -0.9919904                 -0.9906975
2             -0.9833219                  -0.9958539                 -0.9963995
3             -0.9860277                  -0.9950305                 -0.9951274
4             -0.9878358                  -0.9952207                 -0.9952369
5             -0.9890594                  -0.9950928                 -0.9954648
6             -0.9858609                  -0.9951433                 -0.9952387
```

## Task 3
```{r}
# Замінити числа 1-6 на описові назви діяльності
SelectData$activity[SelectData$activity == 1] <- "WALKING"
SelectData$activity[SelectData$activity == 2] <- "WALKING_UPSTAIRS"
SelectData$activity[SelectData$activity == 3] <- "WALKING_DOWNSTAIRS"
SelectData$activity[SelectData$activity == 4] <- "SITTING"
SelectData$activity[SelectData$activity == 5] <- "STANDING"
SelectData$activity[SelectData$activity == 6] <- "LAYING"
# Перетворення на фактор
SelectData$activity <- factor(SelectData$activity)
ANSWER:
> summary(SelectData$activity) # Показано, що все замінено
            LAYING            SITTING           STANDING            WALKING WALKING_DOWNSTAIRS 
              1944               1777               1906               1722               1406 
  WALKING_UPSTAIRS 
              1544 
```

## Task 4
```{r}
# Дати змінним описові імена
names(SelectData) <- gsub("^t", "Time", names(SelectData))
names(SelectData) <- gsub("^f", "Frequency", names(SelectData))
names(SelectData) <- gsub("Acc", "Accelerometer", names(SelectData))
names(SelectData) <- gsub("Gyro", "Gyroscope", names(SelectData))
names(SelectData) <- gsub("Mag", "Magnitude", names(SelectData))

ANSWER:
> colnames(SelectData[,60:64]) # Виведення частини імен змінних, щоб влізло в рядок
[1] "FrequencyBodyGyroscope-std()-Z"                    
[2] "FrequencyBodyAccelerometerMagnitude-mean()"        
[3] "FrequencyBodyAccelerometerMagnitude-std()"         
[4] "FrequencyBodyBodyAccelerometerJerkMagnitude-mean()"
[5] "FrequencyBodyBodyAccelerometerJerkMagnitude-std()" 
```

## Task 5
```{r}
# створити другий незалежний акуратний набір даних із середнім значенням
# для кожної змінної для кожної діяльності та кожного суб’єкту

# Створення нового df схожого на старий, але з групувальними ознаками (feature)
# по суб’єктам (SubjectID) і активності (activity):
GrpData <- group_by(SelectData, SubjectID, activity)

# Створення tidy dataset із середніми величинами для кожної змінної для кожної
# діяльності та кожного суб’єкту:
MyTidyDataset <- summarise_each(GrpData, funs(mean))


ANSWER:
> MyTidyDataset[1:12,1:5] # виведення частини створеного df
# A tibble: 12 x 5
# Groups:   SubjectID [2]
   SubjectID activity        `TimeBodyAccelerometer-mea~ `TimeBodyAccelerometer-me~ `TimeBodyAccelerometer-me~
       <int> <fct>                                 <dbl>                      <dbl>                      <dbl>
 1         1 LAYING                                0.261                   -0.00131                    -0.105 
 2         1 SITTING                               0.222                   -0.0405                     -0.113 
 3         1 STANDING                              0.279                   -0.0161                     -0.111 
 4         1 WALKING                               0.255                   -0.0240                     -0.0973
 5         1 WALKING_DOWNST~                       0.289                   -0.00992                    -0.108 
 6         1 WALKING_UPSTAI~                       0.277                   -0.0174                     -0.111 
 7         2 LAYING                                0.277                   -0.0157                     -0.109 
 8         2 SITTING                               0.281                   -0.0182                     -0.107 
 9         2 STANDING                              0.278                   -0.0184                     -0.106 
10         2 WALKING                               0.247                   -0.0214                     -0.153 
11         2 WALKING_DOWNST~                       0.278                   -0.0227                     -0.117 
12         2 WALKING_UPSTAI~                       0.276                   -0.0186                     -0.106 
```