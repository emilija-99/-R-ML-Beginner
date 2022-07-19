str(test)

# DETECTING MISSING VALUES
summary(train)
summary(test)

length(which(train$Cabin == ""))
length(which(test$Cabin == ""))

# get indices of observations with no Cabin from the first class,
# the tain set
train.class1.no.cabin<-which(train$Pclass == 1 && train$Cabin == "")
length(train.class1.no.cabin)
test.class1.no.cabin <- which(test$Pclass==1 & test$Cabin=="")
train$Cabin[train.class1.no.cabin] <- NA
test$Cabin[test.class1.no.cabin] <- NA
length(test.class1.no.cabin)

# test for empty sting variables in the train set
apply(X-train[,c("Name","Sex","Ticket","Embarked")], MARGIN = 2, FUN = function(x),length(which(x == "")))

library(Amelia)
install.packages('Amelia')
# to display two plots in the same row
par(mfrow = c(1,2))
missmap(obj = train, main = "Traingn_set", legend = TRUE)
missmap(obj = test, main = "Test_set", legend = TRUE)

# HANDLING MISSING VALUE

# CATEGORICAL VARIABLES WITH MISSING VALUES
# get a number of unique values for tge Embarked in the both sets
unique(train$Embarked)
xtabs(~Embarked, data = train)
train$Embarked[(train$Embarked == "")]<-'S'

# NUMERICAL NUMBER WITH MISSING VALUES
shapiro.test(test$Fare)
missing.fare.pclass<-test$Pclass[is.na(test$Fare)]
media.fare<-median(x = test$Fare[test$Pclass == missing.fare.pclass], na.rm = T)
test$Fare[is.na(test$Fare)] <-media.fare
summary(test$Fare)

# FEATURE SELECTION -> to create prediction model
# EXAMINING THE PREDICTIVE POWER OF VARIABLES FROM THE DATA SET

# transform the Sex into factor
train$Sex <-factor(train$Sex)
summary(train$Sex)

# print the proportion table of the sex variables
prop.table(summary(train$Sex))

# SEX VS SURVIVED
sex.survived.counts<-xtabs(~Sex+Survived, data = train)
sex.survived.counts

# get proportions of the contingency table for SEX vs SURVIVED
sex.sur.tbl <-prop.table(sex.survived.counts, margin = 1)

# transform the Survived variable into factor
train$Survived<-factor(train$Survived, levels = c(0,1), labels = c('No','Yes'))
train$Pclass<-factor(train$Pclass, levels = c(1,2,3), labels = c('1st','2st','3st'))

gpl<-ggplot(train, aes(x =Pclass, fill = Survived)) + geom_bar(position = "dodge", width = 0.4)+ylab("Numbers of passengers")+ xlab("Passenger class")+theme_bw()
gpl
     
# add a SEX facet to the plot
gpl2 <- gpl+facet_wrap(~Sex)
gpl2

#plot the number of passengers for different embarkment places and Survived valus
gpl3 <- ggplot(train, aes(x = Embarked, fill =Survived))+geom_bar(position = "dodge", width = 0.45)+ylab("Number of passengers")+xlab("Place of embarkent")+theme_bw()


# FEATURE ENGINEERING
# adding the Survived variable to test set
test$Survived<-factor(NA, levels = c(1,2), labels = c('No','Yes'))
test$Pclass<-factor(x = test$Pclass, levels = c(1,2,3), labels = c('1st','2st','3st'))

# transform sex variable into factor
test$Sex <- factor(test$Sex)
test$Embarked<-factor(test$Embarked)

# merge train and test sets
titanic.all <-rbind(train, test)

# CREATING AN AGE PROXY VARIABLE
titanic.all$Name[1:10]
strsplit(x = titanic.all$Name[1], split = "[,|.]")
unlist(strsplit(x= titanic.all$Name[1], split = "[,|.]"))[2]

# create a variable Title based of the values of the Name variable
titanic.all$Title<-sapply(titanic.all$Name, FUN = function(x) unlist(strsplit(x,split = "[,|.]")))

titanic.all$Title<-trimws(titanic.all$Title, which = "left")
table(titanic.all$Title)
table(titanic.all$Title)

adult.woman<-c('Dona','Lady',"Mse","Mrs","the Countess")
girls<-c("Ms","Mile","Miss")
boys<-c("Master","Jonkbeer")
adult.man<-c("Capt","Col","Don","Dr","Major","Mr","Rev","Sir")
titanic.all$AgeGender<-vector(mode="character", length = nrow(titanic.all))
titanic.all$AgeGender[titanic.all$Title %in% adult.woman]<-'AdultWoman'
titanic.all$AgeGender[titanic.all$Title %in% adult.man]<-"AdultMan"
titanic.all$AgeGender[titanic.all$Title %in% girls] <-"Girls"
titanic.all$AgeGender[titanic.all$Title %in% boys]<-"Boys"

table(titanic.all$AgeGender)
titanic.all$
ggplot(titanic.all[titanic.all$AgeGender == "Girls"], aes(x = titanic.all$Age))+geom_density()+theme_bw()
