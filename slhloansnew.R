library(dplyr) #dplyr package import
library(lubridate) #lubridate (For working with date variables)
library(ggplot2) # for plotting
library(plyr) # for ddply
library(magrittr) #needed it for pipe operator?
library(RecordLinkage)
library(stringdist)

setwd('C:\\Users\\Trevor\\Documents\\slhloans')

loans <- read.csv("loans_final_userid.csv") #imports loan data

#----------Cleaning Data----------

str(loans) #prints structure of loans data frame

#--------Cleaning Status Column------

#using grep to reduce the number of loan statuses with similar terms
loans$Status <- as.character(loans$Status)
loans$Status[grep("forbearance",loans$Status,ignore.case = TRUE)] <- "FBR"

levels(factor(loans$Status)) #prints number of unique levels in status column

#further reducing duplicative levels
loans$Status[grep("deferment", loans$Status,ignore.case = TRUE)] <- "DFR"
loans$Status[grep("repayment", loans$Status,ignore.case = TRUE)] <- "RPM"
loans$Status[grep("grace", loans$Status,ignore.case = TRUE)] <- "grace"
loans$Status[grep("default", loans$Status,ignore.case = TRUE)] <- "default"

levels(loans_final$Status) <- gsub(".*DEFERRED.*", "DFR", ignore.case = TRUE, levels(loans_final$Status))
levels(loans_final$Status) <- gsub(".*DEFERMENT.*", "DFR", ignore.case = TRUE, levels(loans_final$Status))
levels(loans_final$Status) <- gsub(".*repayment.*", "RPM", ignore.case = TRUE, levels(loans_final$Status))
levels(loans_final$Status) <- gsub(".*Grace.*", "DFR", ignore.case = TRUE, levels(loans_final$Status))
levels(loans_final$Status) <- gsub(".*default.*", "Default", ignore.case = TRUE, levels(loans_final$Status))
levels(loans_final$Status) <- gsub(".*forbearance.*", "DFR", ignore.case = TRUE, levels(loans_final$Status))
levels(loans_final$Status) <- gsub(".*bankruptcy.*", "Default", ignore.case = TRUE, levels(loans_final$Status))
levels(loans_final$Status) <- gsub(".*delinq.*", "Default", ignore.case = TRUE, levels(loans_final$Status))
levels(loans_final$Status) <- gsub(".*rpm.*", "RPM", ignore.case = TRUE, levels(loans_final$Status))
levels(loans_final$Status) <- gsub(".*defer.*", "DFR", ignore.case = TRUE, levels(loans_final$Status))
levels(loans_final$Status) <- gsub("DEFERRED", "DFR", levels(loans_final$Status))

levels(loans_final$Status) <- gsub("DEFERRED", "DFR", levels(loans_final$Status))

levels(loans_final$Status) <- gsub("LOAN ORIGINATED", "IN SCHOOL", levels(loans_final$Status))

levels(loans_final$Status) <- gsub("FBR", "DFR", levels(loans_final$Status))

levels(loans_final$Status) <- gsub("grace", "DFR", levels(loans_final$Status))

levels(loans_final$Status) <- gsub("DFR", "DFR/GRC", levels(loans_final$Status))

levels(factor(loans_final$Status)) #checking number of levels now

#------End Cleaning Status Column-----

#Reducing number of levels for profession

levels(loans_final$Profession)

sort(table(loans_final$Profession))

levels(loans_final$Profession) <- gsub(".*Lawyer.*|.*attorney.*", "Attorney", levels(loans_final$Profession))

levels(loans_final$Profession) <- gsub(".*Engineer.*", "Engineer", levels(loans_final$Profession))

levels(loans_final$Profession) <- gsub(".*Teacher.*", "Teacher", levels(loans_final$Profession))

levels(loans_final$Profession) <- gsub(".*Nurse.*", "Doctor/Nurse/Pharmacist", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*Doctor.*", "Doctor/Nurse/Pharmacist", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*Therapist.*", "Doctor/Nurse/Pharmacist", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*Physician.*", "Doctor/Nurse/Pharmacist", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*Dentist.*", "Doctor/Nurse/Pharmacist", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*Dental.*", "Doctor/Nurse/Pharmacist", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*Physician's.*", "Doctor/Nurse/Pharmacist", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*Psychologist.*", "Doctor/Nurse/Pharmacist", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*Psychiatrist.*", "Doctor/Nurse/Pharmacist", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*Medical.*", "Doctor/Nurse/Pharmacist", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*Chiropractor.*", "Doctor/Nurse/Pharmacist", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*Pharmacist.*", "Doctor/Nurse/Pharmacist", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*Nursing.*", "Doctor/Nurse/Pharmacist", levels(loans_final$Profession))

levels(loans_final$Profession) <- gsub(".*Computer.*", "Computer/Tech", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*Developer.*", "Computer/Tech", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*IT.*", "Computer/Tech", levels(loans_final$Profession))


levels(loans_final$Profession) <- gsub(".*Teacher.*", "Teacher", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*Librarian.*", "Teacher", levels(loans_final$Profession))
levels(loans_final$Profession) <- gsub(".*Professor.*", "Teacher", levels(loans_final$Profession))

levels(loans_final$Profession) <- gsub(".*Accounting.*", "Accountant", levels(loans_final$Profession))


levels(loans_final$Profession) <- gsub(".*Student.*|.*Resident.*|.*Researcher.*|.*student.*", "Student", levels(loans_final$Profession))

levels(loans_final$Profession) <- gsub(".*Designer.*|.*Graphic.*", "Designer", levels(loans_final$Profession))

levels(loans_final$Profession) <- gsub(".*Business.*|.*Manager.*|.*Marketing.*|.*Financial.*|.*Analyst.*|.*Insurance.*|.*Consultant*.|.*Resources.*|.*Consulting.*", "General Business", levels(loans_final$Profession))

levels(loans_final$Profession) <- gsub(".*Retail.*|.*Cashier.*", "Retail", levels(loans_final$Profession))

OtherProfessions <- !(loans_final$Profession %in% c("Doctor/Nurse/Pharmacist","General Business", "Engineer", "Attorney", "Teacher", "Computer/Tech", "Accountant", "Student", "Designer", "Retail"))

loans_final$Profession[OtherProfessions]<- "Other"

loans_final$Profession <- factor(loans_final$Profession)

levels(factor(loans_final$Profession))

#-------End cleaning professions----

#------Starting cleaning Majors-----

levels(loans_final$Major)

sort(table(loans_final$Major))

levels(loans_final$Major) <- gsub(".*Law.*|.*Lawyer.*|.*attorney.*|.*JD.*|.*Juris Doctor.*|.*Paralegal.*", "Law", levels(loans_final$Major))
levels(loans_final$Major) <- gsub(".*Business.*|.*Financ.*|.*Account.*|.*Economic.*|.*Marketing.*|.*Human Resources.*", "Business", levels(loans_final$Major))
levels(loans_final$Major) <- gsub(".*Philosophy.*|.*Social.*|.*Politic.*|.*English.*|.*History.*|.*Sociology.*|.*International.*|.*General Studies.*|.*Fashion.*", "Liberal Arts", levels(loans_final$Major))
levels(loans_final$Major) <- gsub(".*Engineer.*", "Engineering", levels(loans_final$Major))
levels(loans_final$Major) <- gsub(".*Medicine.*|.*MD.*|.*Physician*|.*Doctor.*|.*Pharmacy.*|.*Medical.*|.*medicine.*", "Higher Medical Degree", levels(loans_final$Major))
levels(loans_final$Major) <- gsub(".*Teach.*|.*Education.*|.*Teach.*", "Education", levels(loans_final$Major))
levels(loans_final$Major) <- gsub(".*Technology.*|.*Computer.*|.*Systems.*", "Computer Science", levels(loans_final$Major))
levels(loans_final$Major) <- gsub(".*Communication.*|.*Relations.*", "Communications", levels(loans_final$Major))
levels(loans_final$Major) <- gsub(".*Science.*|.*Biology.*|.*Chemistry.*|.*Geology.*|.*Bioengineer.*|.*Physics.*|.*Mathematics.*", "Sciences", levels(loans_final$Major))
levels(loans_final$Major) <- gsub(".*Psychology.*", "Psychology", levels(loans_final$Major))
levels(loans_final$Major) <- gsub("Art|.*Graphic.*|.*Film.*|.*Drama.*|.*Music.*|.*Photography.*", "Arts", levels(loans_final$Major))
levels(loans_final$Major) <- gsub(".*Nurs.*|.*nurs.*", "Nursing", levels(loans_final$Major))

OtherMajors <- !(loans_final$Major %in% c("Business","Sciences", "Higher Medical Degree", "Engineering", "Liberal Artss", "Law", "Education", "Nursing", "Psychology", "Communications", "MBA"))

loans_final$Major[OtherMajors]<- "Other"

loans_final$Major <- factor(loans_final$Major)

levels(loans_final$Major)

#----End Cleaning Majors------

#removing servicer column - unnecessary for predicting the chance a borrower will default
loans_final$Servicer <- NULL


#---------------End Data Cleaning---------------------

ggplot(loans_final, aes(Status))+geom_bar()

loans%>%group_by(Status)%>%summarize(Count=n()) #prints the raw numbers of each status

#adding a new calculated column = difference between original and current principal
loans <- mutate(loans, Difference=Current.Principal-Original.Principal)

summary(loans$Difference)

#Pulling records where difference is positive, and rate is >0
loans_final <- filter(loans, Difference>=0&Rate>0)

#str(loans_final)

#deleting variables that are null or useless
loans_final <- select(loans_final, -(Created.At:Loan.ID..),-X)

#getting rid of records where original principal is 0
loans_final <- filter(loans_final, Original.Principal>0)

#plotting final values
ggplot(loans_final, aes(Education.Degree, fill=Status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plotting final values of defaults by degree
ggplot(filter(loans_final, Status=='default'), aes(Education.Degree, fill=Status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Lubridating the date field from character to true date field
loans_final$Loan.Disbursement.Date <- mdy(loans_final$Loan.Disbursement.Date)

#yearwise breakdown of loans disbursed
yearwise <- loans_final%>%group_by(year(Loan.Disbursement.Date))%>%summarize(Count=n())

#ggplot(yearwise)

#answering Dhiraj's question - arranging dataframe in descending order
#this was my line: yearwise_desc <- yearwise[order(-yearwise$`year(Loan.Disbursement.Date)`),]

yearwise_desc <- arrange(yearwise, desc(Count))

colnames(yearwise) <- c("year", "count")

ggplot(yearwise, (aes(x = year, y = count))) + geom_bar(stat="identity")  #Dhiraj's solution

#my old plot ggplot(yearwise, (aes(x = year, y = count))) + geom_bar()  #NOT plotting?  Differing number of rows?

#Answering Question 1
str(loans_final)

summarise(loans_final)

loans_states <- loans_final%>%group_by(State.of.Residency)%>%summarise(Status=n())

summary(loans_states)

#ggplot(loans_states, (aes(loans_states$State.of.Residency, loans_states$Status))+geom_bar())

#qplot(loans_states,loans_states$State.of.Residency, loans_states$Status) + geom_bar() + facet_grid(loans_states$State.of.Residency~ .)

#ggplot(loans_states, aes(loans_states$State.of.Residency)) + geom_bar() + facet_grid(loans_states$State.of.Residency~ .)

#ggplot(loans_states, aes(loans_states$State.of.Residency, loans_states$Status)) + geom_bar(stat="identity")

density (loans_states$Status)
#anova(loans_states)

#loans_final$TotalBalance <- group_by(loans_final$User.ID..,loans_final$Original.Principal)

#df %>% mutate(Quarter = quarter(month)) %>% group_by(Quarter) %>% mutate(SumPre = sum(precipitation))


#Adding new column for each user's original balance sum
loans_final$Original.Principal[is.na(loans_final$Original.Principal)] <- 0
loans_finalv2 <- loans_final %>% group_by(User.ID..) %>% mutate(origtotalbalance=sum(Original.Principal))
#Adding new column for each user's current balance sum
loans_final$Current.Principal[is.na(loans_final$Current.Principal)] <- 0
loans_finalv2 <- loans_final %>% group_by(User.ID..) %>% mutate(currenttotalbalance=sum(Current.Principal))%>% mutate(origtotalbalance=sum(Original.Principal))

#Find the Percentage the user has paid off all of their loans
loans_finalv2$progress <- mutate(loans_finalv2, progress = currenttotalbalance / origtotalbalance)

#table() function - table(loans_final$college_type)

#lubridate - find age of borrower

#Levenshtein Distance Algorithm for College Name Normalization

#univ <- read.csv("collegedetails.csv")
#desired_names <- tolower(as.character(univ$NAME))
#loans_finalv2$College <- tolower(as.character(loans_finalv2$College))

#Used to standardize college names
#index <- numeric()
#value <- numeric()
#names <- character()
#for (i in 1:nrow(loans_finalv2)) {
#  differences <- levenshteinDist(desired_names, loans_finalv2$College[i])
#  loans_finalv2$College[i] <- desired_names[which.min(differences)]
#  index[i] <- which.min(differences)
#  value[i] <- min(differences)
#  names[i] <- desired_names[which.min(differences)]
#}
#
#exporting university names to spot check??
#univnamecheck2 <- data.frame(names, differences, loans_finalv2$College)

#Matched College Names Dataset
cleanloans <- read.csv("slhloansclean.csv")

cleanloans$univctrl <- univ[match(cleanloans$College, univ$NAME),4]
cleanloans$univstate <- univ[match(cleanloans$College, univ$NAME),3]
cleanloans$univobereg <- univ[match(cleanloans$College, univ$NAME),5]
cleanloans$univlocale <- univ[match(cleanloans$College, univ$NAME),6]
cleanloans$univenrprofile <- univ[match(cleanloans$College, univ$NAME),8]
cleanloans$univmedical <- univ[match(cleanloans$College, univ$NAME),9]
cleanloans$univhbcu <- univ[match(cleanloans$College, univ$NAME),10]
cleanloans$univtribal <- univ[match(cleanloans$College, univ$NAME),11]
cleanloans$univhsi <- univ[match(cleanloans$College, univ$NAME),12]
cleanloans$univwomens <- univ[match(cleanloans$College, univ$NAME),14]
cleanloans$univlibarts <- univ[match(cleanloans$College, univ$NAME),15]

levels(cleanloans$Major)

sort(table(cleanloans$Major))

levels(cleanloans$Major) <- gsub(".*Law.*|.*Lawyer.*|.*attorney.*|.*JD.*|.*Juris Doctor.*|.*Paralegal.*", "Law", levels(cleanloans$Major))
levels(cleanloans$Major) <- gsub(".*Business.*|.*Financ.*|.*Account.*|.*Economic.*|.*Marketing.*|.*Human Resources.*", "Business", levels(cleanloans$Major))
levels(cleanloans$Major) <- gsub(".*Philosophy.*|.*Social.*|.*Politic.*|.*English.*|.*History.*|.*Sociology.*|.*International.*|.*General Studies.*|.*Fashion.*", "Liberal Arts", levels(cleanloans$Major))
levels(cleanloans$Major) <- gsub(".*Engineer.*", "Engineering", levels(cleanloans$Major))
levels(cleanloans$Major) <- gsub(".*Medicine.*|.*MD.*|.*Physician*|.*Doctor.*|.*Pharmacy.*|.*Medical.*|.*medicine.*", "Higher Medical Degree", levels(cleanloans$Major))
levels(cleanloans$Major) <- gsub(".*Teach.*|.*Education.*|.*Teach.*", "Education", levels(cleanloans$Major))
levels(cleanloans$Major) <- gsub(".*Technology.*|.*Computer.*|.*Systems.*", "Computer Science", levels(cleanloans$Major))
levels(cleanloans$Major) <- gsub(".*Communication.*|.*Relations.*", "Communications", levels(cleanloans$Major))
levels(cleanloans$Major) <- gsub(".*Science.*|.*Biology.*|.*Chemistry.*|.*Geology.*|.*Bioengineer.*|.*Physics.*|.*Mathematics.*", "Sciences", levels(cleanloans$Major))
levels(cleanloans$Major) <- gsub(".*Psychology.*", "Psychology", levels(cleanloans$Major))
levels(cleanloans$Major) <- gsub("Art|.*Graphic.*|.*Film.*|.*Drama.*|.*Music.*|.*Photography.*", "Arts", levels(cleanloans$Major))
levels(cleanloans$Major) <- gsub(".*Nurs.*|.*nurs.*", "Nursing", levels(cleanloans$Major))

OtherMajors <- !(cleanloans$Major %in% c("Business","Sciences", "Higher Medical Degree", "Engineering", "Liberal Arts", "Law", "Education", "Nursing", "Psychology", "Communications", "MBA"))

cleanloans$Major[OtherMajors]<- "Other"

cleanloans$Major <- factor(cleanloans$Major)

levels(cleanloans$Major)


cleanloans$Status[grep("forbearance",cleanloans$Status,ignore.case = TRUE)] <- "FBR"

levels(factor(cleanloans$Status)) #prints number of unique levels in status column

#further reducing duplicative levels
cleanloans$Status[grep("deferment", cleanloans$Status,ignore.case = TRUE)] <- "DFR"
cleanloans$Status[grep("repayment", cleanloans$Status,ignore.case = TRUE)] <- "RPM"
cleanloans$Status[grep("grace", cleanloans$Status,ignore.case = TRUE)] <- "grace"
cleanloans$Status[grep("default", cleanloans$Status,ignore.case = TRUE)] <- "default"

levels(cleanloans$Status) <- gsub(".*DEFERRED.*", "DFR", ignore.case = TRUE, levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub(".*DEFERMENT.*", "DFR", ignore.case = TRUE, levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub(".*repayment.*", "RPM", ignore.case = TRUE, levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub(".*Grace.*", "DFR", ignore.case = TRUE, levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub(".*default.*", "Default", ignore.case = TRUE, levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub(".*forbearance.*", "DFR", ignore.case = TRUE, levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub(".*bankruptcy.*", "Default", ignore.case = TRUE, levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub(".*delinq.*", "Default", ignore.case = TRUE, levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub(".*rpm.*", "RPM", ignore.case = TRUE, levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub(".*defer.*", "DFR", ignore.case = TRUE, levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub(".*Forb.*", "DFR", ignore.case = TRUE, levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub(".*In school.*", "In School", ignore.case = TRUE, levels(cleanloans$Status))


#levels(cleanloans$Status) <- gsub("DEFERRED", "DFR", levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub("LOAN ORIGINATED", "IN SCHOOL", levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub("FBR", "DFR", levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub("grace", "DFR", levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub("DFR", "DFR/GRC", levels(cleanloans$Status))

levels(factor(cleanloans$Status)) #checking number of levels now


levels(cleanloans$Profession)

sort(table(cleanloans$Profession))

levels(cleanloans$Profession) <- gsub(".*Lawyer.*|.*attorney.*", "Attorney", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Engineer.*", "Engineer", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Teacher.*", "Teacher", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Nurse.*", "Doctor/Nurse/Pharmacist", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Doctor.*", "Doctor/Nurse/Pharmacist", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Therapist.*", "Doctor/Nurse/Pharmacist", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Physician.*", "Doctor/Nurse/Pharmacist", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Dentist.*", "Doctor/Nurse/Pharmacist", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Dental.*", "Doctor/Nurse/Pharmacist", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Physician's.*", "Doctor/Nurse/Pharmacist", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Psychologist.*", "Doctor/Nurse/Pharmacist", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Psychiatrist.*", "Doctor/Nurse/Pharmacist", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Medical.*", "Doctor/Nurse/Pharmacist", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Chiropractor.*", "Doctor/Nurse/Pharmacist", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Pharmacist.*", "Doctor/Nurse/Pharmacist", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Nursing.*", "Doctor/Nurse/Pharmacist", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Computer.*", "Computer/Tech", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Developer.*", "Computer/Tech", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*IT.*", "Computer/Tech", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Teacher.*", "Teacher", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Librarian.*", "Teacher", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Professor.*", "Teacher", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Accounting.*", "Accountant", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Student.*|.*Resident.*|.*Researcher.*|.*student.*", "Student", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Designer.*|.*Graphic.*", "Designer", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Business.*|.*Manager.*|.*Marketing.*|.*Financial.*|.*Analyst.*|.*Insurance.*|.*Consultant*.|.*Resources.*|.*Consulting.*", "General Business", levels(cleanloans$Profession))
levels(cleanloans$Profession) <- gsub(".*Retail.*|.*Cashier.*", "Retail", levels(cleanloans$Profession))
OtherProfessions <- !(cleanloans$Profession %in% c("Doctor/Nurse/Pharmacist","General Business", "Engineer", "Attorney", "Teacher", "Computer/Tech", "Accountant", "Student", "Designer", "Retail"))

cleanloans$Profession[OtherProfessions]<- "Other"

cleanloans$Profession <- factor(cleanloans$Profession)

levels(factor(cleanloans$Profession))

#removing servicer column - unnecessary for predicting the chance a borrower will default
cleanloans$Servicer <- NULL

#adding a new calculated column = difference between original and current principal
cleanloans <- mutate(cleanloans, Difference=Current.Principal-Original.Principal)

summary(cleanloans$Difference)

#Pulling records where difference is positive, and rate is >0
cleanloans <- filter(cleanloans, Difference>=0&Rate>0)

#str(cleanloans)

#deleting variables that are null or useless
cleanloans <- select(cleanloans, -(Created.At:Loan.ID..),-X)

#getting rid of records where original principal is 0
cleanloans <- filter(cleanloans, Original.Principal>0)

#Lubridating the date field from character to true date field
cleanloans$Loan.Disbursement.Date <- mdy(cleanloans$Loan.Disbursement.Date)

#Adding new column for each user's original balance sum
cleanloans$Original.Principal[is.na(cleanloans$Original.Principal)] <- 0
cleanloans <- cleanloans %>% group_by(User.ID..) %>% mutate(origtotalbalance=sum(Original.Principal))
#Adding new column for each user's current balance sum
cleanloans$Current.Principal[is.na(cleanloans$Current.Principal)] <- 0
cleanloans <- cleanloans %>% group_by(User.ID..) %>% mutate(currenttotalbalance=sum(Current.Principal))%>% mutate(origtotalbalance=sum(Original.Principal))



#setup column for is current principal > original principal
