library(dplyr) #dplyr package import
library(lubridate) #lubridate (For working with date variables)
library(ggplot2) # for plotting
library(plyr) # for ddply
library(magrittr) #needed it for pipe operator?
library(RecordLinkage)
library(stringdist)
library(readr)
library(choroplethr)
library(choroplethrMaps)
library(knitr)
library(verification)
setwd('C:\\Users\\Trevor\\Downloads\\slhloansreport')

loans <- read.csv("loans_final_userid.csv") #imports intial loan data

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

levels(loans$Status) <- gsub(".*DEFERRED.*", "DFR", ignore.case = TRUE, levels(loans$Status))
levels(loans$Status) <- gsub(".*DEFERMENT.*", "DFR", ignore.case = TRUE, levels(loans$Status))
levels(loans$Status) <- gsub(".*repayment.*", "RPM", ignore.case = TRUE, levels(loans$Status))
levels(loans$Status) <- gsub(".*Grace.*", "DFR", ignore.case = TRUE, levels(loans$Status))
levels(loans$Status) <- gsub(".*default.*", "Default", ignore.case = TRUE, levels(loans$Status))
levels(loans$Status) <- gsub(".*forbearance.*", "DFR", ignore.case = TRUE, levels(loans$Status))
levels(loans$Status) <- gsub(".*bankruptcy.*", "Default", ignore.case = TRUE, levels(loans$Status))
levels(loans$Status) <- gsub(".*delinq.*", "Default", ignore.case = TRUE, levels(loans$Status))
levels(loans$Status) <- gsub(".*rpm.*", "RPM", ignore.case = TRUE, levels(loans$Status))
levels(loans$Status) <- gsub(".*defer.*", "DFR", ignore.case = TRUE, levels(loans$Status))
levels(loans$Status) <- gsub("DEFERRED", "DFR", levels(loans$Status))
levels(loans$Status) <- gsub("LOAN ORIGINATED", "IN SCHOOL", levels(loans$Status))

levels(loans$Status) <- gsub("FBR", "DFR", levels(loans$Status))

levels(loans$Status) <- gsub("grace", "DFR", levels(loans$Status))

levels(loans$Status) <- gsub("DFR", "DFR/GRC", levels(loans$Status))

levels(factor(loans$Status)) #checking number of levels now

#------End Cleaning Status Column-----

#-----Reducing number of levels for profession---

levels(loans$Profession)

sort(table(loans$Profession))

levels(loans$Profession) <- gsub(".*Lawyer.*|.*attorney.*", "Attorney", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Engineer.*", "Engineer", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Teacher.*", "Teacher", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Nurse.*", "Doctor/Nurse/Pharmacist", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Doctor.*", "Doctor/Nurse/Pharmacist", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Therapist.*", "Doctor/Nurse/Pharmacist", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Physician.*", "Doctor/Nurse/Pharmacist", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Dentist.*", "Doctor/Nurse/Pharmacist", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Dental.*", "Doctor/Nurse/Pharmacist", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Physician's.*", "Doctor/Nurse/Pharmacist", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Psychologist.*", "Doctor/Nurse/Pharmacist", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Psychiatrist.*", "Doctor/Nurse/Pharmacist", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Medical.*", "Doctor/Nurse/Pharmacist", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Chiropractor.*", "Doctor/Nurse/Pharmacist", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Pharmacist.*", "Doctor/Nurse/Pharmacist", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Nursing.*", "Doctor/Nurse/Pharmacist", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Computer.*", "Computer/Tech", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Developer.*", "Computer/Tech", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*IT.*", "Computer/Tech", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Teacher.*", "Teacher", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Librarian.*", "Teacher", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Professor.*", "Teacher", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Accounting.*", "Accountant", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Student.*|.*Resident.*|.*Researcher.*|.*student.*", "Student", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Designer.*|.*Graphic.*", "Designer", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Business.*|.*Manager.*|.*Marketing.*|.*Financial.*|.*Analyst.*|.*Insurance.*|.*Consultant*.|.*Resources.*|.*Consulting.*", "General Business", levels(loans$Profession))
levels(loans$Profession) <- gsub(".*Retail.*|.*Cashier.*", "Retail", levels(loans$Profession))

OtherProfessions <- !(loans$Profession %in% c("Doctor/Nurse/Pharmacist","General Business", "Engineer", "Attorney", "Teacher", "Computer/Tech", "Accountant", "Student", "Designer", "Retail"))

loans$Profession[OtherProfessions]<- "Other"

loans$Profession <- factor(loans$Profession)

levels(factor(loans$Profession))

#-------End cleaning professions----

#------Starting cleaning Majors-----

levels(loans$Major)

sort(table(loans$Major))

levels(loans$Major) <- gsub(".*Law.*|.*Lawyer.*|.*attorney.*|.*JD.*|.*Juris Doctor.*|.*Paralegal.*", "Law", levels(loans$Major))
levels(loans$Major) <- gsub(".*Business.*|.*Financ.*|.*Account.*|.*Economic.*|.*Marketing.*|.*Human Resources.*", "Business", levels(loans$Major))
levels(loans$Major) <- gsub(".*Philosophy.*|.*Social.*|.*Politic.*|.*English.*|.*History.*|.*Sociology.*|.*International.*|.*General Studies.*|.*Fashion.*", "Liberal Arts", levels(loans$Major))
levels(loans$Major) <- gsub(".*Engineer.*", "Engineering", levels(loans$Major))
levels(loans$Major) <- gsub(".*Medicine.*|.*MD.*|.*Physician*|.*Doctor.*|.*Pharmacy.*|.*Medical.*|.*medicine.*", "Higher Medical Degree", levels(loans$Major))
levels(loans$Major) <- gsub(".*Teach.*|.*Education.*|.*Teach.*", "Education", levels(loans$Major))
levels(loans$Major) <- gsub(".*Technology.*|.*Computer.*|.*Systems.*", "Computer Science", levels(loans$Major))
levels(loans$Major) <- gsub(".*Communication.*|.*Relations.*", "Communications", levels(loans$Major))
levels(loans$Major) <- gsub(".*Science.*|.*Biology.*|.*Chemistry.*|.*Geology.*|.*Bioengineer.*|.*Physics.*|.*Mathematics.*", "Sciences", levels(loans$Major))
levels(loans$Major) <- gsub(".*Psychology.*", "Psychology", levels(loans$Major))
levels(loans$Major) <- gsub("Art|.*Graphic.*|.*Film.*|.*Drama.*|.*Music.*|.*Photography.*", "Arts", levels(loans$Major))
levels(loans$Major) <- gsub(".*Nurs.*|.*nurs.*", "Nursing", levels(loans$Major))

OtherMajors <- !(loans$Major %in% c("Business","Sciences", "Higher Medical Degree", "Engineering", "Liberal Artss", "Law", "Education", "Nursing", "Psychology", "Communications", "MBA"))

loans$Major[OtherMajors]<- "Other"

loans$Major <- factor(loans$Major)

levels(loans$Major)

#----End Cleaning Majors------

#removing servicer column - unnecessary for predicting the chance a borrower will default
loans$Servicer <- NULL


#---------------End Data Cleaning---------------------

#--------------Tutorial 1 from Dhiraj-----------------

ggplot(loans, aes(Status))+geom_bar()

loans%>%group_by(Status)%>%summarize(Count=n()) #prints the raw numbers of each status

#adding a new calculated column = difference between original and current principal
loans <- mutate(loans, Difference=Current.Principal-Original.Principal)

summary(loans$Difference)

#Pulling records where difference is positive, and rate is >0
loans <- filter(loans, Difference>=0&Rate>0)

#str(loans)

#deleting variables that are null or useless
loans <- select(loans, -(Created.At:Loan.ID..),-X)

#getting rid of records where original principal is 0
loans <- filter(loans, Original.Principal>0)

#plotting final values
ggplot(loans, aes(Education.Degree, fill=Status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plotting final values of defaults by degree
ggplot(filter(loans, Status=='default'), aes(Education.Degree, fill=Status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Lubridating the date field from character to true date field
loans$Loan.Disbursement.Date <- mdy(loans$Loan.Disbursement.Date)

#yearwise breakdown of loans disbursed
yearwise <- loans%>%group_by(year(Loan.Disbursement.Date))%>%summarize(Count=n())

#ggplot(yearwise)

#answering Dhiraj's question - arranging dataframe in descending order
#this was my line: yearwise_desc <- yearwise[order(-yearwise$`year(Loan.Disbursement.Date)`),]

yearwise_desc <- arrange(yearwise, desc(Count))

colnames(yearwise) <- c("year", "count")

ggplot(yearwise, (aes(x = year, y = count))) + geom_bar(stat="identity")  #Dhiraj's solution

#my old plot ggplot(yearwise, (aes(x = year, y = count))) + geom_bar()  #NOT plotting?  Differing number of rows?

#Answering Question 1
str(loans)

dplyr::summarize(loans)

loans_states <- loans%>%group_by(State.of.Residency)%>%dplyr::summarise(Status=n())

summary(loans_states)

#ggplot(loans_states, (aes(loans_states$State.of.Residency, loans_states$Status))+geom_bar())

#qplot(loans_states,loans_states$State.of.Residency, loans_states$Status) + geom_bar() + facet_grid(loans_states$State.of.Residency~ .)

#ggplot(loans_states, aes(loans_states$State.of.Residency)) + geom_bar() + facet_grid(loans_states$State.of.Residency~ .)

#ggplot(loans_states, aes(loans_states$State.of.Residency, loans_states$Status)) + geom_bar(stat="identity")

#density (loans_states$Status)
#anova(loans_states)

#loans$TotalBalance <- group_by(loans$User.ID..,loans$Original.Principal)

#df %>% mutate(Quarter = quarter(month)) %>% group_by(Quarter) %>% mutate(SumPre = sum(precipitation))

#---------------End Tutorial 1----------------------------------


#Adding new column for each user's original balance sum (since users can have multiple loans in their name)
loans$Original.Principal[is.na(loans$Original.Principal)] <- 0
#loansv2 <- loans %>% group_by(User.ID..) %>% mutate(origtotalbalance=sum(Original.Principal))
#Adding new column for each user's current balance sum
loans$Current.Principal[is.na(loans$Current.Principal)] <- 0
#loansv2 <- loans %>% group_by(User.ID..) %>% mutate(currenttotalbalance=sum(Current.Principal))%>% mutate(origtotalbalance=sum(Original.Principal))

#Find the Percentage the user has paid off all of their loans
loansv2$progress <- mutate(loansv2, progress = currenttotalbalance / origtotalbalance)

#write.csv(loansv2, "loanscollegecompare.csv", quote=FALSE, row.names=FALSE)

#write_csv(loans, "loansfinaltest.csv")

#write.csv(loansv2, file = "loanscollegecompare.csv", na="")

#write.csv(loans, "loanscollegecompare.csv")

#table() function - table(loans$college_type)

#lubridate - find age of borrower (NEED TO DO!)

#Levenshtein Distance Algorithm for College Name Normalization

univ <- read.csv("collegedetails.csv")
#desired_names <- tolower(as.character(univ$NAME))
#loansv2$College <- tolower(as.character(loansv2$College))

#Used to standardize college names
#index <- numeric()
#value <- numeric()
#names <- character()
#for (i in 1:nrow(loansv2)) {
#  differences <- levenshteinDist(desired_names, loansv2$College[i])
#  loansv2$College[i] <- desired_names[which.min(differences)]
#  index[i] <- which.min(differences)
#  value[i] <- min(differences)
#  names[i] <- desired_names[which.min(differences)]
#}
#
#exporting university names to spot check and ensure all went properly
#univnamecheck2 <- data.frame(names, differences, loansv2$College)

#Matched College Names Dataset - Pulling in the extra university information
cleanloans <- read.csv("slhloansclean.csv") #this is the post-cleaned version of "loans"

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

#Forgot to clean up the university majors/studies!
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
levels(cleanloans$Status) <- gsub("DFR/GRC/GRC", "DFR/GRC", levels(cleanloans$Status))

levels(factor(cleanloans$Status)) #checking number of levels now

#Cleaning up the Professions
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

#change borrower home state to obereg to match the university state obereg

levels(cleanloans$State.of.Residency) <- gsub("VT|CT|ME|MA|NH|RI", "1", levels(cleanloans$State.of.Residency))
levels(cleanloans$State.of.Residency) <- gsub("DE|DC|MD|NJ|NY|PA", "2", levels(cleanloans$State.of.Residency))
levels(cleanloans$State.of.Residency) <- gsub("IL|IN|MI|OH|WI", "3", levels(cleanloans$State.of.Residency))
levels(cleanloans$State.of.Residency) <- gsub("IA|KS|MN|MO|NE|ND|SD", "4", levels(cleanloans$State.of.Residency))
levels(cleanloans$State.of.Residency) <- gsub("AL|AR|FL|GA|KY|LA|MS|NC|SC|TN|VA|WV", "5", levels(cleanloans$State.of.Residency))
levels(cleanloans$State.of.Residency) <- gsub("AZ|NM|OK|TX", "6", levels(cleanloans$State.of.Residency))
levels(cleanloans$State.of.Residency) <- gsub("CO|ID|MT|UT|WY", "7", levels(cleanloans$State.of.Residency))
levels(cleanloans$State.of.Residency) <- gsub("AK|CA|HI|NV|OR|WA", "8", levels(cleanloans$State.of.Residency))
levels(cleanloans$State.of.Residency) <- gsub("AS|FM|GU|MH|MP|PR|PW|VI", "9", levels(cleanloans$State.of.Residency))

#Cleaning up Loan Names to just 2 type: Federal Loans or Private Loans
levels(cleanloans$Name)

levels(cleanloans$Name) <- gsub(".*Private.*", "Private", levels(cleanloans$Name))
levels(cleanloans$Name) <- gsub(".*Fargo.*", "Private", levels(cleanloans$Name))
levels(cleanloans$Name) <- gsub(".*Sallie.*", "Private", levels(cleanloans$Name))
levels(cleanloans$Name) <- gsub(".*Stafford.*|.*STAFFORD.*|.*stafford.*", "Federal", levels(cleanloans$Name))
levels(cleanloans$Name) <- gsub(".*Direct.*|.*direct.*|.*DIRECT.*", "Federal", levels(cleanloans$Name))
levels(cleanloans$Name) <- gsub(".*Federal.*|.*federal.*|.*FEDERAL.*", "Federal", levels(cleanloans$Name))
levels(cleanloans$Name) <- gsub(".*Signature.*|.*signature.*|.*SIGNATURE.*", "Federal", levels(cleanloans$Name))
levels(cleanloans$Name) <- gsub(".*subsidized.*|.*Subsidized.*|.*SUBSIDIZED.*", "Federal", levels(cleanloans$Name))
levels(cleanloans$Name) <- gsub(".*PLUS.*|.*plus.*|.*Plus.*", "Federal", levels(cleanloans$Name))
levels(cleanloans$Name) <- gsub(".*FFEL.*|.*ffel.*|.*Ffel.*", "Federal", levels(cleanloans$Name))
levels(cleanloans$Name) <- gsub(".*CONSOLIDATION.*|.*consolidation.*|.*Consolidation.*", "Federal", levels(cleanloans$Name))

OtherLoans <- !(cleanloans$Name %in% c("Private","Federal"))

cleanloans$Name[OtherLoans]<- "Private"

cleanloans$Name <- factor(cleanloans$Name)

levels(factor(cleanloans$Name))

sort(table(cleanloans$Name))

#Reducing Loan Statuses to just two
table(cleanloans$Status)
levels(cleanloans$Status) <- gsub("DFR/GRC", "Repayment", levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub("CANCELLED", "Repayment", levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub("In School", "Repayment", levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub("Interest Only", "Repayment", levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub("IN SCHOOL", "Repayment", levels(cleanloans$Status))
levels(cleanloans$Status) <- gsub("RPM", "Repayment", levels(cleanloans$Status))


#importing code from Dhiraj

cleanloans$User.DOB <- mdy(cleanloans$User.DOB)
cleanloans$User.ID.. <- as.factor(cleanloans$User.ID..)
cleanloans$Loan.Disbursement.Date <- mdy(cleanloans$Loan.Disbursement.Date)
cleanloans$Joint.Federal.Income.Tax. <- as.character(cleanloans$Joint.Federal.Income.Tax.)

#In order to use choropleth maps, I need the state names
data("state.regions")

#Loans per state
state_loans <- cleanloans%>%group_by(abb=univstate)%>%dplyr::summarize(value=n())
state_loans <- state_loans[complete.cases(state_loans),]
state_loans <- left_join(state_loans, state.regions, by='abb')
state_choropleth(state_loans, title="Loans Per State")

#Defaulted Loans as per status
status <- 'Default'
state_loans <- cleanloans%>%filter(Status==status)%>%group_by(abb=univstate)%>%dplyr::summarize(value=n())
state_loans <- state_loans[complete.cases(state_loans),]
state_loans <- left_join(state_loans, state.regions, by='abb')
state_choropleth(state_loans,title=paste0("Loans As Per Status:",status))

#Loans as per rate of interest
state_loans <- cleanloans%>%group_by(abb=univstate)%>%dplyr::summarize(value=mean(Rate))
state_loans <- state_loans[complete.cases(state_loans),]
state_loans <- left_join(state_loans, state.regions, by='abb')
state_choropleth(state_loans,title="Average Rate of Interest")


#Every user could have more than one loan. Let's sum up total loans per User
#Total Loan Amount
loan_amount <- cleanloans%>%group_by(User.ID..)%>%dplyr::summarize(Total=sum(Original.Principal))
summary(loan_amount$Total)

#TF - adding back to cleanloans dataframe
cleanloans$totaluserbalance <-left_join(cleanloans,loan_amount)
cleanloans$totaluserbalance <- as.numeric(cleanloans$totaluserbalance)

lapply(cleanloans$totaluserbalance, as.numeric)
#TF - Average balance (by user ID) by state
#sapply(cleanloans, class)

state_loans <- cleanloans%>%group_by(abb=univstate)%>%dplyr::summarize(value=mean(cleanloans$totaluserbalance))
state_loans <- state_loans[complete.cases(state_loans),]
state_loans <- left_join(state_loans, state.regions, by='abb')
state_choropleth(state_loans,title="Average User Balance")

#(TF NOTE - NEED TO WORK ON) Are guys who have more number of loans more likely to default?
#loan_count <- tally(group_by(cleanloans, User.ID..))
#loans_count <- 


#Average Interest rate of defaulted loans vs Average Interest rate in repayment
#Average Interest rate by year of loan disbursement

#AVG INTEREST RATES by Fixed vs Variable Interest Rates
#Type <- 'Fixed'
loan_rates <- cleanloans%>%group_by(Type)%>%dplyr::summarize(value=mean(Rate))

ggplot(loan_rates, (aes(x = Type, y = value))) + geom_bar(stat="identity")

#AVG INTEREST RATES by Status
loan_rates_status <- cleanloans%>%group_by(Status)%>%dplyr::summarize(value=mean(Rate))

ggplot(loan_rates_status, (aes(x = Status, y = value))) + geom_bar(stat="identity")

#AVG INTEREST RATES by Loan Type (Federal or Private)
loan_rates_name <- cleanloans%>%group_by(Name)%>%dplyr::summarize(value=mean(Rate))

ggplot(loan_rates_name, (aes(x = Name, y = value))) + geom_bar(stat="identity")

#AVG BALANCE by Loan Type (Federal or Private)
loan_balance_name <- cleanloans%>%group_by(Name)%>%dplyr::summarize(value=mean(Original.Principal))

ggplot(loan_balance_name, (aes(x = Name, y = value))) + geom_bar(stat="identity")

#AVG BALANCE by Rate Type (Fixed vs Variable)
loan_balance_type <- cleanloans%>%group_by(Type)%>%dplyr::summarize(value=mean(Original.Principal))

ggplot(loan_balance_type, (aes(x = Type, y = value))) + geom_bar(stat="identity")

#AVG BALANCE by Status
loan_balance_status <- cleanloans%>%group_by(Status)%>%dplyr::summarize(value=mean(Original.Principal))

ggplot(loan_balance_status, (aes(x = Status, y = value))) + geom_bar(stat="identity")

#Status by Majors
ggplot(cleanloans, aes(Major, fill=Status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Fixed/Variable Loans by Major
ggplot(cleanloans, aes(Major, fill=Type))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Dhiraj notes to really just look at data w/ Status in it, since that's ultimately what we will be predicting

loan_status_type_major <- cleanloans%>%group_by(Status)%>%dplyr::summarize(value=mean(Original.Principal))

ggplot(loan_status_type_major, aes(value, fill=Status)) + geom_bar()

p <- ggplot(loan_status_type_major, aes(value, fill=Status)) + geom_bar()
p + facet_grid(. ~Major)



#-----Finding buckets for balance-----
quantile(cleanloans$Original.Principal)
#cleanloans$loanbal <- replace(DF, DF > -1.5 & DF < 1.5, 0)

#c1 <- cut(cleanloans$Original.Principal, breaks = 4)

cleanloanstest2 <- cleanloans
quantile(cleanloanstest2$Original.Principal)

cuttest <- cut(cleanloans$Original.Principal, breaks = c(0, 2565, 5500, 353327),
                                labels = c("Low", "Average", "High"))

#cleanloanstest <- transform(cleanloanstest, originalprincbucket = cut(cleanloanstest$Original.Principal, breaks = c(0, 2565, 5500, 353327),
#                                                                      labels = c("Low", "Average", "High")))

summarise(cleanloanstest2$Original.Principal)

write.csv(cleanloanstest2, file = "cleanloanstest2.csv")

cleanloans$origtotalbalance <- NULL
cleanloans$currenttotalbalance <- NULL
cleanloans$totaluserbalance <- NULL

cleanloanstest2$origincalprincbucket <- cleanloanstest2$Original.Principal

cleanloanstest2$originalprincipalbucket <- cut(cleanloanstest2$Original.Principal, breaks = c(0, 2565, 5500, 353327),
                                               labels = c("Low", "Average", "High"))

write.csv(cleanloanstest)


as.data.frame(cuttest)

cleanloanstest$originalprincipalbucket <- c(cuttest)

do.call(cbind, cuttest)

cor(cleanloans$Rate, cleanloans$Original.Principal)

p01 <- crs %>%
  with(cleanloans[sample,]) %>%
  dplyr::select(Rate) %>%
  ggplot2::ggplot(ggplot2::aes(x=Rate)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::ggtitle("Distribution of Interest Rates") +
  ggplot2::labs(y="Density")
ggplot2::ggtitle("Distribution of Interest Rates") +
  # Display the plots.
  
  gridExtra::grid.arrange(p01)

p <- ggplot(cleanloans, aes(cleanloans$Rate, cleanloans$Status))

p + geom_boxplot() + geom_jitter(width = 0.2) + coord_flip()
