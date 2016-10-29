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

univ <- read.csv("collegedetails.csv")
desired_names <- tolower(as.character(univ$NAME))
loans_finalv2$College <- tolower(as.character(loans_finalv2$College))

index <- numeric()
value <- numeric()
names <- character()
for (i in 1:nrow(loans_finalv2)) {
  differences <- levenshteinDist(desired_names, loans_finalv2$College[i])
  #loans_finalv2$College[i] <- desired_names[which.min(differences)]
  index[i] <- which.min(differences)
  value[i] <- min(differences)
  names[i] <- desired_names[which.min(differences)]
}

#exporting university names to spot check??
univnamecheck2 <- data.frame(desired_names, differences, loans_finalv2$College)

