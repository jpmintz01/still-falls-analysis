#--------------------Libraries--------------------
library(plyr)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(lme4)
library(lmerTest)
library(rcompanion)
library(DescTools)
library(tibble)
library(caret)
library(MASS)
library(zoo)
library(vcd)
library(neuralnet)
library(RVAideMemoire)
library(ARTool)
library(nlme)
library(nnet)
library(car)
library(fitdistrplus)
library(logspline)
library(scales)
library(corrplot)
library(stratEst)
library(ggplot2)
# library(plotly) #requires a login
if(!require(psych)){install.packages("psych")}
if(!require(nlme)){install.packages("nlme")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}



#--------------------Data Ingestion functions--------------------
returnRowIfNot <- function(data, columnID, value){  #send correct values to this function to return all the rows with INCORRECT values
  completeVec <- data[,columnID] != value #ID the row if the column value is NOT equal to value
  return(data[completeVec,])  #returns rows where column does not equal the value
}

stripBadRows <- function(data, columnID, value){ # send correct values to this function to return all the rows with correct values
  completeVec <- data[,columnID] == value #ID the row if the column value is NOT equal to value
  return(data[completeVec,]) #returns all rows that where c
}
# stripCols

#store #attention_check, contamination, early quits (__current_page_name /= Post-game survey), and informed_consent fails into new data frame
saveFails <- function (data){
  fail_data <- returnRowIfNot(data, "informed_consent.1.player.agree_to_participate", "TRUE")
  fail_data <- rbind(returnRowIfNot(data, "post_game_survey.1.player.experiment_contamination", ""), fail_data)
  ## fix experiment_contamination to binary or radio_button
  ## save experiment_contamination to fail_data but don't strip it from good data (save as "possible fail")
  fail_data <- rbind(returnRowIfNot(data, "post_game_survey.1.player.attention_check", "Georgia"), fail_data)
  fail_data <- rbind(returnRowIfNot(data, "participant._current_app_name", "post_game_survey"), fail_data)
  ## could also change this to compare _current_page_name vs "Debrief"
  ## maybe compare these two in a logical function instead of the previous post_game_survey
  #participant._index_in_pages should be >= participant._max_page_index to indiprinte a player completed the game
  fail_data <- unique(fail_data) #strip dupliprintes that failed multiple checks

  fail_data <- subset(fail_data, participant.label!="")

  return (fail_data) #return an entire data frame of failed participants including POSSIBLE fails from contamination - researcher must check this string to see what they wrote...
}
#strip the fails out
stripFails <- function (data){
  # good_data <- stripBadRows(data, "informed_consent.1.player.agree_to_participate", "TRUE")
  # good_data <- stripBadRows(good_data, "post_game_survey.1.player.attention_check", "Georgia")
  good_data <- subset(data, participant._current_app_name=="post_game_survey")
  good_data <- subset(good_data, post_game_survey.1.player.attention_check=="Georgia")

  #temporarily strip out obvious non-believers in human competitors




  ## maybe compare these two in a logical function instead of the previous post_game_survey
  #participant._index_in_pages should be >= participant._max_page_index to indiprinte a player completed the game
  good_data <- unique(good_data) #remove dupliprintes
  return (good_data)
}

stripBadCols <- function (data, badColList = c("session.mturk_HITGroupId", "session.mturk_HITId", "session.is_demo","session.comment","session.experimenter_name","group.id_in_subsession","player.player_guess_adv_1_type","participant.mturk_assignment_id","participant.mturk_worker_id","participant.visited", "participant.payoff_plus_participation_fee", "session.config.participation_fee","session.config.real_world_currency_per_point","informed_consent.1.player.id_in_group","informed_consent.1.player.payoff","informed_consent.1.group.id_in_subsession","informed_consent.1.subsession.round_number")
){
  #other ones to remove: "prisoner_multiplayer.x.player.id_in_group" where x is round number
  # rps.x.player.id_in_group
  num_RPS_rounds <- data$num_RPS_rounds[1,]
  num_PW_rounds <- data$num_PW_rounds[1,]
  #find column label/id of first columsn unused round and delete them all (or could I just ignore them?)
  #is.na()
  #which(colnames(datafile)=="prisoner_multiplayer.10.player.decision_of_adv_1") # returns the column reference of
  data[,badColList] <- NULL #delete unused columns
  data <- Filter(function(data)!all(is.na(data)), data)
  return (data)
}

getColumns <- function(data, pattern) {#returns column names from dataframe x containing regular expression y
  ### doesn't work right now due to "argument 'pattern' has length > 1 and only the first element will be used" error
  return (data[grepl(pattern, names(data))])

  }

pwChangeColNames <- function (data, num_rounds) { #inputs pw columns and changes the names to round numbers
  # and strips off the pw data not played
  num_cols <- ncol(data)
  if (is.na(data[[1:1]])) { #checks if player played pw or pw_2 (before or after RPS)
    #if the first value of pw is NA then player must have played pw_2
    data <- data[c(((num_cols/2)+1):((num_cols/2)+num_rounds))]
  } else {
    data <- data[c(1:num_rounds)]
  }

  colnames(data) <- c(1:ncol(data))
  return (data)
}



#--------------------Data Ingestion runtime-------------------
#choose and read the experiment data file
#filepath <- file.choose()
exp_data_file_path <- "/Users/johnpaulmintz/Dissertation/Analysis (git)/Still-Falls Analysis/all_apps_wide_2019-01-21.csv"
datafile <- read.csv(exp_data_file_path, header = TRUE, stringsAsFactors = FALSE)
fail_data <- saveFails(datafile) #save the fails to another file for researcher review
good_data <- stripFails(datafile) #strip the fails off the man datafile
# good_data <- stripBadCols(datafile)
new_data <- stripBadCols(good_data)  #some error here...
num_pw_rounds <- new_data$session.config.num_PW_rounds[[1]]
#split off the columns which describe pw_vs_human
num_rps_rounds <- new_data$session.config.num_RPS_rounds[[1]]
filepath <- "/Users/johnpaulmintz/Dissertation/Analysis (git)/Still-Falls Analysis/pw_all_data_onehot.csv"
pw_from_keras <- read.csv(filepath, header=TRUE, stringsAsFactors = TRUE)
filepath <- "/Users/johnpaulmintz/Dissertation/Analysis (git)/Still-Falls Analysis/AI_output_all_strategies.csv"
pw_Axelrod_AI <- read.csv(filepath, header=TRUE, stringsAsFactors = TRUE)
filepath <- "/Users/johnpaulmintz/Dissertation/Analysis (git)/Still-Falls Analysis/HAI_output_all_strategies.csv"
pw_Axelrod_HAI <- read.csv(filepath, header=TRUE, stringsAsFactors = TRUE)
filepath <- "/Users/johnpaulmintz/Dissertation/Analysis (git)/Still-Falls Analysis/Human_output_all_strategies.csv"
pw_Axelrod_Human <- read.csv(filepath, header=TRUE, stringsAsFactors = TRUE)
#read the "vs AI" strategies and chocies from the axelrod library
b <- as.data.frame(matrix(pw_Axelrod_AI$Player.name, nrow=length(pw_Axelrod_AI$Player.name)))
n <- gsub("C", "1", pw_Axelrod_AI$Actions)
n <- gsub("D", "0", n)
a<-strsplit(as.character(n),"")
a_AI <- data.frame(matrix(unlist(a), nrow=length(a), byrow=T),stringsAsFactors=FALSE)
names(a_AI) <- c(1:10)
AI_actions <- cbind(pw_Axelrod_AI$Player.name, a_AI)
names(AI_actions)[1] <- "Strategy"
AI_actions$Adversary <- "AI"

#read the "vs HAI" strategies and chocies from the axelrod library
b <- as.data.frame(matrix(pw_Axelrod_HAI$Player.name, nrow=length(pw_Axelrod_HAI$Player.name)))
n <- gsub("C", "1", pw_Axelrod_HAI$Actions)
n <- gsub("D", "0", n)
a<-strsplit(as.character(n),"")
a_HAI <- data.frame(matrix(unlist(a), nrow=length(a), byrow=T),stringsAsFactors=FALSE)
names(a_HAI) <- c(1:10)
HAI_actions <- cbind(pw_Axelrod_HAI$Player.name, a_HAI)
names(HAI_actions)[1] <- "Strategy"
HAI_actions$Adversary <- "Human+AI"

#read the "vs Human" strategies and chocies from the axelrod library
b <- as.data.frame(matrix(pw_Axelrod_Human$Player.name, nrow=length(pw_Axelrod_Human$Player.name)))
n <- gsub("C", "1", pw_Axelrod_Human$Actions)
n <- gsub("D", "0", n)
a<-strsplit(as.character(n),"")
a_Human <- data.frame(matrix(unlist(a), nrow=length(a), byrow=T),stringsAsFactors=FALSE)
names(a_Human) <- c(1:10)
Human_actions <- cbind(pw_Axelrod_Human$Player.name, a_Human)
names(Human_actions)[1] <- "Strategy"
Human_actions$Adversary <- "Human"

#--------------------Time -data transformation--------------------
#choose and readthe all_data data file
#filepath <- file.choose()
train_data_file_path <-"/Users/johnpaulmintz/Dissertation/Analysis (git)/Still-Falls Analysis/all_data_train.csv"
# #read the file with headers
all_data <- read.csv(train_data_file_path, header = TRUE, stringsAsFactors = TRUE)

time_data_file_path <- "/Users/johnpaulmintz/Dissertation/Analysis (git)/Still-Falls Analysis/TimeSpent (accessed 2019-01-21).csv"
time_data <- read.csv(time_data_file_path, header = TRUE, stringsAsFactors = TRUE)

#participant.label <-> participant.code decoder
label_code <- as.data.frame(cbind(new_data$participant.label, new_data$participant.code))
names(label_code) <- c("id", "code")
#remove participants not in new_data
time_data <- time_data[time_data$participant__code %in% label_code$code,]
colnames(time_data)[colnames(time_data) == "participant__code"] <- "id"
time_data$id <- factor(time_data$id)

time_data$session_id <- NULL
time_data$participant__id_in_session <- NULL
time_data$subsession_pk <- NULL
time_data$auto_submitted <- NULL
time_data$time_stamp <- NULL
time_data$id <- as.character(time_data$id)
for (i in time_data$id) {
  time_data$id[time_data$id == i] <- as.character(label_code[label_code$code == i,]$id)
}
time_data$id <- as.factor(time_data$id)
time_data[time_data$app_name == "prisoner_multiplayer_2",]$app_name <- "prisoner_multiplayer"

#--------------------PW - data Transformation---------
pw_ids <- subset(new_data, select=participant.label)
pw_cols <- bind_cols(pw_ids, new_data[grepl('^prisoner.*decision_vs_adv_.*$', names(new_data))])
pw_vs_human_cols <- bind_cols(pw_ids, new_data[grepl('^prisoner.*decision_vs_adv_1$', names(new_data))]) #get pw_vs_human choices as a df row:player, col:colnames
names(pw_cols) <-  gsub(pattern = "participant.label", replacement = "id", x = names(pw_cols))
pw_cols[pw_cols==""]<-NA #cut out all the "" values and replace with NA
pw_first <- subset(pw_cols, is.na(prisoner_multiplayer_2.1.player.decision_vs_adv_1))
pw_first[,(32:61)] <- NULL

# pw_first$Order <- 1

pw_second <- subset(pw_cols, is.na(prisoner_multiplayer.1.player.decision_vs_adv_1))
pw_second[,(2:31)] <- NULL
# pw_second$Order <- 2

# a <- as.data.table(a)[,(32:61):=NULL]
names(pw_first) <-  gsub(pattern = "prisoner_multiplayer.", replacement = "", x = names(pw_first)) # removes the leading 'rps.' - just a cleanup
names(pw_second) <-  gsub(pattern = "prisoner_multiplayer_2.", replacement = "", x = names(pw_second)) # removes the leading 'rps.' - just a cleanup
pw_cols <- rbind(pw_first,pw_second)
names(pw_cols) <-  gsub(pattern = "player.", replacement = "", x = names(pw_cols)) # removes 'player.' - just a cleanup
# names(pw_cols)[grepl('^[1-9][.](.*)$', names(pw_cols))] <- paste0("0", names(pw_cols)[grepl('^[1-9][.](.*)$', names(pw_cols))])
names(pw_cols) <-  gsub(pattern = "decision_vs_adv_1", replacement = "Human", x = names(pw_cols))
names(pw_cols) <-  gsub(pattern = "decision_vs_adv_2", replacement = "Human+AI", x = names(pw_cols))
names(pw_cols) <-  gsub(pattern = "decision_vs_adv_3", replacement = "AI", x = names(pw_cols))

list_of_human_cols <- names(pw_cols)[grepl('^.*Human$', names(pw_cols))]
list_of_HAI_cols <- names(pw_cols)[grepl('^.*Human\\+AI$', names(pw_cols))]
list_of_AI_cols <- names(pw_cols)[grepl('^(.*)[.]AI$', names(pw_cols))]

pw_human_cols<- melt(pw_cols, id="id", measure=list_of_human_cols)
pw_human_cols$variable <- as.integer(gsub(pattern='[.](.*)$', replacement="", x = pw_human_cols$variable))
human_col_fill <- c(rep("Human",nrow(pw_human_cols)))
pw_human_cols <- add_column(pw_human_cols, human_col_fill, .after="variable")
colnames(pw_human_cols) <- c("id","Round","Adversary","Choice")

pw_HAI_cols<- melt(pw_cols, id="id", measure=list_of_HAI_cols)
pw_HAI_cols$variable <- as.integer(gsub(pattern='[.](.*)$', replacement="", x = pw_HAI_cols$variable))
HAI_col_fill <- c(rep("Human+AI",nrow(pw_HAI_cols)))
pw_HAI_cols <- add_column(pw_HAI_cols, HAI_col_fill, .after="variable")
colnames(pw_HAI_cols) <- c("id","Round","Adversary","Choice")

pw_AI_cols<- melt(pw_cols, id="id", measure=list_of_AI_cols)
pw_AI_cols$variable <- as.integer(gsub(pattern='[.](.*)$', replacement="", x = pw_AI_cols$variable))
AI_col_fill <- c(rep("AI",nrow(pw_AI_cols)))
pw_AI_cols <- add_column(pw_AI_cols, AI_col_fill, .after="variable")
colnames(pw_AI_cols) <- c("id","Round","Adversary","Choice")

pw_cols <- rbind(pw_human_cols,pw_HAI_cols,pw_AI_cols)

pw_cols$Adversary <- as.factor(pw_cols$Adversary)
pw_cols <- pw_cols[!is.na(pw_cols$Choice),] #strip the ones who never did pw
pw_cols <- pw_cols[pw_cols$id!="0pr16",] #strip off the test case
# pw_cols <- pw_cols[!(pw_cols$id %in% c("gdg26","s4441","7ic14","oez14")),] #strip off those who didn't believe the humans in the game were real

pw_ids <- levels(factor(pw_cols$id))
pw_array <- xtabs(~Adversary+my.decision+id, data=pw_all_data_with_demo) #more R-like way of creating the array than the ten lines below
row.names <- c("AI","Human","Human+AI")
col.names <- c("Peace", "War")
# pw_array <- array(NA, dim = c(3,2,length(pw_ids)), dimnames=list(row.names, col.names, pw_ids))
# #creates an array where [x,,] = rows of Adversary types, [,y,] = columns of advisor choices, and [,,z]=player id's
Adversary_list <- c("AI","Human","Human+AI")
Choice_list <- c("Peace", "War")
# for (i in pw_ids) {
#   a <- subset(pw_cols, id==i)
#   for (j in Adversary_list) {
#     b <- subset (a, Adversary==j)
#     for (k in Choice_list){
#       pw_array[j,k,i] <- sum(b$Choice==k)
#     }
#   }
# }
#
# pw_HvAI_array <- pw_array[-3,,]
# rownames(pw_HvAI_array)[1:2] <- rownames(pw_HvAI_array)[2:1] #swap rownames
# pw_HvAI_array[1:2,,] <- pw_HvAI_array[2:1,,] #swap row values (to make Human first)
# pw_HvHAI_array <- pw_array[-1,,]
pw_round_1s <- pw_cols[pw_cols$Round == 1,]
pw_round_1s <- pw_round_1s[!is.na(pw_round_1s$Choice),]
pw_round_1s$id <- factor(pw_round_1s$id)
pw_round_1s$Choice <- factor(pw_round_1s$Choice)
pw_round_1s_sum <- rowSums(xtabs(~Adversary+Choice+id, data=pw_round_1s), dims=2)

# pw_percent_matrix <- matrix(nrow=length(pw_ids),ncol=3)
# rownames(pw_percent_matrix) <- pw_ids
# for (i in pw_ids){
#   pw_percent_matrix[i,] <- pw_array[,"Peace",i]/pw_array[,"War",i]
# }
# pw_vs_human <- bind_cols(new_data[grepl('^prisoner.*')])
# rps_cols <- bind_cols(new_data[grepl('^rps.*advisor_choice$', names(new_data))], new_data[grepl('^rps.*adv_1_type$', names(new_data))])#get rps_vs_human choices as a df row:player, col:colnames
# pw_vs_human <- pwChangeColNames(pw_vs_human_cols, num_pw_rounds) #get a matrix of pw choices (row:player)x(column:round) with only the pw version (pw or pw_2) played
# pw_vs_hai_cols <- new_data[grepl('^prisoner.*decision_vs_adv_2$', names(new_data))] #get pw_vs_human choices as a df row:player, col:colnames
#pw_vs_human_cols <- getColumns(good_data, '^prisoner.*decision_vs_adv_1$')
# pw_vs_hai <- pwChangeColNames(pw_vs_hai_cols, num_pw_rounds) #get a matrix of pw choices (row:player)x(column:round) with only the pw version (pw or pw_2) played
# pw_vs_ai_cols <- new_data[grepl('^prisoner.*decision_vs_adv_3$', names(new_data))] #get pw_vs_human choices as a df row:player, col:colnames
#pw_vs_human_cols <- getColumns(good_data, '^prisoner.*decision_vs_adv_1$')
# pw_vs_ai <- pwChangeColNames(pw_vs_ai_cols, num_pw_rounds) #get a matrix of pw choices (row:player)x(column:round) with only the pw version (pw or pw_2) played
#this loop fills my.round1decision
all_data$my.round1decision[all_data$period == 1]<-(all_data$my.decision[all_data$period ==1]=="coop")*1
# all_data$my.decision<-(all_data$my.decision=="coop")*1
c<- zoo(all_data$my.round1decision)
d<- na.locf(c)
all_data$my.round1decision <- as.integer(d)

pw_cols_peace_by_id <- as.data.frame.matrix(xtabs(~ id + Adversary, data = pw_cols[pw_cols$Choice=="Peace",]))
pw_cols_by_id <- as.data.frame(xtabs(~id + Adversary + Choice, data = pw_cols))


#--------------------PW - Machine Learning/NNET/TFT section--------------------


r <-	1
t <- 3
s <- -3
p <- -1
risk <- 0
error <-0
infin <- 1
delta <- 0.9
contin <- 0
r1 <- 1/3
r2 <- 2/3
human_adv_choices <- c(1, 1,	0,	1,	0,	0,	0,	1,	1,	0) #sum in first 9=5
human_tft_choices <- c("Peace", "Peace",  "Peace",  "War",  "Peace",  "War",  "War",  "War",  "Peace",  "Peace") #sum in first9 = 5
# all_data.human_adv_choices <- subset(subset(subset(subset(subset(subset(subset(subset(subset(all_data,other.decision9==1),other.decision8==1),other.decision7==0),other.decision6==1),other.decision5==0),other.decision4==0),other.decision3==0),other.decision2==1),other.decision1==1)

hai_adv_choices <- c(1,	0,	1,	1,	0,	1,	1,	1,	1,	0) #sum=7
hai_tft_choices <- c("Peace",	"Peace",	"War",	"Peace",	"Peace",	"War",	"Peace",	"Peace",	"Peace",	"Peace") #sum in first9 = 7
# all_data.hai_adv_choices <- subset(subset(subset(subset(subset(subset(subset(subset(subset(all_data,other.decision9==1),other.decision8==0),other.decision7==1),other.decision6==1),other.decision5==0),other.decision4==1),other.decision3==1),other.decision2==1),other.decision1==1)

ai_adv_choices <- c(0,	0,	0,	1,	1,	0,	0,	0,	1,	0)#sum=3
ai_tft_choices <- c("Peace",	"War",	"War",	"War",	"Peace",	"Peace",	"War",	"War",	"War",	"Peace") #sum in first9= 3
# all_data.ai_adv_choices <- subset(subset(subset(subset(subset(subset(subset(subset(subset(all_data,other.decision9==0),other.decision8==0),other.decision7==0),other.decision6==1),other.decision5==1),other.decision4==0),other.decision3==0),other.decision2==0),other.decision1==1)


pw_all_data_colnames <- c("X","id","Adversary","period","my.decision","risk","delta","r1","r2","error","data","my.round1decision","my.decision1","my.decision2","my.decision3","my.decision4","my.decision5","my.decision6","my.decision7","my.decision8","my.decision9","other.decision1","other.decision2", "other.decision3" ,"other.decision4" ,"other.decision5", "other.decision6", "other.decision7", "other.decision8" ,"other.decision9", "my.payoff1","my.payoff2","my.payoff3", "my.payoff4","my.payoff5","my.payoff6","my.payoff7","my.payoff8","my.payoff9","other.payoff1","other.payoff2","other.payoff3","other.payoff4","other.payoff5","other.payoff6","other.payoff7","other.payoff8","other.payoff9","r","s","t","p","infin", "contin","group")
pw_all_data <- matrix(NA, nrow=nrow(pw_cols), ncol=length(pw_all_data_colnames))
colnames(pw_all_data) <- pw_all_data_colnames
pw_all_data <- as.data.frame(pw_all_data)
pw_all_data$r <-	1 #do I need to normalize these?
pw_all_data$t <- 3#do I need to normalize these?
pw_all_data$s <- -3#do I need to normalize these?
pw_all_data$p <- -1#do I need to normalize these?
pw_all_data$risk <- 0
pw_all_data$error <-0
pw_all_data$infin <- 1
pw_all_data$delta <- 0.9
pw_all_data$contin <- 0
pw_all_data$r1 <- 1/3
pw_all_data$r2 <- 2/3
d<- arrange(pw_cols, id)
pw_all_data$period <- d$Round
pw_all_data$Adversary <- d$Adversary
pw_all_data$id <- d$id
pw_all_data$my.decision <- d$Choice  #need to change to coop/defect?
pw_all_data$other.decision <- rep(c(human_adv_choices, hai_adv_choices, ai_adv_choices), nrow(pw_all_data)/30) #delete this after use

#this loop fills my.round1decision
pw_all_data$my.round1decision[pw_all_data$period == 1]<-(pw_all_data$my.decision[pw_all_data$period ==1]=="Peace")*1
c<- zoo(pw_all_data$my.round1decision)
d<- na.locf(c)
pw_all_data$my.round1decision <- as.integer(d)
#this nested for loop fills my.decisionX and other.decisionX with the previous choices (as 1 or zero) based on my.decision or other.decision which are filled above
for (i in 2:10) {
  for (j in 1:(i-1)) {
    my_dec_col <- paste0("my.decision",j)
    pw_all_data[[my_dec_col]][pw_all_data$period == i] <- (pw_all_data$my.decision[pw_all_data$period == (i-1)]=="Peace")*1
    other_dec_col <- paste0("other.decision",j)
    pw_all_data[[other_dec_col]][pw_all_data$period == i] <- pw_all_data$other.decision[pw_all_data$period == (i-1)]
  }
}
pw_all_data$other.decision <- NULL #delete other.decision column since it has no simile in the example
pw_all_data$X <- c(1:nrow(pw_all_data))
#begin machine learning (generalized linear model) section
predictors <- c("period","risk","delta","r1","r2","error", "r","s","t","p","infin","contin","my.round1decision")

# lmFit_exp_2_10 <- train(my.decision~my.round1decision+my.decision1+other.decision1+period, data = pw_all_data[pw_all_data[pw_all_data$Adversary=="Human",]$period >=2,], method = 'glm', na.action = na.pass) #p_exp is as good as the bigger model below
# all_data$my.decision <- as.factor(all_data$my.decision)
# all_data$my.decision1 <- as.factor(all_data$my.decision1)
# all_data$my.decision2 <- as.factor(all_data$my.decision2)
# all_data$my.decision3 <- as.factor(all_data$my.decision3)
# all_data$other.decision1 <- as.factor(all_data$other.decision1)
# all_data$other.decision2 <- as.factor(all_data$other.decision2)
# all_data$other.decision3 <- as.factor(all_data$other.decision3)
# all_data$my.round1decision <- as.factor(all_data$my.round1decision)
# pw_all_data$my.decision <- as.factor(pw_all_data$my.decision)
# pw_all_data$my.decision1 <- as.factor(pw_all_data$my.decision1)
# pw_all_data$my.decision2 <- as.factor(pw_all_data$my.decision2)
# pw_all_data$my.decision3 <- as.factor(pw_all_data$my.decision3)
# pw_all_data$other.decision1 <- as.factor(pw_all_data$other.decision1)
# pw_all_data$other.decision2 <- as.factor(pw_all_data$other.decision2)
# pw_all_data$other.decision3 <- as.factor(pw_all_data$other.decision3)
# pw_all_data$my.round1decision <- as.factor(pw_all_data$my.round1decision)
#this line below uses all_data to train static model
lmFit<-train(my.decision~r1+r2+risk+error+delta+r1:delta+r2:delta+infin+contin, data = subset(all_data, period == 1), method = 'glm', na.action = na.pass)
#this line below uses above model to predict static model (round 1's)
outcomes_1 <- predict(lmFit, subset(pw_all_data, period == 1)) # this is the static outcomes
levels(outcomes_1) <- c("Peace","War")
#this line below uses all_data to train line 2 in the dynamic model (the first dynamic row) - could use something else
lmFit1<-train(my.decision~r1+r2+risk+error+delta+r1:delta+r2:delta+infin+contin+delta:infin+my.decision1+other.decision1+error:other.decision1+period, data = all_data[all_data$period==2,], method = 'glm', na.action = na.pass)
#this line below uses the dynamic model to predict all the rounds 2-10
outcomes_2 <- predict(lmFit1, pw_all_data)
levels(outcomes_2) <- c("Peace","War")

#this line below uses all_data to train lines 3-10 in the dynamic model (the first dynamic row) - could use something else (can remove this since it only gains additional .5% in predictive power)
lmFit2<-train(my.decision~my.round1decision+r1+r2+risk+error+delta+r1:delta+r2:delta+infin+contin+delta:infin+my.decision1+other.decision1+error:other.decision1+period+my.decision2+other.decision2, data = all_data[all_data$period==3,], method = 'glm', na.action = na.pass)
lmFit2<-train(my.decision~my.round1decision, data = all_data, method = 'glm', na.action = na.pass)
#this line below uses the dynamic model to predict all the rounds 2-10
outcomes_3 <- predict(lmFit2, pw_all_data)
levels(outcomes_3) <- c("Peace","War")

lmFit3<-train(my.decision~my.round1decision+r1+r2+risk+error+delta+r1:delta+r2:delta+infin+contin+delta:infin+my.decision1+other.decision1+error:other.decision1+period+my.decision2+other.decision2+my.decision3+other.decision3, data = all_data, method = 'glm', na.action = na.pass)
# lmFit2<-train(my.decision~my.round1decision, data = all_data, method = 'glm', na.action = na.pass)
#this line below uses the dynamic model to predict all the rounds 2-10
outcomes_4_10 <- predict(lmFit3, pw_all_data)
levels(outcomes_4_10) <- c("Peace","War")

# other GLMM
# m <- glmer(my.decision ~ my.round1decision + my.decision1 + my.decision2 + other.decision1 + other.decision2 + Adversary + (1|id), data=df, family = binomial(link = "logit"))


r <- data.frame(matrix(outcomes_2, ncol=(nrow(pw_cols)/10)))[1,]
o <- data.frame(matrix(outcomes_1, ncol=(nrow(pw_cols)/10)))
t <- data.frame(matrix(outcomes_3, ncol=(nrow(pw_cols)/10)))[1,]
u <- data.frame(matrix(outcomes_4_10, ncol=(nrow(pw_cols)/10)))
# r_exp <- data.frame(matrix(outcomes_exp_2_10, ncol=(nrow(pw_cols)/10)))
q <- rbind(o,r,t,u)
q<-gather(q)

# # test nnet on pw_all_data
pw_all_data_subset <- pw_all_data
pw_all_data_subset$my.decision <- as.factor(pw_all_data_subset$my.decision)
pw_all_data_subset$my.decision <- as.numeric(as.factor(pw_all_data_subset$my.decision))
pw_all_data_subset$my.decision[pw_all_data_subset$my.decision==2] <- 0
predictors <- c("my.round1decision","my.decision1","other.decision1","period")
formula_my <- "my.decision~"
formula_predictors <- paste(predictors[1:length(predictors)], collapse="+")
f <- as.formula(paste(formula_my, formula_predictors))



# all_data_subset <- all_data[1:10000,]
set.seed(3456)
trainIndex <- createDataPartition(all_data$period, p = .1,
                                  list = FALSE,
                                  times = 1)
all_data_subset <- all_data[trainIndex,]
all_data_subset$my.decision <- as.factor(all_data_subset$my.decision)
all_data_subset$my.decision <- as.numeric(as.factor(all_data_subset$my.decision))
all_data_subset$my.decision[all_data_subset$my.decision==2] <- 0
# nn_all <- neuralnet(f,data=all_data_subset[all_data_subset$period>1,],hidden=c(5,3,2),act.fct = "logistic",linear.output=FALSE, threshold = 0.4)

nn_all_2 <- neuralnet(f,data=all_data_subset[all_data_subset$period==2,],hidden=c(5,3,2),act.fct = "logistic",linear.output=FALSE, threshold = 0.3)

predictors_3_plus <- c("my.round1decision","my.decision1","other.decision1","period","my.decision2","other.decision2")
formula_my <- "my.decision~"
formula_predictors <- paste(predictors_3_plus[1:length(predictors_3_plus)], collapse="+")
f_3_plus <- as.formula(paste(formula_my, formula_predictors))
nn_all_3_plus<- neuralnet(f_3_plus,data=all_data_subset[all_data_subset$period>2,],hidden=c(5,3,2),act.fct = "logistic",linear.output=FALSE, threshold = 0.3)


pw_all_data_sub_subset <- pw_all_data_subset[,predictors]
pw_nn_output<- compute(nn_all_2, pw_all_data_sub_subset)$net.result
pw_all_data_sub_subset <- pw_all_data_subset[,predictors_3_plus]
pw_nn_output_3_plus<- compute(nn_all_3_plus, pw_all_data_sub_subset)$net.result

r_n <- data.frame(matrix(pw_nn_output, ncol=(nrow(pw_cols)/10)))[c(1:2),]
t_n <- data.frame(matrix(pw_nn_output_3_plus, ncol=(nrow(pw_cols)/10)))[c(3:10),]

q_n <- rbind(r_n,t_n)
q_n<-gather(q_n)[,2]

pw_nn_output <- q_n


pw_nn_output[is.na(pw_nn_output)] <- 1
pw_nn_output_rounded <- round(pw_nn_output) #(is this the right function for classifiprintion?)


# r_exp <- data.frame(matrix(outcomes_exp_2_10, ncol=(nrow(pw_cols)/10)))



pw_nn_outcome <- pw_nn_output_rounded
pw_nn_outcome[pw_nn_outcome==1]<-"Peace"
pw_nn_outcome[pw_nn_outcome==0]<-"War"
# pw_nn_outcome <- c(round(rbind(matrix(rep(1, 84), nrow=1),matrix(pw_nn_output, nrow=9))))#test needed if training on only rounds 2-10

#compute using history-2
# f_2_10 <- as.formula("my.decision~my.round1decision+my.decision1+other.decision1+my.decision2+other.decision2+period")
# pw_all_data_sub_subset_2_10 <- pw_all_data_subset[,c("my.round1decision","my.decision1","my.decision2","other.decision2","other.decision1","period")]
# nn_exp_2_10 <- neuralnet(f,data=pw_all_data_subset[pw_all_data_subset$period >2,],hidden=10,linear.output=FALSE)
# pw_nn_output_2_10<- compute(nn_exp, pw_all_data_sub_subset)$net.result
# pw_nn_output_2_10_rounded <- round(pw_nn_output_2_10) #(is this the right function for classifiprintion?)
# pw_nn_output_2_10_rounded[is.na(pw_nn_output_2_10_rounded)] <- 1
# pw_nn_outcome <- pw_nn_output_2_10_rounded

# p<-pw_all_data

# p <- add_column(p, c(as.matrix(q)), .before = "risk") #insert predicted values into p
# colnames(p)[colnames(p) =="c(as.matrix(q))"] <- "outcome"
# p$outcome <- as.factor(p$outcome)
#

pw_all_data <- add_column(pw_all_data, pw_nn_outcome, .before = "risk") #insert nnet values into p
colnames(pw_all_data)[colnames(pw_all_data) =="pw_nn_outcome"] <- "nnet"
pw_all_data$nnet <- as.factor(pw_all_data$nnet)
levels(pw_all_data$nnet) <- c("Peace","War")

pw_all_data <- add_column(pw_all_data, q$value, .after = "nnet") #insert GLM values into p
colnames(pw_all_data)[colnames(pw_all_data) =="q$value"] <- "GLM"
pw_all_data$GLM <- as.factor(pw_all_data$GLM)
levels(pw_all_data$GLM) <- c("Peace","War")

pw_all_data$my.decision <- as.factor(pw_all_data$my.decision)

#
pw_nnet_array <- xtabs(~Adversary + nnet + id, data=pw_all_data)
pw_GLM_array <- xtabs(~Adversary + GLM + id, data=pw_all_data)

pw_nnet_sum <- rowSums(pw_nnet_array, dims =2)
pw_GLM_sum <- rowSums(pw_GLM_array, dims =2)
pw_sum <- rowSums(pw_array, dims=2)

#creates a matrix of Observed vs predicted
pw_pred_actual_sum <- matrix(NA, nrow=3, ncol = 3)
colnames(pw_pred_actual_sum) <- c("Obs","nnet", "GLM")
rownames(pw_pred_actual_sum) <- rownames(pw_sum)
pw_pred_actual_sum[,1] <- pw_sum[,1]
pw_pred_actual_sum[,2] <- pw_nnet_sum[,1]
pw_pred_actual_sum[,3] <- pw_GLM_sum[,1]

#--------------------RPS - Data Transformation-----------------
#bind the columns advisor_choice and adversary type
# rps_cols <- bind_cols(new_data[grepl('^rps.*advisor_choice$', names(new_data))], new_data[grepl('^rps.*adv_1_type$', names(new_data))])#get rps_vs_human choices as a df row:player, col:colnames
rps_cols <- bind_cols(new_data[grepl('^rps.*advisor_choice$', names(new_data))], new_data[grepl('^rps.*adv_1_type$', names(new_data))], new_data[grepl('^rps.*payoff_vs_adv_1$', names(new_data))])

#rps_vs_human_totals <- rowSums()#pwChangeColNames(rps_vs_human_cols, num_pw_rounds) #get a matrix of rps choices (row:player)x(column:round) with only the rps vs human
# names(rps_cols) <-  gsub(pattern = "rps.", replacement = "", x = names(rps_cols)) # removes the leading 'rps.' - just a cleanup
# names(rps_cols) <-  gsub(pattern = "player.", replacement = "", x = names(rps_cols)) # removes 'player.' - just a cleanup
names(rps_cols) <-  gsub(pattern = "rps.", replacement = "", x = names(rps_cols)) # removes the leading 'rps.' - just a cleanup
names(rps_cols) <-  gsub(pattern = "player.", replacement = "", x = names(rps_cols))


# names(rps_cols)[grepl('^[1-9][.](.*)$', names(rps_cols))] <- paste0("0", names(rps_cols)[grepl('^[1-9][.](.*)$', names(rps_cols))])
# line above adds leading zero to one-digit round numbers
names(rps_cols)[grepl('^[1-9][.](.*)$', names(rps_cols))] <- paste0("0", names(rps_cols)[grepl('^[1-9][.](.*)$', names(rps_cols))])

rps_cols[,names(rps_cols)[grepl('^(.*)([3][1-9]|[4-6][0-9])[.](.*)$', names(rps_cols))]] <- NULL #strip unused columns

a <- rps_cols

rps_cols <- rps_cols[,order(colnames(rps_cols))] #sorts columns by round number (two vars each)






new_data_labels <- c("participant.code","participant.label","session.config.pw_counterbalance","session.config.rps_counterbalance")
df <- subset(new_data, select=new_data_labels)#change ...code to ...label in real sessions
df_labels <- c("code","label","pw_cb","rps_cb")
colnames(df) <- df_labels
list_of_decision_cols <- names(rps_cols)[grepl('^.*choice$', names(rps_cols))]
list_of_type_cols <- names(rps_cols)[grepl('^.*type$', names(rps_cols))]
list_of_payoff_cols <- names(rps_cols)[grepl('^.*payoff.*$', names(rps_cols))]

a <- bind_cols(as.data.frame(new_data$participant.label), rps_cols)  #change participant.label to whatever makes sense

game_order_rps <-2-(3-new_data$session.config.pw_counterbalance%%2)%%2  #will be 2 if rps is played first. will be 2 if rps_played second
#1 - H,HAI,AI (123)
#2 - H, AI, HAI (132)
#3 - HAI, AI, H (231)
#4 - HAI, H, AU (213)
#5 - AI, H, HAI (312)
#6 - AI, HAI, H (321)
game_treatment_order_rps <- 2-(3-new_data$session.config.pw_counterbalance%%2)%%2
# adv_treatment_order_rps <- 0

names(a)[1] <- "id"
df_types = melt(a, id="id", measure=list_of_type_cols)
names(df_types)[2] <- "Round"
names(df_types)[3] <- "Adversary"
df_decs = melt(a, id="id", measure=list_of_decision_cols)
names(df_decs)[3] <- "Choice_of_Advisor"
df_decs$variable = NULL
df_payoffs = melt(a, id="id", measure=list_of_payoff_cols)
#df_payoffs$variable = NULL
names(df_payoffs)[3] <- "Payoff"
# f<- inner_join(df_types,df_decs,by="id")
rps_long<- bind_cols(df_types, as.data.frame(df_decs$Choice_of_Advisor),as.data.frame(df_payoffs$Payoff))
names(rps_long)[4] <- "Choice_of_Advisor"
names(rps_long)[5] <- "Payoff"
rps_long$Round <- gsub(pattern=".adv_1_type", replacement="",x=rps_long$Round)
rps_long$Round <- as.integer(rps_long$Round)
rps_long$Adversary <- as.factor(rps_long$Adversary)
# rps_sum <- xtabs(~Adversary+Choice_of_Advisor, data=rps_long)
# rps_prop <- prop.table(rps_sum)*100
rps_ids <- levels(rps_long$id)
rps_array <- xtabs(~Adversary+Choice_of_Advisor+id, data=rps_long)

row.names <- c("AI","Human","Human+AI")
col.names <- c("AI", "human", "none")

Adversary_list <- c("AI","Human","Human+AI")
Advisor_list <- c("AI", "human", "none")

rps_long_none_by_id <- as.data.frame.matrix(xtabs(~ id + Adversary, data = rps_long[rps_long$Choice_of_Advisor=="none",]))
rps_long_AI_by_id <- as.data.frame.matrix(xtabs(~ id + Adversary, data = rps_long[rps_long$Choice_of_Advisor=="AI",]))
rps_long_human_by_id <- as.data.frame.matrix(xtabs(~ id + Adversary, data = rps_long[rps_long$Choice_of_Advisor=="human",]))
rps_long_by_id <- as.data.frame(xtabs(~id + Adversary + Choice_of_Advisor, data = rps_long))

#rps_long_ten_rounds converts rounds from 1-30 to 1-10 for all players, regardless of counterbalncing order
rps_long_ten_rounds <- rps_long
substrRight <- function(x, n){ #keep this function for later uses even if your delete rps_long_ten_rounds
  substr(x, nchar(x)-n+1, nchar(x))
}
rps_long_ten_rounds$Round <- substrRight(rps_long_ten_rounds$Round, 1)
rps_long_ten_rounds[rps_long_ten_rounds$Round == "0",]$Round <- 10
rps_long_ten_rounds$Round <- as.integer(rps_long_ten_rounds$Round)

#--------------------Demographics - Data Transformation & Merge w/PW & RPS----------------------
demo_ids <- subset(new_data, select=participant.label) #
demo_data <- bind_cols(demo_ids, new_data[grepl('^post_game_survey.1.player.*$', names(new_data))]) #bind ids with data
# need to check pervious line to make sure it doesn't mix them up!
names(demo_data) <-  gsub(pattern = "participant.label", replacement = "id", x = names(demo_data)) #replace participant.label column name with "id"

names(demo_data) <-  gsub(pattern = "post_game_survey.1.player.", replacement = "", x = names(demo_data)) # removes the leading 'post_game_survey.1.player.' - just a cleanup
demo_data$id_in_group <- NULL #remove id_in_group column since it doesn't matter
demo_data$payoff <- NULL #remove payoff column since it doesn't matter
demo_data$age <- 2019-demo_data$age  #convert birth year to age (approximate based on month born...)
demo_data$machine_learning_experience <- as.integer(sub("^(\\d{1}).*$", "\\1", demo_data$machine_learning_experience)) #converts machine learning experience to single digit number 1-5

demo_data$game_theory_experience <- as.integer(sub("^(\\d{1}).*$", "\\1", demo_data$game_theory_experience)) #converts game theory experience to single digit number 1-5
demo_data$gender <- as.factor(demo_data$gender)
demo_data$service <- as.factor(demo_data$service)
demo_data$school <- as.factor(demo_data$school)
demo_data$rank[startsWith(demo_data$rank, "M")] <- 4  #converts "Major" or anything startswith M to a 4
demo_data$rank[startsWith(demo_data$rank, "m")] <- 4  #converts "Major" or anything startswith M to a 4
demo_data$rank[grepl('4', demo_data$rank)] <- 4#converts "O-4" or anything with a 4 to a 4
demo_data$rank[startsWith(demo_data$rank, "L")] <- 5 #converts "Lt Col" or anything startswith to a 5
demo_data$rank[startsWith(demo_data$rank, "l")] <- 5 #converts "Lt Col" or anything startswith to a 5
demo_data$rank[grepl('5', demo_data$rank)] <- 5#converts "O-5" or anything with a 5 to a 5
demo_data$rank[startsWith(demo_data$rank, "COL")] <- 6 #converts "Lt Col" or anything startswith COL to a 6
demo_data$rank <- as.factor(demo_data$rank)
demo_data$pw_order <- as.factor(sub("^.............(.).*", "\\1", demo_data$self_participant_vars_dump))
demo_data$rps_order <- as.factor(sub('.*rps_order\\\'\\:\\ (...).*', '\\1', demo_data$self_participant_vars_dump)) #could also pull directly from self.participant.vars dump from 'first_rps_adv': "
demo_data$consent <- as.factor(sub('.*consent\\\'\\:\\ (....).*', '\\1', demo_data$self_participant_vars_dump))
demo_relevant_data <- demo_data[,c("id","pw_order","rps_order","age","gender","service","school","rank","years_military_experience","game_theory_experience","machine_learning_experience")] #add major and post_grad as coded factors


rps_all_data_with_demo <- merge(rps_long, demo_relevant_data, by="id")

time_data_rps <- time_data[time_data$app_name =="rps",]
time_data_rps_decision <- time_data_rps[time_data_rps$page_name == "Decision",c("id","seconds_on_page")]
time_data_rps_decision$Round <- rep(1:30, nrow(time_data_rps_decision)/30)
rps_all_data_with_demo <- merge(rps_all_data_with_demo, time_data_rps_decision, c("id","Round"))

rps_all_data_with_demo$avg_time <- NA
for (i in rps_ids) { #add average time by round by ID
  rps_all_data_with_demo[rps_all_data_with_demo$id==i,]$avg_time <- rep(mean(rps_all_data_with_demo[rps_all_data_with_demo$id==i,]$seconds_on_page),10)
}

#rps_long_ten_rounds converts rounds from 1-30 to 1-10 for all players, regardless of counterbalncing order
rps_all_data_with_demo$Round <- substrRight(rps_all_data_with_demo$Round, 1)
rps_all_data_with_demo[rps_all_data_with_demo$Round == "0",]$Round <- 10
rps_all_data_with_demo$Round <- as.integer(rps_all_data_with_demo$Round)


pw_all_data_with_demo <- merge(pw_all_data, demo_relevant_data, by="id")
time_data_pw <- time_data[time_data$app_name =="prisoner_multiplayer",]
time_data_pw_decision <- time_data_pw[time_data_pw$page_name == "Decision",c("id","seconds_on_page")]
time_data_pw_decision$period <- rep(1:10, nrow(time_data_pw_decision)/10)
pw_all_data_with_demo <- merge(pw_all_data_with_demo, time_data_pw_decision, c("id","period"))
pw_all_data_with_demo <- pw_all_data_with_demo[,!names(pw_all_data_with_demo) %in% c("my.payoff1","my.payoff2","my.payoff3","my.payoff4","my.payoff5","my.payoff6","my.payoff7","my.payoff8","my.payoff9","other.payoff1","other.payoff2","other.payoff3","other.payoff4","other.payoff5","other.payoff6","other.payoff7","other.payoff8","other.payoff9","r","s","t","p","infin","contin","group","decision_time","data","error","r1","r2","risk","delta","X")] #remove unnecessary columns

pw_all_data_with_demo$avg_time <- NA
for (i in pw_ids) { #add average time by round by ID
pw_all_data_with_demo[pw_all_data_with_demo$id==i,]$avg_time <- rep(mean(pw_all_data_with_demo[pw_all_data_with_demo$id==i,]$seconds_on_page),10)
}
pw_all_data_with_demo$TFT <- NA
for (i in 1:10) {
  pw_all_data_with_demo[pw_all_data_with_demo$period==i & pw_all_data_with_demo$Adversary=="Human","TFT"] <- human_tft_choices[i]
  pw_all_data_with_demo[pw_all_data_with_demo$period==i & pw_all_data_with_demo$Adversary=="AI","TFT"] <- ai_tft_choices[i]
  pw_all_data_with_demo[pw_all_data_with_demo$period==i & pw_all_data_with_demo$Adversary=="Human+AI","TFT"] <- hai_tft_choices[i]
}
pw_all_data_with_demo$keras <- as.factor(pw_from_keras$keras) 
levels(pw_all_data_with_demo$keras) <- c("War","Peace")

#--------------------Data Write to .csv--------------------
#[[remove? “non-human-believers” – failed manipulation check]]
#Clear fails: 7ic14 (3xunk, RPS: ), gdg26 (3xAllC, RPS: ), oez14 (3xAllD, RPS: ), s4441 (4/6, 6/4, 6/4, RPS: )
#Possible Fails: amm45 (2xAllD, RPS: ), k1o66 (2xAllC, 8/2, RPS: ), zxq33 (3xAllC, RPS: )
clear_fails <- c("7ic14","gdg26","oez14","s4441")
possible_fails <- c("amm45","k1o66","zxq33")
pw_all_data_with_demo <- pw_all_data_with_demo[!(pw_all_data_with_demo$id %in% clear_fails),]
rps_all_data_with_demo <- rps_all_data_with_demo[!(rps_all_data_with_demo$id %in% clear_fails),]
#exclude manipulation check fails (those that failed clearly)
#pw_all_data_with_demo <- pw_all_data_with_demo[!(pw_all_data_with_demo$id %in% possible_fails),] #exclude manipulation check fails (those that were possible fails )

write.csv(pw_all_data_with_demo, "pw_all_data_with_demo.csv")
write.csv(rps_all_data_with_demo, "rps_all_data_with_demo.csv")
write.csv(demo_data, "demo_data.csv")
write.csv(all_data, "all_data.csv")

# pw_all_data_with_demo<-read.csv("pw_all_data_with_demo.csv")
# pw_all_data_with_demo$X <- NULL
# rps_all_data_with_demo<-read.csv("rps_all_data_with_demo.csv")
# rps_all_data_with_demo$X <- NULL
# demo_data<-read.csv("demo_data.csv")
# demo_data$X <- NULL
# print("--------MAJOR NOTE---------")
# print("--------DATA IS CURRENTLY READ FROM CSV NOT CALCUATIONS---------")













#--------------------Demographics - Data Visualization------------------

#ggplot(demo_data) + geom_histogram( aes(age) ) #age histogram
#ggplot(demo_data) + geom_histogram( aes(years_military_experience) )
#ggplot(demo_data) + geom_histogram(stat="count", aes(school) )
#ggplot(demo_data) + geom_histogram(stat="count", aes(rank) )
#ggplot(demo_data) + geom_histogram(stat="count", aes(service) )
#genders <- c(length(which(demo_data$gender=="Male")), length(which(demo_data$gender=="Female")))
#pie(genders) #need a better chart here
#ggplot(demo_data) + geom_histogram(stat="count", aes(game_theory_experience) )
# ggplot(demo_data) + geom_histogram(stat="count", aes(machine_learning_experience) )




#------------********PW Analyses********--------------
#--------------------PW - Initial Data Characterization-----------------
print("----------------------------PW analysis-----------------------------")
print("")
print("         --PW: Aggregate Observed Choices--")
print(pw_sum <- xtabs(~Adversary+my.decision, data=pw_all_data_with_demo))
# pw_summary <- ddply(pw_all_data_with_demo, ~Adversary+my.decision, summarise, Obs.sum=sum('my.decision'), Obs.mean=mean('my.decision'), Obs.sd=sd('my.decision'), nnet.sum=sum(nnet), nnet.mean=mean(nnet), nnet.sd=sd(nnet))
# print("P-W summary")
# print(pw_summary)
human_adv_choices <- c(1, 1,	0,	1,	0,	0,	0,	1,	1,	0) #sum in first 9=5
human_tft_choices <- c("Peace", "Peace",  "Peace",  "War",  "Peace",  "War",  "War",  "War",  "Peace",  "Peace") #sum in first9 = 5
hai_adv_choices <- c(1,	0,	1,	1,	0,	1,	1,	1,	1,	0) #sum=7
hai_tft_choices <- c("Peace",	"Peace",	"War",	"Peace",	"Peace",	"War",	"Peace",	"Peace",	"Peace",	"Peace") #sum in first9 = 7
ai_adv_choices <- c(0,	0,	0,	1,	1,	0,	0,	0,	1,	0)#sum=3
ai_tft_choices <- c("Peace",	"War",	"War",	"War",	"Peace",	"Peace",	"War",	"War",	"War",	"Peace") #sum in first9= 3
print("")
print("         --PW: Adversary Choices--")
print(paste0("AI: ",paste0(ai_adv_choices, collapse="")))
print(paste0("Human: ",paste0(human_adv_choices, collapse="")))
print(paste0("Human+AI: ",paste0(hai_adv_choices, collapse="")))

print("")
print("         --PW: Data Normality Tests--")

pw_df_1<- as.data.frame(xtabs(~my.decision + Adversary+id, data=pw_all_data_with_demo))
pw_df_1 <- pw_df_1[pw_df_1$my.decision=="Peace",]
p<- ggplot(pw_df_1, aes(x=Freq))+
  geom_density(data=pw_df_1,fill="black", color="black",alpha=0.3)+
  stat_function(fun = dnorm,color="blue",args = list(mean = mean(pw_df_1$Freq), sd = sd(pw_df_1$Freq)),linetype = "dashed")+ labs(title="PW-Peace Density Distribution", x="Frequency", y = "Density of Choices") +scale_x_discrete(limits=c(0:10))+ theme(plot.title = element_text(size=12))
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
print("RESULT: A visual inspection of the density distribution plot of peace choices indicates the data may be normally distributed. So we conduct a Shapiro-Wilk test...")

#tests on all Peace choices across all players
print(shapiro.test(xtabs(~my.decision+id, data=pw_all_data_with_demo)[1,]))#test for normality (#normal)
print("RESULT: A Shapiro test on the aggregation of peace choices indicates the data is likely normally distributed..")


print(descdist(xtabs(~my.decision+id, data=pw_all_data_with_demo)[1,], discrete = FALSE))
print("how to insert/save cullen & freq graph of data")
print("RESULT: Cullen & Frey plot indicates a close to normal distributon.")
p<-plot(fitdist(xtabs(~my.decision+id, data=pw_all_data_with_demo)[1,], "norm"))
print(p)
print(paste0("Insert fitddist Plot"))
ggsave(paste0("PW - fitdist plot.jpg"), plot=p, device="jpg")
print("RESULT: Fit Dist plot indicates a close to normal distributon.")

l <- as.data.frame(xtabs(~id+Adversary+my.decision, data=pw_all_data_with_demo)[,,1])
m <- aov(Freq ~ Adversary, data = l)
print(shapiro.test(residuals(m))) #overall, data normal
print("RESULT: A Shapiro test on the residuals of the peace choices shows the data IS NORMALLY DISTRIBUTED.")
print("IMPLICATION: Can use parametric tests.")


#--------------------PW - Round 1 Data Analyses-----------------
print("------------------------PW Round 1 Analyses------------------------")

#which one of these is most appropriate to use?
print("       ----PW: Round 1 Choices (aggregate)----")
pw_ids <- unique(pw_all_data_with_demo$id)
pw_round_1 <- xtabs(~Adversary+my.decision+id, data=pw_all_data_with_demo[pw_all_data_with_demo$period==1,])
print(pw_round_1s_sum <- xtabs(~Adversary+my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$period==1,]))
print("")
print("           ----PW: Round 1 Tests----")
pw_varied_round1 = data.frame("id"=pw_ids,"Varied"=NA)
for (i in pw_ids) {
  pw_varied_round1[pw_varied_round1$id==i,"Varied"] <- !(pw_round_1[1,1,i]==pw_round_1[2,1,i] & pw_round_1[1,1,i]==pw_round_1[3,1,i])
}
print(paste0("RESULT: A simple count of participants showed ",length(pw_varied_round1[pw_varied_round1$Varied,]$id)," (",round(length(pw_varied_round1[pw_varied_round1$Varied,]$id)*100/(length(pw_ids))),"%) participants (",paste(pw_varied_round1[pw_varied_round1$Varied,]$id, collapse=", "),") varied their round 1 choices."))

print("IMPLICATION: The majority of participants, when having no history with the adversaries, did not make different initial moves with the adversaries (in round 1).")


# print(chisq.test(pw_round_1s_sum))
# print(paste("RESULT: A Chi-Sq test of aggregate round 1 choices shows IV (Adversary) and DV (Round 1 Choice) are statistically independent at p < .05."))
print(fisher.test(xtabs(~ Adversary + my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$period==1,])))
print(paste("RESULT: A Fisher test of aggregate round 1 choices shows IV (Adversary) and DV (Round 1 Choice) are statistically independent at p < .05."))
print("HELP: Is chisq or fisher test more appropriate? ")

print(friedman.test(Choice ~ Adversary | id, data = as.matrix(pw_round_1s))) #not significant
# print(friedman.test(my.decision ~ Adversary | id, data = xtabs(~Adversary+my.decision+id, data=pw_all_data_with_demo[pw_all_data_with_demo$period==1,])))
print(paste("RESULT: A Friedman test of round 1 choices shows IV (Adversary) and DV (Round 1 Choice) are statistically independent at p < .05."))



#Anova by Adversary
m <- glmer(my.decision ~ Adversary+(1|id), data=pw_all_data_with_demo[pw_all_data_with_demo$period==1,], family=binomial)
# print(summary(m))
print(Anova(m, type="3"))
print("RESULT: A repeated measures logistic regression shows variation on the DV (Round 1 Choice) by the IV (Adversary) is NOT STATISTICALLY SIGNIFICANT at p <.05.")
print("HELP: Is this even needed? Does it add anything to the analysis?")

print("IMPLICATION: Statistical and non-statisticaly analyses show participants did not vary their initial propensity by adversary.")

# these are the players who played "strategically" (not a fixed strategy, and did not indiprinte non-belief)
# pw_played <- c("yyj35","yda14", "w9c21", "v6i85", "txk36", "n1i23", "i5b85", "h0s22", "b8g12", "b1462", "9mp31", "8mh76", "3z144")
# pw_array_played <- pw_array[,,pw_played]
# pw_pred_array_played <- pw_pred_array[,,pw_played]

#--------------------PW - Strategic Decisions Data Analysis-----------------
print("-------------------PW: Strategic Decision-making Analyses------------------")
print("       ----PW: Strategic (All round) Choices (aggregate)----")
print(xtabs(~Adversary+my.decision, data=pw_all_data_with_demo))

print("       ----PW: Distribution comparison of Strategic Choices by Adversary----")
p<- ggplot(pw_df_1, aes(x=Freq))+
  geom_density(data=pw_df_1[pw_df_1$Adversary=="Human",],fill="red", color="red",alpha=0.3)+
  geom_density(data=pw_df_1[pw_df_1$Adversary=="AI",],fill="blue", color="blue",alpha=0.3)+
  geom_density(data=pw_df_1[pw_df_1$Adversary=="Human+AI",],fill="green", color="green", alpha=0.3)+
  stat_function(fun = dnorm,color="blue",args = list(mean = mean(pw_df_1[pw_df_1$Adversary=="AI",]$Freq), sd = sd(pw_df_1[pw_df_1$Adversary=="AI",]$Freq)),linetype = "dashed")+
  stat_function(fun = dnorm,color="red",args = list(mean = mean(pw_df_1[pw_df_1$Adversary=="Human",]$Freq), sd = sd(pw_df_1[pw_df_1$Adversary=="Human",]$Freq)),linetype = "dashed")+
  stat_function(fun = dnorm,color="green",args = list(mean = mean(pw_df_1[pw_df_1$Adversary=="Human+AI",]$Freq), sd = sd(pw_df_1[pw_df_1$Adversary=="Human+AI",]$Freq)),linetype = "dashed") + labs(title="PW - Peace Choices Distribution (by Adversary)",x="Frequency", y = "Density of Choices", color = "Adversary") +scale_x_discrete(limits=c(0:10))+ theme(plot.title = element_text(size=9))
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
print("RESULT: A visual inspection of the density plot (by adversary) indicates possible differences in the distributions.")
print(shapiro.test(pw_df_1[pw_df_1$Adversary=="AI",]$Freq))#normal
print(shapiro.test(pw_df_1[pw_df_1$Adversary=="Human",]$Freq))#normal
print(shapiro.test(pw_df_1[pw_df_1$Adversary=="Human+AI",]$Freq))#normal
print("RESULT: These Shapiro-Wilk tests on the PEace choices by adversary show normal distributions, although the AI data appears to be a worse fit to normal distribution. More data points may show a non-normal distribution. In fact, before the manipulation check fails were removed, the AI data departed significantly from normality.")

print(descdist(xtabs(~my.decision+id, data=pw_all_data_with_demo[pw_all_data_with_demo$Adversary=="Human",])[1,], discrete = FALSE))
print(descdist(xtabs(~my.decision+id, data=pw_all_data_with_demo[pw_all_data_with_demo$Adversary=="Human+AI",])[1,], discrete = FALSE))
print("how to insert/save cullen & freq graph of data")
print("RESULT: Cullen & Frey plot indicates a close to normal distributon for human and Human+AI.")
print(descdist(xtabs(~my.decision+id, data=pw_all_data_with_demo[pw_all_data_with_demo$Adversary=="AI",])[1,], discrete = FALSE))
print("how to insert/save cullen & freq graph of data")
print("RESULT: Cullen & Frey plot indicates a departure from normal distributon in the AI condition.")




print("       ----PW: Strategic Tests----")
print(chisq.test(pw_sum))
print("RESULT: A Chi-Sq test  (X-squared = 11.891, df = 2, p-value = 0.002617) on the aggregate choices shows IV (Adversary) and DV (Aggregate Number of Peace Choices) are statistically dependent at p <.01.")
print("--post-hoc tests")
print(fisher.test(pw_sum[-3,]))
print(chisq.test(pw_sum[-3,]))
n<-as.data.frame(xtabs(~Adversary+my.decision+id, data=pw_all_data_with_demo))
n <- n[n$my.decision=="Peace",]
n$my.decision <- NULL
print(wilcox.test(n[n$Adversary=="Human",]$Freq, n[n$Adversary=="AI",]$Freq, paired=TRUE))
print("RESULT: Paired wilcox test, chisq.test, and fisher.test of the all-round sums comparing HUman and AI show difference at p <. 01")

# print(wilcox.test(n[n$Adversary=="AI",]$Freq, n[n$Adversary=="Human+AI",]$Freq, paired=TRUE))
# print("RESULT: Paired wilcox test of the all-round sums comparing AI and Human+AI show a difference at p <. 05")
print(fisher.test(pw_sum[-1,]))
print(chisq.test(pw_sum[-1,]))
print(wilcox.test(n[n$Adversary=="Human",]$Freq, n[n$Adversary=="Human+AI",]$Freq, paired=TRUE))
print("RESULT: Paired wilcox test, chisq.test, and fisher.test of the all-round sums comparing HUman and Human+AI show NO difference at p <. 05, despite difference in adversary gameplay")

print("IMPLICATION: In this experiment, the group of participants varied their choices between the AI and human adversary, which could be explained by differences in adversary gameplay. However, there was no statistical difference between the Human and Human+AI treatments, indicating other factors at work.")
# print("    --PW: All Rounds: Individual Fisher test p-values: ")
print("METHOD NOTE: Analyses of individual participants using only direct tests of proportions are not practically informative for the strategic gameplay part of this experiment.  For example, Chisq and Fisher tests on individual participant's game choices only yield 3 participants who showed a statistically significant difference by adversary, despite the following results...")
print("fisher test p-values: ")
for (i in pw_ids){
  print(paste(i, as.character(fisher.test(pw_array[,,i])["p.value"])))
}
print("RESULT: A fisher test showed only 3 participants showed a statistically significant difference in gameplay ")
for (i in pw_ids){
  print(paste(i, as.character(fisher.test(pw_array[-3,,i])["p.value"])))
}
for (i in pw_ids){
  print(paste(i, as.character(fisher.test(pw_array[-1,,i])["p.value"])))
}
print("RESULT: Post-hoc pairwise fisher tests showed only one player with statistically significant differences in gameplay and that was in the Human-AI pairwise comparison. This is interesting because one would expect a difference between the Human and Human+AI condition")


print(friedman.test(Freq~Adversary|id,data=pw_cols_by_id[pw_cols_by_id$Choice=="Peace",]))
print("RESULT: A Friedman test shows the relationship between variation on the DV (Choice) and variation on the IV (Adversary) is statistically significant at p <.01 (.05).")

a <- pw_all_data_with_demo[pw_all_data_with_demo$Adversary!="AI",]
a$Adversary <- as.numeric(a$Adversary)
a$Adversary <- as.factor(a$Adversary)
m <- glmer(my.decision ~ Adversary+(1|id)+(1|period), data=a, family=binomial (link="logit"))
print("Human-HAI comparison:")
print(Anova(m, type="3"))
a <- pw_all_data_with_demo[pw_all_data_with_demo$Adversary!="Human+AI",]
a$Adversary <- as.numeric(a$Adversary)
a$Adversary <- as.factor(a$Adversary)
m <- glmer(my.decision ~ Adversary+(1|id)+(1|period), data=a, family=binomial (link="logit"))
print("Human-AI comparison:")
print(Anova(m, type="3"))
print("RESULT:Pairwise (Human-AI) Repeated Measures Logistic Regressions show the relationship between variation on the DV (Choice) and variation on the IV (Adversary) is statistically significant at p < .001 (.05), but the Human-HAI comparison is not")

# a <- pw_all_data_with_demo[pw_all_data_with_demo$Adversary!="AI",]
# a$Adversary <- as.numeric(a$Adversary)
# a$Adversary <- as.factor(a$Adversary)
# m <- glmer(my.decision ~ Adversary+(1|id)+(1|period), data=a, family=binomial (link="logit"))
# m <- glmer(my.decision ~ Adversary+(1|id)+(1|period), data=pw_all_data_with_demo, family=binomial (link="logit"))
# # print(summary(m))
# print(Anova(m, type="3"))
# print("RESULT: NOT A VALID TEST BECAUSE IT'S BINOMIAL (NOT TRI-WISE) Repeated Measures Logistic Regression shows the relationship between variation on the DV (Choice) and variation on the IV (Adversary) is statistically significant at p < .001 (.05).")
print("IMPLICATION: Various statistical analyses show statistically significant variation by adversary type in participant choices across all rounds, particularly between the HUman and the Human-AI conditions, but not between the Human-Human+AI conditions. Were this difference significant across both pairs, competitor gameplay could be the cause of variation.  However, given the lack of difference between the Human and Human+AI competitors, it is possible this difference is caused by the type of competitor. See limitations...")
print("")
print("LIMITATION: Differences in Adversary gameplay are a possible confounding factor.")
print("")

print("REMOVE THIS SECTION       ---PW: Strategic Gameplay BY ROUND and adversary---")

pw_df <- as.data.frame(xtabs(~period + my.decision +Adversary, data=pw_all_data_with_demo))
pw_df <- pw_df[pw_df$my.decision=="Peace",]
pw_df$period <- as.numeric(pw_df$period)
a<- as.data.frame(matrix(NA, nrow=10, ncol=3))
names(a) <- names(pw_df)[-4]
a$period <- c(1:10)
a$Adversary <- "Agg"
n<- aggregate(Freq ~period, data=pw_df, FUN=sum)
a<- merge(a,n, by="period")
a$ratio <- a$Freq/(3*length(pw_ids))
pw_df$ratio <- pw_df$Freq/(length(pw_ids))
pw_df <- rbind(pw_df, a)
pw_df$Adv_delta <- 0.05
pw_df[pw_df$Adversary=="AI",]$Adv_delta <- (ai_adv_choices-.5)/10
pw_df[pw_df$Adversary=="Human",]$Adv_delta <- (human_adv_choices-.5)/10
pw_df[pw_df$Adversary=="Human+AI",]$Adv_delta <- (hai_adv_choices-.5)/10
pw_df[pw_df$Adversary=="Agg",]$Adv_delta <- NA

p<- ggplot(data=pw_df, aes(x=period, y=ratio, group="Adversary"))+  geom_point(aes(color=Adversary)) +
  #geom_smooth(method='auto',formula=y~x, aes(group=Adversary, color=Adversary, fill=Adversary), alpha=0.2)+ 
  geom_line(aes(group=Adversary, color=Adversary))+ 
  labs(title="PW - Peace Choices by Round and Aversary",x="Round", y = "Ratio of Choices", color = "Adversary") +scale_x_discrete(limits=c(1:10))+ylim(0,1)+ theme(plot.title = element_text(size=14), legend.title = element_text(size=9), legend.position = c(0.8, 0.8))+
  geom_segment(aes(xend = period+.25, yend = ratio+Adv_delta, color=Adversary, alpha=0.5),
               arrow = arrow(length = unit(0.1,"cm")))
#or use method="lm"
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
print("need to make aggregate a bold black line")
print("RESULT: INTERESTING - It appears the AI responses mostly (all but 1) followed the adv's previous round's coop/defect (i.e. the aggregate memory is mem-1 and akin to TFT), but for BOTH the human and human+AI, the responses mostly differed or ignored (5/10 or 5/9) the adv's previous round coop/defect decision ")

print("NOTE-opportunity for more here...")
print("")
print("")



print("")
print("REMOVE THIS SECTION       ---PW: Strategic Gameplay Compared to TFT---")
perc_TFT_eq_actual <- length(which(pw_all_data_with_demo$TFT == pw_all_data_with_demo$my.decision))/nrow(pw_all_data_with_demo)
print(paste0("       Percent TFT Predicted choices == Observed: ", round(perc_TFT_eq_actual*100, 2),"%."))
AI_df <- pw_all_data_with_demo[pw_all_data_with_demo$Adversary=="AI",]
Human_df <- pw_all_data_with_demo[pw_all_data_with_demo$Adversary=="Human",]
HAI_df <- pw_all_data_with_demo[pw_all_data_with_demo$Adversary=="Human+AI",]
print(paste0("       Percent TFT Predicted choices vs AI == Observed: ", round(length(which(AI_df$TFT == AI_df$my.decision))/nrow(AI_df)*100, 2),"%."))
print(paste0("       Percent TFT Predicted choices vs Human+AI == Observed: ", round(length(which(HAI_df$TFT == HAI_df$my.decision))/nrow(HAI_df)*100, 2),"%."))
print(paste0("       Percent TFT Predicted choices vs Human == Observed: ", round(length(which(Human_df$TFT == Human_df$my.decision))/nrow(Human_df)*100, 2),"%."))
boxplot((colSums(pw_array)[1,]-colSums(xtabs(~Adversary+TFT+id, data=pw_all_data_with_demo))[1,])/30)
print("visualize the error/accuracy of TFT")
print("")
print("REMOVE THIS SECTION     ---PW-TFT Test of Proportions---")
print(TFT_obs_actual <- pw_sum)
print(TFT_sum <- xtabs(~Adversary + TFT, data=pw_all_data_with_demo))
TFT_obs_actual[,2] <- TFT_sum[,1]
TFT_obs_actual<- cbind(TFT_obs_actual, TFT_obs_actual[,2]-TFT_obs_actual[,1])
TFT_obs_actual<- cbind(TFT_obs_actual, round((100*TFT_obs_actual[,3])/(rowSums(pw_sum)[1]),1))
colnames(TFT_obs_actual) <- c("Obs","TFT","TFTvObs","Perc Diff")
print(TFT_obs_actual)

print(chisq.test(TFT_obs_actual[,-c(3:4)]))
print("RESULT: A Chisq test analyzing dependence of the DV (Obs vs Actual) on the IV (Adversary) was statistically significant at p < .05.")
print("IMPLICATION: This indicates participants LIKELY used different strategies for each of the adversaries.")
# pw_df_tft <- as.data.frame(xtabs(~period + TFT +Adversary, data=pw_all_data_with_demo))
# pw_df_tft <- pw_df_tft[pw_df_tft$TFT=="Peace",]
# pw_df_tft$period <- as.numeric(pw_df_tft$period)
# p<- ggplot(data=pw_df_tft, aes(x=period, y=Freq, group="Adversary"))+  geom_point(aes(color=Adversary)) +
#   geom_smooth(method='auto',formula=y~x, aes(group=Adversary, color=Adversary, fill=Adversary), alpha=0.2)+ 
#   labs(title="PW- tft Predicted Peace Choices by Round and Aversary",x="Round", y = "# of Choices", color = "Adversary") +scale_x_discrete(limits=c(1:10))+ theme(plot.title = element_text(size=14), legend.title = element_text(size=9), legend.position = c(0.8, 0.8))#or use method="lm"
# print(p)
# print(paste0("Insert ", p$labels$title," Plot"))
# ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")

print("REMOVE unless relevant to the fingerprint match        ---PW: Strategic Gameplay Compared to Nay and Vorobeychik's GLM Model---")
print(pw_pred_actual_sum[,-2])
perc_GLM_eq_actual <- length(which(pw_all_data_with_demo$GLM == pw_all_data_with_demo$my.decision))/nrow(pw_all_data_with_demo)
print(paste0("       Percent GLM Predicted choices == Observed: ", round(perc_GLM_eq_actual*100, 2),"%."))
print(paste0("       Percent GLM Predicted choices vs AI == Observed: ", round(length(which(AI_df$GLM == AI_df$my.decision))/nrow(AI_df)*100, 2),"%."))
print(paste0("       Percent GLM Predicted choices vs Human+AI == Observed: ", round(length(which(HAI_df$GLM == HAI_df$my.decision))/nrow(HAI_df)*100, 2),"%."))
print(paste0("       Percent GLM Predicted choices vs Human == Observed: ", round(length(which(Human_df$GLM == Human_df$my.decision))/nrow(Human_df)*100, 2),"%."))
GLM_obs_actual <- pw_pred_actual_sum[,-2]
GLM_obs_actual<- cbind(GLM_obs_actual, GLM_obs_actual[,2]-GLM_obs_actual[,1])
GLM_obs_actual<- cbind(GLM_obs_actual, round((100*GLM_obs_actual[,3])/(rowSums(pw_sum)[1]),1))
colnames(GLM_obs_actual) <- c("Obs","GLM","GLMvObs","Perc Diff")
print(GLM_obs_actual)
print("RESULT: It appears the GLM has ~5% error for the Human (as expected) but 10% for the AI and 6% for the HAI. Like the TFT analysis, these differences also indicate some difference in strategic gameplay between adversaries.")
print(chisq.test(GLM_obs_actual[,-c(3:4)]))
print("RESULT: A Chisq test analyzing dependence of the DV (Obs vs GLM) on the IV (Adversary) was NOT statistically significant at p < .05.")
print("IMPLICATION:")
print("")
print("HELP: Maybe use three separate residuals tests (by adversary) to see the distribution of error?")


print("REMOVE unless relevant to the fingerprint match       ---PW: Strategic Gameplay Compared to NNET Model---")

perc_nnet_eq_actual <- length(which(pw_all_data_with_demo$nnet == pw_all_data_with_demo$my.decision))/nrow(pw_all_data_with_demo)
print(paste0("       Percent NNET Predicted choices == Observed: ", round(perc_nnet_eq_actual*100, 2),"%."))
print(paste0("       Percent nnet Predicted choices vs AI == Observed: ", round(length(which(AI_df$nnet == AI_df$my.decision))/nrow(AI_df)*100, 2),"%."))
print(paste0("       Percent nnet Predicted choices vs Human+AI == Observed: ", round(length(which(HAI_df$nnet == HAI_df$my.decision))/nrow(HAI_df)*100, 2),"%."))
print(paste0("       Percent nnet Predicted choices vs Human == Observed: ", round(length(which(Human_df$nnet == Human_df$my.decision))/nrow(Human_df)*100, 2),"%."))
NNET_obs_actual <- pw_pred_actual_sum[,-3]
NNET_obs_actual<- cbind(NNET_obs_actual, NNET_obs_actual[,2]-NNET_obs_actual[,1])
NNET_obs_actual<- cbind(NNET_obs_actual, round((100*NNET_obs_actual[,3])/(rowSums(pw_sum)[1]),1))
colnames(NNET_obs_actual) <- c("Obs","NNET","NNETvObs","Perc Diff")
print(NNET_obs_actual)
print("RESULT: It appears the neural net has 1.4% error for the Human (as expected) but 10% for the AI and -2.5% for the HAI. This also indicates some difference in strategic gameplay.")
print(chisq.test(NNET_obs_actual[,-c(3:4)]))
print("RESULT: A Chisq test analyzing dependence of the DV (Obs vs NNET) on the IV (Adversary) was NOT statistically significant at p < .05.")
print("HELP: Is this the right test to compare observed and actual?")
# pw_df_nnet <- as.data.frame(xtabs(~period + nnet +Adversary, data=pw_all_data_with_demo))
# pw_df_nnet <- pw_df_nnet[pw_df_nnet$nnet=="Peace",]
# pw_df_nnet$period <- as.numeric(pw_df_nnet$period)
# p<- ggplot(data=pw_df_nnet, aes(x=period, y=Freq, group="Adversary"))+  geom_point(aes(color=Adversary)) +
#   geom_smooth(method='auto',formula=y~x, aes(group=Adversary, color=Adversary, fill=Adversary), alpha=0.2)+ 
#   labs(title="PW- Neural Net Predicted Peace Choices by Round and Aversary",x="Round", y = "# of Choices", color = "Adversary") +scale_x_discrete(limits=c(1:10))+ theme(plot.title = element_text(size=14), legend.title = element_text(size=9), legend.position = c(0.8, 0.8))#or use method="lm"
# print(p)
# print(paste0("Insert ", p$labels$title," Plot"))
# ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
#--------------------PW - Strategic Decisions Rank Analysis----------
for (i in pw_ids) {
  rank_matrix[,i] <- rank(pw_array[,1,i])
}

length(which(rank_matrix[1,]==1))/length(pw_ids) # ratio that were least cooperative against the AI (no ties allowed)
print(paste0("RESULT: A simple rank analysis of the cooperation counts shows that only ",round(100*length(which(rank_matrix[1,]==1))/length(pw_ids),1),"% of the participants were least cooperative with the AI (no ties)"))

print(paste0("RESULT: Even allowing for ties, the simple rank analysis of the cooperation counts shows that only ",round(100*length(which(rank_matrix[1,]<2))/length(pw_ids),1),"% of the participants were least cooperative with the AI"))

length(which(rank_matrix[3,]==3))/length(pw_ids) #ratio that was most cooperative with the Human+AI (no ties allowed)
print(paste0("RESULT: A simple rank analysis of the cooperation counts shows that only ",round(100*length(which(rank_matrix[3,]==3))/length(pw_ids),1),"% of the participants were most cooperative with the Human+AI (no ties)"))

print(paste0("RESULT: Even allowing for ties, the simple rank analysis of the cooperation counts shows that only ",round(100*length(which(rank_matrix[1,]>2))/length(pw_ids),1),"% of the participants were most cooperative with the Human+AI"))

print(paste0("RESULT: A simple rank analysis of the cooperation counts shows that only ",round(100*5/length(pw_ids),1),"% of the participants demonstrated the expected order."))

#--------------------PW - Strat Decisions Same Count Analysis -------------
q <- pw_all_data_with_demo
a <- matrix(NA, nrow=length(pw_ids), ncol=4)
colnames(a) <- c("id","H-AI","HAI-AI","HAI-H")
a<- as.data.frame(a)
a$id <- pw_ids


for (i in pw_ids){
  a[a$id==i,2] = length(which(q[q$id==i & q$Adversary=="Human",]$my.decision==q[q$id==i & q$Adversary=="AI",]$my.decision))
  a[a$id==i,3] = length(which(q[q$id==i & q$Adversary=="Human+AI",]$my.decision==q[q$id==i & q$Adversary=="AI",]$my.decision))
  a[a$id==i,4] = length(which(q[q$id==i & q$Adversary=="Human+AI",]$my.decision==q[q$id==i & q$Adversary=="Human",]$my.decision))
}
a$sum <- rowSums(a[,2:4])
pw_same_count_df <- a
print(pw_same_count_df)
print(ggplot(pw_same_count_df) + geom_bar(aes(sum)))

print(paste0("RESULT: ",100*nrow(pw_same_count_df[pw_same_count_df$`H-AI`==10 | pw_same_count_df$`HAI-AI`==10 |pw_same_count_df$`HAI-H`==10,])/length(pw_ids),"% of participants played same choices across multiple competitors"))
# nrow(pw_same_count_df[pw_same_count_df$sum >24.5 | pw_same_count_df$sum <19.4,])

a<- xtabs(~period+id+my.decision, data=pw_all_data_with_demo)[,,1]
a[a==0|a==3] <- 3
a[a==1|a==2] <- 1
a[a==1] <- 0
a[a==3] <- 1
b<- data.frame(rowMeans(a))
b$period <- c(1:10)
names(b)[1] <- "mean"
b$type <- "players"
c<- xtabs(~period+id+other.decision1, data=pw_all_data_with_demo)[,,1]
c[c==0|c==3] <- 3
c[c==1|c==2] <- 1
c[c==1] <- 0
c[c==3] <- 1
d<- data.frame(rowMeans(c))
d$period <- c(1:9)
names(d)[1] <- "mean"
d$type <-"adv"
e<- rbind(d,b)
p<- ggplot(data=e[e$type=="players",], aes(x=period, y=mean))+
  #geom_smooth(method='auto',formula=y~x)+ 
  geom_line()+ 
  geom_point(data=e[e$type=="adv",])+
  labs(title="PW - Ratio of all3-same Choice by Round",x="Round", y = "Ratio of all 3 same choices", color = "Adversary") +scale_x_discrete(limits=c(1:10))+ theme(plot.title = element_text(size=14), legend.title = element_text(size=9), legend.position = c(0.8, 0.8))+ylim(0,1)#or use method="lm"

print(p)
print("need to fix y labels and cooperative play by adversaries?")
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
#--------------------PW - Strat Probability Analysis---------------
# a context is a condition.  Memory is how many previous turns
#mem0
mem0_df <- as.data.frame(xtabs(~Adversary+id+my.decision, data=pw_all_data_with_demo))
a<- aggregate(Freq~Adversary+id, data=mem0_df, FUN=sum)
names(a)[3] <- "context"
mem0_df <- merge(a, mem0_df)

#mem1
mem1_df <- as.data.frame(xtabs(~Adversary+id+other.decision1+my.decision, data=pw_all_data_with_demo))
a<- aggregate(Freq~Adversary+id+other.decision1, data=mem1_df, FUN=sum)
names(a)[4] <- "context"
mem1_df <- merge(a, mem1_df)

#mem2 
mem2_df<-as.data.frame(xtabs(~Adversary+id+my.decision1+other.decision1+my.decision, data=pw_all_data_with_demo))
a<- aggregate(Freq~Adversary+id+other.decision1+my.decision1, data=mem2_df, FUN=sum)
names(a)[5] <- "context"
mem2_df <- merge(a, mem2_df)

#mem3 
mem3_df<-as.data.frame(xtabs(~Adversary+id+my.decision1+other.decision1+other.decision2+my.decision, data=pw_all_data_with_demo))
a<- aggregate(Freq~Adversary+id+other.decision1+my.decision1+other.decision2, data=mem3_df, FUN=sum)
names(a)[6] <- "context"
mem3_df <- merge(a, mem3_df)


#mem4 
mem4_df<-as.data.frame(xtabs(~Adversary+id+my.decision1+other.decision1+other.decision2+my.decision2+my.decision, data=pw_all_data_with_demo))
a<- aggregate(Freq~Adversary+id+other.decision1+my.decision1+other.decision2+my.decision2, data=mem4_df, FUN=sum)
names(a)[7] <- "context"
mem4_df <- merge(a, mem4_df)

#mem_df is a dataframe, by id & ddversary of the fisher test p.values of each of the contexts for that memory level
mem_df <- NULL
mem_df <- data.frame(rep(pw_ids,3))
names(mem_df) <- "id"
mem_df$Adversary <- "H"
for (i in pw_ids) {
  mem_df[mem_df$id==i,]$Adversary <- c("Human","AI","Human+AI")
}
mem_df$mem0 <- NA
mem_df$mem1 <- NA
mem_df$mem2 <- NA
mem_df$mem3 <- NA
mem_df$mem4 <- NA
# 
for (i in pw_ids) {
#   #print(i)
  for (j in Adversary_list) {
      t <- fisher.test(mem0_df[mem0_df$id==i & mem0_df$Adversary==j, c("Freq","context")])
      mem_df[mem_df$id==i & mem_df$Adversary==j,]$mem0 <- t$p.value
      t <- fisher.test(mem1_df[mem1_df$id==i & mem1_df$Adversary==j, c("Freq","context")])
      mem_df[mem_df$id==i & mem_df$Adversary==j,]$mem1 <- t$p.value
      t <- fisher.test(mem2_df[mem2_df$id==i & mem2_df$Adversary==j, c("Freq","context")])
      mem_df[mem_df$id==i & mem_df$Adversary==j,]$mem2 <- t$p.value
      t <- fisher.test(mem3_df[mem3_df$id==i & mem3_df$Adversary==j, c("Freq","context")])
      mem_df[mem_df$id==i & mem_df$Adversary==j,]$mem3 <- t$p.value
      t <- fisher.test(mem4_df[mem4_df$id==i & mem4_df$Adversary==j, c("Freq","context")])
      mem_df[mem_df$id==i & mem_df$Adversary==j,]$mem4 <- t$p.value

#     e_temp <- e[,,adv,i]
#     e_temp <- e_temp[complete.cases(e_temp),]
#     o<-fisher.test(e_temp)
#     if(length(which(colSums(c[,,adv,i])==0))) {
#       mem_df[mem_df$id==i & mem_df$Adversary==adv,]$mem0 <- 0
#       #print(paste(adv,"Mem0: True"))
#     } else {
#       mem_df[mem_df$id==i & mem_df$Adversary==adv,]$mem0 <- 1
#     }
# 
  }
}
print(mem_df[with(mem_df, order(id)),])


#--------------------PW - Strategy Fingerprint Analysis-----------------
#Axelrod fingerprint function
axelrod_fp <- function(player_vec, strat_actions) {
  if(player_vec[1]=="Peace"|player_vec[1]=="War"){#convert factor to numeric
    for (i in player_vec){ 
      player_vec[player_vec=="Peace"] <- 1
      player_vec[player_vec=="War"] <- 0
      player_vec <- as.numeric(player_vec)
    }
  }
  strat_types <- strat_actions$Strategy
  axelrod_df <- as.data.frame(matrix(data=0, nrow=1, ncol=length(strat_types)))
  names(axelrod_df) <- strat_types
  for (i in strat_types) {
    # print(i)
    # print(as.list(as.numeric(strat_actions[strat_actions$Strategy==i, c(2:11)])))
    # print(length(which(player_vec==as.list(as.numeric(strat_actions[strat_actions$Strategy==i, c(2:11)]))))/length(player_vec))
    axelrod_df[,i] <- length(which(player_vec==as.list(as.numeric(strat_actions[strat_actions$Strategy==i, c(2:11)]))))/length(player_vec)
  }
  
  return(axelrod_df)
}
#strategy fingerprinting function
fingerprint <- function(player_vec, adv_vec, types){  #returns named vector percent match to various fingerprint types  given a player's choices and the adverary's choices
  #inputs: player_vec = a vector of 1's and zero's with 1 being cooperate?
  #inputs: adv_vec = a vector of 1's and zero's with 1 being cooperate
  #inputs: types = a vector names of strategy types
  types <- c("AllC","AllD","TFT","TF2T","NotTF2T","TwoTFT","NotTwoTFT","UC","UD","WSLS","Psycho","DTFT","PTFT","PT2FT","T2","FBF","GLM","nnet","keras")
  fingerprint_df <- as.data.frame(matrix(data=0, nrow=1, ncol=length(types)))
  names(fingerprint_df) <- types
  
  #check if player & adverasry choice vectors are equal length
  if (!(length(player_vec)==length(adv_vec))) {
    print(player_vec)
    print(adv_vec)
    print("Error: vector length not equal.")
    return(fingerprint_df)
  }
  
  # Convert factors to 1 or 0
  #maybe add "coop" or "defect"
  if(player_vec[1]=="Peace"|player_vec[1]=="War"){#convert factor to numeric
    for (i in player_vec){ 
      player_vec[player_vec=="Peace"] <- 1
      player_vec[player_vec=="War"] <- 0
      player_vec <- as.numeric(player_vec)
    }
  }
  if(adv_vec[1]=="Peace"|adv_vec[1]=="War"){#convert factor to numeric
    for (i in adv_vec){
      adv_vec[adv_vec=="Peace"] <- 1
      adv_vec[adv_vec=="War"] <- 0
      adv_vec <- as.numeric(adv_vec)
    }
  }

  #ALLC & ALLD
  if ("AllC" %in% types) {
    if (sum(player_vec)==length(player_vec)){ # if all are cooperate (doesn't check whether adversary all cooperated, which would make it TFT also...)
      fingerprint_df$AllC <- 1.0
    } 
  }
  
  #AllD
  if ("AllD" %in% types) {
    if (sum(player_vec)==0) {# if all are defect(doesn't check whether adversary all cooperated, which would make it TFT also...
      fingerprint_df$AllD <- 1.0
    }
  }
  
  #TFT
  if ("TFT" %in% types) {
    if (player_vec[1]==1) {#TFT
      # print("pl")
      TFT_true <- rep(TRUE, length(player_vec))
      for(i in (2:length(player_vec))){
        # print(player_vec[i])
        # print(adv_vec[i-1])
        if (!player_vec[i]==adv_vec[i-1]) {
          TFT_true[i]<-FALSE
          }
      }
      #print(paste0("TFT: ",(length(which(TFT_true))/length(player_vec))))
      fingerprint_df$TFT <- length(which(TFT_true))/length(player_vec)
    }
  }

  
  #DTFT (defect then TFT)
  if ("DTFT" %in% types) {
    if (player_vec[1]==0) {#DTFT
      # print("pl")
      DTFT_true <- rep(TRUE, length(player_vec))
      for(i in (2:length(player_vec))){
        # print(player_vec[i])
        # print(adv_vec[i-1])
        if (!player_vec[i]==adv_vec[i-1]) {
          DTFT_true[i]<-FALSE
        }
      }
      #print(paste0("DTFT: ",(length(which(DTFT_true))/length(player_vec))))
      fingerprint_df$DTFT <- length(which(DTFT_true))/length(player_vec)
    }
  }
  
  #TF2T
  if ("TF2T" %in% types) {
    if (player_vec[1]==1 & player_vec[2]==1) {#TF2T
      TF2T_true <- rep(TRUE, length(player_vec))
      for(i in (3:length(player_vec))){
        # print(player_vec[i])
        # print(adv_vec[i-1])
        if (!(player_vec[i]==adv_vec[i-1] & player_vec[i]==adv_vec[i-2])) {
          TF2T_true[i]<-FALSE
        }
      }
      #print(paste0("TF2T: ",(length(which(TF2T_true))/length(player_vec))))
      fingerprint_df$TF2T <- length(which(TF2T_true))/length(player_vec)
    }
  }
  
  #TF3T
  if ("TF3T" %in% types) {
    if (player_vec[1]==1 & player_vec[2]==1 & player_vec[3]==1) {#TF2T
      TF3T_true <- rep(TRUE, length(player_vec))
      for(i in (4:length(player_vec))){
        # print(player_vec[i])
        # print(adv_vec[i-1])
        if (!(player_vec[i]==adv_vec[i-1] & player_vec[i]==adv_vec[i-2] & player_vec[i]==adv_vec[i-3])) {
          TF3T_true[i]<-FALSE
        }
      }
     # print(paste0("TF3T: ",(length(which(TF3T_true))/length(player_vec))))
      fingerprint_df$TF3T <- length(which(TF3T_true))/length(player_vec)
    }
  }
  
  #DTF3T ## not added b/c not a factor...
  if (player_vec[1]==0 & player_vec[2]==1 & player_vec[3]==1) {#DTF3T
    DTF3T_true <- rep(TRUE, length(player_vec))
    for(i in (4:length(player_vec))){
      # print(player_vec[i])
      # print(adv_vec[i-1])
      if (!(player_vec[i]==adv_vec[i-1] & player_vec[i]==adv_vec[i-2] & player_vec[i]==adv_vec[i-3])) {
        DTF3T_true[i]<-FALSE
      }
    }
    print(paste0("DTF3T: ",(length(which(DTF3T_true))/length(player_vec))))
    #fingerprint_df$DTF3T <- length(which(DTF3T_true))/length(player_vec)
  }
  
  #DTF2T (not ready yet)
  if (player_vec[1]==1 & player_vec[2]==1) {#TF2T
    DTF2T_true <- rep(TRUE, length(player_vec))
    for(i in (3:length(player_vec))){
      # print(player_vec[i])
      # print(adv_vec[i-1])
      if (!(player_vec[i]==adv_vec[i-1] & player_vec[i]==adv_vec[i-2])) {
        DTF2T_true[i]<-FALSE
      }
    }
    #print(paste0("DTF2T: ",(length(which(DTF2T_true))/length(player_vec))))
    #fingerprint_df$TF2T <- length(which(TF2T_true))/length(player_vec)
  }
  
  #(TF2T) Not Tit for two tats (defects except cooperates after two defections)
  if ("NotTF2T" %in% types) {
    NotTF2T_vec <- rep(0, length(player_vec))
    if (player_vec[1]==0 & player_vec[2]==0 & grepl(paste(c(0,0),collapse=";"),paste(adv_vec,collapse=";"))) {#(TF2T) Pplayer must defect fro round 1 & two and adversary must have played a 0,0 sometime - am I missing a condition here?
      for(i in (2:(length(player_vec)-1))){
        # print(player_vec[i])
        # print(adv_vec[i-1])
        if (adv_vec[i-1]==0 & adv_vec[i]==0){
          NotTF2T_vec[i+1] <- 1
        }
      }
    }
    #print(paste0("(TF2T): ",(length(which(NotTF2T_vec==player_vec))/length(player_vec))))
    fingerprint_df$NotTF2T <- length(which(NotTF2T_vec==player_vec))/length(player_vec)
  }
  
  #TwoTFT
  if ("TwoTFT" %in% types) {
    if (player_vec[1]==1& grepl(paste(c(0,0),collapse=";"),paste(player_vec,collapse=";"))) {#TwoTFT defects twice for adv defect but otherwise coops (so has to cooperaet on round 1 and have at least one D-D sequence) --- assumes adversary defected at least once and not only on round 9.  If adv defected only on round 9, then TFT would be an appropriate match (as would TwoTFT)
      TwoTFT_true <- rep(TRUE, length(player_vec))
      TwoTFT_vec <- rep(1, length(player_vec))
      for (i in (1:(length(player_vec)-2))) {
        if (adv_vec[i]==0){
          TwoTFT_vec[i+1] <- 0
          TwoTFT_vec[i+2] <- 0
        } #first has to check if a Defect after a defect
      }
      #print(paste0("2TFT: ",(length(which(TwoTFT_vec==player_vec))/length(player_vec))))
      fingerprint_df$TwoTFT <- length(which(TwoTFT_vec==player_vec))/length(player_vec)
    }
  }
  
  #(2TFT) NotTwoTFT
  if ("NotTwoTFT" %in% types) {
    if (player_vec[1]==0& grepl(paste(c(1,1),collapse=";"),paste(player_vec,collapse=";"))) {#NotTwoTFT does the opposite of 2TFT (starts with a defect?)
      NotTwoTFT_vec <- rep(0, length(player_vec))
      for (i in (1:(length(player_vec)-2))) { #creates a Not2TFT result from adv_vec
        if (adv_vec[i]==0){
          NotTwoTFT_vec[i+1] <- 1
          NotTwoTFT_vec[i+2] <- 1
        } #first has to check if a Defect after a defect
      }
      #print(paste0("(2TFT): ",(length(which(tNotTwoTFT_vec==player_vec))/length(player_vec))))
      fingerprint_df$NotTwoTFT <- length(which(NotTwoTFT_vec==player_vec))/length(player_vec)
    }
  }
  
  #UC (usually cooperate)
  if ("UC" %in% types) {
    if (player_vec[1]==1) {#UC cooperates except after a C following a D (0,1)
      UC_vec <- rep(1, length(player_vec))
      for (i in (2:(length(player_vec)-1))) { #creates a Not2TFT result from adv_vec
        if (adv_vec[i-1]==0 & adv_vec[i]==1){
          UC_vec[i+1] <- 0
        } #first has to check if a Defect after a defect
      }
      #print(paste0("UC: ",(length(which(UC_vec==player_vec))/length(player_vec))))
      fingerprint_df$UC <- length(which(UC_vec==player_vec))/length(player_vec)
    }
  }
  
  #UD (usually defects)
  if ("UD" %in% types) {
    if (player_vec[1]==0) {#UC cooperates except after a C following a D (0,1)
      UD_vec <- rep(1, length(player_vec))
      for (i in (2:(length(player_vec)-1))) { #creates a Not2TFT result from adv_vec
        if (adv_vec[i-1]==1 & adv_vec[i]==0){
          UD_vec[i+1] <- 1
        } #first has to check if a Defect after a defect
      }
      #print(paste0("UD: ",(length(which(UD_vec==player_vec))/length(player_vec))))
      fingerprint_df$UD <- length(which(UD_vec==player_vec))/length(player_vec)
    }
  }
  
  #PTFT, Round 1= C then C if both players played same, else D
  if ("PTFT" %in% types) {
    if (player_vec[1]==1) {
      PTFT_vec <- rep(1, length(player_vec))
      for (i in (2:length(player_vec))) { #creates a Not2TFT result from adv_vec
        if (adv_vec[i-1]==player_vec[i-1]){
          PTFT_vec[i] <- 1
        } else {
          PTFT_vec[i] <- 0
        }
      }
      #print(paste0("PTFT: ",(length(which(PTFT_vec==player_vec))/length(player_vec))))
      fingerprint_df$PTFT <- length(which(PTFT_vec==player_vec))/length(player_vec)
    }
  }
  
  #PT2FT, coop then C if(last 2 rounds = C/C, C/C, or D/D,D/D, or C/C,D/D) else D
  if ("PT2FT" %in% types) {
    if (player_vec[1]==1) {
      PT2FT_vec <- c(1,1,rep(0, (length(player_vec)-2)))
      for (i in (3:length(player_vec))) { #creates a Not2TFT result from adv_vec
        if ((adv_vec[i-2]==player_vec[i-2] & adv_vec[i-1]==player_vec[i-1])|((adv_vec[i-2]==1 & player_vec[i-2]==1) & (adv_vec[i-1]==0 & player_vec[i-1]==0))){
          PT2FT_vec[i] <- 1
        } else {
          PT2FT_vec[i] <- 0
        }
      }
      #print(paste0("PT2FT: ",(length(which(PT2FT_vec==player_vec))/length(player_vec))))
      fingerprint_df$PT2FT <- length(which(PT2FT_vec==player_vec))/length(player_vec)
    }
  }
  
  #wsls
  if ("WSLS" %in% types) {
    WSLS_vec <- rep(player_vec[1],length(player_vec))
    for (i in (1:(length(player_vec)-1))){
      if (player_vec[i]==adv_vec[i]){
        WSLS_vec[i+1] <- 1
      } else {
        WSLS_vec[i+1] <- 0
      }
    }
    WSLS_vec[1] <- 1 #initial move is coop
    #print(paste0("WSLS: ",(length(which(WSLS_vec==player_vec))/length(player_vec))))
    fingerprint_df$WSLS <- length(which(WSLS_vec==player_vec))/length(player_vec)
  }
  
  #T2
  if ("T2" %in% types) {
    if (player_vec[1]==1){ #needs to start with 1
      if (grepl(paste(c(0,0,1),collapse=";"),paste(player_vec,collapse=";"))){ #needs to see at least one 0,0,1 from the player
        T2_vec <- rep(1, length(player_vec))
        i<-1
        while (i %in% (1:length(adv_vec))) {
          i <- i+1 #i=2
          if (adv_vec[i-1]==0){ #adv_vec[1] = 0
            T2_vec[i] <- 0 #p_v[2] = 0
            T2_vec[i+1] <- 0#p_v[3] = 0
            T2_vec[i+2] <- 1#p_v[4] = 0
            i <- i+2 #i=4
          } else {
            T2_vec[i] <- 1
          }
        }
        T2_vec <- T2_vec[1:10]
        #print(paste0("T2: ",(length(which(T2_vec==player_vec))/length(player_vec))))
        fingerprint_df$T2 <- length(which(T2_vec==player_vec))/length(player_vec)
      }
    }
  }
  
  #Psyc or AntiTFT (psycho does whatever adversary did last round)
  if ("Psycho" %in% types) {
    if (adv_vec[1]==0) {
      Psycho_vec <- rep(0, length(player_vec))
      Psycho_vec[1] <- 1
    } else if (adv_vec[1]==1) {
      Psycho_vec <- rep(1, length(player_vec))
    }
    for (i in (2:length(player_vec))) {
      Psycho_vec[i] <- adv_vec[i-1]
    }

    fingerprint_df$Psycho <- length(which(Psycho_vec==player_vec))/length(player_vec)
  }
  
  #MEM2 play TFT if previous round both CC, play TF2T if CD then DC, otherwise ALLD
  # play ALLD forever if ALLD selected twice
  if ("MEM2" %in% types) {
    all_d_counter <- 0
    if (player_vec[1] == 1) {
      MEM2_vec <- rep(1, length(player_vec))
      for (i in (2:length(player_vec))) {
        if (player_vec[i-1]==adv_vec[i-1] & player_vec[i-1]==1) { #prev round was CC
          #play TFT
          
        } else if (player_vec[i-2]!=adv_vec[i-2] & player_vec[i-2]==1 & player_vec[i-1]!=adv_vec[i-1] & player_vec[i-1]==0) { 
          #play TF2T
        } else {
          #play ALLD
          MEM2_vec[i] <- 0
          all_d_counter <- all_d_counter +1
          if (all_d_counter > 2) {
            temp_vec[i:length(temp_vec)] <-0
          }
        }
      }
      
    }
    print("MEM2")
  }
  
  #FBF Firm but fair.  Coop except after receive sucker payoff (P: C, A:D)
  if ("FBF" %in% types){
    if (player_vec[1]==1) {
      FBF_vec <- rep(1, length(player_vec))
      for (i in (2:length(player_vec))) {
        if (player_vec[i-1]==1 & adv_vec[i-1]==0) {
          FBF_vec[i] <- 0
        }
      }
      #print(paste0("FBF: ",(length(which(FBF_vec==player_vec))/length(player_vec))))
      
      fingerprint_df$FBF <- length(which(FBF_vec==player_vec))/length(player_vec)
    }
  }
  
  #bully 
  if ("Bully" %in% types) {
    if (adv_vec[1]==0) {
      Bully_vec <- rep(0, length(player_vec))
      
    } else if (adv_vec[1]==1) {
      Bully_vec<- rep(1, length(player_vec))
    }
    Bully_vec[1] <- 0
    for (i in (2:length(player_vec))) { 
      Bully_vec[i] <- adv_vec[i-1]
    }
    print(paste0("Bully: ",(length(which(Bully_vec==player_vec))/length(player_vec))))
    #fingerprint_df$Bully <- length(which(Bully_vec==player_vec))/length(player_vec)
  }
  
  return(fingerprint_df)
}
types <- c("id","Adversary","AllC","AllD","TFT","TF2T","NotTF2T","TwoTFT","NotTwoTFT","UC","UD","WSLS","Psycho","DTFT","PTFT","PT2FT","T2","FBF","GLM","nnet","keras")
fp_df <- as.data.frame(matrix(data=0, nrow=(length(pw_ids)*3), ncol=length(types)))
axl_types <- c("id","Adversary", as.character(AI_actions$Strategy))
axl_fp_df <- as.data.frame(matrix(data=0, nrow=(length(pw_ids)*3), ncol=length(axl_types)))
names(fp_df) <- types
names(axl_fp_df) <- axl_types
fp_df$id <- rep(pw_ids,3)
axl_fp_df$id <- rep(pw_ids,3)
for (i in pw_ids) {
  fp_df[fp_df$id==i,]$Adversary <- c("Human","AI","Human+AI")
  axl_fp_df[axl_fp_df$id==i,]$Adversary<- c("Human","AI","Human+AI")
}
for (i in pw_ids) {
  fp_df[fp_df$id==i & fp_df$Adversary=="Human",c(3:length(types))]<- fingerprint(pw_cols[pw_cols$id==i & pw_cols$Adversary=="Human",]$Choice,human_adv_choices, types)
  axl_fp_df[axl_fp_df$id==i & axl_fp_df$Adversary=="Human",c(3:length(axl_types))]<- axelrod_fp(pw_cols[pw_cols$id==i & pw_cols$Adversary=="Human",]$Choice,Human_actions)
  fp_df[fp_df$id==i & fp_df$Adversary=="AI",c(3:length(types))]<- fingerprint(pw_cols[pw_cols$id==i & pw_cols$Adversary=="AI",]$Choice,ai_adv_choices, types)
  axl_fp_df[axl_fp_df$id==i & axl_fp_df$Adversary=="AI",c(3:length(axl_types))]<- axelrod_fp(pw_cols[pw_cols$id==i & pw_cols$Adversary=="AI",]$Choice,AI_actions)
  fp_df[fp_df$id==i & fp_df$Adversary=="Human+AI",c(3:length(types))]<- fingerprint(pw_cols[pw_cols$id==i & pw_cols$Adversary=="Human+AI",]$Choice,hai_adv_choices, types)
  axl_fp_df[axl_fp_df$id==i & axl_fp_df$Adversary=="Human+AI",c(3:length(axl_types))]<- axelrod_fp(pw_cols[pw_cols$id==i & pw_cols$Adversary=="Human+AI",]$Choice,HAI_actions)
  
}
#Add GLM & nnet & keras predictions (needs to add to axl_fp_df)
for (i in pw_ids) {
  #print(i)
  for (j in list("Human","Human+AI","AI")){
    #print(j)
    # for (k in (1:10)) {
    #   player_vec[k] <- pw_all_data_with_demo[pw_all_data_with_demo$id==i &pw_all_data_with_demo$Adversary==j & pw_all_data_with_demo$period==k, ]$my.decision
    #   temp_vec_GLM[k] <- pw_all_data_with_demo[pw_all_data_with_demo$id==i &pw_all_data_with_demo$Adversary==j & pw_all_data_with_demo$period==k, ]$GLM
    #   temp_vec_nnet[k] <- pw_all_data_with_demo[pw_all_data_with_demo$id==i &pw_all_data_with_demo$Adversary==j & pw_all_data_with_demo$period==k, ]$GLM
    #   temp_vec_keras[k] <- pw_all_data_with_demo[pw_all_data_with_demo$id==i &pw_all_data_with_demo$Adversary==j & pw_all_data_with_demo$period==k, ]$keras
    # }
    #fp_df[fp_df$id==i & fp_df$Adversary==j,]$GLM <- length(which(player_vec==temp_vec_GLM))/length(temp_vec_GLM)
    #fp_df[fp_df$id==i & fp_df$Adversary==j,]$nnet <- length(which(player_vec==temp_vec_nnet))/length(temp_vec_nnet)
    #fp_df[fp_df$id==i & fp_df$Adversary==j,]$keras <- length(which(player_vec==temp_vec_keras))/length(temp_vec_keras)
    
  
  fp_df[fp_df$id==i & fp_df$Adversary==j,]$GLM <- length(which(pw_all_data_with_demo[pw_all_data_with_demo$id==i & pw_all_data_with_demo$Adversary==j,]$my.decision==pw_all_data_with_demo[pw_all_data_with_demo$id==i & pw_all_data_with_demo$Adversary==j,]$GLM))/10
  fp_df[fp_df$id==i & fp_df$Adversary==j,]$nnet <- length(which(pw_all_data_with_demo[pw_all_data_with_demo$id==i & pw_all_data_with_demo$Adversary==j,]$my.decision==pw_all_data_with_demo[pw_all_data_with_demo$id==i & pw_all_data_with_demo$Adversary==j,]$nnet))/10
  fp_df[fp_df$id==i & fp_df$Adversary==j,]$keras <- length(which(pw_all_data_with_demo[pw_all_data_with_demo$id==i & pw_all_data_with_demo$Adversary==j,]$my.decision==pw_all_data_with_demo[pw_all_data_with_demo$id==i & pw_all_data_with_demo$Adversary==j,]$keras))/10
  }
}


print("PW Strategy Fingerprint")
print(fp_df)
fp_df_sum <- as.data.frame(matrix(NA, nrow=3, ncol=(length(types)-1)))
names(fp_df_sum) <- c(types[-c(1)])
fp_df_sum$Adversary <- c("Human","AI","Human+AI")
fp_df_sum[fp_df_sum$Adversary=="Human",c(2:(length(types)-1))]<- colSums(fp_df[fp_df$Adversary=="Human",c(3:length(types))])
fp_df_sum[fp_df_sum$Adversary=="AI",c(2:(length(types)-1))]<- colSums(fp_df[fp_df$Adversary=="AI",c(3:length(types))])
fp_df_sum[fp_df_sum$Adversary=="Human+AI",c(2:(length(types)-1))]<- colSums(fp_df[fp_df$Adversary=="Human+AI",c(3:length(types))])
n<- melt(fp_df_sum)
#would be nice to order these by value, but not sure how to do that...
p<- ggplot(n, aes(x= Adversary, y= variable, fill=value))+
  geom_raster(aes(fill=value))+
  labs(title="PW-Strategy Fingerprint: Strategies by Adversary",x="Adversary", y = "Strategy", fill = "Density") +theme(plot.title = element_text(size=12))
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")

# p<-ggplot(n, aes(x= variable, y= value, group=Adversary))+geom_line(aes(group=Adversary))
# 
# print(p)
# print(paste0("Insert ", p$labels$title," Plot"))
# ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")


#create the matrix of ids vs strategies and percent match
pw_bestmatch <- as.data.frame(matrix(NA, nrow=length(pw_ids), ncol=4))
colnames(pw_bestmatch) <- c("id","Human","AI","HumanAI")
pw_bestmatch[,1] <- pw_ids
strat_threshold <- 0.9
for (i in pw_ids) {

  l<-fp_df[fp_df$id==i,]
  t<-l[,3:length(types)]
  # s<-colnames(l)[apply(l,1,which.max)]
  s<-colnames(t)[max.col(t)]
  r<-colnames(l)[l[1,]==l[2,] & l[2,]==l[3,] & l[1,]==0] #ID and 
  m<-l[,!colnames(l) %in% c(r,"id")] #remove columns which are no match

  for (q in c(1:3)) { #for each adversary (1=Human, 2=AI, 3=Human+AI)
    if (length(which(t[q,]==t[q,s[q]]))>1) { #if there are ties
      if (max(t[q,]) < strat_threshold) { #if the accuracy is < threshold (0.81)
        s[q] <- "unk" # say the strategy is unknown
      } else { #otherwise, combine all ties and the accuracy figure
      s[q] <- paste0(paste(colnames(t)[t[q,]==t[q,s[q]]], collapse="/"),"(",max(t[q,]),")")
      }
      # if ((grepl('^(.*)AllC(.*)$',s[q]))&((grepl('^(.*)nnet(.*)$',s[q]))|(grepl('^(.*)GLM(.*)$',s[q]))) ) { #if AllC included wth GLM & nnet, remove GLM & nnet
      #   
      # }
    } else if (max(t[q,]) < strat_threshold) { # if no ties and < 0.8, just add accuracy figure
      s[q] <- "unk"
    } else if (max(t[q,]) >= strat_threshold){
      s[q] <- paste0(s[q],"(",max(t[q,]),")")# if no ties and >0.7, just add accuracy figure
    }
  }
  pw_bestmatch[pw_bestmatch$id==i,]$Human <- s[1]
  pw_bestmatch[pw_bestmatch$id==i,]$AI <- s[2]
  pw_bestmatch[pw_bestmatch$id==i,]$HumanAI <- s[3]

}

# pw_bestmatch$id <- as.factor(pw_bestmatch$id)
# pw_bestmatch$Human <- as.factor(pw_bestmatch$Human)
# pw_bestmatch$AI <- as.factor(pw_bestmatch$AI)
# pw_bestmatch$HumanAI <- as.factor(pw_bestmatch$HumanAI)
print(paste0("Strategy Fingerprint of each player by adversary. Note: matches less than ",strat_threshold*100,"% (",strat_threshold,") are listed as Unk"))

#axl bestmatch create the matrix of ids vs strategies and percent match
axl_bestmatch <- as.data.frame(matrix(NA, nrow=length(pw_ids), ncol=4))
colnames(axl_bestmatch) <- c("id","Human","AI","HumanAI")
axl_bestmatch[,1] <- pw_ids
strat_threshold <- 0.9
for (i in pw_ids) {
  
  l<-axl_fp_df[axl_fp_df$id==i,]
  t<-l[,3:length(axl_types)]
  # s<-colnames(l)[apply(l,1,which.max)]
  s<-colnames(t)[max.col(t)]
  r<-colnames(l)[l[1,]==l[2,] & l[2,]==l[3,] & l[1,]==0] #ID and 
  m<-l[,!colnames(l) %in% c(r,"id")] #remove columns which are no match
  
  for (q in c(1:3)) { #for each adversary (1=Human, 2=AI, 3=Human+AI)
    if (length(which(t[q,]==t[q,s[q]]))>1) { #if there are ties
      if (max(t[q,]) < strat_threshold) { #if the accuracy is < threshold (0.81)
        s[q] <- "unk" # say the strategy is unknown
      } else { #otherwise, combine all ties and the accuracy figure
        s[q] <- paste0(paste(colnames(t)[t[q,]==t[q,s[q]]], collapse="/"),"(",max(t[q,]),")")
      }
      # if ((grepl('^(.*)AllC(.*)$',s[q]))&((grepl('^(.*)nnet(.*)$',s[q]))|(grepl('^(.*)GLM(.*)$',s[q]))) ) { #if AllC included wth GLM & nnet, remove GLM & nnet
      #   
      # }
    } else if (max(t[q,]) < strat_threshold) { # if no ties and < 0.8, just add accuracy figure
      s[q] <- "unk"
    } else if (max(t[q,]) >= strat_threshold){
      s[q] <- paste0(s[q],"(",max(t[q,]),")")# if no ties and >0.7, just add accuracy figure
    }
  }
  axl_bestmatch[axl_bestmatch$id==i,]$Human <- s[1]
  axl_bestmatch[axl_bestmatch$id==i,]$AI <- s[2]
  axl_bestmatch[axl_bestmatch$id==i,]$HumanAI <- s[3]
  
}

##stratEst function - doesn't work right, it seems...
a <- pw_all_data_with_demo
a <- a[,c("Adversary","id","period","my.decision")]
# a$period <- as.integer(period)
a$my.decision <- as.integer(a$my.decision)
a[a$my.decision==2,]$my.decision <- 0
a$other_cooperation <- rep(0,nrow(a))
a$Adversary <- as.integer(as.factor(a$Adversary)) #1 = AI, 2 = Human, 3=HAI
adv_choices <- matrix(ai_adv_choices, ncol=10)
adv_choices <- rbind(adv_choices, human_adv_choices, hai_adv_choices)
colnames(a) <- c("treatment","id","period","cooperation","other_cooperation")
a$id <-as.factor(a$id)
a$cooperation <- as.integer(a$cooperation)
a$other_cooperation <- as.integer(a$other_cooperation)
# a$supergame <- rep(1, nrow(a)) # not needed

#adds adversary choics to other_cooperation
for (j in (1:3)) { #each adversary
  for (i in (1:10)){ #each round
    a[a$treatment==j & a$period ==i,]$other_cooperation <- rep(adv_choices[j,i], length(a[a$treatment==j & a$period ==i,]$other_cooperation))
  }
}
strats <- rbind(ALLD,ALLC,DC,DGRIM2, DGRIM3, DTF2T, DTF3T, DTFT, FC, GRIM, GRIM2, GRIM3, M1BF, PT2FT, PTFT, SGRIM, T2F2T, T2FT, TF2T, TF3T, TFT, WSLS )
l <- c("ALLD","ALLC","DC","DGRIM2", "DGRIM3", "DTF2T", "DTF3T", "DTFT", "FC", "GRIM", "GRIM2", "GRIM3", "M1BF", "PT2FT", "PTFT", "SGRIM", "T2F2T", "T2FT", "TF2T", "TF3T", "TFT", "WSLS")

# strat_df <- as.data.frame(matrix(NA, nrow=length(l), ncol=length(pw_ids)))
#colnames(strat_df) <- pw_ids
strat_array <- array(NA, c(length(l),length(pw_ids),3))
colnames(strat_array) <- pw_ids

for (i in pw_ids) { #each particiapnt
  for (j in (1:3)) { #each adversary
    
    b<-a[a$id==i & a$treatment==j,]
    model <- stratEst(b, strats,print.messages = F)
    strat_array[,i,j] <- round(model$shares,3)
    # strat_df[,i] <- round(model$shares,3)
  }
}
# rownames(strat_df) <- l
rownames(strat_array) <- l
print(strat_array)
#----memory-length estimator (needs work) - 
a <- pw_all_data_with_demo
a$my.decision <- as.integer(a$my.decision)
a[a$my.decision==2,]$my.decision <-0
b <-  array(NA, c(4,2,3,length(pw_ids)), dimnames = list(c("C,C","C,D","D,C","D,D"),c("C","D"),c("Human","AI","Human+AI"),pw_ids))
n<-matrix(NA,nrow=2, ncol=2)
for (i in pw_ids) {
  for (j in c("Human","AI","Human+AI")){
    b[1,1,j,i] <- length(which((a[a$id==i & a$Adversary==j,]$my.decision1==1) & (a[a$id==i & a$Adversary==j,]$other.decision1==1) & a[a$id==i & a$Adversary==j,]$my.decision==1))
    b[1,2,j,i] <- length(which((a[a$id==i & a$Adversary==j,]$my.decision1==1) & (a[a$id==i & a$Adversary==j,]$other.decision1==1) & a[a$id==i & a$Adversary==j,]$my.decision==0))
    b[2,1,j,i] <- length(which((a[a$id==i & a$Adversary==j,]$my.decision1==1) & (a[a$id==i & a$Adversary==j,]$other.decision1==0) & a[a$id==i & a$Adversary==j,]$my.decision==1))
    b[2,2,j,i] <- length(which((a[a$id==i & a$Adversary==j,]$my.decision1==1) & (a[a$id==i & a$Adversary==j,]$other.decision1==0) & a[a$id==i & a$Adversary==j,]$my.decision==0))
    b[3,1,j,i] <- length(which((a[a$id==i & a$Adversary==j,]$my.decision1==0) & (a[a$id==i & a$Adversary==j,]$other.decision1==1) & a[a$id==i & a$Adversary==j,]$my.decision==1))
    b[3,2,j,i] <- length(which((a[a$id==i & a$Adversary==j,]$my.decision1==0) & (a[a$id==i & a$Adversary==j,]$other.decision1==1) & a[a$id==i & a$Adversary==j,]$my.decision==0))
    b[4,1,j,i] <- length(which((a[a$id==i & a$Adversary==j,]$my.decision1==0) & (a[a$id==i & a$Adversary==j,]$other.decision1==0) & a[a$id==i & a$Adversary==j,]$my.decision==1))
    b[4,2,j,i] <- length(which((a[a$id==i & a$Adversary==j,]$my.decision1==0) & (a[a$id==i & a$Adversary==j,]$other.decision1==0) & a[a$id==i & a$Adversary==j,]$my.decision==0))
    for (k in c(1,4)) {
      n[1,] <- colSums(b[,,j,i])
      n[2,] <- b[k,,j,i]
      m <- fisher.test(n)["p.value"]
      if (m < 0.2) {
        print(i)
        print(j)
        print(m)
      }
    }
  }
}



#--------------------PW - Round 1 <-> Strategic Decisions Data Analysis-----------------
print("        ---PW: Is ROUND ONE decision correlated to STRATEGIC DECISIONS?")
pw_df <- as.data.frame(xtabs(~period + my.decision +my.round1decision, data=pw_all_data_with_demo))
pw_df <- pw_df[pw_df$my.decision=="Peace",]
pw_df$period <- as.numeric(pw_df$period)
p<- ggplot(data=pw_df, aes(x=period, y=Freq, group=my.round1decision))+  geom_point(aes(color=my.round1decision))+geom_smooth(method='lm',formula=y~x, aes(group=my.round1decision, color=my.round1decision, fill=my.round1decision), alpha=0.2)+ labs(title="PW - Peace Choices by Round 1 Choice",x="Round", y = "# of Choices", color = "my.round1decision") +scale_x_discrete(limits=c(1:10))+ theme(plot.title = element_text(size=14), legend.title = element_text(size=9), legend.position = c(0.8, 0.8))#or use method="lm instead of auto"
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
# print(p)
print("RESULT: From the plot, it appeares the group of participants who chose War in round 1 (0 in this plot) had little variance across rounds, but that the initial cooperator-group (Peace, 1 in this graph) varied markedly.")

print(xtabs(~ my.round1decision + my.decision, data = pw_all_data_with_demo))
print(chisq.test(xtabs(~ my.round1decision + my.decision, data = pw_all_data_with_demo)))
print("RESULT: A Chi-sq test on the aggregate Peace decisions and round 1 decisions shows the IV(Round 1 decision) and DV (aggregate sum of Peace Choices across all adversaries) are statistically dependent at p < .0001 (.05), so we conduct a RMLR...")
print("")

print(kruskal.test(my.decision ~ my.round1decision, data=pw_all_data_with_demo))
print("RESULT: A Kruskal-Wallis test on the aggregate Peace decisions and round 1 decisions shows the IV(Round 1 decision) and DV (aggregate sum of Peace Choices across all adversaries) are statistically dependent at p < .0001 (.05), so we conduct a RMLR...")
print("")
# 
# m <- glmer(my.decision ~ my.round1decision+(1|id)+(1|pw_order), data=pw_all_data_with_demo, family=binomial)
# # print(summary(m))
# print(Anova(m, type="3"))
# print("RESULT: A Repeated Measures Logisitic Regression (RMLR) shows that ROUND1 decision is statistically significant to variation in overall strategic gameplay (p <.01) ")
# print("")
print("IMPLICATION: Non-parametric analyses of Round 1 decisions show a strong statistical correlation to a participant's gameplay across rounds.")
# m <- glmer(my.decision ~ Adversary+my.decision1+other.decision1+my.round1decision+(1|id)+(1|pw_order), data=pw_all_data_with_demo, family=binomial)
# # print(summary(m))
# print(Anova(m, type="3"))
# print("RESULT: A Repeated Measures Logisitic Regression (RMLR) shows that when analyzed along with Adversary, my.decision1, and other.decision1, ROUND1 decision is a FINDING OF STATISTICALY INTEREST (p <.10) but is NOT STATISTICALLY SIGNIFICANT at p <.05.")




print("        ---PW: Is ROUND ONE decision correlated to variation in strategic decisions BY ADVERSARY?")

print(xtabs(~ Adversary + my.round1decision + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + my.round1decision + my.decision, data = pw_all_data_with_demo)[,,1]))
print("RESULT: A Chi-Sq test of Aggregate peace decisions by adversary and round1 choice shows statistically significant variation in Peacefulness (by adversary and Round 1 decision) at p <.05. So, we segregate the data by round 1 defectors (chose 'WAR') and round 1 cooperators (chose 'Peace')...")

print(" -- Aggregate choices of Round 1 Defectors")
print(xtabs(~Adversary+my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==0,]))

pw_df <- as.data.frame(xtabs(~period + my.decision +Adversary, data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==0,]))
pw_df <- pw_df[pw_df$my.decision=="Peace",]
pw_df$period <- as.numeric(pw_df$period)
p<- ggplot(data=pw_df, aes(x=period, y=Freq, group=Adversary))+  
  geom_point(aes(color=Adversary))+
  geom_smooth(method='lm',formula=y~x, aes(group=Adversary, color=Adversary, fill=Adversary), alpha=0.2)+ 
  labs(title="PW-Round 1 Defectors: Peace Choices by Round and Adversary",x="Round", y = "# of Choices", color = "Adversary") +scale_x_discrete(limits=c(1:10))+ scale_y_discrete(limits=c(0:10))+theme(plot.title = element_text(size=14), legend.title = element_text(size=9), legend.position = c(0.8, 0.8))#or use method="lm instead of auto"
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
print("")

print(chisq.test(xtabs(~Adversary+my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==0,])))
print(fisher.test(xtabs(~Adversary+my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==0,])))
print("RESULT: Overall cooperation by adversary is open for further analyses because a chisq test is siginificant at p <.05 and a fisher exact test is not significant at p <.05.")
pw_r1_defectors <- as.data.frame(xtabs(~id+Adversary+my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==0,]))
print(friedman.test(Freq~Adversary|id,data=pw_r1_defectors[pw_r1_defectors$my.decision=="Peace",]))
print("NOTE: INTERESTING:")
print("RESULT: A Friedman test shows that those that Round 1 defectors (chose WAR on the first round) DID NOT SHOW a statistically significant difference in gameplay by adversary at p <.05.")

print("---PW: Repeated Measures Logistic Regression on ROUND ONE DEFECTORS---\n")
m <- glmer(my.decision ~ Adversary+my.decision1+other.decision1+(1|id)+(1|pw_order), data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==0,], family=binomial)
print(summary(m))
print(Anova(m, type="3"))
print("RESULT: A RMLR on ROUND ONE DEFECTORS showed initial propensity and the participant's previous choice showed graetest statistical significance (p <.05).")
print("IMPLICATON: For round one defectors, Adversary seems to have the least effect despite the difference in adversary gameplay compared to other typical strategic gameplay factors.")


print("")
print(" -- Aggregate choices of Round 1 cooperators")
print(xtabs(~Adversary+my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==1,]))
print(chisq.test(xtabs(~Adversary+my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==1,])))

print("RESULT: A chisq test of proportions shows Round 1 choice correlation to overall cooperation by adversary is statistically significant at  p <.01.")

pw_r1_cooperators <- as.data.frame(xtabs(~id+Adversary+my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==1,]))
print(friedman.test(Freq~Adversary|id,data=pw_r1_cooperators[pw_r1_cooperators$my.decision=="Peace",]))
print("RESULT: A friedman test shows that those that chose PEACE on the first round DID SHOW a statistically significant difference in gameplay by adversary at p <.01 (p <.05). So, we do a RMLR")

print("---PW: RMLR on ROUND ONE COOPERATORS---")
# m <- glmer(my.decision ~ Adversary+my.decision1+other.decision1+(1|id)+(1|pw_order), data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==1,], family=binomial)
# print(summary(m))
# print(Anova(m, type="3"))
# print("RESULT: A RMLR on ROUND ONE COOPERATORS showed that the participant's previous choice, followed by the adversary's decision, followed by initial propensity, show statistical significance at p < .05). INTERESTING:Adversary shows finding of statistical interest (p< .10) but isn't significant at p < .05, despite the difference in adversary gameplay.ALSO: looks like the two greatest factors in the model are initial propensity mediated by participant's previous decision.")

m <- glmer(my.decision ~ Adversary+(1|id)+(1|pw_order), data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==1,], family=binomial)
# print(summary(m))
print(Anova(m, type="3"))
print("RESULT: A RMLR on ROUND ONE COOPERATORS showed that the Adversary type had a statistically significant relationship with the decision at p < .05 (p < .01)")
print("HELP: Would like to know if other.decision2/3/4 (history) matters, but how do I code this? I keep getting 'rank deficient' error if I include my.decision2 or other.decision2. Is this because I'm using a binomial test for a multinomial variable?")



pw_df <- as.data.frame(xtabs(~period + my.decision +Adversary, data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==1,]))
pw_df <- pw_df[pw_df$my.decision=="Peace",]
pw_df$period <- as.numeric(pw_df$period)
p<- ggplot(data=pw_df, aes(x=period, y=Freq, group=Adversary))+  
  geom_point(aes(color=Adversary))+
  geom_smooth(method='auto',formula=y~x, aes(group=Adversary, color=Adversary, fill=Adversary), alpha=0.2)+ 
  labs(title="PW-Round 1 Cooperators: Peace Choices by Round and Adversary",x="Round", y = "# of Choices", color = "Adversary") +scale_x_discrete(limits=c(1:10))+ theme(plot.title = element_text(size=14), legend.title = element_text(size=9), legend.position = c(0.8, 0.8))#or use method="lm instead of auto"
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
# print(p)
print("NOTE: Interesting that changes across rounds for Human+AI and AI are very similar, despite VERY different gameplay from the adversary.  This is in contrast to the human adversary.")
print("NOTE: Is it possible that participants' strategy against a human adversary are memory-1 and something else vs AI or HAI?  THIS would be an important and relevant finding. You could explore this using a GLM with my.decision1 and other.decision1 as the main factors to see what the coefficients are (my.round1decision as an interaction with all variables?).  It's interesting that the initial amount of cooperation has an effect here.")



#--------------------PW - Time <-> Strategic Decision Analysis-----------------
print(" ---PW: Is BY-PAGE DECISION TIME correlated to decision-making?")
p<- ggplot(pw_all_data_with_demo, aes(x=seconds_on_page, group=my.decision))+
  geom_density(aes(fill=my.decision, color=my.decision),alpha=0.3)+
  labs(title="PW-Decision Time Density Plot", x="Seconds per Decision", y = "Density", fill="Decision",color="Decision")

print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")

print(paste("RESULT: Average time for Peace decisions is",round(mean(pw_all_data_with_demo[pw_all_data_with_demo$my.decision=="Peace",]$seconds_on_page),1), "seconds and average time for War decisions is", round(mean(pw_all_data_with_demo[pw_all_data_with_demo$my.decision=="War",]$seconds_on_page),1),"seconds. "))

print(wilcox.test(pw_all_data_with_demo[pw_all_data_with_demo$my.decision=="War",]$seconds_on_page, pw_all_data_with_demo[pw_all_data_with_demo$my.decision=="Peace",]$seconds_on_page))
print("RESULT: There is a statistically significant difference between the time to make peace vs time to make war. ")

#--------------------PW - Counterbalancing Data Analysis-----------------
print("       ---PW: COUNTERBALANCING: Is P-W PLAY ORDER related to number of peace propensity (across all adversaries)--")
print("         ----PW: counterbalancing ROUND ONE choices by PW play order")
# mean(pw_all_data_with_demo[pw_all_data_with_demo$id %in% pw_varied_round1[pw_varied_round1$Varied,]$id,]$pw_order)
# print("RESULT: 40% of the participants that varied their choices played PW first and 60% played pw_second, so counterbalancing did not appear to affect Round 1 choices.")
print(chisq.test(xtabs(~my.decision+pw_order, data=pw_all_data_with_demo[pw_all_data_with_demo$period==1,])))
m <- glmer(my.decision ~ pw_order + (1|id), data=pw_all_data_with_demo[pw_all_data_with_demo$period==1,], family=binomial(link="logit"))
print(Anova(m))
print("RESULT: Both a chisq test and a RMLR on ROUND ONE decisions and pw_order DID NOT show a statistically significant difference at p <.05.")


print("         ----PW: counterbalancing STRATEGIC (all round) choices by PW play order")
print(xtabs(~ pw_order + my.decision, data = pw_all_data_with_demo))
print(chisq.test(xtabs(~ pw_order + my.decision, data = pw_all_data_with_demo)))
print("RESULT: A Chi-Sq test on  pw_order and aggregate number of Peace/War choices showed A STATISTICALLY SIGNIFICANT relationship at p <.05, but...")
n_1<-(xtabs(~ id+ my.decision, data = pw_all_data_with_demo[pw_all_data_with_demo$pw_order==1,]))
n_2<-(xtabs(~ id+ my.decision, data = pw_all_data_with_demo[pw_all_data_with_demo$pw_order==2,]))
n_1<- n_1[,1]/30
n_2 <- n_2[,1]/30
# n<-wilcox.test(n_1,n_2, paired=FALSE)
# n["data.name"] <- "Played PW First and Played PW Second"
# print(n)
# print("RESULT: A Wilcox test on Order of Play (first or second) and total 'peacefulness' (number of Peace choices) did NOT SHOW A STATISTICALLY SIGNIFICANT result at p <.05")
# print("HELP: Which test (chisq on aggregate or wilcox on 'by-id') is more appropriate? Probably the latter")
m <- glmer(my.decision ~ pw_order + (1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
print(Anova(m))
print("RESULT: A RMLR on STRATEGIC decisions and pw_order DID NOT show a statistically significant difference at p <.05.")


n_1<-as.data.frame(xtabs(~ id+Adversary+my.decision, data = pw_all_data_with_demo[pw_all_data_with_demo$pw_order==1,])[,,1])
n_2<-as.data.frame(xtabs(~ id+Adversary+my.decision, data = pw_all_data_with_demo[pw_all_data_with_demo$pw_order==2,])[,,1])
p<- ggplot(n_1, aes(x=Freq))+
  geom_density(data=n_1[n_1$Adversary=="Human",],fill="red", color="red",alpha=0.2, linetype="dashed")+
  geom_density(data=n_1[n_1$Adversary=="AI",],fill="blue", color="blue",alpha=0.2, linetype="dashed")+
  geom_density(data=n_1[n_1$Adversary=="Human+AI",],fill="green", color="green", alpha=0.2, linetype="dashed")+
  geom_density(data=n_2[n_2$Adversary=="Human",],fill="red", color="red",alpha=0.3)+
  geom_density(data=n_2[n_2$Adversary=="AI",],fill="blue", color="blue",alpha=0.3)+
  geom_density(data=n_2[n_2$Adversary=="Human+AI",],fill="green", color="green", alpha=0.3) + labs(title="PW-Peace Choices Distribution (by PW Play order & Adversary)",x="Frequency", y = "Density of Choices", color = "Adversary") +scale_x_discrete(limits=c(0:10))+ theme(plot.title = element_text(size=8))
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
# print(p)
print("RESULT: See PLOT: A visual inspection of the plot shows the AI (blue) means are similar,the human (red) means are similar, but the AI-assisted human means are very different between those who played PW first and those who played RPS first, so,,,,")


print("    -- PW: Aggregate Peace choices by adversary and pw_order")
print(xtabs(~ Adversary + pw_order + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + pw_order + my.decision, data = pw_all_data_with_demo)[,,1]))
print("RESULT: A Chi-Sq test of Peace choices by Adversary and pw_order DID NOT show a statistically significant relationship at p <.05. ")


print(chisq.test(xtabs(~ Adversary + my.decision, data = pw_all_data_with_demo[pw_all_data_with_demo$pw_order==1,]))) 
print("RESULT: A Chi-Sq test of Peace choices by Adversary for those who PLAYED PW FIRST DID SHOW a statistically significant relationship at p <. 05.")
print(chisq.test(xtabs(~ Adversary + my.decision, data = pw_all_data_with_demo[pw_all_data_with_demo$pw_order==2,]))) 
print("RESULT: A Chi-Sq test of Peace choices by Adversary for those who PLAYED PW SECOND did NOT show a statistically significant relationship at p <. 05.")
print("RESULT: Participants who played PW first showed a statistically significant difference in warlike-ness by adversary (as expected).  HOWEVER, participants who played PW second DID NOT show a statistically significant difference in Peace choices by adversary, even through the adversaries' gameplay varied significantly and the same as those who played pw first .        NOTE: Was there something about playing RPS first that made participants more warlike toward the Human+AI adversary?")
print("RESULt: does this mean there was some conditioning factor in the RPS game?")


# m <- glmer(my.decision ~ Adversary+ pw_order + (1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
# # print(summary(m))
# print(Anova(m))
# print("CANT USE THIS BECAUSE BINOMIAL RESULT: A RMLR on my.decision by adversary and pw_order shows the relationship between Counterbalancing order and decision by adversary is NOT statistically significant at p < .05")





#--------------------PW - Demographic <-> Decision-making Data Analysis-----------------
print("--------------- PW<->Demo Analysis--------------- ")
print("")
# print("HELP: should I use the repeated measures logistic regression with all the demographic variables included, like this:")

# NOTE: these three are used because age/school/years of miltiary experience are highly correlated
# m <- glmer(my.decision ~ gender + service + (1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
# print(Anova(m))
# m <- glmer(my.decision ~ rank +years_military_experience+ (1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
# print(Anova(m))
# m <- glmer(my.decision ~ age +school+ (1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
# print(Anova(m))
# m <- glmer(my.decision ~ machine_learning_experience+game_theory_experience  + (1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
# print(Anova(m))


print(chisq.test(xtabs(~ gender + my.decision, data = pw_all_data_with_demo)))
print(chisq.test(xtabs(~ Adversary + gender + my.decision, data = pw_all_data_with_demo)[,,1]))
print("RESULT: Gender was not statistically related to cooperativeness in general or by adversary was NOT statistically significant at p < .05 ")



# print(xtabs(~ Adversary + service + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ service + my.decision, data = pw_all_data_with_demo)))
print(chisq.test(xtabs(~ Adversary + service + my.decision, data = pw_all_data_with_demo)[,,1]))
print("RESULT: SErvice was not statistically related to cooperativeness in general or by adversary at p < .05 ")


# print(xtabs(~ Adversary + rank + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ rank + my.decision, data = pw_all_data_with_demo)))
print("Chi-sq (aggregate) on whether RANK affected decision-making, in general was NOT statistically significant at p < .05 ")
print(chisq.test(xtabs(~ Adversary + rank + my.decision, data = pw_all_data_with_demo)[,,1]))
print("Chi-sq (aggregate) on whether RANK affected decision-making, by adversary was NOT statistically significant at p < .05 ")


print(l<-chisq.test(xtabs(~ age + my.decision, data = pw_all_data_with_demo)))
print(l<-chisq.test(xtabs(~ Adversary+age + my.decision, data = pw_all_data_with_demo)[,,1]))
print("RESULT: Age was related to overall cooperativeness (X-squared = 57.02, df = 11, p-value = 3.3e-08) but not variation by adversary (X-squared = 13.283, df = 22, p-value = 0.925)")
# m <- glmer(my.decision ~  age + (1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
# print(Anova(m))
# print("RMLR  on whether AGE affected decision-making, in general was NOT statistically significant at p < .05 ")
# m <- glmer(my.decision ~  age + Adversary + age:Adversary+ (1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
# print(Anova(m))
# print("RMLR  on whether AGE affected decision-making, by adversary was NOT statistically significant at p < .05 ")
print(l<-chisq.test(xtabs(~ years_military_experience + my.decision, data = pw_all_data_with_demo)))
print(l<-chisq.test(xtabs(~ Adversary+years_military_experience+ my.decision, data = pw_all_data_with_demo)[,,1]))
print("RESULT: YEars of military experience was related to overall cooperativeness (X-squared = 57.02, df = 11, p-value = 3.3e-08) but not variation by adversary (X-squared = 13.283, df = 22, p-value = 0.925)")

# m <- glmer(my.decision ~  years_military_experience+(1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
# print(Anova(m))
# print("RMLR  on whether YEARS OF MILITARY EXPERIENCE affected cooperation was NOT statistically significant at p < .05")

# m <- glmer(my.decision ~  years_military_experience+ Adversary +years_military_experience:Adversary+(1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
# print(Anova(m))
# print("RMLR  on whether YEARS OF MILITARY EXPERIENCE affected cooperation was NOT statistically significant at p < .05")
# print("Chi-sq (aggregate) on whether SCHOOL affected decision-making, by adversary")
# print(xtabs(~ Adversary + school + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~  school + my.decision, data = pw_all_data_with_demo)))
print(chisq.test(xtabs(~ Adversary + school + my.decision, data = pw_all_data_with_demo)[,,1]))
print("RESULT: Chi-sq (aggregate) shows cooperativeness is related to school, but not to variation by adversary at p < .05 ")
print(xtabs(~  school + my.decision, data = pw_all_data_with_demo)[,1]/(rowSums(xtabs(~  school + my.decision, data = pw_all_data_with_demo))))
print("RESULT: So students from ACSC & AWC were equally cooperative, but SAASS was less cooperative.")


# print("Chi-sq (aggregate) on whether GAME THEORY EXPERIENCE  affected decision-making, by adversary")
# print(xtabs(~ Adversary + game_theory_experience + my.decision, data = pw_all_data_with_demo)[,,1])
# print(chisq.test(xtabs(~ Adversary + game_theory_experience + my.decision, data = pw_all_data_with_demo)[,,1])) #not significant
print(chisq.test(xtabs(~game_theory_experience + my.decision, data=pw_all_data_with_demo)))
print(chisq.test(xtabs(~Adversary+game_theory_experience + my.decision, data=pw_all_data_with_demo)[,,1]))
print("RESULT: Chisq tests show game theory experience was statistically related to cooperativeness in general (X-squared = 16.155, df = 2, p-value = 0.0003105) but not to variation in cooperativeness by adversary (X-squared = 2.0875, df = 4, p-value = 0.7197)")
# 
# m <- glmer(my.decision ~  game_theory_experience +  (1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
# print(Anova(m))
# print("RMLR on whether GAME THEORY EXPERIENCE affected cooperation was NOT statistically significant at p < .05")
# m <- glmer(my.decision ~  game_theory_experience + Adversary + game_theory_experience:Adversary+ (1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
# print(Anova(m))
# print("RMLR on whether GAME THEORY EXPERIENCE affected cooperation, by adversary was NOT statistically significant at p < .05")
# print("HELP: How to test this correctly?")


print(n<-chisq.test(xtabs(~ machine_learning_experience + my.decision, data = pw_all_data_with_demo))) #not significant
print(chisq.test(xtabs(~ Adversary + machine_learning_experience + my.decision, data = pw_all_data_with_demo)[,,1])) #not significant
print("RESULT:Chisq on AI EXPERIENCE  was related to cooperativeness in general (X-squared = 18.207, df = 2, p-value = 0.0001113) but not to variation by adversary (X-squared = 3.3287, df = 4, p-value = 0.5044)")
# n<- xtabs(~ Adversary + machine_learning_experience + my.decision, data = pw_all_data_with_demo)[,,1]
# print(friedman.test(Freq~machine_learning_experience|Adversary, data=as.data.frame(n)))
# print("RESULT: Friedman test on aggregate choices as a function of machine learning experience blocked by adversary showed a statistically significant result at p <.05")
# m <- glmer(my.decision ~  machine_learning_experience+ (1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
# print(Anova(m))
# print("RMLR on whether MACHINE LEARNING EXPERIENCE affected decision-making, by adversary was a finding of statistical interest (p < .10) but not statistically significant at p < .05.")
# m <- glmer(my.decision ~  machine_learning_experience+Adversary+machine_learning_experience:Adversary+ (1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
# print(Anova(m))
# print("RMLR on whether MACHINE LEARNING EXPERIENCE affected decision-making.")















#------------********RPS analyses********----------------------------------
print("---------------------------------RPS analysis----------------------------------")
print(rps_sum <- xtabs(~Adversary+Choice_of_Advisor, data=rps_all_data_with_demo))
print(rps_prop <- round(prop.table(rps_sum)*100, 1))
#--------------------RPS - Initial Data characterization-----------------


print("--RPS: Normality tests (by advisor type)--")

rps_df_1<- as.data.frame(xtabs(~Choice_of_Advisor + Adversary+id, data=rps_all_data_with_demo))
# rps_df_1$pois <- rpois(nrow(rps_df_1), lambda=mean(rps_df_1$Freq))
p<-ggplot(data=rps_df_1,aes(x=Freq, group=Choice_of_Advisor, color=Choice_of_Advisor,fill=Choice_of_Advisor)) + 
  #geom_bar(aes(y = ..count../sum(..count..)), alpha = 0.5) +
  geom_density(alpha=0.5)+#, stat="count")+#how to stop the exponential at zero?
  #+ geom_line(aes(y=rps_df_1$pois))
  labs(title="RPS - Choice Frequency Density Plot", x="Frequency", y = "Density of Choices", color = "Choice_of_Advisor") +scale_x_discrete(limits=c(0:10))+ theme(plot.title = element_text(size=12))
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
print("RESULT: See PLOT: What kind of distributions are these?")

print(shapiro.test(rps_df_1[rps_df_1$Choice_of_Advisor=="human",]$Freq)) #not normal
print(shapiro.test(rps_df_1[rps_df_1$Choice_of_Advisor=="AI",]$Freq))  #not normal
print(shapiro.test(rps_df_1[rps_df_1$Choice_of_Advisor=="none",]$Freq)) #not normal




# p<- ggplot(rps_df_1[rps_df_1$Adversary=="Human",], aes(Freq, fill=Choice_of_Advisor))+
#   geom_density(alpha=0.4)+
#   labs(title="RPS- vs Human - Choice Frequency Distribution", x="Frequency", y = "Density of Choices", color = "Choice_of_Advisor") +scale_x_discrete(limits=c(0:10))+ theme(plot.title = element_text(size=10))
# print(p)
# q <- ggplot(rps_df_1[rps_df_1$Adversary=="Human+AI",], aes(Freq, fill=Choice_of_Advisor))+
#   geom_density(alpha=0.4)+
#   labs(title="RPS- vs AI-Assisted Human - Choice Frequency Distribution", x="Frequency", y = "Density of Choices", color = "Choice_of_Advisor") +scale_x_discrete(limits=c(0:10))+ theme(plot.title = element_text(size=10))
# print(q)
# r<- ggplot(rps_df_1[rps_df_1$Adversary=="AI",], aes(Freq, fill=Choice_of_Advisor))+
#   geom_density(alpha=0.4)+
#   labs(title="RPS- vs AI - Choice Frequency Distribution", x="Frequency", y = "Density of Choices", color = "Choice_of_Advisor") +scale_x_discrete(limits=c(0:10))+ theme(plot.title = element_text(size=10))
# print(r)
# print(paste0("Insert ", p$labels$title," Plot"))
# ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
# print("RESULT: See PLOTs: What kind of distribution are these?")
#--------------------RPS - Aggregation of Choice of Advisor (all) - Data Analysis-------------
print("---RPS: Choice of Advisor (regardless of Adversary)") #does the group show a propensity to choose one advisor over the others (or two over the third)? 
n<- xtabs(~Choice_of_Advisor, data = rps_all_data_with_demo)
print(n)
print("This summary show that participants, in aggregate, chose AI advice over human advice, but chose 'no advice' over any advice.")

m<- as.data.frame(n)
m$Freq <- (m$Freq/sum(m$Freq))*100
p<- ggplot(m, aes("Choice of Advisor", y=Freq, fill = Choice_of_Advisor)) +
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Freq), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "RPS-Choice of Advisor (Aggregate)") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")

print(chisq.test(n))
print("RESULT: Chisq test showed significant difference a p < .001 [p <.05]")
print(multinomial.test(as.vector(n)))
print("RESULT: Multinomial test showed significant difference a p < .001 [p <.05]")
print("IMPLICATION: Participants chose advice significantly differently advice ")
descdist(rps_df_1[rps_df_1$Choice_of_Advisor=="human",]$Freq, discrete = TRUE)
plot(fitdist(rps_df_1[rps_df_1$Choice_of_Advisor=="human",]$Freq, "nbinom"))
plot(fitdist(rps_df_1[rps_df_1$Choice_of_Advisor=="human",]$Freq, "pois"))
descdist(rps_df_1[rps_df_1$Choice_of_Advisor=="AI",]$Freq, discrete = FALSE)
plot(fitdist(rps_df_1[rps_df_1$Choice_of_Advisor=="AI",]$Freq, "nbinom"))
descdist(rps_df_1[rps_df_1$Choice_of_Advisor=="none",]$Freq, discrete = FALSE)
plot(fitdist(rps_df_1[rps_df_1$Choice_of_Advisor=="none",]$Freq, "norm"))

#--------------------RPS - Aggregation of Choice of Advisor by Adversary - Data Analysis-------------
#video #9 (tests of proportions)
print("---RPS: Aggregate: Tests of proportions on AGGREGATE choices (by Adversary")
print(df <- xtabs(~ Adversary + Choice_of_Advisor, data=rps_all_data_with_demo))
# ggplot(as.data.frame(xtabs(~id + Adversary + Choice_of_Advisor, data = rps_all_data_with_demo)), aes(x=Adversary, y=Freq, color=Choice_of_Advisor)) +
#   geom_boxplot()
print(chisq.test(xtabs(~ Adversary + Choice_of_Advisor, data=rps_all_data_with_demo))) 
# GTest(xtabs(~ Adversary + Choice_of_Advisor, data=rps_all_data_with_demo)) #more accurate than Chisq?
# fisher.test(xtabs(~ Adversary + Choice_of_Advisor, data=rps_all_data_with_demo))#is exact test more appropriate than chisq.test here
print("RESULT: A Chi-sq test shows advisor choice is statistically independent of Adversary type at p < .05.")



# print("RPS: All: Friedman test of variance in choice of advisor by adversary")
# rps_long_by_id <- as.data.frame(xtabs(~id+Adversary+Choice_of_Advisor, data=rps_all_data_with_demo))
# print(rps_sum)
# print(friedman.test(rps_sum)) # need to check if this needs to be transposed (t(rps_sum))
# 
# friedman.test(Freq ~ Adversary|id, data=rps_long_by_id[rps_long_by_id$Choice_of_Advisor=="none",])
# # am I doing this right? or should it be like this: friedman.test(Freq ~ Choice_of_Advisor|id, data=rps_long_by_id[rps_long_by_id$Adversary=="AI",])
# friedman.test(Freq ~ Adversary|id, data=rps_long_by_id[rps_long_by_id$Choice_of_Advisor=="AI",])
# friedman.test(Freq ~ Adversary|id, data=rps_long_by_id[rps_long_by_id$Choice_of_Advisor=="human",])
# print("RESULT: A 3-factor Friedman Test showed no statistically significant variation on the summed Frequency of choices caused by differences in adversary, at p < .05")



# "Chi Squared for individual participants -- not valid because some expected values are < 5"
#using fisher test instead)
# print ("RPS: Indidividual ID's which showed difference in choices by Adversary using Fisher Test: (since Chisq invalid with some expected value cells=0?")
# rps_ids <- unique(rps_all_data_with_demo$id)
# rps_chi_test <- data.frame("id"=rps_ids,"All"=NA)
# #"fisher test p-values
# rps_array <- xtabs(~Adversary + Choice_of_Advisor + id, data=rps_all_data_with_demo)
# for (i in rps_ids){
#   rps_chi_test[rps_chi_test$id==i,"All"] <- chisq.test(rps_array[,,i])["p.value"]<0.05
# }
# d <- rps_chi_test
# d[is.na(d$All),]$All <- FALSE
# d<- d[d$All=="TRUE",]$id
# print(paste0("RESULT: Chisq tests on individual sum choices (across rounds) showed ", length(d), " participant(s) (",paste(d, collapse=", "),") showed a statistically significant variation in their advisor choices by adversary, at p < .05"))

rps_ids <- unique(rps_all_data_with_demo$id)
rps_fisher_test <- data.frame("id"=rps_ids,"All"=NA)
#"fisher test p-values
rps_array <- xtabs(~Adversary + Choice_of_Advisor + id, data=rps_all_data_with_demo)
for (i in rps_ids){
  rps_fisher_test[rps_fisher_test$id==i,"All"] <- fisher.test(rps_array[,,i])["p.value"]<0.05
}
d<- rps_fisher_test[rps_fisher_test$All=="TRUE",]$id
print(paste0("RESULT: A Fisher test of individual sum choices (across rounds) showed ", round(100*length(d)/length(rps_ids),1), "% of participant(s) (",paste(d, collapse=", "),") showed a statistically significant variation in their advisor choices by adversary, at p < .05"))

# print ("RPS ID's which showed difference in choices by Adversary using G-Test")
# rps_ids <- unique(rps_all_data_with_demo$id)
# rps_G_test <- data.frame("id"=rps_ids,"All"=NA)
# #"fisher test p-values
# rps_array <- xtabs(~Adversary + Choice_of_Advisor + id, data=rps_all_data_with_demo)
# for (i in rps_ids){
#   rps_G_test[rps_G_test$id==i,"All"] <- GTest(rps_array[,,i])["p.value"]<0.05
# }
# f<-rps_G_test[rps_G_test$All=="TRUE",]$id
# print(paste0("RESULT: A G-test of individual sum choices (across rounds) showed ", length(f), " participant(s) (",paste(f, collapse=", "),") showed a statistically significant variation in their advisor choices by adversary, at p < .05"))
# intersect(d,f)
# 
# print ("RPS ID's which showed difference in choices by Adversary using Friedman Test")
# rps_ids <- unique(rps_all_data_with_demo$id)
# rps_friedman_test <- data.frame("id"=rps_ids,"All"=NA)
# #"fisher test p-values
# rps_array <- xtabs(~Adversary + Choice_of_Advisor + id, data=rps_all_data_with_demo)
# for (i in rps_ids){
#   rps_friedman_test[rps_friedman_test$id==i,"All"] <- friedman.test(t(rps_array[,,i]))["p.value"]<0.05
#   # print(paste(i,friedman.test(t(rps_array[,,i]))["p.value"]))
# }
# e<-rps_friedman_test[rps_friedman_test$All=="TRUE",]$id
# print(paste0("RESULT: A Friedman test of individual sum choices (across rounds) showed ", length(e), " participant(s) (",paste(e, collapse=", "),") showed a statistically significant variation in their advisor choices by adversary, at p < .05"))
# 
# print("HELP: It seems odd that the fisher test showed 7 participants, the friedman test showed 9, and none of the participants are the same. Why are these results mutually exclusive?")
# intersect(d,e)

#--------------------RPS - Decisions by Round - Data Analysis-----------------

print("RPS: Choice of Advisor by round (all Adversaries)")
df <- as.data.frame(xtabs(~Round + Choice_of_Advisor, data=rps_all_data_with_demo))
p<- ggplot(data=df, aes(x=Round, y=Freq, group=Choice_of_Advisor))+  
  geom_line(aes(color=Choice_of_Advisor))+  
  geom_point(aes(group=Choice_of_Advisor, color=Choice_of_Advisor)) + 
  labs(title="RPS - Choice of Advisor by Round (all Adversaries)",x="Round", y = "# of Choices", color = "Advisor")+
  geom_smooth(method='glm',formula=y~x, aes(group=Choice_of_Advisor, color=Choice_of_Advisor))
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
print("RESULT: See PLOT: Visual inspection shows advice-taking seems to increase over time (both human and AI), and self-reliance ('none' choice) declined over time.")


print(chisq.test(xtabs(~ Round + Choice_of_Advisor, data = rps_all_data_with_demo))) 
print("RESULT: Chisq test showed variation in choice of advisor by round")

# print(friedman.test(Freq~Choice_of_Advisor|Round, data=df))
# print("Is this (print(friedman.test(Freq~Choice_of_Advisor|Round, data=df)) the right way to see if Choice_of_Advisor varies by round?")
# print("Or should it be: print(friedman.test(Freq~Round|Choice_of_Advisor, data=df))")

#--------------------CUT...RPS - Choice of Advisor by adversary and round---------

print("   -------RPS: Choice of Advisor by adversary and round")





df_1 <- as.data.frame(xtabs(~Round + Choice_of_Advisor+Adversary, data=rps_all_data_with_demo))
p<- ggplot(data=df_1[df_1$Adversary=="Human",], aes(x=Round, y=Freq, group=Choice_of_Advisor))+  
  geom_line(aes(color=Choice_of_Advisor))+  
  geom_point(aes(group=Choice_of_Advisor, color=Choice_of_Advisor))+ 
  labs(title="RPS- vs Human - Choices by Round",x="Round", y = "# of Choices", color = "Advisor")+
  geom_smooth(method='glm',formula=y~x,aes(group=Choice_of_Advisor, color=Choice_of_Advisor))
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
print("RESULT: See PLOT: Against the Human, human Advice-taking stayed approximately stable, but self-reliance('none') advice declined and was replaced by AI advisor.")




p<- ggplot(data=df_1[df_1$Adversary=="AI",], aes(x=Round, y=Freq, group=Choice_of_Advisor))+  
  geom_line(aes(color=Choice_of_Advisor))+  
  geom_point(aes(group=Choice_of_Advisor, color=Choice_of_Advisor))+ 
  labs(title="RPS- vs AI - Choices by Round",x="Round", y = "# of Choices", color = "Advisor") +geom_smooth(method='glm',formula=y~x,aes(group=Choice_of_Advisor, color=Choice_of_Advisor))
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
print("RESULT: See PLOT: Against the AI, AI Advice-taking stayed approximately stable, but self-reliance('none') advice declined and was replaced by human advisor")

p<- ggplot(data=df_1[df_1$Adversary=="Human+AI",], aes(x=Round, y=Freq, group=Choice_of_Advisor))+  
  geom_line(aes(color=Choice_of_Advisor))+  
  geom_point(aes(group=Choice_of_Advisor, color=Choice_of_Advisor)) + 
  labs(title="RPS- vs Human+AI - Choices by Round",x="Round", y = "# of Choices", color = "Advisor")+
  geom_smooth(method='glm',formula=y~x,aes(group=Choice_of_Advisor, color=Choice_of_Advisor))
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
print("RESULT: See PLOT: Against the AI-assisted Human, Advice-taking from AI and human advisors increased across rounds at roughly the same rate, and self-reliance('none') declined markedly ")

print("\nRESULT: A Visual analysis of AGGREGATE choice data by adversary across rounds showed: \n1) Against the adversaries with AI (AI and AI-assisted human), the aggregate use of the human advisor went up across rounds, but against the human adversary, use of a human advisor was low and stayed low.\n2) more here...  ")

p<- ggplot(data=df_1[df_1$Choice_of_Advisor=="none",], aes(x=Round, y=Freq, group=Adversary))+  
  geom_line(aes(color=Adversary))+  
  #geom_point(aes(group=Adversary, color=Adversary)) + 
  labs(title="RPS-Choices of 'no advice' by Round",x="Round", y = "# of Choices", color = "Adversary")+
  geom_smooth(method='auto',formula=y~x,aes(group=Adversary, color=Adversary))
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")

p<- ggplot(data=df_1[df_1$Choice_of_Advisor=="AI",], aes(x=Round, y=Freq, group=Adversary))+  
  geom_line(aes(color=Adversary))+  
  #geom_point(aes(group=Adversary, color=Adversary)) + 
  labs(title="RPS-Choices of 'AI advisor' by Round",x="Round", y = "# of Choices", color = "Adversary")+
  geom_smooth(method='auto',formula=y~x,aes(group=Adversary, color=Adversary))
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")

p<- ggplot(data=df_1[df_1$Choice_of_Advisor=="human",], aes(x=Round, y=Freq, group=Adversary))+  
  geom_line(aes(color=Adversary))+  
  #geom_point(aes(group=Adversary, color=Adversary)) + 
  labs(title="RPS-Choices of 'human' by Round",x="Round", y = "# of Choices", color = "Adversary")+
  geom_smooth(method='auto',formula=y~x,aes(group=Adversary, color=Adversary))
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")

# n<-as.data.frame(xtabs( ~Choice_of_Advisor+ Adversary+Round,data=rps_all_data_with_demo))
# print(chisq.test(n))

print("Should it be this: print(mantelhaen.test(xtabs(~Adversary+Choice_of_Advisor+Round, data=rps_all_data_with_demo))) or this:")
print(mantelhaen.test(xtabs(~Round+Choice_of_Advisor+Adversary, data=rps_all_data_with_demo)))
print("RESULT: A CMH test showed a statistically significant dependence between choice of advisor, adversary, and round.")



# m <- glmer(Choice_of_Advisor ~ Adversary+Round+Adversary*Round+(1|id)+(1|rps_order), data=rps_all_data_with_demo, family=binomial)
# print(summary(m))
# print(Anova(m, type="3"))
print("HELP: How do I code to determine whether Choice_of_Advisor varied across rounds, by Adversary? (i.e.compare (the slope of change across rounds) across adversaries")
print("HELP: How do I do a multinomial version of this (see code) - it keeps excluding 'AI' adversary")



# print("---RPS: Aggregate: Tests of proportions on choices by Adversary AND ROUND ---")
# print("RPS: RMLR:")
# print("HELP: Am I doing the comparison of whether Adversary causes variation in the change across rounds?")
# print("human-AI choice comparison")
# print(Anova(glmer(Choice_of_Advisor ~ Adversary + Round + (1|id), data=rps_all_data_with_demo[!rps_all_data_with_demo$Choice_of_Advisor=="none",], family=binomial), type="3"))
# print("none-AI choice comparison")
# print(Anova(glmer(Choice_of_Advisor ~ Adversary + Round + (1|id), data=rps_all_data_with_demo[!rps_all_data_with_demo$Choice_of_Advisor=="human",], family=binomial), type="3"))
# print("human-none choice comparison")
# print(Anova(glmer(Choice_of_Advisor ~ Adversary + Round + (1|id), data=rps_all_data_with_demo[!rps_all_data_with_demo$Choice_of_Advisor=="AI",], family=binomial), type="3"))
# print("??RESULT: A multiple RMLR (should use multivariate repeated measures logistic regression if possible), showed Adversary did not cause a change across rounds, at p <.05.??")
# 
# print("---code above here RMLR needs to be moved lower...---")
#--------------------RPS - Switch-Loss Analysis----------------
switch_array <- rps_array
rps_switch_df <- as.data.frame(rps_array)

rps_switch_df$Win_Stay <- 0
rps_switch_df$Win_Switch <- 0
rps_switch_df$Loss_Stay <- 0
rps_switch_df$Loss_Switch <- 0
rps_switch_df$Draw_Stay <- 0
rps_switch_df$Draw_Switch <- 0

for (i in rps_ids) {
  for (j in list("Human","AI","Human+AI")) {
    for (k in c(2:10)){
      prev_choice <- rps_long_ten_rounds[rps_long_ten_rounds$id==i & rps_long_ten_rounds$Adversary==j& rps_long_ten_rounds$Round==(k-1),]$Choice_of_Advisor
      current_choice <- rps_long_ten_rounds[rps_long_ten_rounds$id==i & rps_long_ten_rounds$Adversary==j& rps_long_ten_rounds$Round==k,]$Choice_of_Advisor
      prev_payoff <- rps_long_ten_rounds[rps_long_ten_rounds$id==i & rps_long_ten_rounds$Adversary==j&rps_long_ten_rounds$Round==(k-1),]$Payoff
      prev_adv <- rps_long_ten_rounds[rps_long_ten_rounds$id==i & rps_long_ten_rounds$Adversary==j&rps_long_ten_rounds$Round==(k-1),]$Adversary
      # rps_switch_df$prev_choice <- prev_choice
      if ((prev_choice != current_choice) & (prev_payoff==-1)){ #if previous round's choice and current round's choice are different, and previous payoff is -1
        rps_switch_df[rps_switch_df$id==i & rps_switch_df$Adversary==prev_adv & rps_switch_df$Choice_of_Advisor==prev_choice,]$Loss_Switch <- 1 + rps_switch_df[rps_switch_df$id==i & rps_switch_df$Adversary==prev_adv & rps_switch_df$Choice_of_Advisor==prev_choice,]$Loss_Switch  #log a counter as a "switch"
      } else if ((prev_choice != current_choice) & (prev_payoff==0)){
        rps_switch_df[rps_switch_df$id==i & rps_switch_df$Adversary==prev_adv & rps_switch_df$Choice_of_Advisor==prev_choice,]$Draw_Switch <- 1 + rps_switch_df[rps_switch_df$id==i & rps_switch_df$Adversary==prev_adv & rps_switch_df$Choice_of_Advisor==prev_choice,]$Draw_Switch
      } else if ((prev_choice != current_choice) & (prev_payoff==1)){
        rps_switch_df[rps_switch_df$id==i & rps_switch_df$Adversary==prev_adv & rps_switch_df$Choice_of_Advisor==prev_choice,]$Win_Switch <- 1 + rps_switch_df[rps_switch_df$id==i & rps_switch_df$Adversary==prev_adv & rps_switch_df$Choice_of_Advisor==prev_choice,]$Win_Switch
      } else if ((prev_choice == current_choice) & (prev_payoff==-1)) {
        rps_switch_df[rps_switch_df$id==i & rps_switch_df$Adversary==prev_adv & rps_switch_df$Choice_of_Advisor==prev_choice,]$Loss_Stay <- 1 + rps_switch_df[rps_switch_df$id==i & rps_switch_df$Adversary==prev_adv & rps_switch_df$Choice_of_Advisor==prev_choice,]$Loss_Stay
      } else if ((prev_choice == current_choice) & (prev_payoff==0)){
        rps_switch_df[rps_switch_df$id==i & rps_switch_df$Adversary==prev_adv & rps_switch_df$Choice_of_Advisor==prev_choice,]$Draw_Stay <- 1 + rps_switch_df[rps_switch_df$id==i & rps_switch_df$Adversary==prev_adv & rps_switch_df$Choice_of_Advisor==prev_choice,]$Draw_Stay
      } else if ((prev_choice == current_choice) & (prev_payoff==1)){
        rps_switch_df[rps_switch_df$id==i & rps_switch_df$Adversary==prev_adv & rps_switch_df$Choice_of_Advisor==prev_choice,]$Win_Stay <- 1 + rps_switch_df[rps_switch_df$id==i & rps_switch_df$Adversary==prev_adv & rps_switch_df$Choice_of_Advisor==prev_choice,]$Win_Stay
      }
    }
  }
}
names(rps_switch_df)[1] <- "prev_adversary"
names(rps_switch_df)[2] <- "prev_choice"
switchnames <- c("Win_Stay","Win_Switch","Loss_Stay","Loss_Switch","Draw_Stay","Draw_Switch")
rps_switch_df_melt <- melt(rps_switch_df, id=c("prev_adversary","prev_choice","id"))
rps_switch_df_melt_sum <- melt(rps_switch_df, c("prev_adversary","prev_choice",switchnames))
# rps_switch_df_sum <- as.data.frame(rps_switch_df_melt_sum %>%
#   group_by(prev_adversary, prev_choice) %>%
#   summarise (sum(value))) 
# names(rps_switch_df_sum)[3] <- "value"


rps_switch_array <- array(NA, dim=c(3,3,6), dimnames=list(c("AI","Human","Human+AI"),c("AI","human","none"),switchnames))
rps_switch_table_adv <- matrix(NA, nrow=3, ncol=length(switchnames), dimnames=list(c("AI","human","none"),switchnames))
rps_switch_table_adversary <- matrix(NA, nrow=3, ncol=length(switchnames), dimnames=list(c("AI","Human","Human+AI"),switchnames))
for (i in switchnames) {
  
  f <- as.formula(paste0(i, "~prev_adversary+prev_choice"))
  rps_switch_array[,,i] <-xtabs(f, data= rps_switch_df)
  f1 <- as.formula(paste0(i, "~prev_choice"))
  rps_switch_table_adv[,i] <- xtabs(f1, data= rps_switch_df)
  f2 <- as.formula(paste0(i, "~prev_adversary"))
  rps_switch_table_adversary[,i] <- xtabs(f2, data= rps_switch_df)
}
rps_switch_table <- matrix(NA, nrow=3, ncol=2, dimnames=list(c("AI","human","none"),c("Stay","Switch")))

print(rps_switch_table_adv)
print(chisq.test(rps_switch_table_adv))

print("RESULT: A Chisq.test (X-squared = 31.319552, df = 10, p-value = 0.0005194981) showed a statistically significant difference in whether participants stayed or switched, ")
for (i in seq(1,5,by=2)){
  print(rps_switch_table_adv[,c(i,i+1)])
  print(chisq.test(rps_switch_table_adv[,c(i,i+1)]))#["p.value"])

}
print("REWRITE this section to be more similar to pw_probinference section for fisher test with 'contexts' being # of losses, # of wins, etc, then compare it like this: win-stay count, win context")
a<- rbind(rps_switch_table_adv[,c(1:2)], rps_switch_table_adv[,c(3:4)], rps_switch_table_adv[,c(5:6)])
a[,2] <- a[,1]+a[,2]
fisher.test(a[c(1:3),]) #ns
fisher.test(a[c(4:6),]) #ns
fisher.test(a[c(7:9),]) #ns


for (i in c(1:2)){
rps_switch_table[,i] <- rps_switch_table_adv[,i]+rps_switch_table_adv[,i+2]+rps_switch_table_adv[,i+4]
}
print(rps_switch_table)
print(chisq.test(rps_switch_table))
print("RESULT: chisq test (X-squared = 23.48782, df = 2, p-value = 0.000007937518) showed a statistically signifciant relationship between whether a participant sitched or stayed based on their previous choice of advisor, regardless of a win or loss")


for (i in (1:3)) {
  print(rps_switch_table[-i,])
  print(chisq.test(rps_switch_table[-i,]))#["p.value"])
}
print("post-hoc tests also showed that all previous choices of advisor had an effect on whether the participant switched or stayed.")

for (j in seq(1,5,by=2)){

    print(rps_switch_table_adv[,c(j,j+1)])
  print(chisq.test(rps_switch_table_adv[,c(j,j+1)]))#["p.value"])
}
print("RESULT: post hoc tests show the differences in switching and staying are primarily in the win and loss frames, but that when a participant has a draw, their previous choice of advisor doesn't affect whether they switch or stay.")

for (j in seq(1,5,by=2)){
  for (i in (1:3)) {
    print(rps_switch_table_adv[-i,c(j,j+1)])
    print(chisq.test(rps_switch_table_adv[-i,c(j,j+1)]))#["p.value"])
  }
}
cat("Specifically, post-hoc tests showed: \n-Win Frame:\n--Human-none (X-squared = 9.1331159, df = 1, p-value = 0.002510238)\n--Human-AI (X-squared = 4.2615712, df = 1, p-value = 0.03898385) \n--(but not AI-none) \n-Loss Frame:\n--Human-none (X-squared = 12.650194, df = 1, p-value = 0.0003755267)\n--Human-AI (X-squared = 5.859798, df = 1, p-value = 0.01549057)\n--(but not AI-none)\n-Draw Frame:\n--no relationship.")


#now by adversary
rps_switch_table_advers <- matrix(NA, nrow=3, ncol=2, dimnames=list(c("AI","Human","Human+AI"),c("Stay","Switch")))
for (i in c(1:2)){
  rps_switch_table_advers[,i] <- rps_switch_table_adversary[,i]+rps_switch_table_adversary[,i+2]+rps_switch_table_adversary[,i+4]
}
print(rps_switch_table_advers)
print(chisq.test(rps_switch_table_advers))
print("RESULT: chisq test (X-squared = 7.6698859, df = 2, p-value = 0.02160257) showed a statistically signifciant relationship between whether a participant switched or stayed based on their adversary")
for (i in (1:3)) {
  print(rps_switch_table_advers[-i,])
  print(chisq.test(rps_switch_table_advers[-i,]))#["p.value"])
}
print("RESULT: Post-hoc chisq test (X-squared = 6.3197118, df = 1, p-value = 0.0119403) showed a statistically signifciant relationship between whether a participant sitched or stayed based on their adversary (Human vs Human+AI), and a finding of statistical interest (X-squared = 3.317602, df = 1, p-value = 0.06854174) between AI & Human+AI), but no statistically significant relationship between staying and switching between (Human and AI)")

print(rps_switch_table_adversary)
print(chisq.test(rps_switch_table_adversary))
print("RESULT: chisq test (X-squared = 10.295055, df = 10, p-value = 0.4149997) showed NO statistically signifciant relationship between whether a participant switched or stayed based on their adversary across ")

for (i in seq(1,5,by=2)){
  print(rps_switch_table_adversary[,c(i,i+1)])
  print(chisq.test(rps_switch_table_adversary[,c(i,i+1)]))#["p.value"])
  
}
print("RESULT: Chisq tests by Win, Lose, or Draw do not show that adversary statistically affects switching, but the loss frame shows a finding of stastical interest: (X-squared = 4.6976168, df = 2, p-value = 0.09548287) ")

for (j in seq(1,5,by=2)){
for (i in (1:3)) {
  print(rps_switch_table_adversary[-i,c(j,j+1)])
  print(chisq.test(rps_switch_table_adversary[-i,c(j,j+1)]))#["p.value"])
}
}
print("post-hoc tests also showed non-statistically significant relationship between win/lost/draw switch and stay by adversary, but Human-HAI (Loss frame -> X-squared = 3.8131779, df = 1, p-value = 0.05085088) and the Human-HAI (Win frame -> X-squared = 2.9844016, df = 1, p-value = 0.08407036) were findings of statistical interest.")

#win-loss-draw analyses
a <- as.data.frame(matrix(colSums(rps_switch_table_adv), nrow=3, ncol=2, byrow=TRUE))
colnames(a)  <- c("Stay","Switch")
rownames(a) <- c("Win","Loss","Draw")
print(a)
print(chisq.test(a))
print("RESULT: Chisq test shows no statistically significant relationship between (IV) win/loss/draw and (DV) stay or switch")
#post-hoc
print(chisq.test(a[-3,]))
print(chisq.test(a[-2,]))
print(chisq.test(a[-1,]))
print("RESULT: POst-hoc tests confirm no statistically significant relationship between (IV) win/loss/draw and (DV) stay or switch")

#--------------------RPS - Counterbalancing - Data Analysis-----------------
#check for correlation with counterbalancing

rps_fisher_test$pw_order <- NA
rps_fisher_test$rps_order <- NA
for (i in rps_ids) {
    rps_fisher_test[rps_fisher_test$id==i,"pw_order"] <- unique(rps_all_data_with_demo[rps_all_data_with_demo$id==i,]$pw_order)
    rps_fisher_test[rps_fisher_test$id==i,"rps_order"] <- unique(rps_all_data_with_demo[rps_all_data_with_demo$id==i,]$rps_order)
}


# print(n<-xtabs(~  pw_order+Choice_of_Advisor , data = rps_all_data_with_demo))
# print(l<-chisq.test(n))
# print("Chi-sq test on aggregate shows GAME PLAY ORDER (pw_order) was correlated to differences in aggregate propensities for advisor choice in general, p < .01")
# contrib <- 100*l$residuals^2/l$statistic
# round(contrib, 0)
# corrplot(contrib, is.cor=FALSE)
# print("FURTHERMORE, the chi-sq test shows significance in GAME PLAY ORDER (pw_order) was primarly correlated to choices of AI, where people who played RPS second were significantly MORE LIKELY TO CHOOSE THE AI advisor than those who played it first, at p < .001, but...")
# #mantelhaen only for binary predictor?
print(mantelhaen.test(xtabs(~  Choice_of_Advisor +Adversary+pw_order, data = rps_all_data_with_demo)))
print("RESULT: A CMH/Mantelhaen test on aggregate shows GAME ORDER counterbalancing (pw_order) DID NOT affect decision-making by adversary at p < .05")
# print(fisher.test(xtabs(~All + pw_order, rps_fisher_test)))
# print("RESULT: A fisher test on the participants' pw_order play order and whether they showed a difference by adversary showed there was not a statistically significant relationshop between pw_order and variation in decision-making by adversary, p > .05")

# print(n<-xtabs(~  rps_order+Choice_of_Advisor , data = rps_all_data_with_demo))
# print(l<-chisq.test(n))
# print("Chi-sq test on aggregate shows RPS counterbalancing (rps_order) did affect decision-making IN GENERAL, p < .001, but")
print(mantelhaen.test(xtabs(~  Choice_of_Advisor +Adversary+rps_order, data = rps_all_data_with_demo)))
print("A CMH/Mantelhaen test on aggregate shows RPS counterbalancing (rps_order) DID NOT affect decision-making by adversary at p < .05")
# print(fisher.test(xtabs(~All + rps_order, rps_fisher_test)))
# print("RESULT: A fisher test on the participants' rps_order play order and whether they showed a difference by adversary showed there was not a statistically significant relationshop between pw_order and variation in decision-making by adversary, p > .05")


m <- glmer(Choice_of_Advisor ~ pw_order + rps_order + (1|id), data=rps_all_data_with_demo, family=binomial(link="logit"))
print(Anova(m))
print("RESULT: A generalized linear model shows no statistically significant effect of pw order or rps_order on choice of advisor")



#--------------------RPS - Demographics <-> Choices - Data Analysis-----------------
# n <- demo_relevant_data
# o <- rps_fisher_test[,c("id","All")]
# n <- merge(n,o, by="id")
# f1 <- "All~"
# 
# f2 <- paste0(colnames(n)[!colnames(n) %in% colnames(n[,c("id","All")])], collapse="+")
# f <- as.formula(paste0(f1, f2, "+(1|id)"))
# m <- glmer(f, data=n, family="binomial")
m <- glmer(Choice_of_Advisor ~ machine_learning_experience + game_theory_experience +(1|id), data=rps_all_data_with_demo, family=binomial(link="logit"))
print(Anova(m))
print("RESULT: A generalized linear model shows no statistically significant effect of machine_learning_experience + game_theory_experience on choice of advisor")
m <- glmer(Choice_of_Advisor ~gender+service+(1|id), data=rps_all_data_with_demo, family=binomial(link="logit"))
print(Anova(m))
print("RESULT: A generalized linear model shows no statistically significant effect of gender+service on choice of advisor")

m <- glmer(Choice_of_Advisor ~years_military_experience+age+rank+school+(1|id), data=rps_all_data_with_demo, family=binomial(link="logit"))
print(Anova(m))
print("RESULT: A generalized linear model shows no statistically significant effect of years_military_experience+age+rank+school+on choice of advisor")


# print(n<-xtabs(~  machine_learning_experience+Choice_of_Advisor , data = rps_all_data_with_demo))
# print(l<-chisq.test(n))
# print("Chi-sq test on aggregate shows self-reported AI EXPERIENCE was correlated to decision-making, in general, p < .001")
print(mantelhaen.test(xtabs(~  Choice_of_Advisor +Adversary+machine_learning_experience, data = rps_all_data_with_demo)))
# print(n<-xtabs(~  game_theory_experience+Choice_of_Advisor , data = rps_all_data_with_demo))
# print(chisq.test(n))
# print("Chi-sq test on aggregate shows self-reported Game theory EXPERIENCE did not affect decision-making in general, p > .05")
print(mantelhaen.test(xtabs(~  Choice_of_Advisor +Adversary+game_theory_experience, data = rps_all_data_with_demo)))


print(n<-xtabs(~  Choice_of_Advisor+Adversary+gender , data = rps_all_data_with_demo))
print(mantelhaen.test(n))
print("Chi-sq test on aggregate shows GENDER did not affect decision-making BY ADVERASRY, p < .05")
print(n<-xtabs(~  Choice_of_Advisor+Adversary+age , data = rps_all_data_with_demo))
print(mantelhaen.test(n))
print("CMH test on aggregate shows AGE did not affect decision-making BY ADVERASRY, p < .05")

print(n<-xtabs(~  Choice_of_Advisor+Adversary+years_military_experience , data = rps_all_data_with_demo))
print(mantelhaen.test(n))
print("CMH test on aggregate shows YEARS MIL EXP did not affect decision-making BY ADVERASRY, p < .05")
# print(chisq.test(n))
# print("Chi-sq test on aggregate shows YEARS OF MILITARY EXPERIENCE did affect decision-making in general, p < .01")
# print(n<-xtabs(~  years_military_experience+Choice_of_Advisor+Adversary , data = rps_all_data_with_demo))


print(n<-xtabs(~  Choice_of_Advisor+Adversary+rank , data = rps_all_data_with_demo))
print(mantelhaen.test(n))
print("CMH test on aggregate shows RANK did not affect decision-making BY ADVERASRY, p < .05")

# print(chisq.test(n))
# print("Chi-sq test on aggregate shows RANK did affect decision-making in general, p < .05")
# print(n<-xtabs(~  rank+Choice_of_Advisor+Adversary , data = rps_all_data_with_demo))

print(n<-xtabs(~  Choice_of_Advisor+Adversary+school , data = rps_all_data_with_demo))
# print(chisq.test(n))
# print("Chi-sq test on aggregate shows SCHOOL did affect decision-making in general, p < .01")
# print(n<-xtabs(~  school+Choice_of_Advisor+Adversary , data = rps_all_data_with_demo))
print(mantelhaen.test(n))
print("CMH test on aggregate shows SCHOOL did not affect decision-making BY ADVERASRY, p < .05")

# print(n<-xtabs(~  service+Choice_of_Advisor , data = rps_all_data_with_demo))
# print(chisq.test(n))
# print("Chi-sq test on aggregate shows SERVICE did affect decision-making in general, p < .01")
print(n<-xtabs(~  Choice_of_Advisor+Adversary+service, data = rps_all_data_with_demo))
print(mantelhaen.test(n))
print("CMH test on aggregate shows SERVICE did not affect decision-making BY ADVERASRY, p < .05")

#m <- glmer(Choice_of_Advisor ~machine_learning_experience +(1|id), data=rps_all_data_with_demo)
#--------------------RPS - Decision Time <-> Choices - Data Analysis-----------------

p<- ggplot(rps_all_data_with_demo, aes(x=seconds_on_page))+   geom_density(data=rps_all_data_with_demo[rps_all_data_with_demo$Adversary=="Human",],fill="red", color="red",alpha=0.3)+   geom_density(data=rps_all_data_with_demo[rps_all_data_with_demo$Adversary=="Human+AI",],fill="blue", color="blue",alpha=0.3)+   geom_density(data=rps_all_data_with_demo[rps_all_data_with_demo$Adversary=="AI",],fill="green", color="green",alpha=0.3)+ scale_x_continuous(limits = c(0, 25))+labs(title="RPS-Decision Time Density Plot (by Adversary)", x="Seconds per Decision", y = "Density", color = "Adversary type")  #+ scale_color_manual(values = c('Human' = 'red', 'Human+AI' = 'blue', "AI" = 'green'))
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
print("RESULT: PLOT shows average decision time by Adversary did not vary ")
print(chisq.test(xtabs(~Adversary + seconds_on_page, data=rps_all_data_with_demo)))
print("RESULT: Chisq test shows no signifciant relationship between decision time and Adversary")

# 
p<- ggplot(rps_all_data_with_demo, aes(x=seconds_on_page))+   geom_density(data=rps_all_data_with_demo[rps_all_data_with_demo$Choice_of_Advisor=="human",],fill="red", color="red",alpha=0.3)+   geom_density(data=rps_all_data_with_demo[rps_all_data_with_demo$Choice_of_Advisor=="AI",],fill="blue", color="blue",alpha=0.3)+   geom_density(data=rps_all_data_with_demo[rps_all_data_with_demo$Choice_of_Advisor=="none",],fill="green", color="green",alpha=0.3)+ scale_x_continuous(limits = c(0, 25))+labs(title="RPS-Decision Time Density Plot (by Choice_of_Advisor)", x="Seconds per Decision", y = "Density", color = "Choice")  #+ scale_color_manual(values = c('Human' = 'red', 'Human+AI' = 'blue', "AI" = 'green'))
print(p)
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".jpg"), plot=p, device="jpg")
print("RESULT: PLOT shows average decision time by Choice of advisor did not vary (distributions and means are slightly different)")
print(chisq.test(xtabs(~Choice_of_Advisor + seconds_on_page, data=rps_all_data_with_demo)))
print("RESULT: Chisq test shows a signifciant relationship between decision time and Choice of Advisor at p < .001")

#--------------------Multi-Game - Data Analyses-----------------
print("--------------------Multi-Game Analyses-----------------")

#played "non-strategically"
a<-as.data.frame(t(colSums(pw_array)))
a <- rownames(a[a$Peace %in% c(0,30,1,29),])
a <- union(a, pw_same_count_df[pw_same_count_df$sum>28,]$id)
print(paste0(length(a), " participants (",paste(a, collapse=", "),") played the same choice for all rounds and all adversaries in Peace-War (1 decision error allowed)."))
b<- as.data.frame(t(colSums(rps_array)))
b <- rownames(b[b$AI %in% c(0,30,1,29),])
print(paste0(length(b), " participants (",paste(b, collapse=", "),") played the exact same choice for all rounds in Rock-Paper-Scissors (1 decision error allowed)."))
c <- intersect(b,a)
print(paste0(length(c), " participants (",paste(c, collapse=", "),") played the exact same choice for all rounds in both Peace-War and Rock-Paper-Scissors."))


#find the round1 participants who chose differently across adversaries in round 1
d<- xtabs(~id + Adversary + my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$period==1,])
d<- d[,,"Peace"]
d<- rowSums(d)
d<-names(d[d %in% c(1,2)])
print(paste0(length(d), " participant(s) (",paste(d, collapse=", "),") varied their choice by adversary in Peace-War round 1."))


g <- c("2b574","3z144","6lk32","epb14","h0s22","i5b85","k1o66","n1i23","qs924","p8v34","snd43","s4441","txk36","yyj35","yda14","v6i85","epb14")# "b8g12" -uncertain)
print(paste0(length(g), " participant(s) (",paste(g, collapse=", "),") varied their strategies by adversary in Peace-War ."))

e <- rps_fisher_test[rps_fisher_test$All,]$id
print(paste0(length(e), " participant(s) (",paste(e, collapse=", "),") varied their choice by adversary in Rock-Paper-Scissors."))
f<- intersect(d,e)
print(paste0(length(f), " participant(s) (",paste(f, collapse=", "),") varied their choice by adversary in BOTH Peace-War round 1 and in Rock-Paper-Scissors."))


f<- intersect(g,e)
print(paste0(length(f), " participant(s) (",paste(f, collapse=", "),") varied their strategy by adversary in Peace-War and their decisions by adversary in Rock-Paper-Scissors."))

#Trust measurement. compare those who chose peace with the AI vs those who chose the AI Advisor
n <- as.data.frame(matrix(NA, nrow=length(pw_ids), ncol=1))
names(n)[1] <- "id"
n$id <- pw_ids
m<-as.data.frame(pw_array[1,1,])
m$id <- rownames(m)
names(m)[1] <- "Peace_w_AI"
n<- merge(m,n, by="id")
o <-as.data.frame(colSums(rps_array[,1,]))
o$id <- rownames(o)
names(o)[1] <- "AI_Advisor"
p<- merge(n,o, by="id")
print(cor.test(p$Peace_w_AI, p$AI_Advisor, method="spearman"))
print("RESULT: Spearman correlation test (a rank test) (S = 3987.5446, p-value = 0.6441073, rho=-0.09128204218) shows little to no statistical correlation between those who chose Peace with the AI and those who chose the AI Advisor more")

