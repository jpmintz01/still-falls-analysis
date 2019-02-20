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

if(!require(psych)){install.packages("psych")}
if(!require(nlme)){install.packages("nlme")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}


#functions
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
  #participant._index_in_pages should be >= participant._max_page_index to indicate a player completed the game
  fail_data <- unique(fail_data) #strip duplicates that failed multiple checks

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
  #participant._index_in_pages should be >= participant._max_page_index to indicate a player completed the game
  good_data <- unique(good_data) #remove duplicates
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

# session  info
## get participant label
#data <- data.frame(datafile$participant.label
## get IP address
## get time start & end experiment (not including post-game survey)
## get counterbalance code & translate it
## num_RPS_rounds
## num_PW_rounds
##

# informed consent app
## informed consent
## attention check
## contamination check

# prisoner_multiplayer (or prisoner_multiplayer_2 first)
## admin data
### human player ID, HAI player ID
## measured data 
### total player payoff
### total H payoff
### total HAI payoff
### total AI payoff
### ---data by round---
#### PW vs human by round
#### human adv PW by round
#### player payoff vs human
#### PW vs HAI by round
#### player payoff vs HAI
#### human adv PW by round
#### PW vs AI by round
#### player payoff vs AI
####  Round   PWvsH HvsPart P_payoff H_payoff  PWvsHAI HAIvsPart P_payoff HAI_payoff PWvsAI AIvsPart P_payoff AI_payoff
####    1
####    2 


## -------calculated PW data-----------
### number of P-W choices by adversary (how to store this data type?)
#### 3x3 matrix & contingency table
#####                      Peace-War choice
#####                         Peace   War
#####                     -----------------
#####                H    |   3       5     
#####   Adversary    HAI  |   5       5     
#####                AI   |   8       1     

### previous table & add dimension of players (so 3x3xnum_participants)
#### 



# rps
## admin data
### vs human game
#### human adversary ID
#### human advisor ID
### vs HAI game
#### human adversary ID
#### human advisor ID
### vs AI game
#### human advisor ID

## measured data
### for each adversary  (x3 Human, HAI, AI)
#### advisor choice by round
#### player's RPS choice regardless of advisor type
#### adversary's RPS choice
#### player: Win, Lose, Draw

## -------calculated RPS data-----------
### number of advisor choices by adversary (how to store this data type?)
#### 3x3 matrix & contingency table
#####                            Advisor Choice
#####                         none    H     AI
#####                     -----------------------
#####                H    |   3       5     7
#####   Adversary    HAI  |   5       5     5
#####                AI   |   8       1     6

### previous table & add dimension of players (so 3x3xnum_participants)
#### 


# post-game questionnaire
## all models variables
## debrief acknowledgement


#-------------------runtime-------------------
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

#--------PEACE-WAR---------
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
pw_array <- xtabs(~Adversary+Choice+id, data=pw_cols) #more R-like way of creating the array than the ten lines below
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
human_adv_choices <- c(1, 1,	0,	1,	0,	0,	0,	1,	1,	0)
human_tft_choices <- c(1, 1,  1,  0,  1,  0,  0,  0,  1,  1)

hai_adv_choices <- c(1,	0,	1,	1,	0,	1,	1,	1,	1,	0)
hai_tft_choices <- c(1,	1,	0,	1,	1,	0,	1,	1,	1,	1)

ai_adv_choices <- c(0,	0,	0,	1,	1,	0,	0,	0,	1,	0)
ai_tft_choices <- c(1,	0,	0,	0,	1,	1,	0,	0,	0,	1)

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

#train/tst
# all_data_subset <- all_data
# all_data_subset$my.decision <- as.factor(all_data_subset$my.decision)
# all_data_subset$my.decision <- as.numeric(as.factor(all_data_subset$my.decision))
# all_data_subset$my.decision[all_data_subset$my.decision==2] <- 0
# all_data_train <- all_data_subset[1:(nrow(all_data_subset)*.7),]
# all_data_test <- all_data_subset[(nrow(all_data_subset)*.7+1):nrow(all_data_subset)+1,]
# all_data_test <- all_data_test[,predictors]


# pw_all_data$my.decision <- as.numeric(as.factor(pw_all_data$my.decision))
# pw_all_data$my.decision <- pw_all_data$my.decision+1
# pw_all_data$my.round1decision <- pw_all_data$my.round1decision+1
# pw_all_data$my.decision1 <- pw_all_data$my.decision1+1
# pw_all_data[is.na(pw_all_data$my.decision1),]$my.decision1 <- 0
# pw_all_data$other.decision1 <- pw_all_data$other.decision1+1 
# pw_all_data[is.na(pw_all_data$other.decision1),]$other.decision1 <- 0
# pw_all_data_subset <-pw_all_data

nn <- neuralnet(f,data=pw_all_data_subset[pw_all_data_subset$period >1,],hidden=c(10,4),act.fct = "logistic",linear.output=FALSE)
# pw_all_data_subset_human <- pw_all_data_subset[pw_all_data$Adversary=="Human",]
# nn_test_only_human <- neuralnet(f,data=pw_all_data_subset_human[pw_all_data_subset_human$period >1,],hidden=c(10,4),act.fct = "logistic",linear.output=FALSE)
# nn_test <- neuralnet(f,data=pw_all_data_subset,hidden=c(10,4),act.fct = "logistic",linear.output=FALSE)

# nn_exp_all_data_test <- neuralnet(f,data=all_data_train[all_data_train$period >1,],hidden=c(10,4),act.fct = "logistic",linear.output=FALSE)
pw_all_data_sub_subset <- pw_all_data_subset[,predictors]
pw_nn_output<- compute(nn, pw_all_data_sub_subset)$net.result
# pw_nn_output<- compute(nn_test_only_human, pw_all_data_sub_subset)$net.result
#this is just a test round here
# all_data_nn_output_test<- compute(nn_exp, all_data_test[all_data_test$period >1,])$net.result
# all_data_nn_output_test<- compute(nn_exp_all_data_test, all_data_test[all_data_test$period >1,])$net.result
pw_nn_output[is.na(pw_nn_output)] <- 1
pw_nn_output_rounded <- round(pw_nn_output) #(is this the right function for classification?)
pw_nn_outcome <- pw_nn_output_rounded
pw_nn_outcome[pw_nn_outcome ==1]<-"Peace"
pw_nn_outcome[pw_nn_outcome ==0]<-"War"
# pw_nn_outcome <- c(round(rbind(matrix(rep(1, 84), nrow=1),matrix(pw_nn_output, nrow=9))))#test needed if training on only rounds 2-10

#compute using history-2
# f_2_10 <- as.formula("my.decision~my.round1decision+my.decision1+other.decision1+my.decision2+other.decision2+period")
# pw_all_data_sub_subset_2_10 <- pw_all_data_subset[,c("my.round1decision","my.decision1","my.decision2","other.decision2","other.decision1","period")]
# nn_exp_2_10 <- neuralnet(f,data=pw_all_data_subset[pw_all_data_subset$period >2,],hidden=10,linear.output=FALSE)
# pw_nn_output_2_10<- compute(nn_exp, pw_all_data_sub_subset)$net.result
# pw_nn_output_2_10_rounded <- round(pw_nn_output_2_10) #(is this the right function for classification?)
# pw_nn_output_2_10_rounded[is.na(pw_nn_output_2_10_rounded)] <- 1
# pw_nn_outcome <- pw_nn_output_2_10_rounded

p<-pw_all_data
# p <- add_column(p, c(as.matrix(q)), .before = "risk") #insert predicted values into p
# colnames(p)[colnames(p) =="c(as.matrix(q))"] <- "outcome"
# p$outcome <- as.factor(p$outcome)
#

p <- add_column(p, pw_nn_outcome, .before = "risk") #insert nnet values into p
colnames(p)[colnames(p) =="pw_nn_outcome"] <- "nnet"
p$nnet <- as.factor(p$nnet)
levels(p$nnet) <- c("Peace","War")

p <- add_column(p, q$value, .after = "nnet") #insert nnet values into p
colnames(p)[colnames(p) =="q$value"] <- "GLM"
p$GLM <- as.factor(p$GLM)
levels(p$GLM) <- c("Peace","War")

p$my.decision <- as.factor(p$my.decision)
# 

# row.names <- c("AI","Human","Human+AI")
# col.names <- c("Peace", "War")
# v<-p$nnet
# w<- arrange(pw_cols, id)
# x<- cbind(w, v)
# colnames(x)[colnames(x) =="v"] <- "Predicted"
#can i use xtabs here better than this for-loop nonsense?
pw_nnet_array <- xtabs(~Adversary + nnet + id, data=p)
pw_GLM_array <- xtabs(~Adversary + GLM + id, data=p)

# pw_pred_array <- array(NA, dim = c(3,2,length(pw_ids)), dimnames=list(row.names, col.names, pw_ids))
# for (i in pw_ids) {
#   a <- subset(x, id==i)
#   for (j in Adversary_list) {
#     b <- subset (a, Adversary==j)
#     for (k in Choice_list){
#       pw_pred_array[j,k,i] <- sum(b$Predicted==k)
#     }
#   }
# }
# v<- pw_array-pw_pred_array
# w<- rowSums(v, dims = 2)

# x<- pw_array/pw_pred_array
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

pw_cols_peace_by_id <- as.data.frame.matrix(xtabs(~ id + Adversary, data = pw_cols[pw_cols$Choice=="Peace",]))
pw_cols_by_id <- as.data.frame(xtabs(~id + Adversary + Choice, data = pw_cols))

#--------------Rock-Paper-Scissors data pull & transformation-----------------
#bind the columns advisor_choice and adversary type
rps_cols <- bind_cols(new_data[grepl('^rps.*advisor_choice$', names(new_data))], new_data[grepl('^rps.*adv_1_type$', names(new_data))])#get rps_vs_human choices as a df row:player, col:colnames
#rps_vs_human_totals <- rowSums()#pwChangeColNames(rps_vs_human_cols, num_pw_rounds) #get a matrix of rps choices (row:player)x(column:round) with only the rps vs human
names(rps_cols) <-  gsub(pattern = "rps.", replacement = "", x = names(rps_cols)) # removes the leading 'rps.' - just a cleanup
names(rps_cols) <-  gsub(pattern = "player.", replacement = "", x = names(rps_cols)) # removes 'player.' - just a cleanup
names(rps_cols)[grepl('^[1-9][.](.*)$', names(rps_cols))] <- paste0("0", names(rps_cols)[grepl('^[1-9][.](.*)$', names(rps_cols))])
# line above adds leading zero to one-digit round numbers
a <- rps_cols
rps_cols <- rps_cols[,order(colnames(rps_cols))] #sorts columns by round number (two vars each)
  





new_data_labels <- c("participant.code","participant.label","session.config.pw_counterbalance","session.config.rps_counterbalance")
df <- subset(new_data, select=new_data_labels)#change ...code to ...label in real sessions
df_labels <- c("code","label","pw_cb","rps_cb")
colnames(df) <- df_labels
list_of_decision_cols <- names(rps_cols)[grepl('^.*choice$', names(rps_cols))]
list_of_type_cols <- names(rps_cols)[grepl('^.*type$', names(rps_cols))]
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
# f<- inner_join(df_types,df_decs,by="id")
rps_long<- bind_cols(df_types, as.data.frame(df_decs$Choice_of_Advisor))
names(rps_long)[4] <- "Choice_of_Advisor"
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

#---------------Demographic Info Pull----------------------
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


##--------------combined data---------------------
##-----RPS w/demographic & time------

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


##-----PW w/demographic-------
# melted_pw <- melt(pw_array)
# melted_pw_pred <- melt(pw_pred_array)
# colnames(melted_pw) <- c("Adversary","Choice","id","value")
# colnames(melted_pw_pred) <- c("Adversary","Choice","id","pred_value")
# merged_pw <- merge(melted_pw, melted_pw_pred, by=c("id","Adversary","Choice"))
# pw_all <- merge(merged_pw, demo_relevant_data, by="id")
# colnames(pw_all)[colnames(pw_all)=="value"] <- "Obs"
# colnames(pw_all)[colnames(pw_all)=="pred_value"] <- "Pred"
# pw_all$Obs_perc_Peace <- pw_all$Obs/10
# pw_all$Pred_perc_Peace <- pw_all$Pred/10
#add a column for difference between obs&pred_perc
#_------pw_all_data plus demo & time data
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

#write.csv(pw_all_data_with_demo, "pw_all_data_with_demo.csv")

#--------------Data Visualization------------------
#demo data visualization
#ggplot(demo_data) + geom_histogram( aes(age) ) #age histogram
#ggplot(demo_data) + geom_histogram( aes(years_military_experience) )
#ggplot(demo_data) + geom_histogram(stat="count", aes(school) )
#ggplot(demo_data) + geom_histogram(stat="count", aes(rank) )
#ggplot(demo_data) + geom_histogram(stat="count", aes(service) )
# genders <- c(length(which(demo_data$gender=="Male")), length(which(demo_data$gender=="Female")))
# pie(genders) #need a better chart here
#ggplot(demo_data) + geom_histogram(stat="count", aes(game_theory_experience) )
# ggplot(demo_data) + geom_histogram(stat="count", aes(machine_learning_experience) )


# (!require(rcompanion)){install.packages("rcompanion")}
# XT<- xtabs( ~Adversary + Choice_of_Advisor, rps_long) #need to change this variable name
# prop.table(XT)
# barplot(XT,
#         beside = TRUE,
#         legend = TRUE,
#         ylim = c(0, 100),   ### y-axis: used to prevent legend overlapping bars
#         cex.names = 0.8,  ### Text size for bars
#         cex.axis = 0.8,   ### Text size for axis
#         args.legend = list(x   = "topright",   ### Legend location
#                            cex = 0.8,          ### Legend text size
#                            bty = "n"))         ### Remove legend box
# 
# mosaicplot(XT, 
#            color=TRUE, 
#            cex.axis=0.8)
# 
# ##------------------Data Analysis-----------------
# if(!require(DescTools)){install.packages("DescTools")}
# if(!require(PropCIs)){install.packages("PropCIs")}
# 
# MultinomCI(XT, 
#            conf.level=0.95, 
#            method="sisonglaz")


print("-----------PW analysis------------")

print("P-W Aggregate Observed Choices")
print(pw_sum)
# pw_summary <- ddply(pw_all_data_with_demo, ~Adversary+my.decision, summarise, Obs.sum=sum('my.decision'), Obs.mean=mean('my.decision'), Obs.sd=sd('my.decision'), nnet.sum=sum(nnet), nnet.mean=mean(nnet), nnet.sd=sd(nnet))
# print("P-W summary")
# print(pw_summary)

print("tests for parametric assumptions")
#tests on all Peace choices across all players
print(shapiro.test(rowSums(pw_cols_peace_by_id)))#test for normality (#normal)
#normality tests "by adversary"
print(shapiro.test(pw_cols_peace_by_id$AI)) #not normal
print(shapiro.test(pw_cols_peace_by_id$Human)) #normal
print(shapiro.test(pw_cols_peace_by_id$`Human+AI`)) #normal

m <- aov(Freq ~ Adversary, data = pw_cols_by_id[pw_cols_by_id$Choice=="Peace",])
print(shapiro.test(residuals(m))) #not normal

plot(xtabs(~ Round + Choice, data = pw_cols), main="Peace-War Choices by Round")

#print(paste("Woolf Test p-value: ", as.character(WoolfTest(rps_array)["p.value"])))
# print(paste("CMT p-value: ", as.character(mantelhaen.test(pw_array)["p.value"])))
# print(paste("CMT p-value: ", as.character(mantelhaen.test(pw_HvHAI_array)["p.value"])))
# print(paste("CMT p-value: ", as.character(mantelhaen.test(pw_HvAI_array)["p.value"])))
# print("Chi squared test p-values: ")
# for (i in pw_ids){
#   print(paste(i, as.character(chisq.test(pw_array[,,i])["p.value"])))
# }
# for (i in pw_ids){
#   print(paste(i, as.character(chisq.test(pw_HvAI_array[,,i])["p.value"])))
# }
# for (i in pw_ids){
#   print(paste(i, as.character(chisq.test(pw_HvHAI_array[,,i])["p.value"])))
# }
# print("fisher test p-values: ")
# for (i in pw_ids){
#   print(paste(i, as.character(fisher.test(pw_array[,,i])["p.value"])))
# }
# print("wilcox test p-values: ")
# for (i in pw_ids){
#   print(paste(i, as.character(wilcox.test(pw_array[,,i])["p.value"])))
# }
# print("CramerV phi: ")
# n <- rowSums(pw_array, dims = 2)
# cramerV(n)

print("PW Round 1 Tests")
print("PW Round 1: Chi-Sq test")
print(pw_round_1s_sum)
print(chisq.test(pw_round_1s_sum))

print("PW Round 1: Friedman test")
print(friedman.test(Choice ~ Adversary | id, data = as.matrix(pw_round_1s))) #not significant

print("PW Round 1: Simple Count - id's of participants who varied their round 1 choice by adversary (at all): ")
pw_varied_round1 = data.frame("id"=pw_ids,"Varied"=NA)
for (i in pw_ids) {
  pw_varied_round1[pw_varied_round1$id==i,"Varied"] <- !((subset(pw_round_1s, id==i & Adversary == "Human")$Choice == subset(pw_round_1s, id==i & Adversary == "AI")$Choice) & (subset(pw_round_1s, id==i & Adversary == "Human")$Choice == subset(pw_round_1s, id==i & Adversary == "Human+AI")$Choice))
}
print(pw_varied_round1[pw_varied_round1$Varied,]$id)

print("Chi-sq (aggregate) on whether GENDER affected decision-making, by adversary")
print(xtabs(~ Adversary + gender + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + gender + my.decision, data = pw_all_data_with_demo)[,,1]))

print("Chi-sq (aggregate) on whether SERVICE affected decision-making, by adversary")
print(xtabs(~ Adversary + service + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + service + my.decision, data = pw_all_data_with_demo)[,,1]))

print("Chi-sq (aggregate) on whether RANK affected decision-making, by adversary")
print(xtabs(~ Adversary + rank + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + rank + my.decision, data = pw_all_data_with_demo)[,,1]))

print("Chi-sq (aggregate) on whether P-W PLAY ORDER (counterbalancing) affected decision-making:")
print("in general")
print(xtabs(~ pw_order + my.decision, data = pw_all_data_with_demo)[,1])
print(chisq.test(xtabs(~ pw_order + my.decision, data = pw_all_data_with_demo)[,1]))
print("by adversary")
print(xtabs(~ Adversary + pw_order + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + pw_order + my.decision, data = pw_all_data_with_demo)[,,1]))


print("Chi-sq (aggregate) on whether AGE affected decision-making")
number_parts_by_age <- prop.table(xtabs(~ age,data = pw_all_data_with_demo))*28
print("in general - # of peace decisions by age")
print(xtabs(~ age + my.decision, data = pw_all_data_with_demo)[,1]/number_parts_by_age) #first xtabs is number of Peace decisions by age. prop.table is proportion of participants by age
print(chisq.test(xtabs(~ age + my.decision, data = pw_all_data_with_demo)[,1]/number_parts_by_age)) #note: trending significant that age made a difference in Warlike-ness
print("by adversary")
temp_matrix <- matrix(NA, nrow = 3, ncol = 12) #matrix of 
temp_matrix[1,] <- (xtabs(~ Adversary + age + my.decision, data = pw_all_data_with_demo)[1,,1])/number_parts_by_age 
temp_matrix[2,] <- (xtabs(~ Adversary + age + my.decision, data = pw_all_data_with_demo)[2,,1])/number_parts_by_age
temp_matrix[3,] <- (xtabs(~ Adversary + age + my.decision, data = pw_all_data_with_demo)[3,,1])/number_parts_by_age
print(chisq.test(temp_matrix)) #not significant that Age was related to differences in decision-making by adversary

print("Chi-sq (aggregate) on whether YEARS MILITARY EXPERIENCE affected decision-making")
number_parts_by_years <- prop.table(xtabs(~ years_military_experience,data = pw_all_data_with_demo))*28
print("in general")
print(xtabs(~ years_military_experience + my.decision, data = pw_all_data_with_demo)[,1]/number_parts_by_years)#first xtabs is number of Peace decisions by age. prop.table is proportion of participants by age
print(chisq.test(xtabs(~ years_military_experience + my.decision, data = pw_all_data_with_demo)[,1]/number_parts_by_years)) #note: highly significant that years of military experience made a difference in Warlike-ness
print("by adversary")
temp_matrix <- matrix(NA, nrow = 3, ncol = length(number_parts_by_years)) #matrix of 
temp_matrix[1,] <- (xtabs(~ Adversary + years_military_experience + my.decision, data = pw_all_data_with_demo)[1,,1])/number_parts_by_years
temp_matrix[2,] <- (xtabs(~ Adversary + years_military_experience + my.decision, data = pw_all_data_with_demo)[2,,1])/number_parts_by_years
temp_matrix[3,] <- (xtabs(~ Adversary + years_military_experience + my.decision, data = pw_all_data_with_demo)[3,,1])/number_parts_by_years
temp_matrix
print(chisq.test(temp_matrix)) #not significant that years of military experience was related to differences in decision-making by adversary

print("Chi-sq (aggregate) on whether SCHOOL affected decision-making, by adversary")
print(xtabs(~ Adversary + school + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + school + my.decision, data = pw_all_data_with_demo)[,,1]))

print("Chi-sq (aggregate) on whether ROUND1 decision was correlated to strategic decision-making, by adversary")
print(xtabs(~ Adversary + my.round1decision + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + my.round1decision + my.decision, data = pw_all_data_with_demo)[,,1])) #significant

print("Chi-sq (aggregate) on whether GAME THEORY EXPERIENCE  affected decision-making, by adversary")
print(xtabs(~ Adversary + game_theory_experience + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + game_theory_experience + my.decision, data = pw_all_data_with_demo)[,,1])) #not significant

print("Chi-sq (aggregate) on whether AI EXPERIENCE  affected decision-making, by adversary")
print(xtabs(~ Adversary + machine_learning_experience + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + machine_learning_experience + my.decision, data = pw_all_data_with_demo)[,,1])) #not significant

print("anova on whether AVERAGE DECISION TIME was correlated to decision-making")
a <- pw_all_data_with_demo %>%
  group_by(id)%>%
  summarize(avg = mean(seconds_on_page), sd = sd(seconds_on_page), Peace = sum(my.decision=="Peace"))
a <- as.data.frame(a)
a
plot(x=a$Peace, y=a$avg)
m <- aov(Peace ~ avg, data =a)
summary(m)
boxplot(a$Peace)



# these are the players who played "strategically" (not a fixed strategy, and did not indicate non-belief)
# pw_played <- c("yyj35","yda14", "w9c21", "v6i85", "txk36", "n1i23", "i5b85", "h0s22", "b8g12", "b1462", "9mp31", "8mh76", "3z144")
# pw_array_played <- pw_array[,,pw_played]
# pw_pred_array_played <- pw_pred_array[,,pw_played]


print("P-W Aggregate Predictions Choices")
print(pw_pred_actual_sum)

print(paste("Percent Predicted choices == Observed: ", perc_nnet_eq_actual))
perc_GLM_eq_actual <- length(which(p$GLM == p$my.decision))/nrow(p)
print(perc_GLM_eq_actual)
perc_nnet_eq_actual <- length(which(p$nnet == p$my.decision))/nrow(p)
perc_nnet_eq_actual


print("-----------RPS analysis------------")

rps_sum <- xtabs(~Adversary+Choice_of_Advisor, data=rps_all_data_with_demo)
rps_prop <- round(prop.table(rps_sum)*100, 1)


print("tests for normality")
#tests on all "none" choices across all players
shapiro.test(rowSums(rps_long_none_by_id))#not normal
#tests on all "AI" choices across all players
shapiro.test(rowSums(rps_long_AI_by_id)) #not normal
#tests on all "human" choices across all players
shapiro.test(rowSums(rps_long_human_by_id)) #not normal

#normality tests "by adversary" - not needed because all data (above) isn't normal
# shapiro.test(rps_long_none_by_id$AI) #not normal
# shapiro.test(rps_long_human_by_id$AI) #not normal
# shapiro.test(rps_long_AI_by_id$AI) #not normal
# shapiro.test(rps_long_none_by_id$Human) #not normal
# shapiro.test(rps_long_human_by_id$Human) #not normal
# shapiro.test(rps_long_AI_by_id$Human) #not normal
# shapiro.test(rps_long_none_by_id$`Human+AI`) #not normal
# shapiro.test(rps_long_human_by_id$`Human+AI`) #not normal
# shapiro.test(rps_long_AI_by_id$`Human+AI`) #not normal
#other tests for normality - not needed because all data (above) isn't normal
# m <- aov(Freq ~ Adversary, data = rps_long_by_id[rps_long_by_id$Choice_of_Advisor=="none",])
# shapiro.test(residuals(m)) #not normal
# 
# m <- aov(Freq ~ Adversary, data = rps_long_by_id[rps_long_by_id$Choice_of_Advisor=="AI",])
# shapiro.test(residuals(m))#not normal
# 
# m <- aov(Freq ~ Adversary, data = rps_long_by_id[rps_long_by_id$Choice_of_Advisor=="human",])
# shapiro.test(residuals(m))#not normal

#video #9 (tests of proportions)
print("RPS: Tests of proportions on AGGREGATE choices")
print(xtabs(~ Adversary + Choice_of_Advisor, data=rps_all_data_with_demo))
# xtabs(~ Adversary + Choice_of_Advisor, data=rps_all_data_with_demo)
chisq.test(xtabs(~ Adversary + Choice_of_Advisor, data=rps_all_data_with_demo))
GTest(xtabs(~ Adversary + Choice_of_Advisor, data=rps_all_data_with_demo)) #more accurate than Chisq
fisher.test(xtabs(~ Adversary + Choice_of_Advisor, data=rps_all_data_with_demo))#exact test gives exact p-value
print("do I need to do post-hoc binomial tests w/holm correction?")

# "Chi Squared for individual participants -- not valid because some expected values are < 5"
#using fisher test instead)

print("RPS: Individual Fisher test (since Chisq invalid with expected values<0")
rps_fisher_test <- data.frame("id"=rps_ids,"All"=NA)
#"fisher test p-values
for (i in rps_ids){
  rps_fisher_test[rps_fisher_test$id==i,"All"] <- fisher.test(rps_array[,,i])["p.value"]<0.05
}

print ("RPS ID's which showed difference in choices by Adversary: ")
print(rps_fisher_test[rps_fisher_test$All=="TRUE",]$id)
print("summary choices of those who showed significant fisher test")
print(xtabs(~Adversary + Choice_of_Advisor, data = rps_long[rps_long$id %in% rps_fisher_test[rps_fisher_test$All=="TRUE",]$id,]))


friedman.test(rps_sum) # need to check if this needs to be transposed (t(rps_sum))
friedman.test(Freq ~ Adversary|id, data=rps_long_by_id[rps_long_by_id$Choice_of_Advisor=="none",])
# am I doing this right? or should it be like this:
#friedman.test(Freq ~ Choice_of_Advisor|id, data=rps_long_by_id[rps_long_by_id$Adversary=="AI",])
friedman.test(Freq ~ Adversary|id, data=rps_long_by_id[rps_long_by_id$Choice_of_Advisor=="AI",])
friedman.test(Freq ~ Adversary|id, data=rps_long_by_id[rps_long_by_id$Choice_of_Advisor=="human",])

#Use Wilcox tests as 2-level post-hoc
wilcox.test(rps_sum[-1,]) #wilcox test for HUman v HAI
wilcox.test(rps_sum[-2,]) #wilcox test for AI v HAI
wilcox.test(rps_sum[-3,]) #wilcox test for HUman v AI

# print("CramerV phi: ")
# m <- rowSums(rps_array, dims = 2)
# cramerV(m)

print("mcnemar test: participants who showed a significant difference in RPS decision-making ")
for (i in rps_ids){
  temp_n <- mcnemar.test(rps_array[,,i])["p.value"]
  if (!is.na(temp_n) && temp_n < 0.05) {
    print(i)
  } 
}


#display summary
rps_summary <- ddply(rps_all, ~Adversary+Choice_of_Advisor, summarise, Choice_of_Advisor.sum=sum(value), Choice_of_Advisor.mean=mean(value), Choice_of_Advisor.sd=sd(value))
rps_summary


plot(xtabs(~ Round + Choice_of_Advisor, data = rps_long_ten_rounds), main="RPS Choices by Round")
#create "by id" count tables

#non-binomial test? 
summary(goodfit(rps_long_none_by_id$`Human+AI`, type="nbinomial",method="MinChisq") )

print("----------------time analysis-------------")
boxplot(seconds_on_page ~ id, data = time_data[time_data$seconds_on_page <=60,])
