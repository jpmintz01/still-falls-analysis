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
library(zoo)
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
filepath <- file.choose()
datafile <- read.csv(filepath, header = TRUE, stringsAsFactors = FALSE)
fail_data <- saveFails(datafile) #save the fails to another file for researcher review
good_data <- stripFails(datafile) #strip the fails off the man datafile
new_data <- stripBadCols(good_data)  #some error here...
num_pw_rounds <- new_data$session.config.num_PW_rounds[[1]]
#split off the columns which describe pw_vs_human
num_rps_rounds <- new_data$session.config.num_RPS_rounds[[1]]

#choose and readthe all_data data file
filepath <- file.choose()
# #read the file with headers
all_data <- read.csv(filepath, header = TRUE, stringsAsFactors = TRUE)



#--------PEACE-WAR---------
pw_ids <- subset(new_data, select=participant.label)
pw_cols <- bind_cols(pw_ids, new_data[grepl('^prisoner.*decision_vs_adv_.*$', names(new_data))])
pw_vs_human_cols <- bind_cols(pw_ids, new_data[grepl('^prisoner.*decision_vs_adv_1$', names(new_data))]) #get pw_vs_human choices as a df row:player, col:colnames
names(pw_cols) <-  gsub(pattern = "participant.label", replacement = "id", x = names(pw_cols))
pw_cols[pw_cols==""]<-NA #cut out all the "" values and replace with NA
pw_first <- subset(pw_cols, is.na(prisoner_multiplayer_2.1.player.decision_vs_adv_1))
pw_first[,(32:61)] <- NULL
play_order <- list(rep(1,nrow(pw_first)))
pw_first <- add_column(pw_first, play_order, .after="id")
pw_second <- subset(pw_cols, is.na(prisoner_multiplayer.1.player.decision_vs_adv_1))
pw_second[,(2:31)] <- NULL
play_order <- list(rep(2,nrow(pw_first)))
pw_second <- add_column(pw_second, play_order, .after="id")
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
pw_ids <- levels(factor(pw_cols$id))
pw_array <- xtabs(~Adversary+Choice+id, data=pw_cols) #more R-like way of creating the array than the ten lines below
# row.names <- c("AI","Human","Human+AI")
# col.names <- c("Peace", "War")
# pw_array <- array(NA, dim = c(3,2,length(pw_ids)), dimnames=list(row.names, col.names, pw_ids))
# #creates an array where [x,,] = rows of Adversary types, [,y,] = columns of advisor choices, and [,,z]=player id's
# Adversary_list <- c("AI","Human","Human+AI")
# Choice_list <- c("Peace", "War")
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

pw_percent_matrix <- matrix(nrow=length(pw_ids),ncol=3)
rownames(pw_percent_matrix) <- pw_ids
for (i in pw_ids){
  pw_percent_matrix[i,] <- pw_array[,"Peace",i]/pw_array[,"War",i]
}
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

pw_all_data_colnames <- c("X","period","my.decision","risk","delta","r1","r2","error","data","my.round1decision","my.decision1","my.decision2","my.decision3","my.decision4","my.decision5","my.decision6","my.decision7","my.decision8","my.decision9","other.decision1","other.decision2", "other.decision3" ,"other.decision4" ,"other.decision5", "other.decision6", "other.decision7", "other.decision8" ,"other.decision9", "my.payoff1","my.payoff2","my.payoff3", "my.payoff4","my.payoff5","my.payoff6","my.payoff7","my.payoff8","my.payoff9","other.payoff1","other.payoff2","other.payoff3","other.payoff4","other.payoff5","other.payoff6","other.payoff7","other.payoff8","other.payoff9","r","s","t","p","infin", "contin","group")
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
#begin machine learning section
predictors <- c("period","risk","delta","r1","r2","error", "r","s","t","p","infin","contin","my.round1decision")
# control <- rfeControl(functions = rfFuncs,
#                       method = "repeatedcv",
#                       repeats = 3,
#                       verbose = FALSE)
# Pred_Profile <- rfe(all_data[,predictors], all_data$my.decision, rfeControl = control)

#this line below uses all_data to train static model
lmFit<-train(my.decision~r1+r2+risk+error+delta+r1:delta+r2:delta+infin+contin, data = subset(all_data, period == 1), method = 'glm', na.action = na.pass)
#this line below uses above model to predict static model (round 1's)
outcomes_1 <- predict(lmFit, subset(pw_all_data, period == 1)) # this is the static outcomes
levels(outcomes_1) <- c("Peace","War")
#this line below uses all_data to train line 2 in the dynamic model (the first dynamic row) - could use something else
lmFit1<-train(my.decision~my.round1decision+r1+r2+risk+error+delta+r1:delta+r2:delta+infin+contin+delta:infin+my.decision1+other.decision1+error:other.decision1+period, data = all_data, method = 'glm', na.action = na.pass)
#this line below uses the dynamic model to predict all the rounds 2-10
outcomes_2 <- predict(lmFit1, pw_all_data)
levels(outcomes_2) <- c("Peace","War")

#this line below uses all_data to train lines 3-10 in the dynamic model (the first dynamic row) - could use something else (can remove this since it only gains additional .5% in predictive power)
lmFit2<-train(my.decision~my.round1decision+r1+r2+risk+error+delta+r1:delta+r2:delta+infin+contin+delta:infin+my.decision1+other.decision1+error:other.decision1+period+my.decision2+other.decision2, data = all_data, method = 'glm', na.action = na.pass)
# lmFit2<-train(my.decision~my.round1decision, data = all_data, method = 'glm', na.action = na.pass)
#this line below uses the dynamic model to predict all the rounds 2-10
outcomes_3_10 <- predict(lmFit2, pw_all_data)
levels(outcomes_3_10) <- c("Peace","War")



#need to fix levels and stack outcome and outcomes
r <- data.frame(matrix(outcomes_2, ncol=(nrow(pw_cols)/10)))[1,]
o <- data.frame(matrix(outcomes_1, ncol=(nrow(pw_cols)/10)))
t <- data.frame(matrix(outcomes_3_10, ncol=(nrow(pw_cols)/10)))
q <- rbind(o,r,t)


p<-pw_all_data
p <- add_column(p, c(as.matrix(q)), .before = "risk") #insert predicted values into p
names(p)[4] <- "outcome"
compare_my_d_w_outcome <- p$my.decision == p$outcome
num_pred_eq_actual <- length(which(compare_my_d_w_outcome))
perc_pred_eq_actual <- num_pred_eq_actual/nrow(p)

v<-p$outcome
w<- arrange(pw_cols, id)
x<- cbind(w, v)
colnames(x)[5] <- "Predicted" 
#can i use xtabs here better than this for-loop nonsense?
pw_pred_array <- array(NA, dim = c(3,2,length(pw_ids)), dimnames=list(row.names, col.names, pw_ids))
for (i in pw_ids) {
  a <- subset(x, id==i)
  for (j in Adversary_list) {
    b <- subset (a, Adversary==j)
    for (k in Choice_list){
      pw_pred_array[j,k,i] <- sum(b$Predicted==k)
    }
  }
}
v<- pw_array-pw_pred_array
w<- rowSums(v, dims = 2)





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
get_adv_order_rps <- function (data, colname) {#unused now -  add if needed to 
  H_order #1 2 or 3
  HAI_order #1,2, or 3
  AI_order #1, 2, or 3
}
game_treatment_order_rps <- 2-(3-new_data$session.config.pw_counterbalance%%2)%%2
adv_treatment_order_rps <- 0
  
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
rps_ids <- levels(rps_long$id)
rps_array <- xtabs(~Adversary+Choice_of_Advisor+id, data=rps_long) #more R-like way of creating rps_array than the next ten or so lines
# row.names <- c("AI","Human","Human+AI")
# col.names <- c("AI", "human", "none")
# rps_array <- array(NA, dim = c(3,3,length(rps_ids)), dimnames=list(row.names, col.names, rps_ids))
#creates an array where [x,,] = rows of Adversary types, [,y,] = columns of advisor choices, and [,,z]=player id's
# Adversary_list <- c("AI","Human","Human+AI")
# Advisor_list <- c("AI", "human", "none")
# for (i in rps_ids) {
#   a <- subset(rps_long, id==i)
#   for (j in Adversary_list) {
#     b <- subset (a, Adversary==j)
#     for (k in Advisor_list){
#       rps_array[j,k,i] <- sum(b$Choice_of_Advisor==k)
#     }
#   }
# }
# rps_HvAI_array <- rps_array[-3,,] 
# rownames(rps_HvAI_array)[1:2] <- rownames(rps_HvAI_array)[2:1] #swap rownames
# rps_HvAI_array[1:2,,] <- rps_HvAI_array[2:1,,] #swap row values (to make Human first)
# rps_HvHAI_array <- rps_array[-1,,]
#   print(i)
#   a <- count(subset(rps_long, id==i),id, Adversary, Choice_of_Advisor)
#   print(a)
#   b <- spread(a, Choice_of_Advisor, n)
#   print(b)
#   c <- b[,3:5]
#   print(c)
#   print(data.matrix(c))
#   rps_array[,,i] <- data.matrix(c)
# }
# rps_array[is.na(rps_array)] <- 0
# ftable(rps_array) #from stats

#note: subset(subset(rps_long, id=="2r212l3b"), Adversary=="Human")
#note: subset(subset(rps_long, id=="2r212l3b"), Adversary=="Human")$Choice_of_Advisor
#note: sum(subset(subset(rps_long, id=="2r212l3b"), Adversary=="Human")=="human")/num_rps_rounds

#df[,"pw_Peace_human"] <- rowsums(pw_vs_human=="Peace")
# df[,"pw_human"] <- rowSums(pw_vs_human=="Peace")/num_pw_rounds
# df[,"pw_hai"] <- rowSums(pw_vs_hai=="Peace")/num_pw_rounds
# df[,"pw_ai"] <- rowSums(pw_vs_ai=="Peace")/num_pw_rounds
# df[,"rps_none_total"] <- rowSums(rps_cols=="none")/(3*num_rps_rounds)
# df[,"rps_human_total"] <- rowSums(rps_cols=="human")/(3*num_rps_rounds)
# df[,"rps_ai_total"] <- rowSums(rps_cols=="AI")/(3*num_rps_rounds)
# df[,"rps_hexp_v_hadv"] <- rowSums(rps_cols=="Peace")/num_pw_rounds

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
##-----RPS w/demographic------
melted_rps <- melt(rps_array)
colnames(melted_rps) <- c("Adversary","Choice_of_Advisor","id","value")
rps_all <- merge(melted_rps, demo_relevant_data, by="id")

##-----PW w/demographic-------
melted_pw <- melt(pw_array)
melted_pw_pred <- melt(pw_pred_array)
colnames(melted_pw) <- c("Adversary","Choice","id","value")
colnames(melted_pw_pred) <- c("Adversary","Choice","id","pred_value")
merged_pw <- merge(melted_pw, melted_pw_pred, by=c("id","Adversary","Choice"))
pw_all <- merge(merged_pw, demo_relevant_data, by="id")
colnames(pw_all)[colnames(pw_all)=="value.x"] <- "Obs"
colnames(pw_all)[colnames(pw_all)=="value.y"] <- "Pred"

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
print("-----------RPS analysis------------")
# f <- count(rps_long, id, Adversary, Choice_of_Advisor)
# print("Anova")
# rmaModel <- lmer(n ~ Choice_of_Advisor +(1|id), data = f)
# anova(rmaModel)

# print(paste("Woolf Test p-value: ", as.character(WoolfTest(rps_array)["p.value"])))
print(paste("CMT p-value: ", as.character(mantelhaen.test(rps_array)["p.value"])))
# print(paste("CMT p-value: ", as.character(mantelhaen.test(rps_HvAI_array)["p.value"])))
# print(paste("CMT p-value: ", as.character(mantelhaen.test(rps_HvHAI_array)["p.value"])))
print("Chi squared test p-values: ")
for (i in rps_ids){
  print(paste(i, as.character(chisq.test(rps_array[,,i])["p.value"])))
}
# for (i in rps_ids){
#   print(paste(i, as.character(chisq.test(rps_HvAI_array[,,i])["p.value"])))
# }
# for (i in rps_ids){
#   print(paste(i, as.character(chisq.test(rps_HvHAI_array[,,i])["p.value"])))
# }
print("fisher test p-values: ")
for (i in rps_ids){
  print(paste(i, as.character(fisher.test(rps_array[,,i])["p.value"])))
}
print("wilcox p-values: ")
for (i in rps_ids){
  print(paste(i, as.character(wilcox.test(rps_array[,,i])["p.value"])))
}
print("CramerV phi: ")
m <- rowSums(rps_array, dims = 2)
cramerV(m)

# print("mcnemar test p-values: ")
# for (i in rps_ids){
#   print(paste(i, as.character(mcnemar.test(rps_array[,,i])["p.value"])))
# }

#display summary
rps_summary <- ddply(rps_all, ~Adversary+Choice_of_Advisor, summarise, Choice_of_Advisor.sum=sum(value), Choice_of_Advisor.mean=mean(value), Choice_of_Advisor.sd=sd(value))
rps_summary

print("-----------PW analysis------------")
# f <- count(pw_cols, id, Adversary, Choice)
# print("Anova")
# rmaModel <- lmer(n ~ Choice +(1|id), data = f)
# anova(rmaModel)



# print(paste("Woolf Test p-value: ", as.character(WoolfTest(rps_array)["p.value"])))
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
pw_summary <- ddply(pw_all, ~Adversary+Choice, summarise, Obs.sum=sum(Obs), Obs.mean=mean(Obs), Obs.sd=sd(Obs), Pred.sum=sum(Pred), Pred.mean=mean(Pred), Pred.sd=sd(Pred))
pw_summary

