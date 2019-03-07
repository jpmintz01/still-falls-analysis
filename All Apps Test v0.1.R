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

#--------------------PW - Machine Learning/NNET section--------------------
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
human_adv_choices <- c(1, 1,	0,	1,	0,	0,	0,	1,	1,	0) #sum in first 9=5
human_tft_choices <- c(1, 1,  1,  0,  1,  0,  0,  0,  1,  1) #sum in first9 = 5
# all_data.human_adv_choices <- subset(subset(subset(subset(subset(subset(subset(subset(subset(all_data,other.decision9==1),other.decision8==1),other.decision7==0),other.decision6==1),other.decision5==0),other.decision4==0),other.decision3==0),other.decision2==1),other.decision1==1)

hai_adv_choices <- c(1,	0,	1,	1,	0,	1,	1,	1,	1,	0) #sum=7
hai_tft_choices <- c(1,	1,	0,	1,	1,	0,	1,	1,	1,	1) #sum in first9 = 7
# all_data.hai_adv_choices <- subset(subset(subset(subset(subset(subset(subset(subset(subset(all_data,other.decision9==1),other.decision8==0),other.decision7==1),other.decision6==1),other.decision5==0),other.decision4==1),other.decision3==1),other.decision2==1),other.decision1==1)

ai_adv_choices <- c(0,	0,	0,	1,	1,	0,	0,	0,	1,	0)#sum=3
ai_tft_choices <- c(1,	0,	0,	0,	1,	1,	0,	0,	0,	1) #sum in first9= 3
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

nn <- neuralnet(f,data=pw_all_data_subset[pw_all_data_subset$period >1,],hidden=c(4,3,2),act.fct = "logistic",linear.output=FALSE)
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

# p<-pw_all_data

# p <- add_column(p, c(as.matrix(q)), .before = "risk") #insert predicted values into p
# colnames(p)[colnames(p) =="c(as.matrix(q))"] <- "outcome"
# p$outcome <- as.factor(p$outcome)
#

pw_all_data <- add_column(pw_all_data, pw_nn_outcome, .before = "risk") #insert nnet values into p
colnames(pw_all_data)[colnames(pw_all_data) =="pw_nn_outcome"] <- "nnet"
pw_all_data$nnet <- as.factor(pw_all_data$nnet)
levels(pw_all_data$nnet) <- c("Peace","War")

pw_all_data <- add_column(pw_all_data, q$value, .after = "nnet") #insert nnet values into p
colnames(pw_all_data)[colnames(pw_all_data) =="q$value"] <- "GLM"
pw_all_data$GLM <- as.factor(pw_all_data$GLM)
levels(pw_all_data$GLM) <- c("Peace","War")

pw_all_data$my.decision <- as.factor(pw_all_data$my.decision)

# 

# row.names <- c("AI","Human","Human+AI")
# col.names <- c("Peace", "War")
# v<-p$nnet
# w<- arrange(pw_cols, id)
# x<- cbind(w, v)
# colnames(x)[colnames(x) =="v"] <- "Predicted"
#can i use xtabs here better than this for-loop nonsense?
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

pw_cols_peace_by_id <- as.data.frame.matrix(xtabs(~ id + Adversary, data = pw_cols[pw_cols$Choice=="Peace",]))
pw_cols_by_id <- as.data.frame(xtabs(~id + Adversary + Choice, data = pw_cols))

#--------------------RPS - Data Transformation-----------------
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




#--------------------Data Write to .csv--------------------
#write.csv(pw_all_data_with_demo, "pw_all_data_with_demo.csv")
# write.csv(rps_all_data_with_demo, "rps_all_data_with_demo.csv")
# write.csv(demo_data, "demo_data.csv")

pw_all_data_with_demo<-read.csv("pw_all_data_with_demo.csv")
pw_all_data_with_demo$X <- NULL
rps_all_data_with_demo<-read.csv("rps_all_data_with_demo.csv")
rps_all_data_with_demo$X <- NULL
demo_data<-read.csv("demo_data.csv")
demo_data$X <- NULL















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




#--------------------PW - Initial Data Analysis-----------------
print("---------------------------------PW analysis----------------------------------")

cat("         --PW: Aggregate Observed Choices--")
print(pw_sum)
# pw_summary <- ddply(pw_all_data_with_demo, ~Adversary+my.decision, summarise, Obs.sum=sum('my.decision'), Obs.mean=mean('my.decision'), Obs.sd=sd('my.decision'), nnet.sum=sum(nnet), nnet.mean=mean(nnet), nnet.sd=sd(nnet))
# print("P-W summary")
# print(pw_summary)
cat("\n         --PW: Normality Tests--")
#tests on all Peace choices across all players
print(shapiro.test(rowSums(pw_cols_peace_by_id)))#test for normality (#normal)

m <- aov(Freq/10 ~ Adversary, data = pw_cols_by_id[pw_cols_by_id$Choice=="Peace",])
print(shapiro.test(residuals(m))) #overall, data not normal
cat("An Shapiro test on the residuals of the peace choices shows the data is NOT NORMALLY DISTRIBUTED.")
print("NOTE: is this residuals test relevant?")


pw_df_1<- as.data.frame(xtabs(~my.decision + Adversary+id, data=pw_all_data_with_demo))
pw_df_1 <- pw_df_1[pw_df_1$my.decision=="Peace",]
p<- ggplot(pw_df_1, aes(x=Freq))+
  geom_density(data=pw_df_1,fill="black", color="black",alpha=0.3)+
  stat_function(fun = dnorm,color="blue",args = list(mean = mean(pw_df_1$Freq), sd = sd(pw_df_1$Freq)),linetype = "dashed")+ labs(title="PW-Peace Density Distribution", x="Frequency", y = "Density of Choices") +scale_x_discrete(limits=c(0:10))+ theme(plot.title = element_text(size=12))
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".pdf"), plot=p, device="pdf")
print(p)
print(shapiro.test(pw_cols_peace_by_id$AI)) #not normal
print(shapiro.test(pw_cols_peace_by_id$Human)) #normal
print(shapiro.test(pw_cols_peace_by_id$`Human+AI`)) #normal
cat("These Shapiro-Wilk tests on the PEace choices by adversary show Human and Human+AI are normally distributed, but the AI Adversary data is not.")

p<- ggplot(pw_df_1, aes(x=Freq))+
  geom_density(data=pw_df_1[pw_df_1$Adversary=="Human",],fill="red", color="red",alpha=0.3)+
  geom_density(data=pw_df_1[pw_df_1$Adversary=="AI",],fill="blue", color="blue",alpha=0.3)+
  geom_density(data=pw_df_1[pw_df_1$Adversary=="Human+AI",],fill="green", color="green", alpha=0.3)+
  stat_function(fun = dnorm,color="blue",args = list(mean = mean(pw_df_1[pw_df_1$Adversary=="AI",]$Freq), sd = sd(pw_df_1[pw_df_1$Adversary=="AI",]$Freq)),linetype = "dashed")+
  stat_function(fun = dnorm,color="red",args = list(mean = mean(pw_df_1[pw_df_1$Adversary=="Human",]$Freq), sd = sd(pw_df_1[pw_df_1$Adversary=="Human",]$Freq)),linetype = "dashed")+
  stat_function(fun = dnorm,color="green",args = list(mean = mean(pw_df_1[pw_df_1$Adversary=="Human+AI",]$Freq), sd = sd(pw_df_1[pw_df_1$Adversary=="Human+AI",]$Freq)),linetype = "dashed") + labs(title="PW - Peace Choices Distribution (by Adversary)",x="Frequency", y = "Density of Choices", color = "Adversary") +scale_x_discrete(limits=c(0:10))+ theme(plot.title = element_text(size=9))
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".pdf"), plot=p, device="pdf")
# print(p)
#

#--------------------PW - Round 1 Data Analyses-----------------
cat("------------------------PW Round 1 Analyses------------------------")

#which one of these is most appropriate to use?
cat("       ----PW: Round 1 Choices (aggregate)----")
print(pw_round_1s_sum)
cat("           ----PW: Round 1 Tests----")
pw_varied_round1 = data.frame("id"=pw_ids,"Varied"=NA)
for (i in pw_ids) {
  pw_varied_round1[pw_varied_round1$id==i,"Varied"] <- !((subset(pw_round_1s, id==i & Adversary == "Human")$Choice == subset(pw_round_1s, id==i & Adversary == "AI")$Choice) & (subset(pw_round_1s, id==i & Adversary == "Human")$Choice == subset(pw_round_1s, id==i & Adversary == "Human+AI")$Choice))
}
print(paste0("RESULT: A simple count of participants showed ",length(pw_varied_round1[pw_varied_round1$Varied,]$id)," (",round(length(pw_varied_round1[pw_varied_round1$Varied,]$id)*100/(length(pw_ids))),"%) participants (",paste(pw_varied_round1[pw_varied_round1$Varied,]$id, collapse=", "),") varied their round 1 choices."))

print(chisq.test(pw_round_1s_sum))
print(paste("RESULT: A Chi-Sq test of aggregate round 1 choices shows IV (Adversary) and DV (Round 1 Choice) are statistically independent at p < .05."))
print(fisher.test(xtabs(~ Adversary + my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$period==1,])))
print(paste("RESULT: A Fisher test of aggregate round 1 choices shows IV (Adversary) and DV (Round 1 Choice) are statistically independent at p < .05."))
print(friedman.test(Choice ~ Adversary | id, data = as.matrix(pw_round_1s))) #not significant
print(paste("RESULT: A Friedman test of round 1 choices shows IV (Adversary) and DV (Round 1 Choice) are statistically independent at p < .05."))
cat("\nNOTE: why count and chisq and fisher and friedman tests? Do I need all of these?\n")


#cochran's Q for PW data?
#Anova by Adversary
m <- glmer(my.decision ~ Adversary+(1|id), data=pw_all_data_with_demo[pw_all_data_with_demo$period==1,], family=binomial)
# print(summary(m))
print(Anova(m, type="3"))
cat("RESULT: A repeated measures logistic regression shows variation on the DV (Round 1 Choice) by the IV (Adversary) is NOT STATISTICALLY SIGNIFICANT at p <.05.")



# these are the players who played "strategically" (not a fixed strategy, and did not indicate non-belief)
# pw_played <- c("yyj35","yda14", "w9c21", "v6i85", "txk36", "n1i23", "i5b85", "h0s22", "b8g12", "b1462", "9mp31", "8mh76", "3z144")
# pw_array_played <- pw_array[,,pw_played]
# pw_pred_array_played <- pw_pred_array[,,pw_played]

#--------------------PW - Strategic Decisions Data Analysis-----------------
print("\n\n\n------------------------PW: Strategic Decision-making Analyses------------------------")
cat("\n       ----PW: Strategic (All round) Choices (aggregate)----")
print(pw_sum)
cat("\n       ----PW: Strategic Tests----")
print(chisq.test(pw_sum))
cat("RESULT: A Chi-Sq test on the aggregate choices shows IV (Adversary) and DV (Aggregate Number of Peace Choices) are statistically dependent at p <.05.\n\n")
print("    --PW: All Rounds: Individual Fisher test p-values: ")
print("fisher test p-values: ")
for (i in pw_ids){
  print(paste(i, as.character(fisher.test(pw_array[,,i])["p.value"])))
}
print(friedman.test(Freq~Adversary|id,data=pw_cols_by_id[pw_cols_by_id$Choice=="Peace",]))
print("RESULT: A Friedman test shows the relationship between variation on the DV (Choice) and variation on the IV (Adversary) is statistically significant at p <.01 (.05). HOWEVER, adversary gameplay is a confounding factor here.")

m <- glmer(my.decision ~ Adversary+(1|id)+(1|period), data=pw_all_data_with_demo, family=binomial (link="logit"))
# print(summary(m))
print(Anova(m, type="3"))
cat("RESULT: A Repeated Measures Logistic Regression shows the relationship between variation on the DV (Choice) and variation on the IV (Adversary) is statistically significant at p < .001 (.05). HOWEVER, note that the adversary gameplay is a factor here.\n")


print("       ---PW: Strategic Gameplay Compared to Predictive Models---")
print(pw_pred_actual_sum)
perc_nnet_eq_actual <- length(which(pw_all_data_with_demo$nnet == pw_all_data_with_demo$my.decision))/nrow(pw_all_data_with_demo)
print(paste0("       Percent NNET Predicted choices == Observed: ", round(perc_nnet_eq_actual*100, 2),"%."))
perc_GLM_eq_actual <- length(which(pw_all_data_with_demo$GLM == pw_all_data_with_demo$my.decision))/nrow(pw_all_data_with_demo)
print(paste0("       Percent GLM Predicted choices == Observed: ", round(perc_GLM_eq_actual*100, 2),"%."))

pw_df <- as.data.frame(xtabs(~period + my.decision +Adversary, data=pw_all_data_with_demo))
pw_df <- pw_df[pw_df$my.decision=="Peace",]
pw_df$period <- as.numeric(pw_df$period)
p<- ggplot(data=pw_df, aes(x=period, y=Freq, group="Adversary"))+  geom_point(aes(color=Adversary)) +
  geom_smooth(method='auto',formula=y~x, aes(group=Adversary, color=Adversary, fill=Adversary), alpha=0.2)+ 
  labs(title="PW - Peace Choices by Round",x="Round", y = "# of Choices", color = "Adversary") +scale_x_discrete(limits=c(1:10))+ theme(plot.title = element_text(size=14), legend.title = element_text(size=9), legend.position = c(0.8, 0.8))#or use method="lm"
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".pdf"), plot=p, device="pdf")
print(p)
print("NOTE: why does round start at 2?")
print("NOTE: depending on whether the graphing method is 'lm' or 'auto'/'loess' the amount of cooperation (Peace Choices) went down similarly (at almost exactly the same rate) against all adversary types, but very similarly to HAI+AI, but worse vs human.  This is unexpected and INTERESTING because the HAI had more cooperation than the human, the AI had much less, so there was clearly something else going on here.  It looks like people have a memory-1 or memory-2 (or a lag-1 or lag-2) decision schema.")
# m <- glmer(my.decision ~ period + Adversary + period:Adversary+ (1|id)+(1|pw_order), data=pw_all_data_with_demo, family=binomial(link="logit"))
# print(summary(m))
# print(Anova(m, type="3"))

cat("\n NOTE: Should I do a wilcox test comparing H-HAI, H-AI?")
wilcox.test(Freq ~ Adversary, data=pw_df_1[pw_df_1$Adversary!="Human+AI",]) #Human-AI comparison
wilcox.test(Freq ~ Adversary, data=pw_df_1[pw_df_1$Adversary!="Human",]) #AI-HAI comparison
wilcox.test(Freq ~ Adversary, data=pw_df_1[pw_df_1$Adversary!="AI",]) #human-HAI comparison



#--------------------PW - Round 1 <-> Strategic Decisions Data Analysis-----------------
print("        ---PW: Is ROUND ONE decision correlated to STRATEGIC DECISIONS?")

print(xtabs(~ my.round1decision + my.decision, data = pw_all_data_with_demo))
print(chisq.test(xtabs(~ my.round1decision + my.decision, data = pw_all_data_with_demo)))
print("RESULT: A Chi-sq test on the aggregate Peace decisions and round 1 decisions shows the IV(Round 1 decision) and DV (aggregate sum of Peace Choices across all adversaries) are statistically dependent at p < .0001 (.05), so we conduct a RMLR...")
cat("        NOTE: Should this be a Chisq or a Fisher test?\n")

m <- glmer(my.decision ~ my.round1decision+(1|id)+(1|pw_order), data=pw_all_data_with_demo, family=binomial)
# print(summary(m))
print(Anova(m, type="3"))
cat("???RESULT: A Repeated Measures Logisitic Regression (RMLR) shows that compared to my.decision1 and other.decision1, ROUND1 decision is a FINDING OF STATISTICALY INTEREST (p <.10) but is NOT STATISTICALLY SIGNIFICANT at p <.05.")

m <- glmer(my.decision ~ Adversary+my.decision1+other.decision1+my.round1decision+(1|id)+(1|pw_order), data=pw_all_data_with_demo, family=binomial)
# print(summary(m))
print(Anova(m, type="3"))
cat("RESULT: A Repeated Measures Logisitic Regression (RMLR) shows that when analyzed along with Adversary, my.decision1, and other.decision1, ROUND1 decision is a FINDING OF STATISTICALY INTEREST (p <.10) but is NOT STATISTICALLY SIGNIFICANT at p <.05.")




pw_df <- as.data.frame(xtabs(~period + my.decision +my.round1decision, data=pw_all_data_with_demo))
pw_df <- pw_df[pw_df$my.decision=="Peace",]
pw_df$period <- as.numeric(pw_df$period)
p<- ggplot(data=pw_df, aes(x=period, y=Freq, group=my.round1decision))+  geom_point(aes(color=my.round1decision))+geom_smooth(method='lm',formula=y~x, aes(group=my.round1decision, color=my.round1decision, fill=my.round1decision), alpha=0.2)+ labs(title="PW - Peace Choices by Round 1 Choice",x="Round", y = "# of Choices", color = "my.round1decision") +scale_x_discrete(limits=c(1:10))+ theme(plot.title = element_text(size=14), legend.title = element_text(size=9), legend.position = c(0.8, 0.8))#or use method="lm instead of auto"
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".pdf"), plot=p, device="pdf")
# print(p)
print("RESULT: From the plot, it appeares the group of participants who chose War (0 in this plot) had little variance across rounds, but that the initial cooperator-group (Peace, 1 in this graph) varied markedly. So, we analyze it by adversary...")
print(xtabs(~ Adversary + my.round1decision + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + my.round1decision + my.decision, data = pw_all_data_with_demo)[,,1]))
print("RESULT: A Chi-Sq test of Aggregate peace decisions by adversary and round1 choice shows variation in (Round1 decision) Peacefulness (by adversary) and Round 1 decision is statistically dependent at p <.05. So, we segregate the data by round 1 defectors (chose 'WAR') and round 1 cooperators (chose 'Peace')...")

print(" -- Aggregate choices of Round 1 Defectors")
print(xtabs(~Adversary+my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==0,]))
pw_r1_defectors <- as.data.frame(xtabs(~id+Adversary+my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==0,]))
print(friedman.test(Freq~Adversary|id,data=pw_r1_defectors[pw_r1_defectors$my.decision=="Peace",]))
print("NOTE: INTERESTING:")
print("RESULT: A Friedman test shows that those that Round 1 defectors (chose WAR on the first round) DID NOT SHOW a statistically significant difference in gameplay by adversary at p <.05. On the other hand,")


print(" -- Aggregate choices of Round 1 cooperators")
print(xtabs(~Adversary+my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==1,]))
pw_r1_cooperators <- as.data.frame(xtabs(~id+Adversary+my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==1,]))
print(friedman.test(Freq~Adversary|id,data=pw_r1_cooperators[pw_r1_cooperators$my.decision=="Peace",]))
print("RESULT: A friedman test shows that those that chose PEACE on the first round DID SHOW a statistically significant difference in gameplay by adversary at p <.01 (p <.05). So, we do a RMLR")

m <- glmer(my.decision ~ Adversary+my.decision1+other.decision1+my.round1decision+(1|id)+(1|pw_order), data=pw_all_data_with_demo, family=binomial)
print(Anova(m, type="3"))
cat("\n---PW: Repeated Measures Logistic Regression on ROUND ONE DEFECTORS---\n")
m <- glmer(my.decision ~ Adversary+my.decision1+other.decision1+(1|id)+(1|pw_order), data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==0,], family=binomial)
print(summary(m))
print(Anova(m, type="3"))
cat("RESULT: A RMLR on ROUND ONE DEFECTORS showed initial propensity and the participant's previous choice indicate statistical significance (p <.05).\n       INTERESTING: Adversary seems to have the least effect despite the difference in adversary gameplay.")

print("---PW: RMLR on ROUND ONE COOPERATORS---")
m <- glmer(my.decision ~ Adversary+my.decision1+other.decision1+(1|id)+(1|pw_order), data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==1,], family=binomial)
print(summary(m))
print(Anova(m, type="3"))
cat("RESULT: A RMLR on ROUND ONE COOPERATORS showed that the participant's previous choice, followed by the adversary's decision, followed by initial propensity, show statistical significance at p < .05).\n      INTERESTING:Adversary shows finding of statistical interest (p< .10) but isn't significant at p < .05, despite the difference in adversary gameplay.\n       ALSO: looks like the two greatest factors in the model are initial propensity mediated by participant's previous decision.")
cat("\nHELP: Would like to know if other.decision2/3/4 (history) matters, but how do I code this? \n  I keep getting 'rank deficient' error if I include my.decision2 or other.decision2\n")



pw_df <- as.data.frame(xtabs(~period + my.decision +Adversary, data=pw_all_data_with_demo[pw_all_data_with_demo$my.round1decision==1,]))
pw_df <- pw_df[pw_df$my.decision=="Peace",]
pw_df$period <- as.numeric(pw_df$period)
p<- ggplot(data=pw_df, aes(x=period, y=Freq, group=Adversary))+  
  geom_point(aes(color=Adversary))+
  geom_smooth(method='auto',formula=y~x, aes(group=Adversary, color=Adversary, fill=Adversary), alpha=0.2)+ 
  labs(title="PW-Round 1 Cooperators:\nPeace Choices by Round\nand Adversary",x="Round", y = "# of Choices", color = "Adversary") +scale_x_discrete(limits=c(1:10))+ theme(plot.title = element_text(size=14), legend.title = element_text(size=9), legend.position = c(0.8, 0.8))#or use method="lm instead of auto"
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".pdf"), plot=p, device="pdf")
# print(p)
print("NOTE: Interesting that changes across rounds for Human+AI and AI are very similar, despite very different gameplay from the adversary.  This is in contrast to the human advesrary.")
print("NOTE: Is it possible that participants' strategy against a human adversary are memory-1 and something else vs AI or HAI?  THIS would be an important and relevant finding. You could explore this using a GLM with my.decision1 and other.decision1 as the main factors to see what the coefficients are (my.round1decision as an interaction with all variables?).  It's interesting that the initial amount of cooperation ")



#--------------------PW - Time <-> Strategic Decision Analysis-----------------
print(" ---PW: Is BY-PAGE DECISION TIME correlated to decision-making?")
m <- glmer(my.decision ~ my.decision1+other.decision1+seconds_on_page+ (1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
# print(summary(m))
print(Anova(m))
cat("RESULT: A RMLR showed BY-PAGE DECISION TIME did not show a statistically significant correlation to decision at p < .05.\n")

print("   ---PW: Is BY-PAGE DECISION TIME related to Adversary type?")
m <- lm(seconds_on_page ~ Adversary, data=pw_all_data_with_demo)
summary(m)
Anova(m)
cat("A one-way Repeated Measures ANOVA shows relationship between decision time and Adversary is not statistically significant at p < .05.\nHELP: WHY DOESN'T 'AI' Adversary SHOW UP ON ANY OF THESE? \nHELP: Is this the correct way to do a 1-way RM Anova?\n")

#--------------------PW - Counterbalancing Data Analysis-----------------
print("       ---PW: COUNTERBALANCING: Is P-W PLAY ORDER related to number of peace propensity (across all adversaries)--")
print("         ----PW: counterbalancing ROUND ONE choices by PW play order")
m <- glmer(my.decision ~ pw_order + (1|id), data=pw_all_data_with_demo[pw_all_data_with_demo$period==1,], family=binomial(link="logit"))
print(Anova(m))
print("RESULT: A RMLR on ROUND ONE decisions and pw_order DID NOT show a statistically significant difference at p <.05.")

print("         ----PW: counterbalancing STRATEGIC (all round) choices by PW play order")
print(xtabs(~ pw_order + my.decision, data = pw_all_data_with_demo))
print(chisq.test(xtabs(~ pw_order + my.decision, data = pw_all_data_with_demo)))
print("RESULT: A Chi-Sq test on  pw_order and aggregate number of Peace/War choices showed NO STATISTICALLY SIGNIFICANT relationship at p <.05 ")
n_1<-(xtabs(~ id+ my.decision, data = pw_all_data_with_demo[pw_all_data_with_demo$pw_order==1,]))
n_2<-(xtabs(~ id+ my.decision, data = pw_all_data_with_demo[pw_all_data_with_demo$pw_order==2,]))
n_1<- n_1[,1]/30
n_2 <- n_2[,1]/30
n<-wilcox.test(n_1,n_2)
n["data.name"] <- "Played PW First and Played PW Second"
print(n)
print("RESULT: A Wilcox test on Order of Play (first or second) and total 'peacefulness' (number of Peace choices) did NOT SHOW A STATISTICALLY SIGNIFICANT result at p <.05")
print("HELP: Which test (chisq on aggregate or wilcox on 'by-id') is more appropriate? Probably the latter")


n_1<-as.data.frame(xtabs(~ id+Adversary+my.decision, data = pw_all_data_with_demo[pw_all_data_with_demo$pw_order==1,])[,,1])
n_2<-as.data.frame(xtabs(~ id+Adversary+my.decision, data = pw_all_data_with_demo[pw_all_data_with_demo$pw_order==2,])[,,1])
p<- ggplot(n_1, aes(x=Freq))+
  geom_density(data=n_1[n_1$Adversary=="Human",],fill="red", color="red",alpha=0.2, linetype="dashed")+
  geom_density(data=n_1[n_1$Adversary=="AI",],fill="blue", color="blue",alpha=0.2, linetype="dashed")+
  geom_density(data=n_1[n_1$Adversary=="Human+AI",],fill="green", color="green", alpha=0.2, linetype="dashed")+
  geom_density(data=n_2[n_2$Adversary=="Human",],fill="red", color="red",alpha=0.3)+
  geom_density(data=n_2[n_2$Adversary=="AI",],fill="blue", color="blue",alpha=0.3)+
  geom_density(data=n_2[n_2$Adversary=="Human+AI",],fill="green", color="green", alpha=0.3) + labs(title="PW-Peace Choices Distribution (by PW Play order & Adversary)",x="Frequency", y = "Density of Choices", color = "Adversary") +scale_x_discrete(limits=c(0:10))+ theme(plot.title = element_text(size=8))
print(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".pdf"), plot=p, device="pdf")
# print(p)
print("RESULT: See PLOT: A visual inspection of the plot shows the AI means are similar,the human means are similar, but the AI-assisted human means are very different between those who played PW first and those who played RPS first, so,,,,")


print("    -- PW: Aggregate Peace choices by adversary and pw_order")
print(xtabs(~ Adversary + pw_order + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + pw_order + my.decision, data = pw_all_data_with_demo)[,,1]))
print("RESULT: A Chi-Sq test of Peace choices by Adversary and pw_order DID NOT show a statistically significant relationship at p <.05. ")


print(chisq.test(xtabs(~ Adversary + my.decision, data = pw_all_data_with_demo[pw_all_data_with_demo$pw_order==1,]))) 
print("RESULT: A Chi-Sq test of Peace choices by Adversary for those who PLAYED PW FIRST DID SHOW a statistically significant relationship at p <. 05.")
print(chisq.test(xtabs(~ Adversary + my.decision, data = pw_all_data_with_demo[pw_all_data_with_demo$pw_order==2,]))) 
print("RESULT: A Chi-Sq test of Peace choices by Adversary for those who PLAYED PW SECOND did NOT show a statistically significant relationship at p <. 05.")
cat("RESULT: Participants who played PW first showed a statistically significant difference in warlike-ness by adversary (as expected).\n  HOWEVER, participants who played PW second DID NOT show a statistically significant difference in Peace choices by adversary, even through the adversaries' gameplay varied significantly. \n        NOTE: Was there something about playing RPS first that made participants more warlike toward the Human+AI adversary?")

# n_1<-as.data.frame(xtabs(~ id+Adversary+my.decision, data = pw_all_data_with_demo[pw_all_data_with_demo$pw_order==1,])[,,1])
# n_2<-as.data.frame(xtabs(~ id+Adversary+my.decision, data = pw_all_data_with_demo[pw_all_data_with_demo$pw_order==2,])[,,1])
# n_1$pw_order <- 1
# n_2$pw_order <- 2
# n_3 <- rbind(n_1, n_2)
# n_3$pw_order <- factor(n_3$pw_order)
# contrasts(n_3$pw_order) <- "contr.sum"
# m <- multinom(Freq~pw_order+Adversary+pw_order:Adversary, data=n_3)
# # print(summary(m))
# print(Anova(m, type=3))
# print("NOTE: Am I doing this anova right?  Checking for variance on Freq based on Adversary & pw order")
# print("or, should it be like this:")
m <- glmer(my.decision ~ Adversary+ pw_order + pw_order:Adversary+ (1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
# print(summary(m))
print(Anova(m))
print("RESULT: A RMLR on my.decision by adversary and pw_order shows the relationship between Counterbalancing order and decision by adversary is NOT statistically significant at p < .05")





#--------------------PW - Demographic <-> Decision-making Data Analysis-----------------
print("--------------- PW<->Demo Analysis--------------- ")
print("")
print("NOTE: Are tests of proportions the correct tests here? Shouldn't I use some kind of analysis of variance?")
print("OR, should I use the repeated measures logistic regression with all the demographic variables included, like this:")

m <- glmer(my.decision ~ gender + age + years_military_experience + service + school + machine_learning_experience+game_theory_experience + (1|id), data=pw_all_data_with_demo, family=binomial(link="logit"))
print("What does the failed to converge warning mean?")
print(Anova(m))

print("OR, should I have all the demographic variables included as random effects - if so, how do I do this to see if the random effects are actually random?")
cat("\n")


print("Chi-sq (aggregate) on whether GENDER affected decision-making, by adversary")
print(xtabs(~ Adversary + gender + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + gender + my.decision, data = pw_all_data_with_demo)[,,1]))

print("Chi-sq (aggregate) on whether SERVICE affected decision-making, by adversary")
print(xtabs(~ Adversary + service + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + service + my.decision, data = pw_all_data_with_demo)[,,1]))

print("Chi-sq (aggregate) on whether RANK affected decision-making, by adversary")
print(xtabs(~ Adversary + rank + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + rank + my.decision, data = pw_all_data_with_demo)[,,1]))


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



print("Chi-sq (aggregate) on whether GAME THEORY EXPERIENCE  affected decision-making, by adversary")
print(xtabs(~ Adversary + game_theory_experience + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + game_theory_experience + my.decision, data = pw_all_data_with_demo)[,,1])) #not significant

print("Chi-sq (aggregate) on whether AI EXPERIENCE  affected decision-making, by adversary")
print(xtabs(~ Adversary + machine_learning_experience + my.decision, data = pw_all_data_with_demo)[,,1])
print(chisq.test(xtabs(~ Adversary + machine_learning_experience + my.decision, data = pw_all_data_with_demo)[,,1])) #not significant
























#--------------------RPS - Initial Data Analysis-----------------
print("---------------------------------RPS analysis----------------------------------")

print(rps_sum <- xtabs(~Adversary+Choice_of_Advisor, data=rps_all_data_with_demo))
# print(rps_prop <- round(prop.table(rps_sum)*100, 1))


print("--RPS: Normality tests (by advisor type)--")
print(shapiro.test(rps_long_by_id[rps_long_by_id$Choice_of_Advisor=="human",]$Freq)) #not normal
print(shapiro.test(rps_long_by_id[rps_long_by_id$Choice_of_Advisor=="AI",]$Freq))  #not normal
print(shapiro.test(rps_long_by_id[rps_long_by_id$Choice_of_Advisor=="none",]$Freq)) #not normal


rps_df_1<- as.data.frame(xtabs(~Choice_of_Advisor + Adversary+id, data=rps_all_data_with_demo))
# rps_df_1$pois <- rpois(nrow(rps_df_1), lambda=mean(rps_df_1$Freq))
p<-ggplot(data=rps_df_1,aes(x=Freq, group=Choice_of_Advisor, color=Choice_of_Advisor,fill=Choice_of_Advisor)) + 
  #geom_bar(aes(y = ..count../sum(..count..)), alpha = 0.5) +
  geom_density(alpha=0.5)+#, stat="count")+#how to stop the exponential at zero?
  #+ geom_line(aes(y=rps_df_1$pois))
  labs(title="RPS - Choice Frequency Distribution", x="Frequency", y = "Density of Choices", color = "Choice_of_Advisor") +scale_x_discrete(limits=c(0:10))+ theme(plot.title = element_text(size=12))
print(p)
cat(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".pdf"), plot=p, device="pdf")
print("RESULT: See PLOT: What kind of distributions are these?")


p<- ggplot(rps_df_1[rps_df_1$Adversary=="Human",], aes(Freq, fill=Choice_of_Advisor))+
  geom_density(alpha=0.4)+
  labs(title="RPS- vs Human - Choice Frequency Distribution by Round", x="Frequency", y = "Density of Choices", color = "Choice_of_Advisor") +scale_x_discrete(limits=c(0:10))+ theme(plot.title = element_text(size=10))
print(p)
q <- ggplot(rps_df_1[rps_df_1$Adversary=="Human+AI",], aes(Freq, fill=Choice_of_Advisor))+
  geom_density(alpha=0.4)+
  labs(title="RPS- vs AI-Assisted Human - Choice Frequency Distribution by Round", x="Frequency", y = "Density of Choices", color = "Choice_of_Advisor") +scale_x_discrete(limits=c(0:10))+ theme(plot.title = element_text(size=10))
print(q)
r<- ggplot(rps_df_1[rps_df_1$Adversary=="AI",], aes(Freq, fill=Choice_of_Advisor))+
  geom_density(alpha=0.4)+
  labs(title="RPS- vs AI - Choice Frequency Distribution by Round", x="Frequency", y = "Density of Choices", color = "Choice_of_Advisor") +scale_x_discrete(limits=c(0:10))+ theme(plot.title = element_text(size=10))
print(r)
# cat(paste0("Insert ", p$labels$title," Plot"))
# ggsave(paste0(p$labels$title,".pdf"), plot=p, device="pdf")
print("RESULT: See PLOTs: What kind of distribution are these?")

print("RPS: Choice of Advisor (regardless of Adversary)") #does the group show a propensity to choose one advisor over the others (or two over the third)? 
print(xtabs(~Choice_of_Advisor, data = rps_all_data_with_demo))
print("This x-tabs show that participants, in aggregate, chose AI advice over human advice, but chose 'no advice' over any advice.")
#--------------------RPS - Aggregation of Choice of Advisor by Adversary - Data Analysis-------------
#video #9 (tests of proportions)
print("RPS: Aggregate: Tests of proportions on AGGREGATE choices (by Adversary")
print(df <- xtabs(~ Adversary + Choice_of_Advisor, data=rps_all_data_with_demo))
ggplot(as.data.frame(xtabs(~id + Adversary + Choice_of_Advisor, data = rps_all_data_with_demo)), aes(x=Adversary, y=Freq, color=Choice_of_Advisor)) +
  geom_boxplot()
chisq.test(xtabs(~ Adversary + Choice_of_Advisor, data=rps_all_data_with_demo)) 
GTest(xtabs(~ Adversary + Choice_of_Advisor, data=rps_all_data_with_demo)) #more accurate than Chisq?
fisher.test(xtabs(~ Adversary + Choice_of_Advisor, data=rps_all_data_with_demo))#is exact test more appropriate than chisq.test here
print("RESULT: A Chi-sq, G-test, and fisher test all show Adversary type is statistically independent at p < .05.")
cat("?Cochran Q test relevant here?\n\n")


print("RPS: All: Friedman test of variance in choice of advisor by adversary")
rps_long_by_id <- as.data.frame(xtabs(~id+Adversary+Choice_of_Advisor, data=rps_all_data_with_demo))
print(rps_sum)
print(friedman.test(rps_sum)) # need to check if this needs to be transposed (t(rps_sum))
friedman.test(Freq ~ Adversary|id, data=rps_long_by_id[rps_long_by_id$Choice_of_Advisor=="none",])
# am I doing this right? or should it be like this: friedman.test(Freq ~ Choice_of_Advisor|id, data=rps_long_by_id[rps_long_by_id$Adversary=="AI",])
friedman.test(Freq ~ Adversary|id, data=rps_long_by_id[rps_long_by_id$Choice_of_Advisor=="AI",])
friedman.test(Freq ~ Adversary|id, data=rps_long_by_id[rps_long_by_id$Choice_of_Advisor=="human",])
print("RESULT: A 3-factor Friedman Test showed no statistically significant variation on the summed Frequency of choices caused by differences in adversary, at p < .05")

print(" can't do a RMLR because choice is 3-level? glmer(Choice_of_Advisor ~ Adversary + Round + (1|id), data=rps_all_data_with_demo, family=binomial)")

cat("\n RPS: RMLR:")
print("HELP: Am I doing the comparison of whether Adversary causes variation in the change across rounds?")
cat("\n human-AI choice comparison")
print(Anova(glmer(Choice_of_Advisor ~ Adversary + Round + (1|id), data=rps_all_data_with_demo[!rps_all_data_with_demo$Choice_of_Advisor=="none",], family=binomial), type="3"))
cat("\n none-AI choice comparison")
print(Anova(glmer(Choice_of_Advisor ~ Adversary + Round + (1|id), data=rps_all_data_with_demo[!rps_all_data_with_demo$Choice_of_Advisor=="human",], family=binomial), type="3"))
cat("\n human-none choice comparison")
print(Anova(glmer(Choice_of_Advisor ~ Adversary + Round + (1|id), data=rps_all_data_with_demo[!rps_all_data_with_demo$Choice_of_Advisor=="AI",], family=binomial), type="3"))
print("??RESULT: A multiple RMLR (should use multivariate repeated measures logistic regression if possible), showed Adversary did not cause a change across rounds, at p <.05.??")

#--------------------RPS - Decisions by Individual - Data Analysis-----------------
# "Chi Squared for individual participants -- not valid because some expected values are < 5"
#using fisher test instead)
print ("RPS: Indidividual ID's which showed difference in choices by Adversary using Fisher Test: (since Chisq invalid with some expected value cells=0?")
rps_ids <- unique(rps_all_data_with_demo$id)
rps_fisher_test <- data.frame("id"=rps_ids,"All"=NA)
#"fisher test p-values
rps_array <- xtabs(~Adversary + Choice_of_Advisor + id, data=rps_all_data_with_demo)
for (i in rps_ids){
  rps_fisher_test[rps_fisher_test$id==i,"All"] <- fisher.test(rps_array[,,i])["p.value"]<0.05
}
d<- rps_fisher_test[rps_fisher_test$All=="TRUE",]$id
print(paste0("RESULT: A Fisher test of individual sum choices (across rounds) showed ", length(d), " participant(s) (",paste(d, collapse=", "),") showed a statistically significant variation in their advisor choices by adversary, at p < .05"))



print ("RPS ID's which showed difference in choices by Adversary using Friedman Test")
rps_ids <- unique(rps_all_data_with_demo$id)
rps_friedman_test <- data.frame("id"=rps_ids,"All"=NA)
#"fisher test p-values
rps_array <- xtabs(~Adversary + Choice_of_Advisor + id, data=rps_all_data_with_demo)
for (i in rps_ids){
  rps_friedman_test[rps_friedman_test$id==i,"All"] <- friedman.test(rps_array[,,i])["p.value"]<0.05
}
e<-rps_friedman_test[rps_friedman_test$All=="TRUE",]$id
print(paste0("RESULT: A Friedman test of individual sum choices (across rounds) showed ", length(d), " participant(s) (",paste(e, collapse=", "),") showed a statistically significant variation in their advisor choices by adversary, at p < .05"))

print("HELP: It seems odd that the fisher test showed 7 participants, the friedman test showed 9, and none of the participants are the same. Why are these results mutually exclusive?")


#--------------------RPS - Decisions by Round - Data Analysis-----------------
print("RPS: Choice of Advisor by round (all Adversaries)")
df <- as.data.frame(xtabs(~Round + Choice_of_Advisor, data=rps_all_data_with_demo))
p<- ggplot(data=df, aes(x=Round, y=Freq, group=Choice_of_Advisor))+  
  geom_line(aes(color=Choice_of_Advisor))+  
  geom_point(aes(group=Choice_of_Advisor, color=Choice_of_Advisor)) + 
  labs(title="RPS - Choice of Advisor by Round (all Adversaries)",x="Round", y = "# of Choices", color = "Advisor")+
  geom_smooth(method='glm',formula=y~x, aes(group=Choice_of_Advisor, color=Choice_of_Advisor))
print(p)
cat(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".pdf"), plot=p, device="pdf")
print("RESULT: See PLOT: Visual inspection shows advice-taking seems to increase over time (both human and AI), and self-reliance ('none' choice) declined over time.")

print(friedman.test(Freq~Choice_of_Advisor|Round, data=df))
print("Is this (print(friedman.test(Freq~Choice_of_Advisor|Round, data=df)) the right way to see if Choice_of_Advisor varies by round?")
print("Or should it be: print(friedman.test(Freq~Round|Choice_of_Advisor, data=df))")

print(friedman.test(xtabs(~ id + Choice_of_Advisor, data = rps_all_data_with_demo))) #is this the right test to answer that question? or should it be like this:


m <- glmer(Choice_of_Advisor ~ Round*Adversary+(1|id)+(1|rps_order), data=rps_all_data_with_demo, family=binomial)
print(summary(m))
print(Anova(m, type="3"))
print("HELP: How do I code this to determine whether Choice_of_Advisor varied across rounds, by Adversary? (i.e.compare (the slope of change across rounds) across adversaries")
print("RESULT: ")


df_1 <- as.data.frame(xtabs(~Round + Choice_of_Advisor+Adversary, data=rps_all_data_with_demo))
p<- ggplot(data=df_1[df_1$Adversary=="Human",], aes(x=Round, y=Freq, group=Choice_of_Advisor))+  
  geom_line(aes(color=Choice_of_Advisor))+  
  geom_point(aes(group=Choice_of_Advisor, color=Choice_of_Advisor))+ 
  labs(title="RPS- vs Human - Choices by Round",x="Round", y = "# of Choices", color = "Advisor")+
  geom_smooth(method='glm',formula=y~x,aes(group=Choice_of_Advisor, color=Choice_of_Advisor))
print(p)
cat(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".pdf"), plot=p, device="pdf")
print("RESULT: See PLOT: Against the Human, human Advice-taking stayed approximately stable, but self-reliance('none') advice declined and was replaced by AI advisor.")




p<- ggplot(data=df_1[df_1$Adversary=="AI",], aes(x=Round, y=Freq, group=Choice_of_Advisor))+  
  geom_line(aes(color=Choice_of_Advisor))+  
  geom_point(aes(group=Choice_of_Advisor, color=Choice_of_Advisor))+ 
  labs(title="RPS- vs AI - Choices by Round",x="Round", y = "# of Choices", color = "Advisor") +geom_smooth(method='glm',formula=y~x,aes(group=Choice_of_Advisor, color=Choice_of_Advisor))
print(p)
cat(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".pdf"), plot=p, device="pdf")
print("RESULT: See PLOT: Against the AI, AI Advice-taking stayed approximately stable, but self-reliance('none') advice declined and was replaced by human advisor")

p<- ggplot(data=df_1[df_1$Adversary=="Human+AI",], aes(x=Round, y=Freq, group=Choice_of_Advisor))+  
  geom_line(aes(color=Choice_of_Advisor))+  
  geom_point(aes(group=Choice_of_Advisor, color=Choice_of_Advisor)) + 
  labs(title="RPS- vs Human+AI - Choices by Round",x="Round", y = "# of Choices", color = "Advisor")+
  geom_smooth(method='glm',formula=y~x,aes(group=Choice_of_Advisor, color=Choice_of_Advisor))
print(p)
cat(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".pdf"), plot=p, device="pdf")
print("RESULT: See PLOT: Against the AI-assisted Human, Advice-taking from AI and human advisors increased across rounds at roughly the same rate, and self-reliance('none') declined markedly ")

cat("\nRESULT: A Visual analysis of AGGREGATE choice data by adversary across rounds showed: \n1) Against the adversaries with AI (AI and AI-assisted human), the aggregate use of the human advisor went up across rounds, but against the human adversary, use of a human advisor was low and stayed low.\n2) more here...  ")
#--------------------RPS - Counterbalancing - Data Analysis-----------------
#check for correlation with counterbalancing
rps_all_data_with_demo[rps_all_data_with_demo$id==rps_fisher_test[rps_fisher_test$All=="TRUE",]$id,]$pw_order# Not clearly correlated with pw_order
rps_all_data_with_demo[rps_all_data_with_demo$id==rps_fisher_test[rps_fisher_test$All=="TRUE",]$id,]$rps_order #not clearly correlated with rps_order

#check for correlation with counterbalancing
rps_all_data_with_demo[rps_all_data_with_demo$id==rps_friedman_test[rps_friedman_test$All=="TRUE",]$id,]$pw_order# Not clearly correlated with pw_order
rps_all_data_with_demo[rps_all_data_with_demo$id==rps_friedman_test[rps_friedman_test$All=="TRUE",]$id,]$rps_order #not clearly correlated with rps_order
#--------------------RPS - Decision Time <-> Choices - Data Analysis-----------------
#--------------------RPS - Demographics <-> Choices - Data Analysis-----------------
print("--NOTE: how to analyze demo variables?  Probably similar answer to PW")
print("Chi-sq (aggregate) on whether AI EXPERIENCE  affected decision-making, by adversary")
print(xtabs(~ Adversary + machine_learning_experience + Choice_of_Advisor, data = rps_all_data_with_demo)[,,1])
print(fisher.test(xtabs(~ Adversary + machine_learning_experience + Choice_of_Advisor, data = rps_all_data_with_demo)[,,1])) #not significant

print("shouldn't these demographic tests be 'by id' like an anova or something?")

#--rps time Analysis
p<- ggplot(rps_all_data_with_demo, aes(x=seconds_on_page))+   geom_density(data=rps_all_data_with_demo[rps_all_data_with_demo$Adversary=="Human",],fill="red", color="red",alpha=0.3)+   geom_density(data=rps_all_data_with_demo[rps_all_data_with_demo$Adversary=="Human+AI",],fill="blue", color="blue",alpha=0.3)+   geom_density(data=rps_all_data_with_demo[rps_all_data_with_demo$Adversary=="AI",],fill="green", color="green",alpha=0.3)+ scale_x_continuous(limits = c(0, 25))+labs(title="RPS-Decision Time Density Plot (by Adversary)", x="Seconds per Decision", y = "Density", color = "Adversary type")  #+ scale_color_manual(values = c('Human' = 'red', 'Human+AI' = 'blue', "AI" = 'green'))
cat(paste0("Insert ", p$labels$title," Plot"))
ggsave(paste0(p$labels$title,".pdf"), plot=p, device="pdf")
print(p)
print("RESULT: See PLOT: ")


#--------------------Multi-Game - Data Analyses-----------------
print("--------------------Multi-Game Analyses-----------------")

#played "non-strategically"
a<-as.data.frame(t(colSums(pw_array)))
a <- rownames(a[a$Peace %in% c(0,30),])
print(paste0(length(a), " participants (",paste(a, collapse=", "),") played the exact same choice for all rounds in Peace-War."))
b<- as.data.frame(t(colSums(rps_array)))
b <- rownames(b[b$AI %in% c(0,30),])
print(paste0(length(b), " participants (",paste(b, collapse=", "),") played the exact same choice for all rounds in Rock-Paper-Scissors."))
c <- intersect(b,a)
print(paste0(length(c), " participants (",paste(c, collapse=", "),") played the exact same choice for all rounds in both Peace-War and Rock-Paper-Scissors."))

#find the round1 participants who chose differently across adversaries in round 1
d<- xtabs(~id + Adversary + my.decision, data=pw_all_data_with_demo[pw_all_data_with_demo$period==1,])
d<- d[,,"Peace"]
d<- rowSums(d)
d<-names(d[d %in% c(1,2)])
print(paste0(length(d), " participant(s) (",paste(d, collapse=", "),") varied their choice by adversary in Peace-War round 1."))

e <- rps_fisher_test[rps_fisher_test$All,]$id
print(paste0(length(e), " participant(s) (",paste(e, collapse=", "),") varied their choice by adversary in Rock-Paper-Scissors."))
f<- intersect(d,e)
print(paste0(length(f), " participant(s) (",paste(f, collapse=", "),") varied their choice by adversary in BOTH Peace-War round 1 and in Rock-Paper-Scissors."))


