library(catR)
stimulus_dir <-
  "http://52.17.155.167/concerto4/media/stimuli/BAT/v1/stimuli_v1"
example_stimulus_dir <- "http://52.17.155.167/concerto4/media/stimuli/BAT/v1/BP_training"

concerto.template.show("Adaptive BAT - intro",
                       workspaceID = 2)

#	Starting parameters
if(!exists("test_length")) {test_length <- 15}
if(!exists("take_training")) {take_training <- TRUE}
if(!exists("admin")) {admin <- FALSE}
if(!exists("partial_difficulty_mode")) {partial_difficulty_mode <- TRUE}
if(!exists("give_feedback")) {give_feedback <- TRUE}

show.chart <- 0

if(admin){
  out <- concerto.template.show("Training check", workspaceID = 9)$LAST_PRESSED_BUTTON_NAME
  if(out == "btn_yes") {take_training <- 1} else {take_training <- 0}
  
  out <- concerto.template.show("Graph check", workspaceID = 2)$LAST_PRESSED_BUTTON_NAME
  if(out == "btn_yes") {show.chart <- 1} else {show.chart <- 0}
  
  out <- concerto.template.show("Num questions check", workspaceID = 2)
  out$num_questions <- as.numeric(out$num_questions)
  print(out$num_questions)
  if(is.na(out$num_questions)) {test_length <- 25} else {
    if(out$num_questions > 25) {test_length <- 25} else {test_length <- out$num_questions}}
  print(out$num_questions)
  print(class(out$num_questions))
}

#	Get the item bank

item.bank <- concerto.table.query(
  sql=paste("
            SELECT
            *
            FROM `concerto_2`.`BAT - item bank`
            ",sep=""),
  params=list()
  )

##	Set up IRT model

#	Item parameters
discriminations <- item.bank$discrimination
if(partial_difficulty_mode){
  difficulties <- item.bank$difficultypartial} else {
    difficulties <- item.bank$difficulty}
guessings <- 0.5
inattentions <- 1

itemPar <- data.frame(discriminations, difficulties, guessings, inattentions)

#	Content balancing
cbGroup <- item.bank$audio_name

cbControlNames <- names(table(cbGroup))
cbControlProps <- rep(x = 1/(length(cbControlNames)), times = length(cbControlNames))

cbControl <- list(names = cbControlNames, props = cbControlProps)

#	Ability
theta_BM <- "Ability estimate not available yet"

##	Misc.

admin.info <- ""

#	was the participant wrong or right, on each question
score.vector.test.order <- NULL

music.track.list <- NULL

##########################################

#	The following are also monitored

#	time spent on last question
time.taken <- "NA"

#	time participant takes on each question
time.vector.test.order <- NULL

#	what button did the participant press for each question
response.vector.test.order <- NULL

#########################################

#	Declare the "please select an answer" variable
check_response <- ""

items.administered <- NULL

# 	Set the first question (low difficulty)
current.item <- 1

theta_BM <- 0
theta.vector <- 0

#########################################

#	Training
while(take_training) {
  
  concerto.template.show("Adaptive BAT - Training 1", workspaceID=2)
  
  concerto.template.show("Adaptive BAT - Training 2", workspaceID=2,
                         params = list(audio_src = file.path(example_stimulus_dir, "training1.mp3")))
  
  concerto.template.show("Adaptive BAT - Training 3", workspaceID=2,
                         params = list(audio_src = file.path(example_stimulus_dir, "training2.mp3")))
  
  concerto.template.show("Adaptive BAT - Training 4", workspaceID=2)
  
  #	Show first practice question
  
  response <- concerto.template.show("Adaptive BAT - Training 5",
                                     workspaceID=2,
                                     params = list(audio_src = file.path(example_stimulus_dir, "training3.mp3"))
  )$LAST_PRESSED_BUTTON_NAME
  
  if(response == "respond_first") {
    feedback <- "Correct!"} else {
      feedback <- "Incorrect!"}
  
  concerto.template.show("Adaptive BAT - Training 5 feedback",
                         params = list(feedback = feedback), workspaceID=2)
  
  #	Show second practice question
  
  response <- concerto.template.show("Adaptive BAT - Training 6", workspaceID=2,
                                     params = list(audio_src = file.path(example_stimulus_dir, "training4.mp3")))$LAST_PRESSED_BUTTON_NAME
  
  if(response == "respond_second") {
    feedback <- "Correct!"} else {
      feedback <- "Incorrect!"} 
  
  response.continue <- concerto.template.show("Adaptive BAT - Training 6 feedback",
                                              params = list(feedback = feedback),
                                              workspaceID=2
  )$LAST_PRESSED_BUTTON_NAME
  
  #	Leave the training loop if the test-taker so requests
  if(response.continue == "btn_next") {take_training <- FALSE}  
}

concerto.template.show("Adaptive BAT - intro to main test", workspaceID=2)

##	Main test

for(i in 1:test_length){
  ##	Choose the question to administer next
  if(i == 1){
    #	The first question is chosen in advance and is the same difficulty (c. 0) for everyone.
    if(sample(0:1,1)) {current.item <- 23} else {current.item <- 24}
  } else {
    #	For all subsequent questions, the next question is chosen on the basis of prev. responses
    current.item <- nextItem(itemPar,
                             theta = theta_BM,
                             out = as.numeric(items.administered),
                             criterion = "bOpt",
                             cbControl = cbControl,
                             cbGroup = cbGroup)[[1]]
  }
  
  ##	Retrieve this question from the item bank
  current.item.info <- item.bank[item.bank$id == current.item,]
  question.file.name <- file.path(stimulus_dir, current.item.info[, "file_name"])
  correct.answer <- current.item.info[, "answer"]
  
  ##	Show the question page, and score the response
  chart_URL <- ""
  if(show.chart){
    
    file_name <- paste("adaptive_BAT_demo", concerto$sessionID, "_", i, ".jpg", sep = "")
    chart_path <- paste(concerto$mediaPath, file_name, sep = "")
    chart_URL <- paste(concerto$mediaURL, file_name, sep = "")
    
    jpeg(chart_path, width = 350, height = 350)
    plot(1:length(theta.vector),
         theta.vector,
         xlab = "Item number",
         ylab = "Estimated ability",
         main = "Ability estimates over course of test",
         pch = 16)
    dev.off()
  }
  admin.info <- ""
  
  output <- concerto.template.show("Adaptive BAT - question",
                                   params = list(question.file.name = question.file.name,
                                                 num.question = i,
                                                 test.length = test_length,
                                                 admin.info = admin.info,
                                                 chart_URL = chart_URL),
                                   workspaceID=2)
  
  items.administered <- c(items.administered, current.item)
  
  time.taken <- output$TIME_TAKEN
  time.vector.test.order <- c(time.vector.test.order, time.taken)
  
  if(output$LAST_PRESSED_BUTTON_NAME == "respond_first"){response <- 1} else {response <- 2}
  
  response.vector.test.order <- c(response.vector.test.order, response)
  ifelse(response == correct.answer, score <- 1, score <- 0)
  score.vector.test.order <- c(score.vector.test.order, score)
  
  ##	Compute a new ability estimate
  it <- itemPar[items.administered, 1:4, drop = F]
  theta_BM <- thetaEst(it, score.vector.test.order, method = "BM")
  theta.vector <- c(theta.vector, theta_BM)
  
}

theta_BM_sem <- semTheta(theta_BM, it, score.vector.test.order,
                         method = "BM")
# Compute a ML ability estimate
theta_ML <- thetaEst(it, score.vector.test.order, method = "ML")
theta_ML_sem <- semTheta(theta_ML, it, score.vector.test.order,
                         method = "ML")
# Compute WL ability estimate
theta_WL <- thetaEst(it, score.vector.test.order, method = "WL")
theta_WL_sem <- semTheta(theta_WL, it, score.vector.test.order,
                         method = "WL")
# Compute EAP ability estimate
theta_EAP <- thetaEst(it, score.vector.test.order, method = "EAP")
theta_EAP_sem <- semTheta(theta_EAP, it, score.vector.test.order,
                          method = "EAP")

test.output.main <- list(BAT.ability = theta_ML)
test.output.raw <- list(
  theta_BM = theta_BM,
  theta_BM_sem = theta_BM_sem,
  theta_ML = theta_ML,
  theta_ML_sem = theta_ML_sem,
  theta_WL = theta_WL,
  theta_WL_sem = theta_WL_sem,
  theta_EAP = theta_EAP,
  theta_EAP_sem = theta_EAP_sem,
  test_length = test_length,
  items.administered = items.administered,
  response.vector.test.order = response.vector.test.order,
  score.vector.test.order = score.vector.test.order,
  time.vector.test.order = time.vector.test.order,
  theta.vector = theta.vector)

if(give_feedback){
  
  #	Plot a graph
  file_name <- paste("BAT_v2", "_",
                     concerto$testID, "_",
                     concerto$workspaceID, "_",
                     concerto$sessionID, "_",
                     ".jpg", sep = "")
  chart_path <- paste(concerto$mediaPath, file_name, sep = "")
  chart_URL <- paste(concerto$mediaURL, file_name, sep = "")
  jpeg(chart_path, width = 550, height = 450)
  
  mean=100; sd=15
  lb=-100; ub = (theta_ML*15) + 100
  x <- seq(-4,4,length=100)*sd + mean
  hx <- dnorm(x,mean,sd)
  main_title <- paste("Your Beat Perception IQ was:",
                      round(ub, digits = 0))
  plot(x, hx, type="n", xlab="Beat Perception IQ", ylab="",
       main=main_title, axes=FALSE)
  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 
  area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
  axis(1, at=seq(40, 160, 20), pos=0)
  
  dev.off()
  
  concerto.template.show("Adaptive BAT - feedback 2",
                         workspaceID = 2,
                         params = list(chart_URL = chart_URL))
} else {
  concerto.template.show("Adaptive BAT - thanks",
                         workspaceID = 2)
}