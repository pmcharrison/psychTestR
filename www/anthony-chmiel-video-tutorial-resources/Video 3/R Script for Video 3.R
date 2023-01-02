###### Video 3 - R Script for Introductory Video Series to PsychTestR
# Video series made by Anthony Chmiel (The MARCS Institute for Brain, Behaviour and Development. Western Sydney University. Copyright 2021)
# If you are new to R, please see Video 1 for some introductory information.
# Thanks go to Peter M. C. Harrison and Seb Silas for their assistance throughout the creation of this video series.



## In this video we will look at more advanced features within PsychTestR, and will also look at how to publish an experiment online


# Generally speaking, if you need to do anything beyond collecting basic data, as demonstrated in the last video, then you will need to learn some more advanced topics - two main areas are:
#  a) custom data processing. i.e., doing something with your data before storing it
#  b) using information collected at test time in the test to influence the running of the test


# We will begin with custom data processing. And the most simple aspect of this is creating and running functions.
# A function can best be thought of as something with an input and an output, and in between this something happens to change your input into your output.
# We have already been using functions in last video (e.g., the make_test, list, one_button_page commands are in fact are functions)

# In video 1 we discussed that we can store an object in the Global Environment (working memory) of R. As discussed, we can use the assignment operator "<-" to do this. 
# In the below code, I am creating variable called "myvariable" and it corresponds to "a" (although "a" has not yet been set to anything)

myvariable <- "a"

# Run the above command, and "myvariable" will appear in the Global Environment
# NB you can also replace <- with just = and the same end result will occur. If you run the command rm("a") it will remove the variable "a" from the working memory; or use rm(list=ls()) to clear all working memory. This command can also be found in one of the Dropdown menus ("Session" > "Clear workspace")


# Next we will create a function called "add_2". It will be quite simple - it will take the input, and add a value of 2 to it:

add_2 <- function(input) {
  output <- input + 2
  
  output
}


# Writing your own functions is useful for additional arguments that can be added to PsychTestR pages to make them more useful.
# Here we will use two examples, known as the validate and the on_complete arguments

# In the below example we are creating a PsychTestR experiment with "maketest" and we have created a single text input page asking for a Participant ID, just like we did at the beginning of Video 2.
# But some additional lines have been added, and this contains a function. In the line beginning with "if" you will see that I have written that if the participant leaves the Participant ID box blank (blank is denoted with == ""), then the experiment will not allow them to continue.
# And in this case, the experiment will show the message "This cannot be blank". In other words, this is how you can force a response so that the participant doesn't leave the text entry empty. 

# Run the below script and try it. This will be useful to add into your own experiments.

library(psychTestR)
make_test(
  list(
    text_input_page(
      label = "name",
      prompt = "Please enter your participant ID",
      validate = function(answer, ...) {
        if (answer == "")
          "This cannot be blank"
        else TRUE
      }),
    elt_save_results_to_disk(complete = TRUE),
    final_page("Thank you for completing the test!")
  )
)



# The below example is more advanced, and it relates to the question asking for the participant's age.
# in this example, the input age must not be blank, and also it must be numerical. Try this example out yourself.

library(psychTestR)
make_test(
  list(
    text_input_page(label = "age", 
                    prompt = "What is your age in years?",
                    validate = function(state, ...) {
                      x <- answer(state)
                      if (is.na(suppressWarnings(as.numeric(x))) ||
                          x < 0) {
                        paste0("Please ensure that age is entered correctly. ",
                               "Your answer should be numeric, and cannot be blank.")
                      } else TRUE
                    }),
    final_page("Thank you for completing the test!")
  )
)


# You might be asking: How do I know what commands can be added here?
# To see what can be changed on each page, it is best to type "?" and then the page type into the console
# e.g., for a test input page you would run the command:
?text_input_page

# You can see that the "Help" panel now shows various options and arguments that can be used for this text input page. 
# In these two above examples we have been using the "validate" argument (the ninth argument listed). But other arguments are available - such as performing a function automatically once the page is finished (see the "on_complete" argument)

# By using these information pages and examining existing experiments you should be able to get a good understanding of various arguments available, including the types of functions that can be run. With a little creativity you should be able to create your own examples
# (note - the psychTestR library/package must be loaded for the help files to be viewable)




# Styling of the experiments
# As above let's examine the hep page, but this time for a NAFC page (n-alternative forced choice page, as used in Video 2)
?NAFC_page

# In the Help section of RStudio, you should see the below arguments listed, and also some further information

# NAFC_page(
#   label,
#   prompt,
#   choices,
#   labels = NULL,
#   save_answer = TRUE,
#   arrange_vertically = length(choices) > 2L,
#   hide_response_ui = FALSE,
#   response_ui_id = "response_ui",
#   on_complete = NULL,
#   admin_ui = NULL,
#   button_style = ""
# )



# This gives us some new arguments that can be edited to modify the page.
# The example below is a NAFC page similar to an example used in Video 2, but the buttons have been aligned vertically rather than horizontally (which is the default)
# If you were to change "TRUE" to "FALSE", then the buttons would display horizontally (or you could simply remove that line of code and the page would refer to the default setting)

library(psychTestR)
make_test(
  list(
    one_button_page("We will now ask you some questions about your personal background."),
    
    NAFC_page(label = "height",
              prompt = "Are you less than 6 feet tall?",
              choices = c("Less than 6 feet", "6 feet or taller"),
              arrange_vertically = TRUE
    ),
    final_page("Thank you for completing this experiment.
               You may now close this window.")
  )
)

# Try to explore some of the additional arguments for the NAFC page, and then some of the other pages we have covered.
# You can also edit the width and height arguments for some page types, as shown below. Specifically, the example below is editing the wdith and height of the text response box.


library(psychTestR)
make_test(
  list(
    one_button_page("We will now ask you some questions about your personal background."),
    text_input_page(label = "age",
                    prompt = "What is your age in years?",
                    width = "200px",
                    height = "100px"
    ),
    final_page("Thank you for completing this experiment.
               You may now close this window.")
  )
)



# In the example below italicised and bold text have been included

library(psychTestR)
make_test(
  list(
    one_button_page(
      tags$div(
        tags$p("An example of ", tags$strong("bold"), "text;"),
        tags$p("An example of ", tags$em("italicised"), "text.")
      )
    ),
    final_page("Thank you for completing this experiment.
               You may now close this window.")
  )
)




## Modules
# If you are running an experiment with clearly defined sections, it can be a good idea to wrap them into specific sections (called "modules").
# E.g., you might have a module for demographics, and then a module for Experiment 1 and a module for Experiment 2, which differs to Experiment 1.
# I have included a commented line to show where the demographics module begins, and where the Experiment 1 module begins. NB there is no Experiment 2 included in the example below.

library(psychTestR)
make_test(
  join(
    # the demographics module::
    module(label = "demographics",
           list(
             one_button_page("We will now ask you some questions about your personal background."),
             
             NAFC_page(label = "gender",
                       prompt = "How do you describe your gender?",
                       # in addition
                       choices = c("Male", "Female", "Other", "Prefer not to answer")
             ),
             
             text_input_page(label = "age", 
                             prompt = "What is your age in years?"),
             
             NAFC_page(label = "occupation",
                       prompt = "What is your occupational status?",
                       # in addition
                       choices = c("Still at school",
                                   "At university",
                                   "In full- or part-time employment",
                                   "Self-employed",
                                   "Homemaker/full-time parent",
                                   "Unemployed",
                                   "Retired",
                                   "Rather not say"))
           )),
    
    # the ratings module::
    
    module("ratings", list(
      slider_page(label = "ice_cream",
                  prompt = "On a scale of 1-10, how much do you like ice cream?",
                  min = 0, 
                  max = 10, 
                  value = 0), 
      
      slider_page(label = "cake",
                  prompt = "On a scale of 1-10, how much do you like cake?",
                  min = 0, 
                  max = 10, 
                  value = 0) 
    )),
    elt_save_results_to_disk(complete = TRUE),
    final_page("Final Page")
  )
)

# You will notice that the way the data are now saved into the csv file is somewhat different. The data label hasthe module name followed by the label name. E.g., "demographics.age"







## Now we will discuss the differences between two types of code that you can use in PsychTestR: build time versus test time
# Up to now we have mainly been using build time code. Build time code is run when the participant starts the test. e.g.,   make_test is called, and the pages are built. In this sense, it is static
# Test time code gets triggered in the middle of the test; it is dynamic and reactive. And in the examples below, we will be saving them into our global environment before beginning our syntax, and the syntax will call upon that code halfway through


# Before we get to some examples, I list below some useful functions for dealing with test time code execution:


# code_block()
# reactive_page()
# set/get_global() / set/get_local(),        # NB these are two partnered/related functions - set_global and get_global, and also set_local and get_local
# conditional()  





# The next few examples make use of these. code_block is a simple way to demonstrate test time code. 
# use a code_block to get the time the test started (this can only be gathered at test time): 

# Save the below two functions to your global environment before running an experiment. 
# NB some of the newer arguments we are adding here such as "state" and "..." will be explained in detail in the final example within this video.

# 1. to get the test time started
save_time_started <- code_block(function(state, ...) {
  set_global(key = "time_started", 
             value = Sys.time(), 
             state = state)
})


# 2. to get the test time taken
compute_time_taken <- code_block(function(state, ...) {
  time_taken <- Sys.time() - get_global("time_started", state)
  msg <- paste0("Time taken: ", format(time_taken, digits = 3))
  shiny::showNotification(msg)
})


# And once those two are saved in your global environment, you can run the below syntax. At the end of the experiment, it will let you know how long you took. 

library(psychTestR)
make_test(
  list(
    save_time_started,
    one_button_page("Continue when you're ready."),
    compute_time_taken,
    final_page("End")
  ))



# similarly, a reactive_page is a page that presents some information at test time, like the current time
library(psychTestR)
make_test(list(
  reactive_page(function(...) {
    msg <- format(Sys.time(), "The current time is %H:%M:%S.")
    final_page(msg)
  })
))


# or, what if you collect some data and you want to re-present it later?
library(psychTestR)
make_test(list(
  text_input_page(label = "name",
                  prompt = "What is your name?",
                  on_complete = function(state, ...) {
                    set_global("name", answer(state), state)
                  }),
  
  reactive_page(function(state, ...) {
    name <- get_global("name", state)
    msg <- paste0("Thank you ", name, " for completing our test.")
    final_page(msg) # the function must return a page!
  })
))


# As another example, provided on Peter Harrison's website:
library(psychTestR)
make_test(list(
  NAFC_page(label = "q1", 
            prompt = "What is your favourite colour?",
            choices = c("Red", "Green", "Blue")),
  reactive_page(function(answer, ...) {
    msg <- sprintf("Your favourite colour is %s.", answer)
    final_page(msg) # the function must return a page!
  })
))


# The next example is bound to be quite useful. 
# What if you want a test to react to previous participant responses? use the conditional command. 
# In this example, the NAFC pages asks you if you are in a quiet environment, with available responses as "Yes" or "No"
# As you will see in the code, if a particiapnt responds with "Yes", then the conditional logic is set to allow them to advance to the test. 
# But if the "No" response is selected, then the conditional logic moves this person to the final page of the experiment instead. 
# NB The reason the logic sends the participant to the final page if they select "No", is because it is set to do this if any action other than clicking "Yes" occurs


library(psychTestR)
main_test <- get_basic_demographics()        # see bonus content from Video 2 for information on 'get_basic_demographics' - this is at the end of Script #2. I have included this here simply because it is the quickest and easiest way to implement various demographic questions

make_test(
  join(
    one_button_page("Hello"),
    
    NAFC_page(label = "quiet", 
              prompt = "Are you in a quiet space?", 
              choices = c("Yes", "No")),
    
    conditional(test = function(state, ...) {
      ans <- answer(state)
      ans == "Yes"
    },
    logic = main_test),
    
    final_page("You have finished the test")
  )
)



## Now, let's look at an example where participants are given an option to go back and repeat a section of the test. 
# This kind of approach can be very useful for a training section that participants are allowed to repeat until they feel ready to proceed to the actual experiment
# Peter Harrison's cabat, mdt, and mpt experiments contain training sections like this. 
# The sound in this example can be loud, so make sure your headphones or speakers are not set too high before running the below syntax. 


# This example uses the "while_loop" function, which keeps the user in a repeated lopping state until a certain pre-specified criterion or state has been satisfied. 
# We will skip the "code_block" section and first look at the "while_loop section". The condition/criterion is specified by the "test" argument, and the input for this argument is a function. 
# See the video for a more detailed explanation of this part



library(psychTestR)
make_test(
  join(
    one_button_page("Hello"),
    
    code_block(function(state, ...) {
      set_global("satisfied", "Yes", state) # NB: if using modules, and you only want the variables to be used within a module, use the alternative set_local
    }),
    
    while_loop(test = function(state, ...) {
      ans <- get_global("satisfied", state) # NB: if using modules, and you only want the variables to be used within a module, use the alternative get_local
      ans == "Yes"
    }, logic = join(
      audio_NAFC_page(label = "bleep",
                      prompt = "How many bleeps in the sound?",
                      choices = as.character(1:10),
                      url = "https://actions.google.com/sounds/v1/alarms/alarm_clock.ogg"
      ),
      NAFC_page(label = "try_again",
                prompt = "Would you like to try again?",
                choices = c("Yes", "No"),
                on_complete = function(state, answer, ...) {
                  set_global("satisfied", answer, state) # or state(answer) instead of answer
                })
      
    )),
    final_page("You have finished the test")
  )
)



#### Finally, I will show you several places where you can access other PsychTestR experiments and examples: 


# 1. You can access examples written by Peter Harrison from his PsychTestR website (https://pmcharrison.github.io/psychTestR/articles/b-introduction) and heading to the "Articles Tab"
#     Within that list you can find at least ten pages with tutorials and useful tricks. E.g., https://pmcharrison.github.io/psychTestR/articles/b-introduction.html



# 2. Peter Harrison has created numerous experiments for PsychTestR (see https://pmcharrison.github.io/psychTestR/articles/a2-research-examples.html)
#    These include the 3 music aptitude tests that I referred to in video 1. There is also the PIAT pitch imagery test, and other music aptitude resources within the LongGold project (https://longgold.org/)
#    Apart from just running these experiments within R, it can be useful to go to the corresponding Github page (e.g., see packages from Peter Harrison here: https://github.com/pmcharrison?tab=repositories)
#    From the Github page, you can download the folder for each PsychTestR package — e.g., try downloading the package folder for mdt or cabat — and by opening up the internal R files you can examine how Peter has created that particular experiment
#    You will see that the approaches used are similar to what we have discussed in this video series - the best way for you to expand your skills is to deconstruct these existing experiments. Remember to consult the help pages frequently. 




# 3. You can also find other tests at: http://testing.musikpsychologie.de/dots_home/  (NB this site is in German - but there are ways to translate a webpage automatically for you in real time)
#    You can find great experiments online via resources such as this page, and also via the Githubs of people who create experients. 
#    E.g., why not run and deconstruct the Jack and Jill working memory test, by Kraus Frieler. This test can be found on the above link, and you can find the relevant Github site here: https://github.com/klausfrieler/JAJ
#    Another is the musicassessr package, by Seb Silas (https://github.com/sebsilas/musicassessr)


# Why not run the Jack and Jill experiment with the below code, and download the relevant files from the Github site and deconstruct how the experiment was built by looking at each page in the timeline. 


# Load devtools: 
library(devtools)
#Install the JAJ package/experiment:
devtools::install_github('klausfrieler/JAJ')

# Load the JAJ package
library(JAJ)

# Run a demo test, with feedback as you progress through the test,
# and not saving your data
JAJ_demo()

# Run a demo test, skipping the training phase, and only asking 5 questions:
JAJ_demo(num_items = 5, take_training = FALSE)

# If you like, you can also change the language to certain languages that have already been added into the experiment code - languages are generally noted in the documentation/github descriptions. e.g.: 
JAJ_demo(num_items = 5, language = "DE")





# Best of luck! In the fourth and final video I will show you how to publish an experiment so that it is no longer a demo (i.e., naming your experiment, creating your own password) and also how to publish experiments online for remote testing




