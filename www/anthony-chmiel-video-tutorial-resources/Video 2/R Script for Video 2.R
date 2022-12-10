###### Video 2 - R Script for Introductory Video Series to PsychTestR
# Video series made by Anthony Chmiel (The MARCS Institute for Brain, Behaviour and Development. Western Sydney University. Copyright 2021)
# If you are new to R, please see Video 1 for some introductory information.
# Thanks go to Peter M. C. Harrison and Seb Silas for their assistance throughout the creation of this video series.



## In this video we will begin building our own psychTestR experiment.


# 1. INTRODUCTORY CONCEPTS
#
# - the main unit of psychTestR is a timeline
#
# - a timeline is a *list* of pages => in R list = list()
#
# to make a psychTestR test, you need to:
#
#   1) import/load the psychTestR package
#   2) tell psychTestR you want to "make a test" => make_test()
#   3) since a psychTestR test is a list of pages (page), you need to provide a list of at least one page to make_test, with multiple pages separated by a comma
#   4) you must also tell psychTestR when the test finishes => final_page(). And again, this must be separated by a comma from the previous page
#   5) consequently, the commands below make up the most minimal possible psychTestR test you can make. Highlight the whole eight lines and run them in one go:

library(psychTestR)

make_test(
  list(
    page("test page"),
    final_page("This is a final page")
  )
)

# You should have seen an experiment box open up, and a single basic page saying "test page" would have been visible. Close the experiment window by clicking the corner button once you have looked at it.

# to recap, you must:
# - import/load psychTestR
# - make the test (make_test)
# - to make_test, you must give a timeline; a list of pages using list()
# - the most general page is page()
# - you always have to specify a final_page in psychTestR


# We are off to a good start, although we have a small problem: we weren't able to move to the "final_page" in the example that we just made. We only saw a single "test page" and then closed that page.
# And the reason we couldn't move to the final page was because in our syntax we did not include a next button or a response button that allowed us to proceed.
# there are two ways of solving this:

# 1) we could add a button to the page ourselves, but this more advanced and would involve us including our own HTML/CSS aspects. We might cover this in the following advanced video if we have time.
#    Unless you have experience with this, I will recommend that you avoid the "page()" command for the moment.
# 2) Instead of the advanced custom pages approach, we will use the simpler pre-made pages within psychTestR. These are already setup to be useful for us and require minimal effort from us:

# The first pre-made page we will look at is the "one_button_page". It adds a nameable proceed button to the page, and is essentially a really easy way to have a page with text on it.
# Run the below commands and you will see that we can now proceed to the "final page". Close the experiment window when you reach the second (final) page.

make_test(
  list(
    one_button_page("test page"),
    final_page("This is a final page")
  )
)


#####

# 2. EXPLORING THE DIFFERENT TYPES OF PRE-MADE PAGES

# In section 1 we worked through a way of presenting some information (text in this case) but the purpose of a PsychTestR experiment is to collect data, which we will now do.
# Certain pre-made psychTestR pages are aimed to do this.
#
# But first, pages that collect data generally need two things:
# 1) a label; this label helps us identify the data we collect later.
# 2) a prompt; something to instruct the participant on what to do.


# The most basic page is the text_input_page. Here we will create a text input page in which the participant is asked to state their name (the prompt) and the label for the data is "name".
# NB that a specific command is required to save data, and we have not reached that point yet, but if that command is included then this data will be saved under "name".
# Run this initial example:

make_test(
  list(
    
    text_input_page(label = "name",
                    prompt = "What is your name?"
    ),
    
    final_page("Thank you for completing the test!")
  )
)




# We will now run the above code, but with an extra page in the middle. This extra page is called NAFC, which stands for "n-Alternative Forced Choice".
# In other words, you can list as many options as you want (as represented by the "n") but the participant must choose one of those response options to proceed.
# In this particular NAFC we have used the label "Gender", and have provided three options. You could include additional options if you wanted to.
# Importantly, in the NAFC page, we are using the term "c()", within the "choices" line of the syntax. This is one of the primary ways that R strings a collection of information together.
# We will be using this "c()" term regularly

# Try this three-page example:

make_test(
  list(
    
    text_input_page(label = "name",
                    prompt = "What is your name?"
    ),
    NAFC_page(label = "Gender",
              prompt = "What is your gender?",
              choices = c("Male", "Female", "Other")),
    
    final_page("Thank you for completing the test!")
  )
)


# Now we add in an extra three kind of response pages to the list. These are a) a checkbox page, b) a dropdown page, and c) a slider page. NB that the Checkbox page allows more than one selection.
# We are collecting data called "Health", "Age", and "Climate" respectively. Run this command, and try to follow the syntax for the three new sections. They are similar to the earlier pages.
# For the slider page, the "min" and "max" values refer to the minimum and maximum ends of the scale presented, whereas "value" refers to the default value that the slider will initially show

make_test(
  list(
    
    text_input_page(label = "name",
                    prompt = "What is your name?"
    ),
    
    
    NAFC_page(label = "Gender",
              prompt = "What is your gender?",
              choices = c("Male", "Female", "Other")),
    
    checkbox_page(
      label = "Health",
      prompt = "Do you have any of the following health conditions?",
      choices = c("Lung disease", "Kidney failure", "Neither")),
    
    dropdown_page(label = "age",
                  prompt = "What age category do you fit in?",
                  choices = c("Under 18", "18-49", "50 or over")),
    
    slider_page(label = "climate",
                prompt = "On a scale of 0 - 10, how hot is it in your location today?",
                min = 0,
                max = 10,
                value = 0),
    
    final_page("Thank you for completing the test!")
  )
)


# Additional functionality is possible for the above pages; as noted in Video 1 this is not a comprehensive guide.
# See the PsychTestR examples for additional details and options - you can find this under "Articles" on the homepage (https://pmcharrison.github.io/psychTestR/index.html)


# Now let's look at some different types of pages that allow us to embed multimedia (audio, video, and images).
# In the below example there will be two experiment pages. NB that it is recommended you click the "open in browser" option as some machines may have issues playing the video unless it is in browser mode.
# The first page will play a song, and then ask if you enjoyed it, with Yes or No response options. You will see that within the audio_NAFC page we have a new line called "url" within the syntax.
# Use this line to link to a publicly available audio file for it to embed. Feel free to put your own URL into this line if you want to experiment with it.

# The second page will present a short clip of the world spinning. As noted above, this might only work in the browser mode (by clicking "open in browser"). You will be presented a yes/no responses after the video plays.
# As above, feel welcome to copy in your own URL to a publicly available video for the video_NAFC page.


library(psychTestR)

make_test(
  list(
    audio_NAFC_page(
      label = "audio_example",
      prompt = "Do you like this track?",
      choices = c("Yes", "No"),
      url = "https://file-examples-com.github.io/uploads/2017/11/file_example_MP3_700KB.mp3"
    ),
    
    
    video_NAFC_page(label = "video_example",
                    prompt = "Is the world spinning?",
                    choices = c("Yes", "No"),
                    url = "https://file-examples-com.github.io/uploads/2017/04/file_example_MP4_480_1_5MG.mp4"),
    
    final_page("Thank you for completing the test!")
  )
)



# If there was an error in audio or video playback, try to find a URL to a publicly available audio/video track, and replace the above URL within the code with your newer one.
# NB a link to youtube or similar will not suffice. It needs to be a URL to a file. An online repository (e.g. via a Github) is a good option for storing these.

# Now lets look at how to include an image on a page.
# There is no pre-made NAFC page for images, so the approach is a little different and the syntax more complicated.
# This example was taken from a demo written by Peter Harrison at https://pmcharrison.github.io/psychTestR/articles/c-media-files.html
# Peter has written detailed notes at the above link, so I will not go into detail regarding the inclusion of images.

# The below syntax should open a single page that displays a grey box with "150x150" written inside - this is the linked image.
# Feel welcome to insert your image image via a publicly available URL. Also NB that the image page requires us to have the htmltoools package installed and loaded first.



library(psychTestR)
library(htmltools)

ui <- div(
  img(src = "https://via.placeholder.com/150"),
  p("What do you think of this image?")
)

shiny::runApp(
  make_test(
    list(
      final_page(ui)
    )
  )
)


# It is also possible to link to multimedia that are locally hosted (i.e., are on your computer) rather than via URL.
# We will not cover that in this series, as it is more complex than linking to a URL and has also has some disadvantages, as described here: (https://pmcharrison.github.io/psychTestR/articles/c-media-files.html)
# For information on how to link to local files see the above link


# 3. SAVING DATA, AND ACCESSING DATA VIA THE ADMIN PANEL

# We will now learn how to tell psychTestR to save the collected data.
# Below I include several new examples of pages that could be created. These are aimed to demonstrate additional responses, by using the "c()" list command that was discussed earlier.
# A new line of syntax has been added to the end, as follows: elt_save_results_to_disk(complete = TRUE)
# This new line tells psychTestR to save all data, and as it includes the argument "complete = TRUE" this is a command to be put towards the end of your psychTestR experiment - directly before the final page, to be exact.
# If you wanted to incrementally save data, you would use this command instead: elt_save_results_to_disk(complete = FALSE)
# This incremental save command can occur after each test where some sort of data is collected. So in the below example I do not include it after the introductory "one button page" but I do include it after each subsequent page.
# You can tell psychTestR to incrementally save data as many times as you wish. It is a good idea to use incremental saves to avoid data loss, especially in longer experiments.


library(psychTestR)

make_test(
  list(
    one_button_page("We will now ask you some questions about your personal background."),
    
    text_input_page(label = "Name",
                    prompt = "What is your name?"),
    
    
    NAFC_page(label = "gender",
              prompt = "What gender do you identify as?",
              choices = c("Male", "Female", "Other", "Prefer not to answer")
    ),
    
    elt_save_results_to_disk(complete = FALSE),
    
    text_input_page(label = "age",
                    prompt = "What is your age in years?"),
    
    elt_save_results_to_disk(complete = FALSE),
    
    NAFC_page(label = "occupation",
              prompt = "What best describes your occupational status?",
              choices = c("Still at school",
                          "At university",
                          "In full- or part-time employment",
                          "Self-employed",
                          "Homemaker/full-time parent",
                          "Unemployed",
                          "Retired",
                          "Rather not say")),
    
    elt_save_results_to_disk(complete = TRUE),
    
    final_page("Thank you for completing this experiment.
               You may now close this window.")
  )
)



# Why not go through the above short experiment 2 or 3 times, using different responses each time.
# Once you have done this, on the final page of the experiment click the button that says "Admin login".
# This will open a box prompting for a password. For demoing PsychTestR, the password default is "demo". So type that in and submit it.

# Here you have a very important aspect of PsychTestR - the Admin Panel.
# You can download the results in csv or RDS format for all saved sessions, or just for the current session (i.e., the most recent session that you opened)
# NB that exactly what is saved in csv and RDS formats depends on the epxeriment that you write. For the cabat, mdt, and mpt experiments written by Peter Harrison the main output is an ability score ranging from -4 (lowest score) to 4 (highest score), and also a standard error of this ability score (SEM).
# Peter has also enabled extra data be accessed via the RDS files, although he acknolwedges this is for more advanced users and for the majority of people using cabat, mdt, and mpt, the csv results would be plenty.

# Other functions here are:
#  - Statistics: access a summary of the number of sessions, and the descriptives statistics regarding time for all sessions
#  - Clear sessions: Remove all collected data in sessions
#  - Open/Close test. If closed, the experiment will not open
#  - Pilot/Live. Pilot mode will collect data, but this data will be labelled as pilot data so as not to be confused with actual collected data. This can be useful to road test your experiments. Although I would also recommend you clear sessions and delete results before testing begins
#  - Error log: This can provide useful insights if you have errors occurring. You can also Delete all previous log entries
#  - Below the options to download the csv and RDS results you also have the option to delete all results. Be careful with this, as you cannot recover deleted data

# In the video, I will now show you some sample csv data - this is the same data we briefly looked at in Video 1.
# Also note that within the folder that you have placed the R script for Video 2, there should now be a folder created called "output" and within that there should be another four folders called: 
# "deleted-results" "errors" "results" "sessions"

# PsychTestR will automatically store data and error logs here, for your convenience. NB it is quite normal for there to be no error logs within the "errors" folder, but if you are having issues that is a good place to look.



# This is the end of Video 2, although I include some additional notes below this point for those reading the R script. 
# In video 3 we will look at some more advanced features of PsychTestR, and where you can find research examples to study on your own.

# In the third video I will also show you some areas to look at example syntax provided by Peter Harrison,
# and where to find some existing experiments from which to take inspiration for your own, more advanced experiments.







### BONUS CONTENT: 
# This is only brief, and not covered in the video. We will now look at the "Get Basic Demographics" function in PsychTestR. 
# PsychTestR has a feature built in that requires little setup, and is able to automatically put in preset demographics pages, which is quicker and required less effort/knowledge. 

# Importantly, for this to work we will not use the maketest() and list() commands here. We will use the "join" command shown below, which is an alternative and in some ways is "more secure": 
psychTestR::make_test(
  psychTestR::join(
    psychTestR::get_basic_demographics(), 
    psychTestR::final_page("Final Page")
  )
) 


# The above syntax present around 5 preset pages that collects a range of demographic information. 
# If you want to examine how this works, type in the below command: 
psychTestR::get_basic_demographics

# R will present you with a function and several lines of syntax as shown below. If we work through this syntax you will see that each page (gender, age, occupation, education) is selected as "TRUE".



# R OUTPUT:

# function (intro = basic_demographics_default_intro(), gender = TRUE, 
#           age = TRUE, occupation = TRUE, education = TRUE) 
# {
#   stopifnot(is.null(intro) || is(intro, "page"))
#   join(begin_module("demographics"), intro, if (gender) 
#     get_basic_demographics.gender(), if (age) 
#       get_basic_demographics.age(), if (occupation) 
#         get_basic_demographics.occupation(), if (education) 
#           get_basic_demographics.education_highest_achieved(), 
#     end_module())
# }
# <bytecode: 0x7fd9017b18b8>
#   <environment: namespace:psychTestR>



# if we break the above syntax down even further, you will see that it is a simple function which produces a short psychTestR timeline.
# additionally, it:
#   - wraps the timeline in a psychTestR module, so it can be distinguished in the results section
#   - does some checks (e.g., to make sure that the input is what it is expecting, like making sure the introduction page is really a psychTestR page, and not something else)
# 
# instead of producing pages directly, each page is also produced by a function. i.e., basic_demographics_default_intro, get_basic_demographics.age, get_basic_demographics.occupationand get_basic_demographics.education_highest_achieved are each functions which produce pages
# Looking at them individually, you will notice that they are nothing more than standard pages beneath the hood:

# R OUTPUT:

# basic_demographics_default_intro <- function() {
#   one_button_page("We will now ask you some questions about your personal background.")
# }
# 
# get_basic_demographics.gender <- function() {
#   NAFC_page("gender", prompt = "How do you describe your gender?",
#             choices = c("Female", "Male", "Other", "Prefer not to answer"))
# }
# 
# get_basic_demographics.age <- function() {
#   text_input_page("age", prompt = "What is your age in years?",
#                   width = "100px",
#                   validate = function(state, ...) {
#                     x <- answer(state)
#                     if (is.na(suppressWarnings(as.numeric(x))) ||
#                         x < 0) {
#                       paste0("Please ensure that age is entered correctly. ",
#                              "Your answer should be numeric.")
#                     } else TRUE
#                   })
# }
# 
# get_basic_demographics.occupation <- function() {
#   NAFC_page("occupation",
#             prompt = shiny::div(shiny::p("What is your occupational status?"),
#                                 shiny::p("(choose the one most appropriate option)")),
#             choices = c("Still at school",
#                         "At university",
#                         "In full- or part-time employment",
#                         "Self-employed",
#                         "Homemaker/full-time parent",
#                         "Unemployed",
#                         "Retired",
#                         "Rather not say"))
# }
# 
# get_basic_demographics.education_highest_achieved <- function() {
#   NAFC_page(
#     "education_qualification_highest",
#     prompt = shiny::div(
#       shiny::p("What is the highest educational qualification that you have completed?"),
#       shiny::p("(choose the one most appropriate option)")),
#     choices = c(
#       `Postgraduate degree` = "postgraduate",
#       `Undergraduate degree or professional qualification` = "undergraduate",
#       `Completed second qualification (e.g. A levels/High School)` = "senior_school",
#       `Completed first school qualification at about 16 years (e.g. GCSE/Junior High School)` = "middle_school",
#       `Did not complete any school qualification` = "none",
#       `Rather not say` = "rather_not_say"))
# }



# We will not cover Get Basic Demographics in further detail, but it is discussed by Peter Harrison at the following URL: 
# https://github.com/pmcharrison/psychTestR/blob/17614b8bd98e3dbe7b8854451064f5660ab38aab/R/basic-demographics.R




# How to load RDS files in R Studio?
readRDS("filepathhere/filename.rds")

#Depending on how you have set up your variables/saved results, different possibilties will be available to you. 
#E.g., for Peter Harrison's MDT package, he supplies the following example:
x <- readRDS("output/results/id=1&p_id=german_test&save_id=1&pilot=false&complete=true.rds")
attr(x$MDT$ability, "metadata")

# See the MDT Readme file for more information


