###### Video 4 - R Script for Introductory Video Series to PsychTestR
# Video series made by Anthony Chmiel (The MARCS Institute for Brain, Behaviour and Development. Western Sydney University. Copyright 2021)
# If you are new to R, please see Video 1 for some introductory information.
# Thanks go to Peter M. C. Harrison and Seb Silas for their assistance throughout the creation of this video series.



## In this video we will learn: 

# a) Setting up PsychTestR experiments so that they are no longer in "demo" form (i.e. so that they have a title, a custom password, and other setup options can be enabled)
# b) Running your own experiments in standalone mode (i.e., in a face to face setting, on your own computer running R/R studio)
# c) Running your own experiments online for remote testing (requires an online server) 
# 




# a) Setting up PsychTestR experiments so that they are no longer in "demo" form

# Below I have copied one of the examples we looked at in Script 3. 
# This particular example asks if you are in a quiet space - if you answer 'Yes' it will continue to the pre-set 'get_basic_demographics' section, whereas if you select 'No' the test logic will move to the final page, and so automatically end 
# You will also notice that I have added a comma after the second last close parentheses (after the final_page) and that here I have added in a new type of command called "opt" and it refers to a function called "test_options"

library(psychTestR)
main_test <- get_basic_demographics()        # see bonus content from Video 2 for information on 'get_basic_demographics' - this is at the end of Script #2

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
    elt_save_results_to_disk(complete = TRUE),
    final_page("You have finished the test")
  ),
  opt = test_options(title = "Our Experiment",
                     admin_password = "MYpassword",
                     researcher_email = "dummy.email@address.com"
    
  )
)


# the opt argument can be viewed in the help file for "make_test", and within that file it simply notes that "opt" draws it's arguments from that separate function called "test_options". So that is where we need to look for more information. 
# If we look at the help file for test_options you will see that we have a large list of possible options to add. 
# These include whether or not to enable automatic resuming of running sessions (default is set to true), the language to be set, whether or not put a logo on the page, and formatting options for the logo, 
# a maximum number of allowed participants, a minimum number of seconds that must pass before the participant can move to the following page, whether or not the Admin Login button should be enabled, and so on.

# Above in the example syntax  I have included the three most typical options.  (NB that title and password are mandatory options if you use test_options)
# title                 - this adds a title to your experiment, which is displayed in the top left when the experiment is opened (set to "Our Experiment" in my example)
# admin_password        - this is how you can add in your own password (set to "MYpassword" in my example)
# researcher_email      - this is how you can add in your own contact details in case of any participant errors. This information is displayed near the Admin Login button. I have used dummy.email@address.com
# If you run the example code above, you will see all of these three options have been included. 






# b) Running your own experiments in standalone (Face to face) mode
# With the information presented in Videos 2 and 3, and now with these final "opt" and "test_options" sections, you have all the information that you need to run a face to face PsychTestR experiment locally. 
# The easiest way to do this is simply to highlight your complete code, and to run it for your participant. However, I will recommend that you click the "open in browser" mode before having the participant begin, because in many cases
# (as highlighted with the video_NAFC page in Video 2) aspects of PsychTestR tend to run best in a browser. Be sure to thoroughly road test your experiment beforehand. 



# To demonstrate this, let's quickly look at Peter Harrison's MDT experiment in a standalone mode (i.e., running locally on the computer). 
# Make sure that you have already installed mdt (see Video 1). Here I have put the password as simply "MYpassword" but you can choose to use whatever password you like. You will use that to access results via the admin login page
library(mdt)
standalone_mdt(admin_password = "MYpassword")



# You will notice that Peter has used a different approach - instead of selecting the entire experiment code and running that, the experiment has been saved as a package (called mdt) which is then loaded. 
# If you look within the mdt folder (available on Peter's Github) and navigate to the folder called "R" you will see a file called "standalone.R"
# This is the file that you are calling upon with the second line of syntax. This is a more advanced approach, and you would need to export your experiment as a package (which I do not cover here) but it is a cleaner end result. 
# You can find information on how to create your own package online, such as at: http://web.mit.edu/insong/www/pdf/rpackage_instructions.pdf



## Optional information concerning mdt results
# After you complete MDT at least 1 or 2 times, download your results in CSV and RDS formats. The CSV file will give a brief insight into ability, ranging from -4 (worst outcome) to 4 (best outcome).
# You can access additional information such as correct/incorrect results for each stimulus via the RDS file. Each participant will have their own RDS file. 

# Peter has also set up a specific way that you can extract additional information (also saved as "ability") within the RDS files.
# Step 1: Check your current working directory using the below line. The working directory is the folder that R is currently reading from and writing to
getwd()
# Step 2: If needed, change the working directory using 
setwd()    #e.g., setwd("~/Desktop")
# Alternatively, use this command to automatically set the working directory to whatever folder is being used by an active document (e.g., the folder you ran this script from)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# (you may wish to run getwd again to check this worked)

# Step 3: Place the RDS file(s) into your working directory
#Step 4: by using a similar command to the one below (edit it to match your working directory and the RDS filename) you will see that the RDS file provides much more detail. 

x <- readRDS("id=1&p_id=testuser&save_id=1&pilot=false&complete=true.rds")
attr(x$MDT$ability, "metadata")

# to reiterate, the reason the RDS files give much more data than the CSV file because that is how Peter set up the MDT experiment. You could always set your experiment up to report more data within the CSV files. 
# I have included the above "testuser" RDS file alongside the Video 4 script






# c) Running your own experiments online for remote testing
# NB for step c) you will need a server to host R, your experiment, and your collected data. Before proceeding you should already be familiar with my server guide.
#                      current link server guide as of Jan '22: https://s3-eu-west-1.amazonaws.com/research.pmcharrison.com/psychTestR/psychTestR-server-docs-latest.pdf



# From my server guide make sure that you have a server set up, a .pem key, and that you are able to log into the server. There is a special "chmod" command that must be run the very first time you access the server, so don't forget that.
# Make sure that you have also installed R to the server, as well as the necessary packages and  so on, as noted within the guide. This includes installing ShinyServer to your server. 
# You should be able to see the Shiny-Server webpage as shown in Video 4,but with your own Public IPv4 address put into the URL instead of my address.



####### NB The below commands are to be run in your server, *NOT* in R! ####### 


# As I am running a Mac machine, I will be referring to Terminal, although there are similar ways to access a server via Windows, such as via Windows Terminal, Command Prompt, Putty, and so on. See the server guide for more information on this. 
# Log into the home directory of your server. In the video you can see I am logged into an Ubutu server. Let's run a few basic commands first, just in case this is new to you
# You can use the "cd" command to change to a new directory. So I can use "cd Documents/" to change to my Documents directory. If I want to return to my home directory, the quickest way is just to type "cd" and it will return home. 
# NB that at the end of this script I have listed some common commands for use in Terminal.


#PsychTestR uses ShinyServer to run online experiments. Once ShinyServer is installed, your apps will need to be run from the directory /srv/shiny-server
# to move to this directory within terminal we will use the cd command below
cd /srv/shiny-server
# Once you move to that ShinyServer directory, we will see what is inside this directory (if anything) by using the list command, below
ls

# On the video, you will see that I already have some things within this file. I have two files that come default with Shiny Server, which are called index and sample apps. 
# And then I have two apps that I have installed, which are the CABAT and MDT apps. Each of these is a folder, that has the app details housed within. It is likey that here you will only have those two default files. 

# PsychTestR experiments run best when they are in an text file called app.R, and that file is located within a folder named after that experiment. 
# E.g., If you follow the ReadMe guide for Peter Harrison's cabat and mdt packages, you will see they also that same process


# So first, we want to create a new folder within the shiny-server directory. We do that with the command below, although change the last word to whatever you want to call your experiment (I have used "OurExperiment")
# You cannot use spaces or slashes (/ or \) in your experiment's name, and it is a good idea to also avoid hyphens. If required, use underscores to separate words, or separate words with capital letters AsShownHere.
sudo mkdir OurExperiment
# NB that sudo is a common command that allows you to run commands with admin privileges. We will begin many commands with Sudo, and if you followed my Server Guide this will be familiar to you

# Next we want to create a text file called app.R within our newly created folder. This is done with the below command, and of course change "OurExperiment" to whatever you have chosen to name your own.
# The file *MUST* be called app.R
sudo nano OurExperiment/app.R

# The text file will automatically open within Terminal. We have two options for what we could write: 


## OPTION 1
# We could write our experiment out here in full. Be sure to begin this by loading whatever packages would be required. For example, you could write: 

library(psychTestR)
main_test <- get_basic_demographics()        # see bonus content from Video 2 for information on 'get_basic_demographics' - this is at the end of Script #2

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
    elt_save_results_to_disk(complete = TRUE),
    final_page("You have finished the test")
  ),
  opt = test_options(title = "My Example Experiment",
                     admin_password = "MYpassword",
                     researcher_email = "dummy.email@address.com"
                     
  )
)


# OPTION 2
#As discussed with running standalone (face to face experiments), you could save your experiment as a package and then just refer to that. Below I use the example of Peter's mdt package. 
# This is a shorter, cleaner approach, rather than including an entire experiment. See above for an example link on how to export your work as an R package; there are many other resources available online as well. 

library(mdt)
standalone_mdt(admin_password = "MYpassword")



# Regardless of which of the two options above that you choose, I recommend writing it in R Studio and then copying it across to the server text page, as it is far easier to make changes and to test things out this way. 
# But you could always just write it straight into the server text page if you preferred
# Once we have written in the text that we want, we need to save it. We do this with the "Write Out" command, which on a Mac is shown as control" and "o" pressed together. On non-Mac keyboards it may be a different key to "control"
# Next we need to click enter/return on our keyboard. After this, it should say "Wrote x lines" down the bottom of the text page, with x referring to the number of lines of text you wrote/copied. 
# We now exit the text page by pressing control" and "x" pressed together. (again, on non-Mac keyboards it may be a different key to "control")


# We have final step to take. In our shiny server directory we need to rewrite some permissions with a "change owner" command. We do this with the below command, with "OurExperiment" replaced with the name of your own experiment
sudo chown -R shiny OurExperiment


# You should now be able to access your experiment online! Using your own Public IPv4 address (see server guide) you should be able to access your experiment online. 
# In a web browser you would type in something like shown below, but changing the numbers to the ones that match your Public IPv4 address, and the experiment name to match your own:
http://3.26.236.121:3838/OurExperiment

# The simplicity of this approach is that you would only need to send this URL to your participant for them to run the experiment (so long as they have internet access). 
# These experiments also run quite well on tablets and smartphones, but if you do plan on letting participants use a range of devices be sure to thoroughly road test the experiment out on these




## NB # If you forgot to run that final "sudo chown..." line, or if there was an issue with that line (e.g., if it is run in the wrong directory) then when you go to view your experiment online it will show with an error similar to:
# " An error has occurred. The Application failed to start. The application exited during initialization."
# If you see this error, try to re-run the line (sudo chown -R shiny OurExperiment), making sure to run it from the shiny-server directory (e.g., from /srv/shiny-server/)






# See Video 4 for additional discussion, and also an alternative way to upload your experiment online, wherein you create the app folder locally on your computer, including the App.R file, and then you simply upload the folder. 

# I also list some helpful commands for use in your server (i.e. within Terminal or similar), which will be discussed in Video 4: 


# Command                     Example (if necessary)                        Brief description
#________________________________________________________________________________________________________________________________________________________________________________________________________
# cd                          cd Documents/                                 Change Directory
# ls                                                                        List folders/objects
# sudo rm                     sudo rm app.R                                 Remove an object or folder. The example to the left will remove the app.R object in whatever directory you are currently in
# sudo rm -r                  sudo rm -r OurExperiment                      As above, but the added "-r" section stands for recursive - if you delete a directory, it also delete everything within that directory
# sudo mkdir                  sudo mkdir anthony_app                        Creates a folder in this location, called "anthony_app"
# cd /var/log/shiny-server                                                  Moves to the log files for shiny server. By running ls, you can see if there are any log files. See the video for more details
# sudo tail logfilenamehere   sudo tail OurExperiment-shiny-date.log        Opens a log file (change everything after "tail" so that it matches the log file you are trying to open - the date will likely be quite long)
# sudo rm -rf *.log                                                         Delete all log files within the current directory. This can be useful, as log folders can fill up quickly
# cd ../                                                                    Move up one directory. E.g., to move up from the OurExperiment directory to the shiny-server directory that the app was within
# cd ../../                                                                 As above, but moving up two directories instead of one



# NB that if you make changes to a shiny-server app (e.g., you remove an app.R file or a directory, and then create a new one) you may run into a problem where the server will not see the new file/directory. 
# Here you would need to restart shiny-server after creating the new file/directory, and it should work fine. Run the below command, preferably from within the shiny-server directory
# sudo systemctl restart shiny-server.service



# As a final example you can also use the "Publish" button that appears in the top right of an experiment window when you are creating an experiment. It would upload your app to shinyapps.io
# This is an easy way to share your experiments with other PsychTestR users. You can also use this publish button to automatically run an experiment online **BUT** this should not be used for anything other than demonstration or piloting
# The reason is that there is only a tiny amount of dedicated server space, and so it is possible that much or alll of your data will not be saved, or will not be saved for long. If you use this, do not expect your data to be available for download






# There is more information and extra examples/discussion within Video 4, but I will leave this script here.
# I hope this has been helpful, and wishing you best of luck with your PsychTestR experiments!
#
#                                                                               - Dr. Anthony Chmiel, 2022



