title <- "Music imagery test"

pages <- 
  withTags(
    list(new("one_btn_page",
             body = p("The Pitch Imagery Arrow Task has been designed to teach you to successfully imagine musical tones from a visual prompt.")),
         new("one_btn_page",
             body = p("Each trial starts with the word “Begin” on the screen, and you will hear an ascending major scale, which provides the key or context for that trial. You will then see a dot on the screen and hear a start note. Press 'Next' for an example of this.")),
         new("one_btn_page",
             body = div(
               p("Here is an example context:"),
               video(source(src = "training/Scale_C_ton.mp4",
                            type = "video/mp4"),
                     width = "50%",
                     autoplay = "autoplay"))),
         new("one_btn_page",
             body = p("A variable number of up and/or down arrows will then appear in a sequence, with a corresponding tone, that is stepping up or down the scale. Press 'Next' for an example of these arrows appearing after the ascending scale and start note.")),
         new("one_btn_page",
             body = div(
               p("Here is an example of arrows appearing after the ascending scale and start note:"),
               video(source(src = "training/Example_Trial_sounded_arr.mp4",
                            type = "video/mp4"),
                     width = "50%",
                     autoplay = "autoplay"))),
         new("one_btn_page",
             body = p("At some point in the trial, an arrow is shown with no tone heard. Your job is to imagine that exact missing tone. Initially there is one tone to be imagined per trial, but the number of tones to be imagined increases over the task, up to 5 tones. The word “hold” will appear with the last silent arrow of the sequence. Hold in your mind the sound of this last tone as you prepare to hear a test tone. Press 'Next' for an example of a single silent arrow added to our trial example.")),
         new("one_btn_page",
             body = div(
               p("Here is an example trial:"),
               video(source(src = "training/Example_Trial_all_arr.mp4",
                            type = "video/mp4"),
                     width = "50%",
                     autoplay = "autoplay"))),
         new("one_btn_page",
             body = p("To test the accuracy of your imagery, a test tone will be sounded and a white fixation cross will display. The tone will either match the note you are imagining, or will be incorrect. If the tone is correct press the left mouse button, if it is incorrect, press the right mouse button. Press 'Next' for the full example trial and try to respond correctly.")),
         new("final_page",
             body = p("You completed the test! You may now close the browser window."))))
