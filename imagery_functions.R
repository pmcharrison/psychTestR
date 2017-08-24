shuffle <- function(vec) {
  vec[sample(x = length(vec),
             size = length(vec))]
}

getStimuli <- function(items) {
  item_bank <- suppressMessages(
    readr::read_delim("Stimuli_PIAT_Matrix.txt", 
                      "\t", escape_double = FALSE, trim_ws = TRUE))
  spec <- data.frame()
  Levels <- 1:5
  for (Level in Levels) {
    HeardArr <- rep(3:5, each = 2) %>% shuffle
    StartNote <- rep(0:1, each = 3) %>% shuffle
    ProbeAcc <- rep(0:1, each = 3) %>% shuffle
    spec <- rbind(spec, 
                  data.frame(Level = Level,
                             HeardArr = HeardArr,
                             StartNote = StartNote,
                             ProbeAcc = ProbeAcc))
  }
  spec <- spec[sample(nrow(spec), nrow(spec)), ]
  row.names(spec) <- NULL
  stimuli <- data.frame()
  for (i in seq_len(nrow(spec))) {
    candidates <- merge(item_bank, spec[i, , drop = F])
    stimuli <- rbind(stimuli,
                     candidates[sample(nrow(candidates), 1), ])
  }
  if (!(length(unique(table(stimuli$Level))) == 1 &&
        length(unique(table(stimuli$StartNote)) == 1) &&
        length(unique(table(stimuli$ProbeAcc)) == 1) &&
        length(table(stimuli$HeardArr))) == 1) {
    stop("Unbalanced stimuil found")
  }
  stimuli$ParticipantResponse <- NA
  stimuli$ParticipantCorrect <- NA
  row.names(stimuli) <- NULL
  stimuli <- cbind(data.frame(Position = seq_len(nrow(stimuli))),
                   stimuli)
  stimuli
}

msg <- function(msg) {
  message(paste(Sys.time(), "-", format(msg)))
}
