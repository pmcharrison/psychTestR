confirm_resume_session = function() {
  if (confirm("Resuming ongoing testing session.\n" +
               "Please click 'OK' to confirm, " +
               "or click 'Cancel' to restart as a new user.")) {
    return(true);
  } else {
    return(request_new_session());
  }
};
