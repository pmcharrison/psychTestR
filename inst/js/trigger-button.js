trigger_button = function(inputId) {
  Shiny.onInputChange("last_btn_pressed", inputId);
  document.getElementById("current_page.ui").style.visibility = "hidden";
  setTimeout(function() {
    next_page();
  }, test_options.advance_delay * 1000);
};
