trigger_button = function(inputId) {
  Shiny.onInputChange("last_btn_pressed", inputId);
  document.getElementById("current_page.ui").style.visibility = "hidden";
  setTimeout(function() {
    Shiny.onInputChange("next_page", performance.now());
  }, 500);
};
