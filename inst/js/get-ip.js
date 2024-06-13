


/*
$(document).ready(function(){
  $.get("http://ipinfo.io", function(response) {
    Shiny.onInputChange("getIP", response);
  }, "json");
});
*/


$(document).on("shiny:sessioninitialized",function(){$.get("https://api.ipify.org", function(response) {

  console.log(response);
  Shiny.setInputValue("getIP", response);


});});

