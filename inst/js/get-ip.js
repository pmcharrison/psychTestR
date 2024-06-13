


/*
$(document).ready(function(){
  $.get("http://ipinfo.io", function(response) {
    Shiny.onInputChange("getIP", response);
  }, "json");
});
*/


$(document).on("shiny:sessioninitialized",function(){$.get("https://freeipapi.com/api/json", function(response) {

  console.log(response);
  Shiny.setInputValue("getIP", response);


});});

