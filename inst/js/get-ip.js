
$(document).on("shiny:sessioninitialized",function(){$.get("https://freeipapi.com/api/json", function(response) {
  Shiny.setInputValue("getIP", response);

});});

