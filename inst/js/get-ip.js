
$(document).on("shiny:sessioninitialized",function(){$.get("https://freeipapi.com/api/json", function(response) {

  console.log(response);
  Shiny.setInputValue("getIP", response);


});});

