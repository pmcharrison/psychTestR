

// Add an event listener for when the Shiny session is initialized
$(document).on("shiny:sessioninitialized",function() {

  // Make a GET request to the API using the fetch API
  fetch("https://freeipapi.com/api/json")

    // Parse the response as JSON
    .then(response => response.json())

    // Use the parsed JSON data
    .then(data => {

      // Set the Shiny input value to the response data
      Shiny.setInputValue("getIP", data);

      // Grab additional info through the browser
      const navigatorJSON = getBrowserAndHardwareInfo();
      Shiny.setInputValue("user_navigator_info", navigatorJSON);

    })

    // Handle any errors that occur during the fetch
    .catch(error => console.error('Error:', error));
});


function getBrowserAndHardwareInfo () {
    var _navigator = {};
    for (var i in navigator) _navigator[i] = navigator[i];
    delete _navigator.plugins;
    delete _navigator.mimeTypes;
    navigatorJSON = JSON.stringify(_navigator);
    return(navigatorJSON)
}
