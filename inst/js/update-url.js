Shiny.addCustomMessageHandler("update_url", function(x) {
  history.replaceState(null, null, encodeURIComponent(x));
});
