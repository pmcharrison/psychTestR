Shiny.addCustomMessageHandler("session_start", function(data) {
  var search = location.search;

  // ""
  // "?"
  // "?p_id=x"
  // "?p_id=x&foo=bar"
  // "?foo=bar&p_id=x"
  // "?foo=bar&p_id=x&baz=quux"
  // "?foo=bar"
  // "?foo=bar&baz=quux"
  var rep_id = /([?&])p_id=[^&]*&?/g;

  if (search.length == 0) {
    search = "?p_id=" + encodeURIComponent(data);
  } else if (rep_id.test(search)) {
    search = search.replace(rep_id, "$1");
    if (!/[?&]$/.test(search))
      search += "&";
    search += "p_id=" + encodeURIComponent(data);
  }

  // Work around ShinyApps.io/SSP/RSC base href silliness
  var path = location.pathname.replace(/\/_w_(\w+)/, '');

  history.replaceState(null, null, path + search);
})
