function show_footer() {
  x = document.getElementById("footer");
  x.removeAttribute("hidden");
}

function hide_footer() {
  x = document.getElementById("footer");
  x.setAttribute("hidden", "hidden");
}

function show_admin_panel() {
  show_footer();
}

function hide_admin_panel() {
  hide_footer();
}
