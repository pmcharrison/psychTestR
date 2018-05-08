confirm_delete_errors = function() {
  if (confirm("Are you sure you want to delete all error logs?")) {
    Shiny.onInputChange("admin_panel.confirm_delete_errors", performance.now());
  }
};
