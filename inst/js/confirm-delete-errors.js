var skip_confirm = false;

confirm_delete_errors = function() {
  if (skip_confirm || confirm("Are you sure you want to delete all error logs?")) {
    Shiny.onInputChange("admin_panel.confirm_delete_errors", performance.now());
  }
};
