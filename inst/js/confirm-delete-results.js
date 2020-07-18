var skip_confirm = false;

confirm_delete_results = function() {
  if (skip_confirm || confirm("Are you sure you want to delete all results?")) {
    Shiny.onInputChange("admin_panel.confirm_delete_results", performance.now());
  }
};
