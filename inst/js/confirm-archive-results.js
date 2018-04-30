confirm_archive_results = function() {
  if (confirm("Are you sure you want to archive all results?")) {
    Shiny.onInputChange("admin_panel.confirm_archive_results", performance.now());
  }
};
