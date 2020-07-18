var skip_confirm = false;

confirm_clear_sessions = function() {
  if (skip_confirm || confirm("Are you sure you want to clear all session files?")) {
    Shiny.onInputChange("admin_panel.confirm_clear_sessions", performance.now());
  }
};
