# =============================================================
# deploy_app.R
# Helper for deploying Shiny apps to Posit Connect.
#
# Usage:
#   source("deploy_app.R")
#   deploy_app("week01-datasaurus", title = "VizBiz Week 1: Datasaurus Explorer")
# =============================================================

deploy_app <- function(app_folder, title = NULL) {
  library(rsconnect)

  app_dir <- file.path("apps", app_folder)
  if (!dir.exists(app_dir)) {
    stop("App directory not found: ", app_dir,
         "\nAvailable apps: ", paste(list.dirs("apps", full.names = FALSE, recursive = FALSE), collapse = ", "))
  }

  app_name <- paste0("vizbiz-", app_folder)
  if (is.null(title)) title <- paste("VizBiz:", app_folder)

  message("Deploying ", app_dir, " as '", app_name, "'...")
  rsconnect::deployApp(
    appDir      = app_dir,
    appName     = app_name,
    appTitle    = title,
    account     = "patil@gonzaga.edu",
    server      = "analytics.gonzaga.edu",
    forceUpdate = TRUE
  )
  message("✓ Deployed: ", app_name)
}
