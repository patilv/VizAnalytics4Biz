# =============================================================
# deploy_app.R
# Helper for deploying Shiny apps to Posit Connect.
#
# Usage:
#   source("deploy_app.R")
#   deploy_app("week01-datasaurus", title = "VizBiz Week 1: Datasaurus Explorer")
#
# Deploy all game apps:
#   deploy_all_games()
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

# Deploy all 8 game apps at once
deploy_all_games <- function() {
  games <- list(
    list(folder = "week01-game", title = "VizBiz Week 1: Stats Trap"),
    list(folder = "week02-game", title = "VizBiz Week 2: Design Detective"),
    list(folder = "week03-game", title = "VizBiz Week 3: Layer Cake"),
    list(folder = "week04-game", title = "VizBiz Week 4: Chart Matchmaker"),
    list(folder = "week05-game", title = "VizBiz Week 5: Pipe Dream"),
    list(folder = "week06-game", title = "VizBiz Week 6: Static or Interactive?"),
    list(folder = "week07-game", title = "VizBiz Week 7: Map ER"),
    list(folder = "week08-game", title = "VizBiz Week 8: Dashboard Jury")
  )

  for (g in games) {
    tryCatch(
      deploy_app(g$folder, title = g$title),
      error = function(e) message("✗ Failed: ", g$folder, " — ", e$message)
    )
  }
  message("\n✓ All game deployments attempted.")
}
