# =============================================================
# build_site.R
# Run this script to build the full VizAnalytics4Biz site.
#
# Usage (from project root):
#   source("build_site.R")
#
# Output: docs/  (served by GitHub Pages)
# =============================================================

library(rmarkdown)

message("── Rendering R Markdown site...")
rmarkdown::render_site(".")
message("   Site rendered to: docs/")

message("── Writing .nojekyll...")
writeLines("", "docs/.nojekyll")

message("\n✓ Build complete. Commit and push the docs/ folder to deploy.")
message("  Site will be live at: https://patilv.github.io/VizAnalytics4Biz/")
