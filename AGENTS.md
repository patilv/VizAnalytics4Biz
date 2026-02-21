# VizAnalytics4Biz — Project Memory

## What This Is
An async course site on Visual Analytics & Data Storytelling with R, aimed at non-coders. 8 weeks of content with 12 interactive Shiny apps hosted on Posit Connect, embedded as iframes. Site is hosted on GitHub Pages.

**Live URL:** https://patilv.github.io/VizAnalytics4Biz/
**GitHub repo:** https://github.com/patilv/VizAnalytics4Biz
**Posit Connect server:** analytics.gonzaga.edu (account: patil@gonzaga.edu)

---

## Repository Structure

```
VizAnalytics4Biz/
├── _site.yml                 ← site config (output_dir: "docs")
├── _footer.html              ← shared footer HTML
├── styles.css                ← custom CSS (Gonzaga branding)
├── index.Rmd                 ← landing page
├── syllabus.Rmd
├── resources.Rmd
├── week01.Rmd – week08.Rmd   ← weekly chapter content
├── images/
│   ├── 01/ – 07/            ← chapter images
├── apps/                     ← Shiny app source (12 apps)
│   ├── week01-datasaurus/app.R
│   ├── week02-preattentive/app.R
│   ├── week03-layers/app.R
│   ├── week03-aesthetics/app.R
│   ├── week04-charttypes/app.R
│   ├── week04-samestory/app.R
│   ├── week05-pipeline/app.R
│   ├── week05-tidydata/app.R
│   ├── week06-interactive/app.R
│   ├── week07-geography/app.R
│   ├── week08-dashboard/app.R
│   └── week08-portfolio/app.R
├── build_site.R              ← renders site to docs/
├── deploy_app.R              ← helper for Posit Connect deploys
├── .github/workflows/deploy.yml
├── .gitignore
├── AGENTS.md                 ← this file
├── VizAnalytics4Biz.Rproj
└── docs/                     ← rendered output (served by GitHub Pages)
```

**Key point:** `_site/` is gitignored — it's an intermediate build artifact. Only `docs/` is committed.

---

## Interactive App Architecture

Each week has 1-2 Shiny apps embedded as iframes pointing to Posit Connect:
```html
<iframe class="sandbox-frame" src="https://analytics.gonzaga.edu/content/<guid>/"
        height="600" loading="lazy" allowfullscreen></iframe>
```

Apps are placed **mid-chapter** at the pedagogically appropriate moment (after the concept is introduced, before the coding walkthrough), with:
- A "Try It" section heading
- Exploration tasks in a `.try-it-box`
- A "What You Should Have Noticed" debrief callout
- An AI callout connecting the concept to AI tool usage

### Deployed App URLs

| App Folder | App Name | Connect URL |
|------------|----------|-------------|
| week01-datasaurus | Datasaurus Explorer | https://analytics.gonzaga.edu/content/8bbb857b-cc04-4c9a-8491-4afb0c7f5aee/ |
| week02-preattentive | Preattentive Processing Lab | https://analytics.gonzaga.edu/content/b52f3a3d-3ee1-4191-9f7f-ba83cea3347d/ |
| week03-layers | Grammar of Graphics Layer Builder | https://analytics.gonzaga.edu/content/64046116-7100-479f-a200-f3e7aa34e4a9/ |
| week03-aesthetics | Aesthetic Mapping Sandbox | *Not yet deployed* |
| week04-charttypes | Chart Type Explorer | https://analytics.gonzaga.edu/content/84662c36-158f-47a8-ae95-5f9f7b0e4652/ |
| week04-samestory | Same Data, Different Story | *Not yet deployed* |
| week05-pipeline | dplyr Pipeline Builder | https://analytics.gonzaga.edu/content/df058898-7f3a-4778-abb6-89ddd9271bf3/ |
| week05-tidydata | Tidy Data Transformer | *Not yet deployed* |
| week06-interactive | Interactive Viz Toolkit | https://analytics.gonzaga.edu/content/f36d9e69-d7b9-47bc-b864-f23de1332b23/ |
| week07-geography | US Choropleth Map Explorer | https://analytics.gonzaga.edu/content/666be650-fcb9-4ccf-bb37-7e93e573449b/ |
| week08-dashboard | Business Dashboard Composer | https://analytics.gonzaga.edu/content/0506ada3-1ff9-4913-8fff-69cecf1e070a/ |
| week08-portfolio | Portfolio Showcase Template | *Not yet deployed* |

### Re-deploying an app after changes

```r
source("deploy_app.R")
deploy_app("week01-datasaurus", title = "VizBiz Week 1: Datasaurus Explorer")
```

Or manually:
```r
library(rsconnect)
rsconnect::deployApp(
  appDir      = "apps/week01-datasaurus",
  appName     = "vizbiz-week01-datasaurus",
  appTitle    = "VizBiz Week 1: Datasaurus Explorer",
  account     = "patil@gonzaga.edu",
  server      = "analytics.gonzaga.edu",
  forceUpdate = TRUE
)
```

---

## Build & Deploy Workflow

### Updating content (normal edit cycle)

1. Edit `.Rmd` files at the project root
2. Run `source("build_site.R")` — renders the site to `docs/`
3. Commit and push:
   ```
   git add docs/ *.Rmd styles.css _site.yml _footer.html
   git commit -m "Update site content"
   git push
   ```
4. GitHub Pages serves `docs/` from the `main` branch automatically

### Deploying Shiny apps

Apps are hosted independently on Posit Connect. After editing an app:
```r
source("deploy_app.R")
deploy_app("week03-aesthetics", title = "VizBiz Week 3: Aesthetic Mapping Sandbox")
```
Then update the iframe URL in the corresponding `.Rmd` file if the GUID changed.

### Testing locally

```r
httpuv::runStaticServer("docs", port = 7474, background = TRUE)
browseURL("http://localhost:7474")
# Stop when done:
httpuv::stopAllServers()
```

---

## GitHub Pages Configuration

- **Source:** Deploy from branch `main`, folder `/docs`
- Settings page: https://github.com/patilv/VizAnalytics4Biz/settings/pages
- The `deploy.yml` workflow exists as a manual-trigger backup

---

## Key Files

| File | Purpose |
|------|---------|
| `_site.yml` | Site config; sets `output_dir: "docs"`, toc, code folding |
| `styles.css` | Gonzaga-branded CSS (navy/red/gold), TOC, sandbox, responsive |
| `build_site.R` | Renders site with `rmarkdown::render_site(".")` |
| `deploy_app.R` | Helper to deploy any app to Posit Connect |
| `.github/workflows/deploy.yml` | Manual-trigger GitHub Pages deploy (backup) |

---

## App Standards

All 12 Shiny apps follow these conventions:
- **Theme:** `bs_theme(bootswatch = "flatly", primary = "#002967")` with Google Inter font
- **Layout:** `bslib::page_sidebar()` with sidebar on light gray (`#f1f5f9`)
- **Spinners:** `shinycssloaders::withSpinner(color = "#002967", type = 6)` with graceful fallback
- **Instruction accordion:** Collapsible "How to Use This Explorer" at top of sidebar
- **Error handling:** `req()` + `validate()` on all reactive outputs
- **Footer:** "Gonzaga University | Dr. Vivek H. Patil"
- **Colors:** NAVY `#002967`, RED `#C41E3A`, GOLD `#B4975A`

---

## Notes

- The repo was restructured in Feb 2026: moved from `SrcCode/` subdirectory to flat root layout, expanded from 8 to 12 apps.
- No individual file exceeds 50MB, so Git LFS is not needed.
- The 4 new apps (week03-aesthetics, week04-samestory, week05-tidydata, week08-portfolio) need initial deployment to Posit Connect before their iframes will work.
