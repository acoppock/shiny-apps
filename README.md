# Teaching apps

Interactive tools for teaching experimental design, rebuilt from the 2014 to 2017 shinyapps.io originals. Each app runs entirely in the browser through [shinylive](https://posit-dev.github.io/r-shinylive/), so there is no server to sleep or expire.

| App | Original |
|---|---|
| `apps/power/` | `acoppock.shinyapps.io/power-app-update` |
| `apps/factorial/` | `acoppock.shinyapps.io/factorial` |
| `apps/multiple-comparisons/` | `acoppock.shinyapps.io/MC_app` |
| `apps/clt/` | `acoppock.shinyapps.io/CLT-app` |
| `apps/random-sampling/` | `acoppock.shinyapps.io/simple_rs` |
| `apps/spillovers/` | `acoppock.shinyapps.io/spillover-app` |
| `apps/gotv/` | `acoppock.shinyapps.io/GOTV` |

## Layout

- `apps/<name>/app.R` plus a `DESCRIPTION` whose `Title` becomes the browser tab title.
- `theme/brand.scss` is a verbatim copy of the personal site's `acoppock_site_build/brand.scss`; refresh it by copying when the site theme changes. `theme/apps.scss` holds the app-specific overrides layered after it.
- `build.R` stages each app with the theme files and exports it to `site/<name>/`. `index.html` is the landing page.

## Redirects

Every original URL at `acoppock.shinyapps.io` (listed in `redirects/redirects.csv`) is a stub that forwards to its new home. `redirects/deploy_redirects.R` redeploys them; it needs the `acoppock` rsconnect account.

## Build and preview

```r
source("build.R")
httpuv::runStaticServer("site")
```

Pushing to `main` builds and deploys to GitHub Pages through `.github/workflows/deploy.yml`.

The original bundles are archived in Dropbox at `shiny_apps/originals/`.

## Verification

`verification/verify_power.R` checks the power formula, read straight from `apps/power/app.R`, against `power.t.test`, `power.prop.test`, and DeclareDesign simulations in all four modes (4,000 sims each). Results are in `verification/power_simulation_results.csv`. Every scenario agrees within 1.6 Monte Carlo standard errors.
