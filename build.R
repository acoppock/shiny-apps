# Build the static site ----
# Each app in apps/ is staged with the shared theme files beside it, then
# exported as a shinylive app into site/<app>/. The landing page is copied last.
# Preview locally: httpuv::runStaticServer("site")

library(shinylive)

app_names <- list.dirs("apps", full.names = FALSE, recursive = FALSE)
theme_files <- list.files("theme", pattern = "\\.scss$", full.names = TRUE)

unlink(c("_stage", "site"), recursive = TRUE)
dir.create("_stage")

for (app in app_names) {
  stage <- file.path("_stage", app)
  dir.create(stage)
  file.copy(list.files(file.path("apps", app), full.names = TRUE, all.files = FALSE), stage, recursive = TRUE)
  file.copy(theme_files, stage)
  export(stage, "site", subdir = app)
  title <- read.dcf(file.path("apps", app, "DESCRIPTION"), fields = "Title")[1, 1]
  index <- file.path("site", app, "index.html")
  writeLines(sub("<title>Shiny App</title>", paste0("<title>", title, "</title>"), readLines(index)), index)
}

file.copy("index.html", "site/index.html")
unlink("_stage", recursive = TRUE)
