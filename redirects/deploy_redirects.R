# Redirect stubs at the old shinyapps.io URLs ----
# Each stub replaces an original app at acoppock.shinyapps.io/<old>/ with a page
# that forwards to alexandercoppock.com/shiny-apps/<new>/. A stub that someone
# visits at least once a year survives the free tier's inactivity deletion.
# Requires the `acoppock` rsconnect account. Run from the repo root.

library(shiny)

redirects <- read.csv("redirects/redirects.csv")
apps <- rsconnect::applications("acoppock", "shinyapps.io")
stub_root <- file.path(tempdir(), "redirect_stubs")

stub_app <- function(target, title) {
  str_target <- shQuote(target, type = "cmd")
  paste0(
    'library(shiny)\n',
    'target <- ', str_target, '\n',
    'ui <- fluidPage(\n',
    '  tags$head(tags$title(', shQuote(paste(title, "has moved"), type = "cmd"), '),\n',
    '    tags$script(HTML(paste0("window.top.location.replace(\'", target, "\');")))),\n',
    '  tags$div(style = "max-width: 40em; margin: 4em auto; font-family: sans-serif;",\n',
    '    tags$h2(', shQuote(paste(title, "has moved"), type = "cmd"), '),\n',
    '    tags$p("The new version runs in your browser at ", tags$a(href = target, target = "_top", target), ".")))\n',
    'server <- function(input, output, session) {}\n',
    'shinyApp(ui, server)\n'
  )
}

for (i in seq_len(nrow(redirects))) {
  old <- redirects$old[i]
  target <- paste0("https://alexandercoppock.com/shiny-apps/", redirects$new[i], "/")
  dir <- file.path(stub_root, old)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(stub_app(target, redirects$title[i]), file.path(dir, "app.R"))
  result <- tryCatch({
    rsconnect::deployApp(
    dir,
    appName = old,
    appId = apps$id[apps$name == old],
    account = "acoppock",
    server = "shinyapps.io",
    forceUpdate = TRUE,
    launch.browser = FALSE,
    logLevel = "quiet"
    )
    "deployed"
  }, error = function(e) conditionMessage(e))
  print(paste(old, "->", result))
}
