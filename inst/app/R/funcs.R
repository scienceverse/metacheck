## Functions ----

# https://github.com/daattali/advanced-shiny/tree/master/reactive-trigger
# instantiate a reactive trigger with myTrigger <- makeReactiveTrigger()
# call myTrigger$depend() in any reactive code that should re-run when the trigger is fired
# call myTrigger$trigger() to set off the trigger
makeReactiveTrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}

debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  if (is_local) {
    message(...)
    #} else {
    list(...) |>
      toString() |>
      shinyjs::logjs()
  }
}
