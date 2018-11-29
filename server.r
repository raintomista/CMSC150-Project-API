library(plumber)
r <- plumb("~/cmsc150-project-api/router.r")
r$run(port=8000)

  