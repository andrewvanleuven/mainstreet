# API KEY: AIzaSyBkIVoP91oduorwBuz-V7jtinJ7AXBA82s
library(googleway)
googleway::set_key("AIzaSyBkIVoP91oduorwBuz-V7jtinJ7AXBA82s")


try <- google_directions(origin = c(42.461106, -93.830681),
                  destination = c(42.469417, -93.819308),
                  mode = "walking")

try$routes$legs[[1]][["duration"]][["value"]]/60 # 22.92 minutes
