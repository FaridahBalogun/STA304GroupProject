DF_choices <- list("Longer Commute Times" = "LongCommute",
                   "Shorter Commute Times" = "ShortCommute")

DF_options <- list("Primarily Online",
                   "Primarily In-Person",
                   "Hybrid",
                   "No Preference")

DF_label_of <- function(v) {
  lab <- names(DF_choices)[match(v, DF_choices)]
  if (is.na(lab)) lab <- names(DF_options)[match(v, DF_options)]
  lab
}

LS_choices <- list("Auditory Learning" = "Benefit of Auditory Learning",
                   "Visual Learning" = "Benefit of Visual Learning",
                   "Hands-on Learning" = "Benefit of Hands On Learning")

LS_label_of <- function(v) {
  lab <- names(LS_choices)[match(v, LS_choices)]
  lab
}