#Function to transform logit values
library(scales)

inverse_logit_trans <-
  trans_new("inverse logit",
            transform = plogis,
            inverse = qlogis,
            breaks = Compose(plogis, extended_breaks(), qlogis),
            format = Compose(plogis, format_format()))
