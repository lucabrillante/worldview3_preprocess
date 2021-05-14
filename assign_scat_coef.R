assign_scat_coef <- function(shv) {
  scat_coef <- c("very_clear" = -4, "clear" = -2, "moderate" =  -1,
                 "hazy" = -0.7, "very_hazy" = -0.5)
  if (shv <= 55) { conditions <- "very_clear"
  } else 
    if (shv >= 56 & shv <= 75) {conditions <- "clear"
    } else 
      if (shv >= 76 & shv <= 95)  {conditions <- "moderate"
      } else 
        if (shv >= 96 & shv <= 115) {conditions <- "hazy"
        } else {conditions <- "very_hazy"
        }
  scat_coef[conditions]
}