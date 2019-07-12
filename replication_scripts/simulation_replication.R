######################################################################
##                                                                  ## 
##      PROJECT TITLE:    Multi-Modes for Detecting Experimental    ##
##                        Measurement Error                         ##
##                                                                  ## 
##      CODE AUTHOR:      THOMAS S. ROBINSON (APRIL 2019)           ##
##                                                                  ##
##      EMAIL:            thomas.robinson@politics.ox.ac.uk         ##
##                                                                  ##
##      DESCRIPTION:      Simulation replication file               ##
##                                                                  ##
######################################################################

#### Prerequisites ####
library(plyr)
library(tidyverse)

set.seed(89)

## Expected measurement error function
expect_me <- function(errors, probs, u, rule) {
  
  #####################################################################
  ## Function to calculate expected measurement error 
  ## of single and multi-mode sampling
  ##
  ## Inputs:
  ##  errors  = 3 element vector of measurement errors
  ##  probs   = 2 element vector of probabilities of state
  ##  u       = weighting applied to true state
  ##  rule    = "stick" or "twist"
  ##
  ## Outputs:
  ##  eu[1]   = expected value for single mode sampling
  ##  eu[2]   = expected value for multi-mode sampling
  #####################################################################
  
  ## Error messages
  if (!is.vector(probs) | length(probs) != 2) {
    stop(paste0("Please supply a probability vector of length 2 (p3 calculated as 1 - p1 - p2)",
                "\nSupplied length: ", length(probs)))
  }
  
  if (!is.vector(errors) | length(errors) != 3 ){
    stop(paste0("Please supply an error vector of length 3",
                "\nSupplied length: ", length(errors)))
  }
  
  if (typeof(u) != "double" | u < 0 | u > 1) {
    stop("Please supply update value between 0 and 1")
  }
  
  # Simplify inputs for readability
  me1 <- errors[1]
  me2 <- errors[2]
  me3 <- errors[3]
  
  p1 <- probs[1]
  p2 <- probs[2]
  p3 <- 1 - p1 - p2
  
  # Stick is single-mode sampling, twist is multi-mode sampling
  if (rule == "stick") {
  
    exp_err_stick <- p1*me1 + p2*me2 + p3*me3
    return(exp_err_stick)
    
  } else if (rule == "twist") {
  
    # Calculation as detailed in paper:
    exp_err_twist <- (p1 * (p2/(p2+p3)) * ((1-u)*me1 + u*me2)) +
                (p1 * (p3/(p2+p3)) * (0.5*me1 + 0.5*me3)) +
                (p2 * (p1/(p1+p3)) * (u*me2 + (1-u)*me1)) +  
                (p2 * (p3/(p1+p3)) * (u*me2 + (1-u)*me3)) +  
                (p3 * (p1/(p1+p2)) * (0.5*me3 + 0.5*me1)) +  
                (p3 * (p2/(p1+p2)) * ((1-u)*me3 + u*me2)) 
    
    return(exp_err_twist)
    
  } else {
    
    stop("Error: incorrect decision rule specified")
    
  }
}

## Full schedule of parameter values
schedule <- expand.grid(p1  = seq(0,1,0.05),
                        p2  = seq(0,1,0.05),
                        u   = seq(0.5,1,0.1),
                        me1 = c(5,10,50),
                        me3 = c(5,10,50)) %>%
  filter(p1 + p2 <= 1) %>%
  mutate(probs = map2(p1,p2, c),
         errors = pmap(list(me1,0,me3), c)) %>%
  
  # Run expected measurement error calculations, for both single and multi-mode replication
  mutate(single = pmap_dbl(list(errors, probs, u, "stick"),
                           .f = expect_me),
         multi = pmap_dbl(list(errors, probs, u, "twist"),
                          .f = expect_me)) %>%
  
  filter(p1 > 0,
         p2 > 0) %>%
  
  # Calculate optimal strategy per parameter configuration
  mutate(multi_better = ifelse(single >= multi, 1,0),
         multi_error = multi-single)

## Figure 1

figure_1 <- ggplot(schedule, aes(x=p1, y = p2, color = multi_error)) +
  facet_wrap(u ~., ncol = 3,
             labeller = label_bquote(mu == .(u))) +
  geom_point(position = "jitter", size = 3.5) + 
  scale_color_gradient2(low = "darkgreen", mid = "white", high = "darkred") +
  theme_minimal() +
  labs(x = "P(Mode 1)", y = "P(Mode 2)",
       color = "Change in expected error (relative to single mode sampling)") +
  theme(legend.position = "bottom")
  
ggsave("figures/figure_1.pdf",figure_1, device = "pdf", dpi = 300, width = 28, height = 21, units = "cm")

