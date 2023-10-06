# synthetic data function
syntdat <- function(n_Run,
                    n_Treatment,
                    n_Sample,
                    n_Repl,
                    b_Treatment,
                    sigma_plate,
                    sigma_repl,
                    p_sigma_sample,
                    seed  = 12432) {
  
  set.seed(seed)
  
  # Make indices
  n <- n_Run * n_Treatment * n_Sample * n_Repl
  n_Plate <- n_Run * n_Treatment
  Treatment <- c(rep(rep(1:3), each = n/n_Treatment))
  Plate <- c(rep(rep(1:(n_Plate)), each=n_Sample * n_Repl))
  Sample <- c(rep(rep(1:(n/n_Repl)), each=n_Repl))
  Repl <- c(rep(rep(1:n_Repl), times=n/n_Repl))
  
  # Generate data
  sigma_sample <- rexp(9, rate = p_sigma_sample)
  z_Plate  <- rnorm(n/(n_Repl * n_Sample), 0, sigma_plate)
  z_Sample <- rnorm(n/n_Repl, 0, sigma_sample[Plate[rep(c(TRUE,FALSE),n/n_Repl )]])
  z_Repl   <- rnorm(n, 0, sigma_repl)
  Value <- b_Treatment[Treatment] + z_Plate[Plate] + z_Sample[Sample] + z_Repl
  Value <- Value/mean(Value)
  
  # bind everything together and 
  d <- cbind.data.frame(Value,Plate,Treatment,Sample,Repl)
  d$Treatment <- paste0("Tr",d$Treatment)
  d$Plate <- paste0("Pl",d$Plate)
  d$Sample <- paste0("S",d$Sample)
  d$Repl <- paste0("R",d$Repl)
  
  return(list(data = d,
              sigma_sample = sigma_sample,
              z_Sample = z_Sample,
              z_Plate = z_Plate))
}

# plot 
plot_dat <- function(data) {
  p <- ggplot(data, 
         aes(
           x = Repl,
           y = Value,
           col = factor(Repl)
         )) +
    geom_hline(yintercept = 1, lty = 2, col = "orangered")+
    geom_line(aes(group = Sample),col = "black", linewidth =0.25)+
    geom_point(position = position_dodge(width =0.5),show.legend = FALSE) +
    ylim(0,NA)+
    # scale_x_continuous(breaks = 1:2, minor_breaks = NULL)+
    scale_color_manual(name = "Repl",values = c("#00baff","goldenrod"))+
    ggh4x::facet_nested(.~Treatment+Plate) +
    labs(#title = m,
         y = "Signal (adjusted, scaled)",
         x = "Replicate")+
    theme_bw() +
    theme(axis.title.x = element_text(family = "mono"),
          axis.text = element_text(family = "mono"),
          axis.title.y = element_text(family = "mono"),
          legend.text = element_text(family = "mono"),
          legend.title = element_text(family = "mono")
          )
  plot(p)
  ggsave("synth_data.png",plot = p, dpi = 300, height=3.5,width = 6)
}



