#' Plot functions
#'
#' @description
#' `box` creates boxplots showing the range of expert estimates.
#' `range` creates pointrange plots showing individual expert estimates along
#' with the average value.
#'
#' @param df A list containing the dataframes with the individual estimates to
#'   be plotted. For `range`, each dataframe should include an additional (logical)
#'   column specifying which expert estimate should be highlighted.
#' @param avg A list containing the dataframes with the averaged data to be
#'   plotted. Dataframes should be in the same order as `df` for the pointrange
#'   plots.
#' @param page Index specifying which dataframe from `df` should be plotted.
#' @param expert Index specifying which expert estimates should be highlighted
#'   in the plots.
#' @param nexp Number of expert estimates to plot for pointrange plots
#'
#' @details
#' Uses ggplot to create boxplots and pointrange plots showing expert estimates
#' for review as part of structured expert elicitation process.
#'
#' @return
#' Multi-panel plots corresponding to a dataframe in `df` specified by `page`,
#' with the individual estimates from `expert` highlighted.

box <- function (df, page, expert) {
  require(ggplot2)
  require(cowplot)

  temp.plot <-
    ggplot(df, aes(x = Estimate, # for each Ecological group, plot Estimate Type on x-axis
                   y = Value) # and Value on y-axis,
           # fill = Estimate) # and colour the boxplots by estimate type
    ) +
    # geom_violin() +  # option to add a violin plot around the boxplot - if doing so, reduce boxplot width in next line
    geom_boxplot(aes(fill = Scenario),
                 position = position_dodge(0.9), outlier.size = 1, width=0.6, fatten = 1) +
    # geom_dotplot(aes(fill = Scenario), binaxis='y', stackdir='center', position = position_dodge(0.9)) +
    geom_point(data = subset(df, Expert == expert), # plot expert [j]'s estimates as blue points
               aes(x = Estimate, y = Value, fill = Scenario),
               position = position_dodge(0.9)
               , size = 2
               , shape = 17
               , color = 'royalblue2'
    ) + # include the geom_point option only if you want to highlight individual expert responses
        theme_cowplot() +  # use the minimalist theme "cowplot"
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), # adjust margins around the outside of the plot (T, R, B, L)
          panel.spacing = unit(1, "lines"), # adjust margins and between panels of the plot (spacing of 1)
          axis.title = element_text(size = 12),
          axis.title.y = element_text(margin = margin(t = 0,
                                                      r = 10,
                                                      b = 0,
                                                      l = 0) # adjust space bet. y-axis numbers and y-axis label
          ),
          axis.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          plot.caption = element_text(size = 10, hjust = 0)
    ) +
    #try a facet_wrap by species instead, and do a paired (baseline/action) plot above
    facet_wrap( ~ Biodiversity, nrow = 3) +  # create a separate panel of estimates for each management strategy
    theme(strip.text.x = element_text(size = 10, margin = margin(0, 0, 0, 0))) +
    scale_x_discrete(name = "Estimates",
                     breaks = c("LOWEST", "BEST GUESS", "HIGHEST"),
                     labels = c("Low", "Best", "High") # Give the x-axis variables shortened labels
    ) +
    scale_fill_manual(values = c("gray80", "#E69F00")) +
    # theme(legend.position = c(0.9,0.1)) +

    # scale_fill_manual(values = c("white", "gray80", "white"), # Assign colours to each type of estimate
    #                   guide = "none" # remove legend
    #                   ) +
    labs(x = "Estimates",
         y = "Probability of persistence (%)"
         # ,title = paste(box.levels[page])
         # ,caption = str_wrap(paste0(
         #   "Figure ", page, ". Boxplots summarizing the distribution of the lowest (L), best guess (B), and highest (H) expert
         #     estimates of the probability of persistence of ", box.levels[page], " under the Baseline scenario and with the Action.
         #     The thick horizontal lines indicate the median estimate, while the surrounding box shows the interquartile range.
         #     Any outliers are shown as black dots beyond the plot whiskers. Your individual estimates are shown as blue triangles."), 200)
    ) +
    ylim(0, 100) # set the y-axis limits from 0-100

}

range <- function (df, avg, page, nexp) {
  require(ggplot2)
  require(cowplot)
  
  temp.plot2 <-
    ggplot(df, aes(x = Expert, # using the data Ecological group, plot Experts on X-axis
                             y = `BEST GUESS` # and corresponding standardized estimates on y-axis
                             , color = expi # use this only if highlighting individual expert responses
    )
    ) +
    geom_pointrange(aes(ymin = LOWEST, ymax = HIGHEST)) +
    scale_color_manual(values = c("gray10", "blue"), guide = "none") + # only needed if highlighting individual expert responses
    geom_hline(aes(yintercept = `BEST GUESS`), avg, colour = "gray40") +
    geom_hline(aes(yintercept = LOWEST), avg, colour = "gray40", lty = "dashed") +
    geom_hline(aes(yintercept = HIGHEST), avg, colour = "gray40", lty = "dashed") +
    theme_cowplot() +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), # adjust margins around the outside of the plot
          panel.spacing = unit(1, "lines"), # adjust margins and between panels of the plot (spacing of 1)
          axis.title = element_text(size = 12),
          axis.title.y = element_text(margin = margin(t = 0,
                                                      r = 10,
                                                      b = 0,
                                                      l = 0)), # adjust space between y-axis numbers and y-axis label
          axis.text = element_text(size=10),
          legend.justification=c(1,0), legend.position=c(0.98,-0.05), # re-positions legend box
          plot.caption = element_text(size = 10, hjust = 0)
    ) +
    scale_x_continuous(breaks = seq(1,nexp,1), limits = c(1,nexp)) +
    facet_wrap( ~ Biodiversity + Scenario, ncol = page) +  # create a separate panel of estimates for each management strategy
    theme(strip.text.x = element_text(size = 10, margin = margin(0, 0, 0, 0)))+
    labs(x = "Expert",
         y = "Probability of persistence (%)"
         # , title = paste(pointrange.levels[page])
         # ,caption = str_wrap(paste0(
         #   "Figure ", i, ". Plots of the probability of persistence of ", pointrange.levels[page], " under the Baseline scenario and with the Action.
         #     Each point represents the estimates from one expert (Best Guess = solid dots, Lowest and Highest estimates = vertical lines).
         #     Your individual estimates are plotted in blue. The horizontal lines indicate the probability of persistence averaged over the
         #     number of experts that provided estimates (Best Guess = solid line, Lowest and Highest estimates = dashed lines)."), 200)
    ) +
    ylim(0, 100) # set the y-axis limits from 0-100

}
