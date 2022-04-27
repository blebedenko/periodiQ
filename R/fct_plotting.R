#' Plot the rate function
#'
#' @param params  names list of quantiles (output of 'listParams()').
#' @param n_cycles of cycles to plot (defaults to 5)
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' pltRate(exampleParams())
pltRate <- function(params, n_cycles = 2) {
  gamma <- params$gamma
  lambda_0 <- params$lambda_0
  lam <- function(t) {
    lambda_0 + (gamma / 2) * (cos(2 * pi * t) + 1)
  }
  graphics::curve(
    lam,
    from = 0,
    to = n_cycles,
    lwd = 2,
    xlab = "time",
    ylab = expression(lambda(t)),
    main =  bquote("quantiles:" ~ gamma == .(gamma) ~ "and" ~ lambda[0] == .(lambda_0))
  )
}





#' Plot the queue dynamics in a specified time interval
#'
#' @param segments number of segments to divide one cycle into. Defaults to 24 (hours per day).
#' @param RES full simulations results list
#' @param params scenario quantiles
#' @importFrom magrittr %>%
#' @export
pltArrivalsByTimeSegment <- function(segments = 24, RES, params) {
  if (length(segments) != 1 || segments <= 0)
    stop ("number of segments has to be a positive integer")
  A_tilde <- cumsum(RES$Aj)
  time_of_day <- A_tilde - trunc(A_tilde)
  breaks <- seq(0, 1, by = 1 / segments)
  arrivals_segmented <- cut(time_of_day, breaks)
  levels(arrivals_segmented) <- 1:segments
  dat <-
    data.frame(
      time = time_of_day,
      queue = RES$Qj,
      waiting = pmax(RES$Qj - params$s, 0)
    )
  dat$segment <- cut(dat$time, breaks, labels = 1:segments)
  dat <- dat %>% group_by(segment) %>% summarise(L = mean(queue),
                                                 Lw = mean(waiting),
                                                 arrivals = n())
  p1 <-
    lattice::xyplot(
      arrivals ~ segment,
      data = dat,
      type = "h",
      lwd = 3,
      lty = 3,
      col = 1
    )
  p2 <-
    lattice::xyplot(
      dat$Lw ~ as.numeric(segment) - 0.5,
      data = dat,
      type = "h",
      lty = 1,
      ylab = "No. Waiting",
      col = 2,
      lwd = 4,
      key = list(
        lines = list(col = 1:2 , lwd = 3:4),
        text = list(lab = c(
          'Arrivals/segment', 'No. waiting / segment'
        )),
        title = "Arrivals and queue length"
      )
    )
  latticeExtra::doubleYScale(p1, p2, add.ylab2 = T, under =)
}



# pltPatienceByJoin <- function(RES) {
#   dat <- RES$simulator
#   dat.j <- dat %>%
#     filter(join == 1) %>% as_tibble()
#   dat.ext <- dplyr::bind_rows(
#     dat %>% mutate(customer = "all") ,
#     dat.j %>% mutate(customer = "joined"),
#     dat %>% filter(join == 0) %>% mutate(customer = "balked")
#   )
#
#   # patience boxplots
#   pl_patience_boxplots <-
#     dat.ext %>%
#     ggplot() +
#     aes(x = sqrt(patience), fill = customer) +
#     geom_boxplot(notch = TRUE) +
#     theme_bw()
#
#   pl_patience_boxplots
# }


#' Plot average arrival and joined by daily time segments
#'
#' @param params model quantiles
#' simulator_data the dataframe from the full simulation
#' @param segment_n number of segments per "day", defaults to 24.
#' @importFrom magrittr %>%
#' @export
#'
plot_segments <- function(params,simulator_data, segment_n = 24) {
  range01 <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  } # for future use

  segment_breaks <- seq(0, segment_n, by = 1) #
  lambda <- rate_factory(params)
  simulator <- simulator_data
  numeric_vars <- apply(simulator, 2, is.numeric) %>% names()
  simulator %>%
    mutate(rate = lambda(arrival)) %>%
    mutate(time_of_day_h = time_of_day * segment_n) %>% # to work in "hours"
    mutate(segment = cut(time_of_day_h, segment_breaks, labels = FALSE)) %>%
    mutate(segment = as.numeric(segment) - 1) %>% # to start from zero
    group_by(day, segment) %>%
    summarize(
      total_joined = sum(join),
      total_arrived = n(),
      mean_patience_eff = sum(patience * join) / sum(join)
    ) %>%
    ungroup() %>%
    group_by(segment) %>%
    summarize(
      joined = mean(total_joined),
      arrived = mean(total_arrived)
    ) %>%
    ungroup() %>%
    pivot_longer(cols = 2:3) %>%
    set_names(c("segment","process","average")) %>%
    ggplot() +
    aes(x = segment, y = average, color = process) +
    geom_line() +
    ylab("events/segment")+
    theme(
      text = element_text(size = 20),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      panel.background = element_blank(),
      panel.grid = element_blank()
    )
}


#' Plot patience by hour
#'
#' @param simulator
#' @param params
#'
#' @return
#' @export
#'
plot_patience <- function(simulator, params){


  simulator.joined <- simulator %>%
    filter(join == 1) %>% as_tibble()


  simulator.copy <- bind_rows(
    simulator %>% mutate(customer = "all") ,
    simulator.joined %>% mutate(customer = "joined"),
    simulator %>% filter(join == 0) %>% mutate(customer = "balked")
  )

  # for mean/median lines

  line_dat <- tibble(value =  c(1 / params$theta , qexp(.5, rate = params$theta)),
                     quantile = factor(c("mean", "median")))



  simulator.copy %>%
    mutate(hour = factor(hour)) %>%
    ggplot(aes(
      x = hour,
      y = sqrt(patience),
      fill = customer
    )) +
    geom_boxplot() +
    theme(
      text = element_text(size = 20),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    ylab(expression(sqrt(Patience))) +
    geom_abline(data = line_dat, mapping = aes(intercept = value, slope = 0, color = quantile),lwd = 1.5, lty = 2) +
    scale_color_manual(values = c("purple", "deepskyblue"))


}



#' Plot the queue dynamics in a specified time interval
#'
#' @param simulator_data full simulations results list
#' @param params scenario quantiles
#' @param from_to vector of start and stop of the time interval. Defaults to c(1,2).
#' @importFrom magrittr %>%
#' @return plot only
#' @export

plot_interval <- function(simulator_data, params, from_to = c(0, 1)) {
  from <-  from_to[1]
  to <- from_to[2]
  lambda <- rate_factory(params)

  # partial data for the purpose of this plot:

  pdat <- simulator_data %>%
    mutate(arrival_time = cumsum(arrival)) %>%
    mutate(rate = lambda(arrival_time)) %>%
    filter(arrival_time >= from, arrival_time<= to)
  # separate data for the rate so the curve always comes out smooth
  rdat <- tibble(time = seq(from, to, length.out = 1000),
                 rate = lambda(time))



  p1 <-
    lattice::xyplot(
      queue ~ arrival_time,
      type = "s",
      lwd = 2,
      data = pdat,
      xlab = "time",
      col = "blue"
    )

  p2 <-
    lattice::xyplot(
      rate ~ time,
      data = rdat,
      type = "l",
      col = "purple",
      lwd = 3,
      lty = 3,
      key = list(
        #corner = c(0,0),
        # space = c("top"),
        title = "Arrival rate and queue length",
        lines =
          list(
            col = c("purple", "blue"),
            lty = c(3, 1),
            lwd = c(3, 2)
          ),
        text =
          list(c(expression(lambda(
            t
          )), expression(Q(
            t
          ))))
      )
    )

  latticeExtra::doubleYScale(p1, p2, add.ylab2 = T)
}


#' Barplot of hourly averages - queue and arrivals
#'
#' @param simulator_data
#' @param params
#'
#' @return
#' @export
plot_hourly_queue_arrivals <- function(simulator_data, params){
  simulator_data %>%
    filter(day > 0) %>% # remove the first day
    group_by(day,hour) %>%
    summarize(queue = mean(queue),
              joined= sum(join)) %>%
    group_by(hour) %>%
    summarize(queue = mean(queue),
              joined = mean(joined)) -> dat_queue_joined

  p_queue <-
    lattice::xyplot(
      queue ~ hour ,
      type = "h",
      lwd = 2,
      data = dat_queue_joined %>% mutate(hour = hour + .5),
      xlab = "time",
      col = "blue"
    )

  p_join <-
    lattice::xyplot(
      joined ~ hour,
      data = dat_queue_joined,
      type = "s",
      col = "purple",
      lwd = 3,
      lty = 3,
      key = list(
        #corner = c(0,0),
        # space = c("top"),
        title = "Queue length and hourly arrivals",
        lines =
          list(
            col = c("blue", "purple"),
            lty = c(3, 1),
            lwd = c(3, 2)
          ),
        text =
          list(lab = c("hourly queue length", "customers joining"))
      )
    )
  latticeExtra::doubleYScale(p_queue,p_join)
}



#' Breakdown of hourly states
#'
#' @param simulator_data
#' @param params
#'
#' @return
#' @export
plot_hourly_join_prob_queue <- function(simulator_data, params){
  lambda <- rate_factory(params)
  simulator_data %>%
    filter(day > 0) %>% # remove the first day
    group_by(day,hour) %>%
    summarize(prob_join = mean(join),
              customers = mean(queue)) %>%
    mutate(rate = lambda(hour/24))   -> dat_scatter_queue_joined



   StatMeanLine <- ggproto("StatMeanLine", Stat,
                          compute_group = function(data, scales) {
                            transform(data, yintercept=mean(y))
                          },
                          required_aes = c("x", "y")
  )

  stat_mean_line <- function(mapping = NULL, data = NULL, geom = "hline",
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, ...) {
    layer(
      stat = StatMeanLine, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }

  StatMeanLineV <- ggproto("StatMeanLineV", Stat,
                          compute_group = function(data, scales) {
                            transform(data, xintercept=mean(x))
                          },
                          required_aes = c("x", "y")
  )

  stat_mean_line_v <- function(mapping = NULL, data = NULL, geom = "vline",
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, ...) {
    layer(
      stat = StatMeanLineV, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }

  ggplot(dat_scatter_queue_joined) +
    aes(x = customers, y = prob_join) +
    geom_point(aes(color = rate, alpha = day))+
    stat_mean_line(color = "purple") +
    stat_mean_line_v(color = "darkgreen") +
    facet_wrap(~hour,nrow = 6,ncol = 4) +
    xlab("customers in system") +
    ylab("probability of joining") +
    ggplot2::scale_color_distiller(palette = "Spectral") +
    ylim(c(0,1))

}



