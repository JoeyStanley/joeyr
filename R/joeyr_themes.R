theme_joey <- function () {
    theme_bw(base_size=12, base_family="Avenir") %+replace%
        theme(panel.background  = element_blank(),
              # Setting this as "transparent" doesn't work with custom fonts for some reason. Hard code it in for now. )
              plot.background   = element_rect(fill="gray96",      colour=NA), # Smokewhite.
              legend.background = element_rect(fill="transparent", colour=NA), # For some reason this works.
              legend.key        = element_rect(fill="transparent", colour=NA),
              legend.justification=c(1,0),
              legend.position=c(1,0),
              plot.title = element_text(hjust = 0.5)) # center the title for R 2.0
}

theme_joey_white <- function () {
    theme_bw(base_size=12, base_family="Avenir") %+replace%
        theme(panel.background  = element_blank(),
              # Setting this as "transparent" doesn't work with custom fonts for some reason. Hard code it in for now. )
              plot.background   = element_rect(fill="white",       colour=NA),
              legend.background = element_rect(fill="transparent", colour=NA), # For some reason this works.
              legend.key        = element_rect(fill="transparent", colour=NA),
              legend.justification=c(1,0),
              legend.position=c(1,0),
              plot.title = element_text(hjust = 0.5)) # center the title for R 2.0
}

theme_joey_legend <- function () {
    theme_bw(base_size=12, base_family="Avenir") %+replace%
        theme(panel.background  = element_blank(),
              # Setting this as "transparent" doesn't work with custom fonts for some reason. Hard code it in for now. )
              plot.background   = element_rect(fill="gray96",      colour=NA), # Smokewhite.
              legend.background = element_rect(fill="transparent", colour=NA), # For some reason this works.
              legend.key        = element_rect(fill="transparent", colour=NA),
              plot.title = element_text(hjust = 0.5)) # center the title for R 2.0
}

theme_whitesmoke <- function() {
    theme_bw() %+replace%
        theme(panel.background  = element_blank(),
              # Setting this as "transparent" doesn't work with custom fonts for some reason. Hard code it in for now. )
              plot.background   = element_rect(fill="gray96",      colour=NA), # Smokewhite.
              legend.background = element_rect(fill="transparent", colour=NA), # For some reason this works.
              legend.key        = element_rect(fill="transparent", colour=NA),
              legend.justification=c(1,0),
              legend.position=c(1,0),
              plot.title = element_text(hjust = 0.5)) # center the title for R 2.0
}

# An arrow I happen to like for vowel plots.
joey_arrow <- function() {
  arrow(type = "closed", length = unit(0.1, "inches"), angle = 20)
}