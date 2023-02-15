# load necessary packages
library(tidyverse)
library(showtext)
source("functions.r")

set_plot_theme()
# add fonts for plotting
font_add(
    family = "lmroman",
    regular = "Fonts/lmroman10_regular.ttf",
    bold = "Fonts/lmroman10_bold.ttf",
    italic = "Fonts/lmroman10_italic.ttf",
    bolditalic = "Fonts/lmroman10_bolditalic.ttf",
    symbol = "Fonts/lmroman_math.ttf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 600)

# Bilateral Clearing
bilateral_clearing <- NULL |>
    ggplot() +
    labs(title = "Bilateral Clearing")

# save plot
ggsave(
    "Plots/Output/Empty_Bilateral_Clearing.svg", bilateral_clearing,
    width = 6.2, height = 6.2, unit = "cm", dpi = 600, device = "svg"
)

# CCP Clearing
ccp_clearing <- NULL |>
    ggplot() +
    labs(title = "CCP Clearing")

# save plot
ggsave(
    "Plots/Output/Empty_CCP_Clearing.svg", ccp_clearing,
    device = "svg",
    width = 6.2, height = 6.2, unit = "cm", dpi = 600
)

# Client Position Porting
client_porting <- NULL |>
    ggplot() +
    labs(title = "Client Position Porting")

# save plot
ggsave(
    "Plots/Output/Empty_Client_Porting.svg", client_porting,
    width = 7.1, height = 5.8, unit = "cm", dpi = 600, devic = "svg"
)

# Closing out of Direct Member
closing_out <- NULL |>
    ggplot() +
    labs(title = "Closing out of Direct Member")

# save plot
ggsave(
    "Plots/Output/Empty_Closing_Out.svg", closing_out,
    device = "svg",
    width = 7.1, height = 5.8, unit = "cm", dpi = 600
)
