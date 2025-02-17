# Este me puede servir para VISITAS y VENTAS
### Example with simpler data, few data points
set.seed(1)
z <- ts(cbind(a = 1:5, b = 11:15, c = 21:25) + rnorm(5))
xyplot(z, screens = 1)
xyplot(z, screens = list(a = "primary (a)", "other (b & c)"),
       type = list(a = c("p", "h"), b = c("p", "s"), "o"),
       pch = list(a = 2, c = 3), auto.key = list(type = "o"))


xyplot(EuStockMarkets, screens = list(FTSE = "UK", "Continental"),
       col = list(DAX = "red", FTSE = "blue", CAC='green',"black"), auto.key = TRUE)

xyplot(EuStockMarkets, screens = list(FTSE = "UK", CAC="UK", "Continental"),
       superpose = TRUE)

xyplot(sunspot.year, aspect = "xy",
       strip = FALSE, strip.left = TRUE,
       cut = list(number = 3, overlap = 0.05))

asTheEconomist(xyplot(window(sunspot.year, start = 1900),
                      main = "Sunspot cycles", sub = "Number per year"))
trellis.last.object() +
  layer_(panel.xblocks(x, x >= 1980, col = "#6CCFF6", alpha = .5)) +
  layer(panel.text(1988, 180, "Forecast", font = 3, pos = 2))

opar <- trellis.par.get()
trellis.par.set(theEconomist.theme(box = "transparent"))
oopt <- lattice.options(theEconomist.opts())

barchart(Titanic[,,,"No"], main = "Titanic deaths", layout = 1:2,
         sub = "by sex and class", auto.key = list(columns = 2),
         scales = list(y = list(alternating = 2)))

asTheEconomist(
  dotplot(VADeaths, main = "Death Rates in Virginia (1940)",
          auto.key = list(corner = c(.9,0.1))),
  type = "b", with.bg = TRUE)

dotplot(VADeaths, auto.key = TRUE, type = "b",
        par.settings = theEconomist.theme(with.bg = TRUE))

asTheEconomist(
  densityplot(~ height, groups = voice.part, data = singer,
              subset = grep("1", voice.part), plot.points = FALSE)) +
  glayer(d <- density(x), i <- which.max(d$y),
         ltext(d$x[i], d$y[i], paste("Group", group.number), pos = 3))

trellis.par.set(opar)
lattice.options(oopt)


data(barley)
barchart(variety ~ yield | year * site, data = barley, aspect = 0.4,
         xlab = "Barley Yield (bushels/acre)")
