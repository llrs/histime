library("dplyr")
library("ggplot2")
library("lubridate")

ciber <- read.csv("~/Downloads/CIBER_BCN.csv") %>%
    mutate(
        FechaInicio = dmy(FechaInicio),
        FechaNacimiento = dmy(FechaNacimiento),
        EdadInicio = (FechaInicio-FechaNacimiento)/as.duration(years(1)),
        Ciber = toupper(Ciber),
        FechaFinal = case_when(
            TipoContrato == "Postdoctoral 5 años" ~ FechaInicio + years(5),
            TipoContrato == "Predoctoral 4 años" ~ FechaInicio + years(4))
    )
ciber$EdadInicio[ciber$EdadInicio <20] <- NA
ciber %>%
    count(SEXO)
ciber %>%
    count(Categoria, sort = TRUE)
ciber %>%
    count(TipoContrato, sort = TRUE)
ciber %>%
    count(Mesa)
ciber %>%
    count(Ciber, sort = TRUE)

ggplot(ciber) +
    geom_jitter(aes(TipoContrato, EdadInicio, col = SEXO)) +
    theme_minimal()

ciber %>%
    ggplot() +
    geom_line()
ciber %>%
    filter(!is.na(FechaFinal)) %>%
    mutate(index = 1:n()) %>%
    ggplot() +
    geom_linerange(aes(x = index, ymin = FechaInicio, ymax = FechaFinal), size = 2) +
    coord_flip() +
    theme_minimal()

ciber %>%
    filter(!is.na(FechaFinal)) %>%
    ggplot() +
    geom_histogram(aes())
