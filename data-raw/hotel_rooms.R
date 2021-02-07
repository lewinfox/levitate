## Dataset provided by Susan Li https://github.com/susanli2016
f <- system.file("extdata", "hotel_rooms.csv", package = "levitate", mustWork = TRUE)
hotel_rooms <- readr::read_csv(f, col_types = "cc")
usethis::use_data(hotel_rooms, overwrite = TRUE)
