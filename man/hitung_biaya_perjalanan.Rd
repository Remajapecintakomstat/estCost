% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estCost.R
\name{hitung_biaya_perjalanan}
\alias{hitung_biaya_perjalanan}
\title{Hitung Biaya Perjalanan
Menghitung biaya bahan bakar dan durasi perjalanan.}
\usage{
hitung_biaya_perjalanan(jarak, konsumsi, harga, kecepatan = NULL)
}
\arguments{
\item{jarak}{Numeric. Jarak perjalanan (dalam km).}

\item{konsumsi}{Numeric. Konsumsi bahan bakar kendaraan (km/liter).}

\item{harga}{Numeric. Harga bahan bakar per liter.}

\item{kecepatan}{Numeric (optional). Kecepatan rata-rata kendaraan (km/jam).}
}
\value{
List dengan elemen:
  - `biaya_bahan_bakar`: Total biaya bahan bakar.
  - `durasi_perjalanan`: Durasi perjalanan dalam jam (jika kecepatan diberikan).
}
\description{
Hitung Biaya Perjalanan
Menghitung biaya bahan bakar dan durasi perjalanan.
}
