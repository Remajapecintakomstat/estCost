#' Hitung Biaya Perjalanan
#' Menghitung biaya bahan bakar dan durasi perjalanan.
#'
#' @param jarak Numeric. Jarak perjalanan (dalam km).
#' @param konsumsi Numeric. Konsumsi bahan bakar kendaraan (km/liter).
#' @param harga Numeric. Harga bahan bakar per liter.
#' @param kecepatan Numeric (optional). Kecepatan rata-rata kendaraan (km/jam).
#'
#' @return List dengan elemen:
#'   - `biaya_bahan_bakar`: Total biaya bahan bakar.
#'   - `durasi_perjalanan`: Durasi perjalanan dalam jam (jika kecepatan diberikan).
#' @export
hitung_biaya_perjalanan <- function(jarak, konsumsi, harga, kecepatan = NULL) {
  biaya_bahan_bakar <- (jarak / konsumsi) * harga
  durasi <- if (!is.null(kecepatan)) jarak / kecepatan else NA
  list(
    biaya_bahan_bakar = biaya_bahan_bakar,
    durasi_perjalanan = durasi
  )
}

#' Estimasi Biaya Produksi
#' Menghitung total biaya produksi dan biaya per unit.
#'
#' @param bahan_baku Numeric. Biaya bahan baku.
#' @param tenaga_kerja Numeric. Biaya tenaga kerja.
#' @param overhead Numeric. Biaya overhead.
#' @param jumlah_produksi Numeric. Total unit produksi.
#'
#' @return List dengan elemen:
#'   - `total_biaya_produksi`: Total biaya produksi.
#'   - `biaya_per_unit`: Biaya produksi per unit.
#' @export
estimasi_biaya_produksi <- function(bahan_baku, tenaga_kerja, overhead, jumlah_produksi) {
  total_biaya_produksi <- bahan_baku + tenaga_kerja + overhead
  biaya_per_unit <- total_biaya_produksi / jumlah_produksi
  list(
    total_biaya_produksi = total_biaya_produksi,
    biaya_per_unit = biaya_per_unit
  )
}

#' Simulasi Harga Jual
#' Menghitung harga jual dan margin keuntungan.
#'
#' @param biaya_per_unit Numeric. Biaya produksi per unit.
#' @param markup Numeric. Persentase markup untuk keuntungan.
#'
#' @return List dengan elemen:
#'   - `harga_jual`: Harga jual per unit.
#'   - `margin_keuntungan`: Margin keuntungan per unit.
#' @export
simulasi_harga_jual <- function(biaya_per_unit, markup) {
  harga_jual <- biaya_per_unit * (1 + markup / 100)
  margin_keuntungan <- harga_jual - biaya_per_unit
  list(
    harga_jual = harga_jual,
    margin_keuntungan = margin_keuntungan
  )
}

#' Hitung Cicilan Kredit
#' Menghitung cicilan bulanan untuk kredit.
#'
#' @param jumlah_pinjaman Numeric. Total jumlah pinjaman.
#' @param bunga_tahunan Numeric. Suku bunga tahunan (dalam persen).
#' @param lama_pinjaman Integer. Lama pinjaman dalam bulan.
#'
#' @return Numeric. Besar cicilan bulanan.
#' @export
hitung_cicilan <- function(jumlah_pinjaman, bunga_tahunan, lama_pinjaman) {
  bunga_bulanan <- bunga_tahunan / 12 / 100
  cicilan_bulanan <- (jumlah_pinjaman * bunga_bulanan) /
    (1 - (1 + bunga_bulanan)^-lama_pinjaman)
  cicilan_bulanan
}

#' Hitung Untung Rugi
#' Menghitung total keuntungan atau kerugian dari penjualan barang.
#'
#' @param harga_beli Numeric. Harga beli per unit.
#' @param harga_jual Numeric. Harga jual per unit.
#' @param jumlah_barang Integer. Jumlah barang yang dijual.
#'
#' @return List dengan elemen:
#'   - `untung_rugi`: Total keuntungan atau kerugian.
#'   - `status`: Status "Untung" atau "Rugi".
#' @export
hitung_untung_rugi <- function(harga_beli, harga_jual, jumlah_barang) {
  untung_rugi <- (harga_jual - harga_beli) * jumlah_barang
  status <- ifelse(untung_rugi > 0, "Untung", "Rugi")
  list(
    untung_rugi = untung_rugi,
    status = status
  )
}

#' Hitung Diskon
#' Menghitung harga setelah diskon dan nilai diskon.
#'
#' @param harga_awal Numeric. Harga awal sebelum diskon.
#' @param diskon_persen Numeric. Persentase diskon.
#'
#' @return List dengan elemen:
#'   - `harga_diskon`: Harga setelah diskon.
#'   - `diskon`: Nilai diskon.
#' @export
hitung_diskon <- function(harga_awal, diskon_persen) {
  diskon <- harga_awal * diskon_persen / 100
  harga_diskon <- harga_awal - diskon
  list(
    harga_diskon = harga_diskon,
    diskon = diskon
  )
}

#' Hitung Bagi Hasil
#' Menghitung nilai bagi hasil berdasarkan persentase.
#'
#' @param keuntungan_total Numeric. Total keuntungan.
#' @param persentase Numeric. Persentase bagi hasil.
#'
#' @return Numeric. Nilai bagi hasil.
#' @export
hitung_bagi_hasil <- function(keuntungan_total, persentase) {
  keuntungan_total * (persentase / 100)
}

#' Kalkulator Pajak
#' Menghitung total pajak berdasarkan pendapatan dan tarif pajak.
#'
#' @param pendapatan Numeric. Total pendapatan.
#' @param tarif_pajak Numeric. Tarif pajak (dalam persen).
#'
#' @return Numeric. Jumlah pajak yang harus dibayar.
#' @export
hitung_pajak <- function(pendapatan, tarif_pajak) {
  pendapatan * tarif_pajak / 100
}

#' Hitung Break-Even Point (BEP)
#' Menghitung jumlah unit yang harus terjual untuk mencapai BEP.
#'
#' @param biaya_tetap Numeric. Total biaya tetap.
#' @param biaya_variabel Numeric. Biaya variabel per unit.
#' @param harga_jual Numeric. Harga jual per unit.
#'
#' @return Numeric. Jumlah unit yang harus terjual untuk mencapai BEP.
#' @export
hitung_bep <- function(biaya_tetap, biaya_variabel, harga_jual) {
  biaya_tetap / (harga_jual - biaya_variabel)
}

#' Hitung Total Belanja
#' Menghitung total belanja berdasarkan harga dan kuantitas barang.
#'
#' @param harga Numeric vector. Harga barang.
#' @param kuantitas Numeric vector. Kuantitas barang.
#'
#' @return Numeric. Total belanja.
#' @export
hitung_total_belanja <- function(harga, kuantitas) {
  sum(harga * kuantitas)
}
