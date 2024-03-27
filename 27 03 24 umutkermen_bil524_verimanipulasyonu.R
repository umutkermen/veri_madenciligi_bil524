# temel fonksiyonlar ve veri manipülasyonu

getwd()
kup_alma <- function(x) {
  return(x^3)
}
print(kup_alma(7))

#------------------

sayac <- function(a) {
  print(a) #sayıları basıyor
  while (a != 0){ #sonlanacağı değer
    Sys.sleep(1) #aralık
    a <- a-1 # 1 azalarak atama yapacak
    print(a)
    
  }
}
sayac (11) # sayaç fonksiyonu oluşturduk

#--------------------

veri = read.csv("https://johnmuschelli.com/intro_to_r/data/Youth_Tobacco_Survey_YTS_Data.csv")
View(veri)
head(veri) 
View(mtcars)
dim(veri)
ncol(veri)
nrow(veri)

help("dim")

#-----------------

install.packages("dplyr")
library(dplyr)
veri_1 <- rename(veri, year_new=YEAR)
names(veri_1)

install.packages("readr")
library(readr)
write.csv(veri_1, path = "C:/Users/emel/Desktop/tutun.csv")
ls()

install.packages("tidyverse")
library(tidyverse)

mtcars_data = data("mtcars")
mtcars_1 = data.frame(mtcars)
dim(mtcars_1)
head(mtcars_1)
view(mtcars_1)


mtcars_1_tibble = as_tibble(mtcars_1) # mtcars_1 verisini tibble a dönüştürüp yeni bir nesne ile kaydettik.
head(mtcars_1_tibble)

mtcars_2 = dplyr::rename(mtcars_1_tibble, CARB=carb)
mtcars_2_hepsi_buyuk_harf = dplyr:: rename_all(mtcars_1_tibble, toupper) # yeni nesnelere ataya ataya gidiyorum. bütün değişken isimlerini büyük harf yaptık.

names(mtcars)

mtcars$hp
mtcars_hp = mtcars$hp
mtcars_hp_veri = data.frame(mtcars$hp)
dim(mtcars_hp_veri)


mtcars_wt = select(mtcars, wt)
dim(mtcars_wt)
pull(select(mtcars, wt))
mtcars_pull_wt = pull(select(mtcars, wt))
dim(mtcars_pull_wt)


filter(mtcars, cyl > 8 | cyl < 6)
mtcars_cyl_aralik = filter(mtcars, cyl > 8 | cyl < 6)
mtcars_geara_gore = select(filter(mtcars, cyl > 8 | cyl < 6), gear)
mycars_gear_1 = select(mtcars_cyl_aralik, gear)
