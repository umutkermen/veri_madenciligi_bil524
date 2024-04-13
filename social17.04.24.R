# Kendi ara??t??rmalar??mdan birinin verisi ??zerinde ??devi ger??ekle??tirece??im. 
#Sadece sentetik veri olu??turmada sentetik veriyi kullanaca????m. 
#Gerisinde "social" veri setini kullanaca????m. "social" veri seti age, addic, stres ve anxie de??i??kenlerini i??eren 400 sat??rl?? bir veri setidir.

# SENTET??K VER?? 

install.packages("MASS")
library (MASS) # sentetik veriyi ??retmek i??in MASS paketi indirildi.

set.seed(123) #tekrarlanabilirlik seed de??eri belirlendi.
n <- 215 # 215ki??ilik veri seti istendi.

b1 <- rnorm(n, mean = 15, sd = 6)
b2 <- rnorm(n, mean = 13, sd = 7)
b3 <- rnorm(n, mean = 4, sd = 1.5)
c1 <- rnorm(n, mean = 9, sd = 1.05)
c2 <- rnorm(n, mean = 14, sd = 6.5) # 5 adet ba????ms??z de??i??ken ??retildi.

sort(b1) # b1 de??i??keni de??erleri k??????kten b??y????e s??raland??.

y <- 12 - 4*b1 + 1.5*b2 + 2.75*b3 - 3*c1 - 1.5*c2 + rnorm(n, mean = 5, sd = 2)
# ba????ms??z de??i??kenler b ve c'leri kullanarak, hata terimi ekleyerek, y isimli ba????ms??z bir de??i??ken ??rettik.

sentetik_veri_seti <- data.frame (y, b1, b2, b3, c1, c2) 
# ??rettimiz ba????ms??z de??i??kenler ve ba????ml?? de??i??ken ile sentetik bir veri seti olu??turuldu.

b2_factoral <- as.factor (b2) #b2 de??i??keni fakt??re ??evirildi.
c2_numeric <- as.numeric(c2) #c2 de??i??keni n??merik veriye ??evrildi.
str(b2_factoral, c2_numeric) #b2 ve c2 verilerinin yap??s?? kontrol edildi.


# Bu noktadan itibaren "social" veri setinden devam ediyoruz.
#VER?? KAL??TES??

social <- read_sav("C:/Users/DB/Desktop/social.sav") # social veri seti bilgisayardan ??ekildi.

isna_social <- is.na(social) # Bo?? g??zlemler i??in TRUEve FALSE sogulams?? yap??ld??. 

mean_age <- mean(social$age, na.rm = T) # NA lar?? dikkate almadan ortalama ya?? bulundu.
age_mean_doldurulmus <- ifelse(is.na(social$age),
                               mean_age, social$age) # age de??i??keni ortalama ya?? de??eri ile dolduruldu.

install.packages("zoo")
library(zoo)
age_sonraki_ile_doldurulmus <- na.locf(social$age)
print(age_sonraki_ile_doldurulmus) #zoo paketinin na.locf fonksiyonunu kullanarak age i??indeki bo?? g??zlemleri sonraki g??zlem de??erleri ile doldurduk

medyan_age <- median(social$age, na.rm = T)
medyan_addic <- median(social$addic, na.rm = T)
medyan_stres <- median(social$stres, na.rm = T)
medyan_anxie <- median(social$anxie, na.rm = T)
age <- ifelse(is.na(social$age),
              medyan_age, social$age)
addic <- ifelse(is.na(social$addic),
                medyan_addic, social$addic)
stres <- ifelse(is.na(social$stres),
                medyan_stres, social$stres)
anxie <- ifelse(is.na(social$anxie),
                medyan_anxie, social$anxie)
veri_seti_dolu <- data.frame(age, addic, stres, anxie)
# age, addic, stres ve anxie i??in medyanlar?? bulundu.
# 4 de??i??ken i??indeki NA'lar kendi medyanar?? ile dolduruldu.
# ilk uygulamay?? "academic" veri setinde yapm????, ve bo?? g??zlemler nedeniyle bir??ok hata alm????t??m.
# bu hatalardan ka??mak, i??imi kolayla??t??rmak i??in doldurulmu?? veri seti olu??turdum.
# bu a??amadan itibaren veri_seti_dolu ??zerinden gidece??iz.

# AYKIRI DE??ERLER

boxplot(veri_seti_dolu) # age, addic, stres ve anxie i??in boxplot ??izdirildi.
#age verisinde u?? de??erler oldu??u g??r??lmektedir.

hist(veri_seti_dolu$age)
hist(veri_seti_dolu$addic)
hist(veri_seti_dolu$stres)
# age verisi normal da????l??ma sahip de??ildir. addic ve stres ise normal da????l??ma sahip g??r??nmektedir.

z_skor_veri<- scale(veri_seti_dolu) #de??i??kenleri z skorlara ??evirdik. Age versinde +3 z skor ??zerinde 5 veri oldu??unu g??rd??k.
outliers <- abs (z_skor_veri) > 3 # mutlak de??er kullanarak -3/+3 aras??ndaki de??erleri u?? de??er olarak istedik.
sort(veri_seti_dolu$age) # age verisinde +3 z skor ??st??ndeki 5 veriyi s??ralam???? ve g??rm???? olduk.

#ALT ve ??ST ??EYREKLER/ALT ve ??ST SINIRLAR

Q1_age <- quantile(veri_seti_dolu$age, probs = (0.25))
Q3_age <- quantile(veri_seti_dolu$age, probs = (0.75))
IQR <- Q1_age - Q3_age
# age i??in birinci/??????nc?? ??eyrek de??erleri ve iki ??eyrek fark?? hesapland??.

lower_age <- Q1_age - 1.5*IQR
upper_age <- Q3_age + 1.5*IQR
# age i??in alt ve ??st s??n??r de??erleri hesapland??.

outliers_yeni <- veri_seti_dolu$age <lower_age | veri_seti_dolu$age>upper_age
View(outliers_yeni)
# age verisi i??in u?? de??erleri mant??ksal sorgulama olarak olu??turduk ve g??rd??k.

# DA??ILIMLAR

hist(veri_seti_dolu$anxie)
boxplot(veri_seti_dolu$anxie)
qqnorm(veri_seti_dolu$anxie, pch = 1, frame = FALSE)
qqline(veri_seti_dolu$anxie, col= "red", lwd = 2)
summary(veri_seti_dolu)
# anxie i??in histogram, boxplot, QQ plot olu??turduk. QQ plota k??rm??z?? bir ??izgi ??izdik.
# verinin ??zetini ald??k.

# KORELASYON

korelasyon_matris <- cor(veri_seti_dolu, method = "pearson")
# veri_seti_dolu verisindeki ya??, sosyal medya ba????ml??l??????, stres ve anksiyete i??in korelasyon matrisi olu??turuldu.

# ??OKLU DO??RUSAL REGRESYON ve MULTICOLLINEARITY

# ??nce modeli olu??tural??m.

model <- lm(veri_seti_dolu$addic ~ veri_seti_dolu$age + veri_seti_dolu$stres + veri_seti_dolu$anxie)
summary(model)
# sosyal medya ba????ml??l??????n anlaml?? ve pozitif a????klay??c??lar?? stres ve anksiyetedir.
# ya?? sosyal medya ba????ml??l??????n??n anlaml?? a????klay??c??s?? de??ildir.
# model i??in a????klanan varyans % 9.9'dur.

# yukar??da, ba????ml?? de??i??kenin de oldu??u birkorelasyon matrisi olu??turmu??tuk.
# ??oklu ba??lant??l??kta korelasyon de??erlerini g??rmek i??in sadece ba????ms??z de??i??kenlerin oldu??u bir veri seti olu??tural??m.
# daha sonra ba????ms??z de??i??kenler i??in korelasyon matrisi olu??tural??m.

bagimsizlar <- data.frame(age, stres, anxie)
bagimsizlar_korelasyon_matris <- cor(bagimsizlar, method = "pearson")

# ??s?? haritas??n?? ??izelim.

install.packages("corrplot")
library(corrplot)
corrplot(bagimsizlar_korelasyon_matris, method = "color")

# BA??IMSIZ DE??????KENLER??N GRAF??KLER??
 
par(mfrow = c (1,3)) # ???? grafi??in yanyana durmas?? i??in ayar yapt??k.
plot(veri_seti_dolu$age, veri_seti_dolu$addic, xlab = "Ya??", ylab = "Sosyal Medya Ba????ml??l??????",
     col = "red", pch = 16)
plot(veri_seti_dolu$stres, veri_seti_dolu$addic, xlab = "Stres", ylab = "Sosyal Medya Ba????ml??l??????",
     col = "purple", pch = 16)
plot(veri_seti_dolu$anxie, veri_seti_dolu$addic, xlab = "Anksiyete", ylab = "Sosyal Medya Ba????ml??l??????",
     col = "orange", pch = 16)
# Sosyal medya ba????ml??l??????n??n a????klay??c??lar??ndan ya?? k??rm??z?? ile, stres mor ile, anksiyete turuncu ile g??rselle??tirildi.

# TEST VE E????T??M GRUPLARINA B??LME

install.packages("caTools") # Alt gruplara b??lme i??in caTools paketini indirdik.
library(caTools)

bolme_islemi <- sample.split(veri_seti_dolu$addic, SplitRatio = 0.8) # %80 e??itim %20 test grubu olarak belirledik.

train <- subset(veri_seti_dolu, split == TRUE)
test <- subset(veri_seti_dolu, split == FALSE)

# e??itim ve test alt gruplar??n?? almak i??in ??ok u??ra??t??m. ald??????m hatalar?? internette ara??t??rd??m ama ????zemedim.
