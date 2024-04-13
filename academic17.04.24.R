# Kendi ara??t??rmalar??mdan birinin verisi ??zerinde ??devi ger??ekle??tirece??im. 
#Sadece sentetik veri olu??turmada sentetik veriyi kullanaca????m. 
#Gerisinde "academic" veri setini kullanaca????m. "academic" veri seti agno, dayan??kl??l??k, doyum, yeterlik ve erteleme de??i??kenlerini i??eren 400 sat??rl?? bir veri setidir.

# SENTET??K VER?? OLU??TURMA

install.packages("MASS")
library (MASS) # sentetik veriyi ??retmek i??in MASS paketini indirdim ve k??t??phaneden paketi ??a????rd??m.

set.seed(123) #tekrarlanabilirlik i??in seed belirlendi.
n <- 250 # 250 ki??ilik veri seti istendi.

a1 <- rnorm(n, mean = 20, sd = 4)
a2 <- rnorm(n, mean = 10, sd = 3)
a3 <- rnorm(n, mean = 5, sd = 0.5)
a4 <- rnorm(n, mean = 12, sd = 7)
a5 <- rnorm(n, mean = 8, sd = 1.25)
a6 <- rnorm(n, mean = 9, sd = 4.5) # 6 adet ba????ms??z de??i??ken ??retildi.

sort(a1) # a1 de??i??keni de??erleri k??????kten b??y????e s??raland??.

y <- 7 + 5*a1 + 3*a2 + 2.5*a3 + 3.25*a4 - 1.65*a5 - 2.95*a6 + rnorm(n, mean = 10, sd = 4)
# ba????ms??z de??i??kenler a'lar?? kullanarak, hata terimi ekleyerek, y isimli ba????ms??z bir de??i??ken ??rettik.

veri_seti <- data.frame (y, a1, a2, a3, a4, a5, a6) 
# ??rettimiz ba????ms??z de??i??kenler ve ba????ml?? de??i??ken ile bir veri seti olu??turuldu.

a1_factoral <- as.factor (a1) #a1 de??i??keni fakt??re ??evirildi.
a5_numeric <- as.numeric(a5) #a5 de??i??keni n??merik veriye ??evrildi.
str(a1_factoral, a5_numeric) # Veri yap??lar?? de??i??tirilmi?? ve nesnelere ??evrilmi?? a1 ve a5 verilerinin yap??s?? kontrol edildi.


# Bu noktadan itibaren "academic" veri setinden devam ediyoruz.

#VER?? KAL??TES??

isna_academic <- is.na(academic) #academik veri setindeki de??i??kenlerdeki eksik veriler tespit edildi.
# Yani TRUE veya FALSE sonucunu veren mant??ksal sorgulama yap??ld??.

agno_ortalama <- mean(academic$agno, na.rm = TRUE) # NA'lar?? dikkate almadan agno i??in ortalama bulundu.

agno_doldurulmus <- ifelse(is.na (academic$agno),
                           agno_ortalama, academic$agno) # agno de??i??keni i??ideki NA g??zlemleri ortalma de??er ile dolduruldu.

#Eksik g??zlemi bir ??nceki de??er ile doldurmay?? yapal??m.

install.packages("zoo")
library(zoo)

agno_onceki_ile <- na.locf(academic$agno) # vekt??r olu??turmadan, agno de??i??kenini kullanarak, NA'lar bir ??nceki g??zlem ile dolduruldu.
print(agno_onceki_ile) # agno'daki NA'lar??n bir ??nceki de??erler ile doldurudu??unu g??rd??k.

#eksik g??zlemi medyan ile doldural??m.

agno_medyan<- median(academic$agno, na.rm = T) # agno i??in medyan de??eri 
agno_medyan_ile_doldurma <- ifelse(is.na(academic$agno),
                                   agno_medyan, academic$agno) # agnodaki NA'lar agnonun medyan?? ile dolduruldu.


# AYKIRI DE??ER TESP??T??

boxplot(academic) # academi veri setindeki agno, dayan??kl??l??, doyum, yeterlik ve erteleme de??i??kenleri i??in 5 adet boxplot ??izdirildi.
#agno ve yeterlikteki birer adet u?? de??er g??r??nmektedir.

sort(academic$yeter) #yeterlik de??i??eni i??in verileri k??????kten b??y????e s??ralad??k. boxplotta g??sterilen de??erin "7" oldu??unu g??rd??k.
sort(academic$agno) #agno i??in verileri k??????kten b??y????e s??ralad??k. boxplotta g??sterilen de??erin "1" oldu??unu g??rd??k.

z_scores_academic <- scale(academic) # academic veri setindeki de??i??lkenleri ??l??eklendirdik.

outliers_academic <- abs(z_scores_academic) > 3 # Mutlak de??erleri de kullanarak t??m de??i??kenler i??in +3/-3 aral?????? d??????ndaki z skorlar??n?? TRUE olarak g??rd??k. ????indekileri FALSE olarak g??rd??k.

# ??EYREKL??KLER?? HESAPLAMA

# NA'lar??n oldu??u veri setinde hata ald??????m i??in medyan ile doldurulmu?? agno de??erlerini kulland??m.

Q1_agno <- quantile(agno_medyan_ile_doldurma, probs = (0.25)) # agno i??in 1. ??eyrek de??eri 
Q3_agno <- quantile(agno_medyan_ile_doldurma, probs = (0.75)) # agno i??in 3. ??eyrek de??eri
IQR <- Q3_agno - Q1_agno #iki ??eyrek fark??
alt_sinir <- Q1_agno - 1.5*IQR # agno i??in alt s??n??r
ust_sinir <- Q3_agno + 1.5*IQR # agno i??in ??st s??n??r

outliers_agno <- agno_medyan_ile_doldurma < alt_sinir | agno_medyan_ile_doldurma > ust_sinir
print(outliers_agno) #alt ve ??st s??n??rlara g??re u?? de??erleri bulduk ve print komutu ile bast??rd??k.

# DA??ILIMLAR

hist(academic$agno) #agno i??in histogram
boxplot(academic$agno) # agno i??in boxplot

x <- academic$ertele
y <- academic$agno
plot(x, y, main = "Sa????l??m Grafi??i",
     xlab = "Erteleme", ylab = "Agno",
     pch = 19, frame = FALSE) # Erteleme ve Agno i??in Sa????l??m Grafi??i
abline(lm(y ~ x, data = academic), col = "red") # sa????l??m grafi??ine regresyon do??rusu eklendi.

# KORELASYON

ertele_medyan<- median(academic$ertele, na.rm = T) # ertele de??i??keni i??in medyan de??eri 
ertele_medyan_ile_doldurma <- ifelse(is.na(academic$ertele),
                                   ertele_medyan, academic$ertele) # erteledeki NA'lar ertelenin medyan?? ile dolduruldu.

# ??ok fazla hata ald??m. Agno ve ertele ayn?? uzunlukta vekt??rler de??ildi.
# Sorunun verilerdeki NA'lardan kaynakland??????n?? d??????nd??m.
# ertele de??i??kenindeki NA'lar?? agnodaki gibi medyan ile doldurup bir vekt??r olu??turdum.
# Nesnelere atayarak korelasyonu ger??ekle??tirdim.

x_agno <- agno_medyan_ile_doldurma
y_ertele <- ertele_medyan_ile_doldurma

korelasyon_agno_ertele <- cor(x_agno, y_ertele)
print(korelasyon_agno_ertele)

# ??OKLU DO??RUSAL REGRESYON VE MULTICOLLINEARITY

regresyon_model <- lm(academic$ertele ~ academic$agno + academic$yeter) # modeli olu??turduk.
summary(regresyon_model) # regresyon sonucunu g??rd??k.
# regresyon analizinde agno ve yeterli??in erteleme davran??????n??n negatif ve anlaml?? a????klayc????lar?? oldu??u sonucuna ula????lm????t??r.

#Multicollinearity

dayan_medyan<- median(academic$dayan, na.rm = T) # dayan de??i??keni i??in medyan de??eri 
dayan_medyan_ile_doldurma <- ifelse(is.na(academic$dayan),
                                     dayan_medyan, academic$dayan) # dayandaki NA'lar dayan medyan?? ile dolduruldu.

doyum_medyan<- median(academic$doyum, na.rm = T) # doyum de??i??keni i??in medyan de??eri 
doyum_medyan_ile_doldurma <- ifelse(is.na(academic$doyum),
                                     doyum_medyan, academic$doyum) # doyumdaki NA'lar doyum medyan?? ile dolduruldu.

yeter_medyan<- median(academic$yeter, na.rm = T) # yeter de??i??keni i??in medyan de??eri 
yeter_medyan_ile_doldurma <- ifelse(is.na(academic$yeter),
                                     yeter_medyan, academic$yeter) # yeterdeki NA'lar yeter medyan?? ile dolduruldu.
x_dayan <- dayan_medyan_ile_doldurma
x_doyum <- doyum_medyan_ile_doldurma
x_yeter <- yeter_medyan_ile_doldurma
dolu_veriler_seti <- data.frame(y_ertele, x_agno, x_dayan, x_doyum, x_yeter)
 
# ba????ms??z de??i??kenler aras??ndaki korelasyonlar?? yine e??it uzunlukta olmayan vekt??rler nedeniyle g??remedim.
# bunun ??zerine ba????ms??z de??i??kenlerdeki NA'lar?? da medyanlar?? ile doldurarak nesne haline getirdim.
# son olarak bu nesneler ile bo?? g??zlem olmayan bir veri seti yapt??m.

korelasyon_matrisi <- cor.test(dolu_veriler_seti$x_agno, dolu_veriler_seti$x_yeter, 
                 method = "pearson") # agno ve yeter aras??nda korelasyon analizi
print(korelasyon_matrisi) #korelasyon analizi sonucunu yazd??rd??k. 
# 0.30 korelasyon katsay??s?? ba????ms??z de??i??kenler aras??nda ??oklu ba??lant??l??l??k problemi olmad??????n?? g??stermektedir.

install.packages("corrplot")
library(corrplot)
corrplot(korelasyon_matrisi, method = "color") #??s?? haritas??

#BA??IMSIZ DE??????KENLER??N GRAF??????N?? ????ZME

par(mfrow = c(1,2)) # ??ki grafi??i yanyana yerle??tirmesi i??in 
plot(dolu_veriler_seti$x_agno, dolu_veriler_seti$y_ertele, xlab = "Agno", ylab = "Erteleme",
     col = "green", pch = 16)
plot(dolu_veriler_seti$x_yeter, dolu_veriler_seti$y_ertele, xlab = "Yeterlik", ylab = "Erteleme",
     col = "blue", pch = 16)
# Ertelemenin a????klay??c??lar??ndan agno ye??il, yeterlik mavi ile grafikle??tirildi.

# VER??Y?? TEST VE E????T??M GRUPLARINA B??LME

install.packages("caTools") # Alt gruplara b??lme i??in caTools paketi gerekiyor.
library(caTools)

split <- sample.split(dolu_veriler_seti$y_ertele, SplitRatio = 0.65) # ertele de??i??kenini % 65 e??itim, % 35 test grubuna b??ld??k.
egitim_verisi <- subset(dolu_veriler_seti(y_ertele, x_agno, x_dayan, x_doyum, x_yeter), split == TRUE)
test_verisi <- subset(dolu_veriler_seti(y_ertele, x_agno, x_dayan, x_doyum, x_yeter), split == FALSE) # b??lme i??lemini veri seti/ba????ml?? de??i??ken ??zerinden yapmama ra??men e??itim ve test verisini olu??turmak i??in baya u??ra??t??m ama hatay?? ????zemedim.

