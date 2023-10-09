# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

subsector_mun <- read_excel("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZMVM_cof\\Bases\\Subsector i en el municipio j_18.xlsx")

tot_mun <- read_excel("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZMVM_cof\\Bases\\Total de subsectores en el municipio j_18.xlsx")

# Crear vector subsec_mun

subsector_zm <- subsector_mun %>% group_by(cve_sub) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                                        af = sum(af, na.rm = TRUE),  
                                                                        fb = sum(fb, na.rm = TRUE), 
                                                                        pb = sum(pb, na.rm = TRUE), 
                                                                        po = sum(po, na.rm = TRUE), 
                                                                        re = sum(re, na.rm = TRUE), 
                                                                        va = sum(va, na.rm = TRUE))

# Crear vector tot_mun

tot_zm <- tot_mun %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                            af = sum(af, na.rm = TRUE),  
                                                            fb = sum(fb, na.rm = TRUE), 
                                                            pb = sum(pb, na.rm = TRUE), 
                                                            po = sum(po, na.rm = TRUE), 
                                                            re = sum(re, na.rm = TRUE), 
                                                            va = sum(va, na.rm = TRUE))

# Crear vector subsec_zm

subsec_zm <- datos %>% group_by(cve_sub) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                               af = sum(af, na.rm = TRUE),  
                                                               fb = sum(fb, na.rm = TRUE), 
                                                               pb = sum(pb, na.rm = TRUE), 
                                                               po = sum(po, na.rm = TRUE), 
                                                               re = sum(re, na.rm = TRUE), 
                                                               va = sum(va, na.rm = TRUE))

# Crear vector tot_zm

tot_zm <- datos  %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                   af = sum(af, na.rm = TRUE),  
                                                   fb = sum(fb, na.rm = TRUE), 
                                                   pb = sum(pb, na.rm = TRUE), 
                                                   po = sum(po, na.rm = TRUE), 
                                                   re = sum(re, na.rm = TRUE), 
                                                   va = sum(va, na.rm = TRUE))
  

# 1. Coeficiente de Localización (QL)

# 1.1 Numerador

numerador <- left_join(datos, tot_mun, by = c("cvegeo" = "cvegeo")) %>% 
  mutate(ue = ue.x/ue.y, af = af.x/af.y, fb = fb.x/fb.y, pb = pb.x/pb.y, po = po.x/po.y, re = re.x/re.y, va = va.x/va.y) %>% 
  select(-ue.x, -ue.y, -af.x, -af.y, -fb.x, -fb.y, -pb.x, -pb.y, -po.x, -po.y, -re.x, -re.y, -va.x, -va.y)

# 1. 2 Denominador

# Dividir las columnas de la Base 1 por los valores únicos de la Base 2

denominador <- sapply(subsec_zm[, -1], function(col) col / unlist(tot_zm))

# Agregar la columna cve_sub a los resultados

denominador <- cbind(subsec_zm[, 1, drop = FALSE], denominador)

# 1.3 Resultado final QL

# Unir subsec_mun_div y subsec_tot_zm_div por CVE_ZM y cve_sub

QL <- left_join(numerador, denominador, by = c("cve_sub"))

# Dividir cada variable de subsec_mun_div entre la variable correspondiente de subsec_tot_zm_div

QL <- QL %>% mutate(QLue = ue.x / ue.y, QLaf = af.x / af.y, QLfb = fb.x/fb.y, QLpb = pb.x/pb.y, QLpo = po.x/po.y, QLre= re.x/re.y, QLva = va.x/va.y) %>% 
  select(-ue.x, -ue.y, -af.x, -af.y, -fb.x, -fb.y, -pb.x, -pb.y, -po.x, -po.y, -re.x, -re.y, -va.x, -va.y)

View(QL)

# Estimar coeficiente PR

PR <- datos %>% 
  left_join(subsec_zm, by = c("cve_sub")) %>% 
  mutate(PRue = ue.x / ue.y,
         PRaf = af.x / af.y,
         PRfb = fb.x / fb.y,
         PRpb = pb.x / pb.y,
         PRpo = po.x / po.y,
         PRre = re.x / re.y,
         PRva = va.x / va.y) %>% 
  select(cvegeo, cve_sub,PRue, PRaf, PRfb, PRpb, PRpo, PRre, PRva)

View(PR)

# Estimar coeficiente HH

# Estimar la parte que se resta

# Dividir las columnas de la Base 1 por los valores únicos de la Base 2

resta <- sapply(tot_mun[, -1], function(col) col / unlist(tot_zm))

# Agregar la columna cve_sub a los resultados

resta <- cbind(tot_mun[, 1, drop = FALSE], resta)


# Estimar HH

HH <- PR %>% 
  
  left_join(resta, by = c("cvegeo")) %>% 
  mutate(HHue = PRue - ue,
         HHaf = PRaf - af,
         HHfb = PRfb - fb,
         HHpb = PRpb - pb,
         HHpo = PRpo - po,
         HHre = PRre - re,
         HHva = PRva - va) %>% 
  select(cvegeo, cve_sub, HHue,HHaf, HHfb, HHpb, HHpo, HHre, HHva)

View(HH)

# Estimar IHH

IHH <- HH %>%
  mutate_at(vars(HHue, HHaf, HHfb, HHpb, HHpo, HHre, HHva), ~ 1 - .) %>%
  rename_with(~ paste0("IHH", gsub("HH", "", .)), starts_with("HH"))

View(IHH)

# Unir datos 

BLzm19_final <- left_join(datos, QL, by = c("cvegeo", "cve_sub")) %>%
  left_join(PR, by = c("cvegeo", "cve_sub")) %>%
  left_join(HH, by = c("cvegeo", "cve_sub")) %>%
  left_join(IHH, by = c("cvegeo", "cve_sub"))

View(BLzm19_final)

# Guardar archivo

library(openxlsx)

write.xlsx(BLzm19_final, "BLzm99_final.xlsx")



