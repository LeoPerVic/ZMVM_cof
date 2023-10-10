# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

subsector_mun <- read_excel("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZMVM_cof\\Bases\\Subsector i en el municipio j_18.xlsx")

tot_mun <- read_excel("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZMVM_cof\\Bases\\Total de subsectores en el municipio j_18.xlsx")

# Crear vector subsec_zm

subsector_zm <- subsector_mun %>% group_by(cve_sub) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                                  af = sum(af, na.rm = TRUE),  
                                                                  fb = sum(fb, na.rm = TRUE), 
                                                                  pb = sum(pb, na.rm = TRUE), 
                                                                  po = sum(po, na.rm = TRUE), 
                                                                  re = sum(re, na.rm = TRUE), 
                                                                  va = sum(va, na.rm = TRUE))

# Crear vector tot_zm

tot_zm <- tot_mun %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                     af = sum(af, na.rm = TRUE),  
                                     fb = sum(fb, na.rm = TRUE), 
                                     pb = sum(pb, na.rm = TRUE), 
                                     po = sum(po, na.rm = TRUE), 
                                     re = sum(re, na.rm = TRUE), 
                                     va = sum(va, na.rm = TRUE))



# 1. Coeficiente de Localización (QL)

# 1.1 Numerador

numerador <- left_join(subsector_mun, tot_mun, by = c("cve_geo" = "cve_geo")) %>% 
  mutate(ue = ue.x/ue.y, af = af.x/af.y, fb = fb.x/fb.y, pb = pb.x/pb.y, po = po.x/po.y, re = re.x/re.y, va = va.x/va.y) %>% 
  select(cve_ent.x, ent.x, cve_mun.x, mun.x, cve_geo, cve_sub, ae, ue, af, fb, pb, po, re, va) %>% 
  rename(cve_ent=cve_ent.x, cve_mun=cve_mun.x, ent=ent.x, mun=mun.x)

# 1. 2 Denominador

# Dividir las columnas de la Base 1 por los valores únicos de la Base 2

denominador <- sapply(subsector_zm[, -1], function(col) col / unlist(tot_zm))

# Agregar la columna cve_sub a los resultados

denominador <- cbind(subsector_zm[, 1, drop = FALSE], denominador)

# 1.3 Resultado final QL

# Unir subsec_mun_div y subsec_tot_zm_div por CVE_ZM y cve_sub

QL <- left_join(numerador, denominador, by = c("cve_sub"))

# Dividir cada variable de subsec_mun_div entre la variable correspondiente de subsec_tot_zm_div

QL <- QL %>% mutate(QLue = ue.x / ue.y, QLaf = af.x / af.y, QLfb = fb.x/fb.y, QLpb = pb.x/pb.y, QLpo = po.x/po.y, QLre= re.x/re.y, QLva = va.x/va.y) %>% 
  select(cve_geo, cve_sub, QLue, QLaf, QLpb, QLfb, QLpo, QLre, QLva)

View(QL)

# Estimar coeficiente PR

PR <- subsector_mun %>% 
  left_join(subsector_zm, by = c("cve_sub")) %>% 
  mutate(PRue = ue.x / ue.y,
         PRaf = af.x / af.y,
         PRfb = fb.x / fb.y,
         PRpb = pb.x / pb.y,
         PRpo = po.x / po.y,
         PRre = re.x / re.y,
         PRva = va.x / va.y) %>% 
  select(cve_ent, ent, cve_mun, mun, cve_geo, cve_sub, ae,PRue, PRaf, PRfb, PRpb, PRpo, PRre, PRva)

View(PR)


# Estimar coeficiente HH

# Estimar la parte que se resta

# Dividir las columnas de la Base 1 por los valores únicos de la Base 2

tot_mun[, 7:13] <- sapply(tot_mun[, 7:13], as.numeric)

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

BLzm19_final <- left_join(PR, QL, by = c("cve_geo", "cve_sub")) %>%
  left_join(PR, by = c("cvegeo", "cve_sub")) %>%
  left_join(HH, by = c("cvegeo", "cve_sub")) %>%
  left_join(IHH, by = c("cvegeo", "cve_sub"))

View(BLzm19_final)

# Guardar archivo

library(openxlsx)

write.xlsx(tot_zm, "tot_zm.xlsx")



