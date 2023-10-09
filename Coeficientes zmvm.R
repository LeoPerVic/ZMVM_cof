# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

datos <- read_excel("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZMVM_cof\\ZMVM_2019.xlsx")

# Crear vector subsec_mun

subsec_mun <- datos %>% group_by(cvegeo, cve_sub) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                                        af = sum(af, na.rm = TRUE),  
                                                                        fb = sum(fb, na.rm = TRUE), 
                                                                        pb = sum(pb, na.rm = TRUE), 
                                                                        po = sum(po, na.rm = TRUE), 
                                                                        re = sum(re, na.rm = TRUE), 
                                                                        va = sum(va, na.rm = TRUE))

# Crear vector tot_mun

tot_mun <- datos %>% group_by(cvegeo) %>% summarize(ue = sum(ue, na.rm = TRUE), 
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

QL <- left_join(subsec_mun_div, subsec_tot_zm_div, by = c("CVE_ZM", "cve_sub"))

# Dividir cada variable de subsec_mun_div entre la variable correspondiente de subsec_tot_zm_div

QL <- QL %>% mutate(QLue = ue / ue.div, QLaf = af / af.div, QLfb = fb/fb.div, QLpb = pb/pb.div, QLpo = po/po.div, QLre= re/re.div, QLva = va/va.div) %>% 
  select(-ue, -ue.div, -af, -af.div, -fb, -fb.div, -fb, -fb.div, -pb, -pb.div, -po, -po.div, -re, -re.div, -va, -va.div)

View(QL)

# Estimar coeficiente PR

PR <- subsec_mun %>% 
  left_join(subsec_zm, by = c("cve_sub", "CVE_ZM")) %>% 
  mutate(PRue = ue.x / ue.y,
         PRaf = af.x / af.y,
         PRfb = fb.x / fb.y,
         PRpb = pb.x / pb.y,
         PRpo = po.x / po.y,
         PRre = re.x / re.y,
         PRva = va.x / va.y) %>% 
  select(cvegeo, cve_sub, CVE_ZM, PRue, PRaf, PRfb, PRpb, PRpo, PRre, PRva)

View(PR)

# Estimar coeficiente HH

# Estimar la parte que se resta

resta <- tot_mun %>% 
  left_join(tot_zm, by = c("CVE_ZM")) %>% 
  mutate(Rue = ue.x / ue.y,
         Raf = af.x / af.y,
         Rfb = fb.x / fb.y,
         Rpb = pb.x / pb.y,
         Rpo = po.x / po.y,
         Rre = re.x / re.y,
         Rva = va.x / va.y) %>% 
  select(cvegeo, CVE_ZM, Rue,Raf, Rfb, Rpb, Rpo, Rre, Rva)

View(resta)
View(tot_mun)
View(tot_zm)
# Estimar HH

HH <- PR %>% 
  
  left_join(resta, by = c("cvegeo", "CVE_ZM")) %>% 
  mutate(HHue = PRue - Rue,
         HHaf = PRaf - Raf,
         HHfb = PRfb - Rfb,
         HHpb = PRpb - Rpb,
         HHpo = PRpo - Rpo,
         HHre = PRre - Rre,
         HHva = PRva - Rva) %>% 
  select(cvegeo, cve_sub, CVE_ZM, HHue,HHaf, HHfb, HHpb, HHpo, HHre, HHva)

View(HH)

# Estimar IHH

IHH <- HH %>%
  mutate_at(vars(HHue, HHaf, HHfb, HHpb, HHpo, HHre, HHva), ~ 1 - .) %>%
  rename_with(~ paste0("IHH", gsub("HH", "", .)), starts_with("HH"))

View(IHH)

# Unir datos 

BLzm99_final <- left_join(datos, QL, by = c("cvegeo", "cve_sub", "CVE_ZM")) %>%
  left_join(PR, by = c("cvegeo", "cve_sub", "CVE_ZM")) %>%
  left_join(HH, by = c("cvegeo", "cve_sub", "CVE_ZM")) %>%
  left_join(IHH, by = c("cvegeo", "cve_sub", "CVE_ZM"))

View(BLzm99_final)

# Guardar archivo

library(openxlsx)

write.xlsx(BLzm99_final, "BLzm99_final.xlsx")



