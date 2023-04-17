
library(devtools)
library(prepkit)
library(viskit)


load_all()
# Wie wird kontrolliert wo die Präsentation erstellt wird? Vermutlich aktueller Arbeitsornder
presi<-report_pptx(path = "test.pptx")

# Abbildung ergänzen:
tab_cut <- prep_freq_table(diamonds,cut)
plot_cut <- vis_barplot(tab_cut,x=cut,y=n)
presi$add_ggplot(plot_cut,header = "Balkendiagramm")

# Abbildung auf der rechten Seite einer Folie erstellen:
presi$add_ggplot(plot_cut,header = "Balkendiagramm",align = "right")

#Tabelle ergänzen
ft_tab <- reportkit::quick_table(head(diamonds))
presi$add_ggplot(ft_tab,current_slide = T,align = "left")

# Folie interaktiv ergänzen:
presi$ppt_length
presi$open()
# Folie ergänzen
presi$reload()
# Prüfen ob sich die Länge verändert hat
presi$ppt_length
