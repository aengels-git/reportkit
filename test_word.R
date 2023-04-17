
library(devtools)
library(prepkit)
library(viskit)

load_all()
# Wie wird kontrolliert wo die Präsentation erstellt wird? Vermutlich aktueller Arbeitsornder
word<-report_word(path = "test.docx")

# Abbildung ergänzen:
tab_cut <- prep_freq_table(diamonds,cut)
plot_cut <- vis_barplot(tab_cut,x=cut,y=n)
word$add_ggplot(plot_cut)

#Tabelle ergänzen
ft_tab <- reportkit::quick_table(head(diamonds))
word$add_flextable(ft_tab)
word$save("penis.docx")
browseURL(".")
