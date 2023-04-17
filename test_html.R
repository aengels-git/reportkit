library(devtools)
library(prepkit)
library(viskit)
library(reactable)
load_all()
# Wie wird kontrolliert wo die Präsentation erstellt wird? Vermutlich aktueller Arbeitsornder
report <- report_html()
cat(report$header$create())
report$filemanager<- "pander"
report$add_df_info(diamonds)
report$header$toc<-T
report$header$toc_float<-T
# Abbildung ergänzen:
tab_cut <- prep_freq_table(diamonds,cut)
plot_cut <- vis_barplot(tab_cut,x=cut,y=n)
report$add_object(plot_cut,header = "Balkendiagramm")
report$add_break()

#Flextable ergänzen
ft_tab <- report_quick_table(head(diamonds),total_width = 8)
report$add_object(ft_tab,name = "ft_tab3")
report$add_break()

# Reactable ergänzen
rt_tab <- report_reactable(head(diamonds,20)%>%mutate(penis="penissa"),total_width = 8,
                           defaultPageSize = 20)
report$add_object(rt_tab,name = "rt_tab4")
report$add_break()

# Interaktive Abbildung ergänzen:
plotly_cut <- plotly::ggplotly(plot_cut)
report$add_object(plotly_cut,header = "Interaktives Balkendiagramm")
report$add_break()

# Ergänzen von Nav Tabs:
nav_tabs <- report_nav_tabs(titles = c("tab_cut","plot_cut"),
                bodies = list(tab_cut%>%mutate_if(is.numeric,round,digits=2),
                              plot_cut),height = 4,width=10)

report$add_object(nav_tabs,name = "nav_Tabs")
report$header$available_themes
report$header$theme <- "lumen"
report$render()


