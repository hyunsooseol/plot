install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
options(jamovi_home='/Applications/jamovi')
options(jamovi_home='/Applications//jamovi-x86_64')
jmvtools::check()



jmvtools::create('vijPlots') # Module Name

jmvtools::addAnalysis(name='histogram', title='Histogram') # name = function/files name, title = menu item name
jmvtools::addAnalysis(name='boxplot', title='Box Plot') # name = function/files name, title = menu item name

jmvtools::addAnalysis(name='piechart', title='Pie Chart') # name = function/files name, title = menu item name

jmvtools::install()

