# вариант №6
# http://www.rambler.ru
# Запросы:
# 1) Модели KIA до...
# 2) Модели KIA в ...
# 3) Модели KIA после ...

library('XML')
library('RCurl')


# объявление переменных
year <- vector(mode = 'numeric', length = 0)
header <- vector(mode = 'character', length = 0)
source <- vector(mode = 'character', length = 0)
URL <- vector(mode = 'character', length = 0)
l2 = 0

# три вида запроса в цикле for
for (i in 1:3){
  if (i==1) {fileURL = "https://nova.rambler.ru/search?query=%D0%9C%D0%BE%D0%B4%D0%B5%D0%BB%D0%B8%20KIA%20%D0%B4%D0%BE%20"}
  if (i==2) {fileURL = "https://nova.rambler.ru/search?query=%D0%9C%D0%BE%D0%B4%D0%B5%D0%BB%D0%B8%20KIA%20%D0%B2"}
  if (i==3) {fileURL = "https://nova.rambler.ru/search?query=%D0%9C%D0%BE%D0%B4%D0%B5%D0%BB%D0%B8%20KIA%20%D0%BF%D0%BE%D1%81%D0%BB%D0%B5"}

  # цикл с диапазоном рассматриваемых лет
  for(j in 2013:2022){
    
    Sys.sleep(10)
    
    fileURL <- paste(fileURL,j)
    
    # загрузка текста html-страницы
    html <- getURL(fileURL)
    
    # разборка как html
    doc <- htmlTreeParse(html, useInternalNodes = T)
    
    # корневой элемент
    rootNode <- xmlRoot(doc)
    
    # выбираем все заголовки результатов запроса
    header <- c(header,xpathSApply(rootNode, '//a[@class = "b-serp-item__link"]', xmlValue))
    
    # выбираем все источники результатов запроса
    source <- c(source,xpathSApply(rootNode, '//article/span[2]/span', xmlValue))
    
    # выбираем все полные ссылки на источники
    URL <- c(URL,xpathSApply(rootNode, '//h2[@class = "b-serp-item__header"]/a', xmlGetAttr,'href'))
    
    # блок для расчета длины вектора year
    l <- length(header) - l2
    l2 <- length(header)
    
    # выбираем все года
    year <- c(year,rep(j,l))
    
  }
  
  
}

# преобразуем данные во фрейм
data <- data.frame(Year = year, Header = header, Source = source, URL = URL, stringsAsFactors = F)

# запись данных в файл Timeline.csv
write.csv(data, './Timeline.csv', row.names = F)

print("Результаты запросов записаны в файл Timeline.csv")
