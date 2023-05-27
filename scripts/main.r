accountData <- read.csv('../data/mainData.csv', stringsAsFactors = TRUE, colClasses = c("id" = "character", "entity" = "character", "addressType" = "character"))
# Prehlad dat
  # prvych par riadkov bez prveho stlpca, prvy stlpec ukayuje priebeh balancu v kazdom bloku. z toho sa pocital priemer a ostal v datovej sade kdyby 'neco',, tj. mohol mat nejake vyuzitie.
  accountData[1:5, -c(1) ]
  # nejake statistiky
    # pocet roznych uctov
    length(accountData[,1])
    # kolko je z danych penazeniek istotne s oznacecim (miner,exchange,defi), tj mame olablovanych 39 entit a ostatne su nezname ale podla znamych statistik penazenky sukr. osob
    length(accountData[accountData[,4] ==  'Mining', 1])
    length(accountData[accountData[,4] ==  'DeFi', 1])
    length(accountData[accountData[,4] ==  'Exchange', 1])
    length(accountData[accountData[,14] ==  'Wallet', 1])
    length(accountData[accountData[,14] ==  'Smart Contract', 1])
    # priemery a mediany pre
    # minBalance maxBalance countAsSender totalSent totalReceived coununtAsReceiver  avgSent avgReceived avgBalance
    # pre countAsSender, countAsReceiver nie je jednoznacna jednotka, mozno som pri parsingu prehliadol a urobil nejaku malu chybicku
    colMeans(accountData[, c(2, 3, 5, 6, 7, 8, 9, 10, 13)])
    apply(accountData[, c(2, 3, 5, 6, 7, 8, 9, 10, 13)], 2, median)
    # teraz si pozrieme plot velkosti balancu vsetkych penazeniek v log skale 
    # PCA
    # vyberieme len data, s ktorymi v pca budeme narabat
    pcaData <- accountData[,c(2,3,5,6,7,8,9,10,13)]
    colors <- rep('black', 10049)
    colors[accountData[,4] == 'Exchange'] <- 'blue'
    colors[accountData[,4] == 'Mining'] <- 'red'
    colors[accountData[,4] == 'DeFi'] <- 'green'
    # 2D pca s outlierom
     pcaResultFull <- prcomp(pcaData, scale = TRUE)  
    # summary(pcaResult)
    # lepsie pca zobrazenie 
    library(ggfortify)
    # autoplot( pcaResult, 
    #          data=pcaResult, 
    #          col = colors, 
    #          loadings=TRUE,
    #          loadings.label = TRUE)
   
    # to je fajn ale ten jeden ma obrovsky balance a kvoli tomu ostatnych balance sa tazsie vie navzajom provnavat, tak ho odstranime
    maxIdx <- which.max(pcaData[,9])
    pcaDataOriginal <- pcaData
    pcaData <- pcaData[-maxIdx,]
    pcaResult <- prcomp(pcaData, scale = TRUE)  
    colors <- colors[-maxIdx]
    autoplot( pcaResult, 
              data=pcaResult, 
              col = colors, 
              loadings=TRUE,
              loadings.label = TRUE)
    # 3D PCA
    library(rgl)
    scale <- 100
    options(rgl.printRglwidget = TRUE)
    plot3d(pcaResult$x[,1:3], col=colors)
    coords <- NULL
    for (i in 1:nrow(pcaResult$rotation)) {
      coords <- rbind(coords, rbind(c(0,0,0),pcaResult$rotation[i,1:3]))
    }
    
    text3d(pcaResult$rotation[,1:3] * scale , 
           texts=rownames(pcaResult$rotation), 
           col="red", 
           cex=0.8)
    coords <- coords * scale
    lines3d(coords, col="red", lwd=2)
    
    
    # K-Means
    # vyber, podla nas najlepsieho K
    best <- rep(0, 30)
    for (k in 1:30) best[k] <- kmeans(scale(pcaData), centers = k, nstart = 100)$tot.withinss
    plot(1:30, xlab = 'k', ylab = 'the total within-cluster sum of squares', best, type = "b")
    
    kMeansResult <- kmeans(scale(pcaData), 4)
    # plot vysledkov kMenas v 2D PCA
    autoplot( pcaResult, 
              data = pcaResult, 
              col = kMeansResult$cluster, 
              loadings=TRUE,
              loadings.label = TRUE)
    
    # plot vysledkov kMeans v 3D PCA
    scale <- 100
    options(rgl.printRglwidget = TRUE)
    plot3d(pcaResult$x[,1:3], col=kMeansResult$cluster)
    coords <- NULL
    for (i in 1:nrow(pcaResult$rotation)) {
      coords <- rbind(coords, rbind(c(0,0,0),pcaResult$rotation[i,1:3]))
    }
    
    text3d(pcaResult$rotation[,1:3] * scale , 
           texts=rownames(pcaResult$rotation), 
           col="red", 
           cex=0.8)
    coords <- coords * scale
    lines3d(coords, col="red", lwd=1)
    library(cluster)
    # tu pokracujes adame
    library(rpart)
    library(rpart.plot)
    set.seed(123)
    treeData <- sampledData[,c(2,3,4,5,6,7,8,9,10,13)]
    sampledData <- accountData[sample(nrow(accountData)),]
    treeData[treeData[,3] == '', 3] <- 'Neznama adresa'
    treeData[treeData[,3] != 'Neznama adresa', 3] <- 'Burza|DeFi|Minner'
    # 25 neprazdnych entit, naktorych sa model moze ucit
    treeDataTrain <- treeData[(1:floor(.7*nrow(treeData))),]
    # 14 entit, na ktorych model ukaze aky je
    treeDataTest <- treeData[((floor(.7*nrow(treeData)) + 1) : nrow(treeData)),]
    entityTree <- rpart(entity~minBalance+maxBalance+countAsSender+totalSent+totalReceived+coununtAsReceiver+avgSent+avgReceived+avgBalance, 
                        data = treeDataTrain,
                       method = "class", parms = list(split = "information"),
                       control = rpart.control(xval = 100))
    summary(entityTree)
    rpart.plot(entityTree, extra = 104); 
    predictResult <- predict(entityTree, treeDataTest)
    # zostrojime si confusion matrix a zobrazime pre nu zakladne statistiky
    cM <- matrix(0, ncol = 2, nrow = 2)
    predRes <- apply(predictResult, 1, which.max)
    cM[2, 2] <- sum((treeDataTest[,3] == 'Neznama adresa') & (predRes == 2))
    cM[2, 1] <- sum((treeDataTest[,3] == 'Neznama adresa') & (predRes == 1))
    cM[1, 2] <- sum((treeDataTest[,3] == 'Burza|DeFi|Minner') & (predRes == 2))
    cM[1, 1] <- sum((treeDataTest[,3] == 'Burza|DeFi|Minner') & (predRes == 1))
    confusion.info <- function(nnC) {
      # Nenormalizovana aj normalizovana confusion matrix a odhady roznych charakteristik
      # nnC ... nenormalizovana kontingencna tabulka klasifikacie (confusion matrix)
      print("Non-normalized confusion matrix"); print(nnC)
      C <- nnC/sum(nnC); print("Normalized confusion matrix"); print(round(C, 4))
      print(c("Sensitivity", round(C[1, 1]/(C[1, 1] + C[1, 2]), 4)))
      print(c("Specificity", round(C[2, 2]/(C[2, 1] + C[2, 2]), 4)))
      print(c("Accuracy", round(C[2, 2] + C[1, 1], 4)))
      PY <- (C[1, 1]*C[2, 2] - C[1, 2]*C[2, 1]) /
        sqrt(sum(C[1, ])*sum(C[2, ])*sum(C[, 1])*sum(C[, 2]))
      print(c("Pearson-Yule", round(PY, 4)))
    }
    confusion.info(cM)    
    # a co random forest
    library(randomForest)
    entityForest <- randomForest(factor(entity)~minBalance+maxBalance+countAsSender+totalSent+totalReceived+coununtAsReceiver+avgSent+avgReceived+avgBalance, 
                                data = treeDataTrain,
                                ntrees = 2000)
    
    forestPredResult <- predict(entityForest, treeDataTest)
    
    cM <- matrix(0, ncol = 2, nrow = 2)
    cM[2, 2] <- sum((treeDataTest[,3] == 'Neznama adresa') & (forestPredResult == 'Neznama adresa'))
    cM[2, 1] <- sum((treeDataTest[,3] == 'Neznama adresa') & (forestPredResult == 'Burza|DeFi|Minner'))
    cM[1, 2] <- sum((treeDataTest[,3] == 'Burza|DeFi|Minner') & (forestPredResult == 'Neznama adresa'))
    cM[1, 1] <- sum((treeDataTest[,3] == 'Burza|DeFi|Minner') & (forestPredResult == 'Burza|DeFi|Minner'))
    confusion.info(cM)    
    
    # DBScan
    library(dbscan)
    # graf "k-distance"
    epsVec <- seq(0.1, 1, by = 0.01)
    nEps <- length(epsVec)
    clustersNum <- rep(0, nEps)
    # ktory nam da 4 clustre?
    for (i in 1:nEps) {
      clustersNum[i] <- length(table(dbscan(scale(pcaData), eps = epsVec[i])$cluster))
    }
    plot(epsVec, clustersNum, type = "b", pch = 19); grid()

    dbScanResult <- dbscan(scale(pcaData), eps = 0.14 , minPts = 5)
    
    scale <- 100
    options(rgl.printRglwidget = TRUE)
    plot3d(pcaResult$x[,1:3], col=dbScanResult$cluster + 1)
    coords <- NULL
    for (i in 1:nrow(pcaResult$rotation)) {
      coords <- rbind(coords, rbind(c(0,0,0),pcaResult$rotation[i,1:3]))
    }
    
    text3d(pcaResult$rotation[,1:3] * scale , 
           texts=rownames(pcaResult$rotation), 
           col="green", 
           cex=0.8)
    coords <- coords * scale
    
    lines3d(coords, col="green", lwd=0.5)
    text3d(pcaResult$rotation[,1:3]  * (scale/20) , 
           texts=rownames(pcaResult$rotation), 
           col="blue", 
           cex=0.8)
    coords <- coords / 20
    lines3d(coords, col="blue", lwd=1)
    