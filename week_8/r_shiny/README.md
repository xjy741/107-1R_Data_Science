## 第5678周作業 簡單選股系統與個人化投組

**網頁連結**

[網頁](https://albertshinyfinance1.shinyapps.io/r_shiny/)



### 基本構想

現在機器人理財非常盛行，希望能夠藉由一些簡單的數值方法與資料分析，讓大家依照自己的風險屬性建構自己的投組。並且也可以研究這些投資組合在各式狀態下會發生的結果。


### 資料來源與資料前處理
主要分析目標為台灣50指數成分股。

利用python抓取每日股價資料(Pandas-datareader)，並利用python爬蟲抓取包刮Eps、董監事持股、流動比例等.....特徵。

本次分析為主要分析的目標為最近月，因為這些資料並非都是長度單位一樣，所以對於股價資料會先計算成月報酬，並利用最近十二個月計算平均與共變異數，去當作真實股價報酬與共變異數的估計量。其他資料若為則用最近資料，若為季資料則為近四季，年則為近一年。

### 分析方法
首先我們會先用箱型圖進行基本的資料探索，這時候會發現財金特徵的資料極端值的影響非常明顯，有可能是因為資料錯誤，或是股本大小的不同，因此我們為了表現的美觀，去掉一部分的極端值。

接著我們同時利用pca及k means去觀察這些特徵對於報酬的解釋程度如何，我們會發現當我們使用 k = 3時，基本上可以把較差表現資料與較好表現資料分開來，但有一群只會有一個樣本為大立光，可能是因為他的股價與成長程度較為異常。

同時我們也可以觀察Sharp Ratio及Return在k means的表現如何。
並從下面的data中，可以找尋符合自己需求的標的。

### 投組建構與效率前緣

效率前緣為給定標定務的情形下，所有給定風險下（通常是標準差）的最高報酬的投組即為效率前緣，我們可以將這些點描寫在橫坐標為標準差、縱坐標為報酬的平面上。

在這個部分可以一次進行建構效率前緣與建構最佳化投組部分，主要的方法使用的是蒙地卡羅法，基本概念就是利用大樣本性質去找尋最佳解，但是只要將這些抽出來的投組也畫在圖上，也可以完成效率前緣的概念。在完成效率投組的同時，選取裡面最佳的投組（怎樣選），同時我們也可以使用之前的方法選出要放在此投組的標的。


### 未來展望

此程式未來若可以定時更新股價資料與特徵值等等，基本上投資人可以做一個很基本的個人理財機器人。

其他增加的方向為將最佳化的條件的設定變得更為自由並增加回測的功能，使投資人可以對自己策略更有信心，並更能貼近個人的風險喜好。