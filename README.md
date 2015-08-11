






# LDA example by using topicmodels package

Some notes:

* The default behavior for the TermDocumentMatrix function from the tm pacakge is that is only tracks words that are longer than three characters.So use control option to specify the wordLenthsi
* If there is some rows without terms in dtm, we should delete them by using:      
  ```r
  rowTotals <- apply(dtm , 1, sum)
  dtm<- dtm[rowTotals> 0, ]
  ```
* apps structure is like following:

   ```
   str(apps)
   chr [1:17517] "35 44 33 40 33 40 44 38 33 37 37" ... 
   ```
