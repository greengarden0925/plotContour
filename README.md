# plotContour
```{r}
library(plotly)
library(ggplot2)
library(stringr)


plotContour=function(dt,x,y,z,incre=0.3){
  
  # ##debug
  # dt=mtcars
  # x="disp"
  # y="qsec"
  # z="hp"
  # incre=0.3

  mtcars
  
  #---------
  
  # data.loess <- loess(qsec ~ wt * hp, data = mtcars)
  fm=formula(sprintf(" %s ~ %s*%s",z,x,y))
  fm
  data.loess <- loess(fm, data = dt)
  
  
  
  # Create a sequence of incrementally increasing (by 0.3 units) values for both x and y
  xgrid <-  seq(min(dt[[x]]), max(dt[[x]]), incre)
  ygrid <-  seq(min(dt[[y]]), max(dt[[y]]), incre)
  
  
  
  # Generate a dataframe with every possible combination of x and y
  data.fit <-  expand.grid(xgrid, ygrid)
  names(data.fit)=c(x,y)
  head(data.fit)
  
  # Feed the dataframe into the loess model and receive a matrix output with estimates of
  # acceleration for each combination of x and y
  mtrx3d <-  predict(data.loess, newdata = data.fit)
  # Abbreviated display of final matrix
  mtrx3d[1:4, 1:4]
  
  
  
  # Transform data to long form
  mtrx.melt <- reshape2::melt(mtrx3d, id.vars = c(x, y), measure.vars = z)
  names(mtrx.melt) <- c(x,y,z)
  mtrx.melt
  
  # Return data to numeric form
  mtrx.melt[[x]] <- as.numeric(str_sub(mtrx.melt[[x]], str_locate(mtrx.melt[[x]], '=')[1,1] + 1))
  mtrx.melt[[y]] <-  as.numeric(str_sub(mtrx.melt[[y]], str_locate(mtrx.melt[[y]], '=')[1,1] + 1))
  head(mtrx.melt)
  
  
  fig <- plot_ly(mtrx.melt, 
                 x=formula(paste("~",x)),
                 y=formula(paste("~",y)), 
                 z=formula(paste("~",z)),
                 type = "contour",
                 width = 600, height = 500)
  
  fig
  
  return(fig)
}

plotContour(dt=mtcars,x="disp",y="qsec",z="hp",incre=0.3)
plotContour(dt=mtcars,x="hp",y="qsec",z="disp",incre=0.3)

```

![image](https://user-images.githubusercontent.com/18394024/193991991-e16f7bfe-f507-4845-8bc0-98944b1caa1c.png)

