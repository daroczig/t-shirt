---
layout: default
title: useR! 2014 t-shirt designs
---

This page lists all the pull requests submitted to [user2014/t-shirt](https://github.com/user2014/t-shirt) in chronological sequence.

# [#1](https://github.com/user2014/t-shirt/pull/1) by @jimhester

- recycled lengths rather than repeated calls to rep
- formatted lens specification by row
- use strsplit of character string rather than explicit declaration of split characters
- specify byrow in matrix construction rather than transposing post creation
- use spaces for column names rather than dots
- print without row names

## Code

{% highlight r %}
lens = c(19,5,
        20,4,
        22,2,
        6,10,7,1,
        6,11,7,0,
        6,12,6,0,
        6,12,6,0,
        6,12,6,0,
        6,12,6,0,
        6,10,7,1,
        23,1,
        22,2,
        21,3,
        19,5,
        6,7,5,6,
        6,8,4,6,
        6,8,5,5,
        6,9,4,5,
        6,9,5,4,
        6,10,4,4,
        6,10,5,3,
        6,11,5,2,
        6,11,6,1,
        6,12,6,0)
R<-rep(rep(c(TRUE,FALSE), length.out=length(lens)), times=lens)
R2<-rep(strsplit('useR12014', '')[[1]],64)
R <- ifelse(R, R2, "")

R[265:286]<-strsplit('user!2014--Los Angeles', '')[[1]]

R<-data.frame(matrix(R,ncol=24, byrow=T))
names(R) = rep(' ', ncol(R))
print(R, row.names=F)
# write.table(R, file="tshirtImage.txt", quote=FALSE)
{% endhighlight %}

## Generated image

![](hanel/front.png)

## T-shirt with highlighted code

![](hanel/t-shirt.png)

#