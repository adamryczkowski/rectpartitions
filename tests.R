example1<-rectpartitions:::gen_test(size_x = 200, size_y=200, proc = 0.2, exp_n = 50)
df<-as_tibble(which(example1$m==1, arr.ind = TRUE))
ggplot(data = df, mapping = aes(x=col, y=row)) + geom_raster()
ans<-get_rectangles_shuffle(m$m, method = 'single', flag_allow_stripes = TRUE)
