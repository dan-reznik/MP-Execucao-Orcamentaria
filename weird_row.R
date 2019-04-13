# binary search
find_weird_row <- function(n0,n_min,n_max) {
  l_skip <- read_lines(fname,skip=n0,n_max=1)
  l_all <- all_lines[n0+1]
  print(n0)
  if(n0%in%c(n_min,n_max)) {
    print("done")
    n0
  } else {
    if (l_skip!=l_all)
      find_weird_row(as.integer((n0+n_min)/2),n_min,n0)
    else
      find_weird_row(as.integer((n0+n_max)/2),n0,n_max)
  }
}

find_weird_row(15000,1,length(all_lines))
