
teasel<-list(
T=matrix(
  c(
0,     0,    0,     0,     0,     0,
0.966, 0,    0,     0,     0,     0,
0.013, 0.01, 0.125, 0,     0,     0,
0.007, 0,    0.125, 0.238, 0,     0,
0.008, 0,    0.038, 0.245, 0.167, 0,
0,     0,    0,     0.023, 0.750, 0
    ),
  nrow=6, byrow=TRUE,
 dimnames=list(c("seed1", "seed2", "small", "medium", "large", "flowering"),c("seed1", "seed2", "small", "medium", "large", "flowering"))),
F=matrix(
  c(
0, 0, 0, 0, 0, 322.388,
0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 3.488,
0, 0, 0, 0, 0, 30.170,
0, 0, 0, 0, 0, 0.862,
0, 0, 0, 0, 0, 0
    ),
  nrow=6, byrow=TRUE,
 dimnames=list(c("seed1", "seed2", "small", "medium", "large", "flowering"),c("seed1", "seed2", "small", "medium", "large", "flowering")))
)
