# Written by Artur Araujo

# artur.stat@gmail.com
# aamarinhodearaujo1@sheffield.ac.uk

# October 2020

# define working directory
while ( !"meta_plots_parallel.R" %in% list.files() ) {
  file <- file.choose();# choose this file
  WorkingDir  <- dirname(file);# get path to file
  setwd(dir=WorkingDir); # define working directory
  rm(file, WorkingDir); # remove objects
}

if ( !"varRatio" %in% ls() ) { # if function does not exist
  source(file="./functions/varRatio.R"); # source file containing function
}

if ( # check if objects exist
  !exists( x="p.ratio.data", where=environment() ) ||
  !exists( x="p.var.data", where=environment() )
) { # if any of the objects does not exist
  load( file="./rdata/metaDataParallel.RData", envir=environment() );
} # load objects from data file

dfree <- as.numeric(colnames(p.ratio.data)[-1]);

#####################################################
##### Plot ratio vs degrees of freedom in color #####
#####################################################

tiff(
  filename="./plots/parallel_Ratio_vs_df_color.tif",
  width=1920,
  height=1080,
  units="px",
  pointsize=12,
  compression="lzw",
  bg="white",
  type="cairo"
);

#jpeg(
#  filename="./plots/parallel_Ratio_vs_df_color.jpg",
#  width=1920,
#  height=1080,
#  units="px",
#  pointsize=12,
#  quality=100,
#  bg="white",
#  type="cairo"
#);

#png(
#  filename="./plots/parallel_Ratio_vs_df_color.png",
#  width=1920,
#  height=1080,
#  units="px",
#  pointsize=12,
#  bg="white",
#  type="cairo"
#);

mai <- par("mai");

# define margin dimensions
par( "mai"=mai+c(0.54, 0.63, 0, 0) );

#trial.col <- hcl.colors( n=nrow(p.ratio.data) );
#trial.col <- rainbow( n=nrow(p.ratio.data) );
#trial.col <- heat.colors( n=nrow(p.ratio.data) );
trial.col <- terrain.colors( n=nrow(p.ratio.data) );
#trial.col <- topo.colors( n=nrow(p.ratio.data) );
#trial.col <- cm.colors( n=nrow(p.ratio.data) );
trial.pch <- 0:(nrow(p.ratio.data)-1);

plot(
  x=dfree,
  y=p.ratio.data[1,-1],
  ylim=c(1, 3),
  type="p",
  pch=trial.pch[1],
  col=trial.col[1],
  cex=2.5,
  xlab="",
  ylab="",
  bty="n",
  axes=FALSE
);

for ( i in 2:nrow(p.ratio.data) ) {
  points(
    x=dfree,
    y=p.ratio.data[i,-1],
    type="p",
    pch=trial.pch[i],
    col=trial.col[i],
    cex=2.5
  );
}
rm(i);

x.nu <- seq(
  from=dfree[1],
  to=dfree[length(dfree)],
  by=0.5
);

# inf trials
lines(
  x=x.nu,
  y=x.nu/(x.nu-4),
  type="l",
  lty=1,
  lwd=2.5,
  col="red"
);

rm(x.nu);

# define x-axis
axis(
  side=1,
  at=dfree,
  labels=TRUE,
  tick=TRUE,
  outer=FALSE,
  lty="solid",
  lwd=2.5,
  lwd.ticks=2.5,
  cex.axis=2.5,
  padj=1
);

# define y-axis
axis(
  side=2,
  at=seq(from=1, to=3, by=0.5),
  labels=TRUE,
  tick=TRUE,
  outer=FALSE,
  lty="solid",
  lwd=2.5,
  lwd.ticks=2.5,
  cex.axis=2.5,
  padj=-1
);

# plot x-axis label
mtext(
  text=expression(
    paste(
      "Degrees of freedom (",
      nu,
      ")",
      sep=""
    )
  ),
  side=1, # bottom
  line=-2.5,
  outer=TRUE, # use outer margin
  cex=2.5
);

# plot y-axis label
mtext(
  text="Ratio of true variance to reported variance",
  side=2, # left
  line=-2,
  outer=TRUE, # use outer margin
  cex=2.5
);

legend(
  x="topright",
  legend=c(
    paste(rev(p.ratio.data$trials), "trials", sep=" "),
    expression(
      paste(
        "Inflation factor: ",
        nu/(nu-4),
        sep=""
      )
    )
  ),
  col=c(rev(trial.col), "red"),
  lty=c(rep( NA, times=nrow(p.ratio.data) ), 1),
  lwd=2.5,
  pch=c(rev(trial.pch), NA),
  bty="n",
  cex=2.5
);

dev.off();

rm(trial.col, trial.pch);

# restore original settings
par("mai"=mai);
rm(mai);

#############################################################
##### Plot ratio vs degrees of freedom in black & white #####
#############################################################

tiff(
  filename="./plots/parallel_Ratio_vs_df_bw.tif",
  width=1920,
  height=1080,
  units="px",
  pointsize=12,
  compression="lzw",
  bg="white",
  type="cairo"
);

#jpeg(
#  filename="./plots/parallel_Ratio_vs_df_bw.jpg",
#  width=1920,
#  height=1080,
#  units="px",
#  pointsize=12,
#  quality=100,
#  bg="white",
#  type="cairo"
#);

#png(
#  filename="./plots/parallel_Ratio_vs_df_bw.png",
#  width=1920,
#  height=1080,
#  units="px",
#  pointsize=12,
#  bg="white",
#  type="cairo"
#);

mai <- par("mai");

# define margin dimensions
par( "mai"=mai+c(0.54, 0.63, 0, 0) );

trial.pch <- 0:(nrow(p.ratio.data)-1);

plot(
  x=dfree,
  y=p.ratio.data[1,-1],
  ylim=c(1, 3),
  type="p",
  pch=trial.pch[1],
  col="black",
  cex=2.5,
  xlab="",
  ylab="",
  bty="n",
  axes=FALSE
);

for ( i in 2:nrow(p.ratio.data) ) {
  points(
    x=dfree,
    y=p.ratio.data[i,-1],
    type="p",
    pch=trial.pch[i],
    col="black",
    cex=2.5
  );
}
rm(i);

x.nu <- seq(
  from=dfree[1],
  to=dfree[length(dfree)],
  by=0.5
);

# inf trials
lines(
  x=x.nu,
  y=x.nu/(x.nu-4),
  type="l",
  lty=1,
  lwd=2.5,
  col="gray"
);

rm(x.nu);

# define x-axis
axis(
  side=1,
  at=dfree,
  labels=TRUE,
  tick=TRUE,
  outer=FALSE,
  lty="solid",
  lwd=2.5,
  lwd.ticks=2.5,
  cex.axis=2.5,
  padj=1
);

# define y-axis
axis(
  side=2,
  at=seq(from=1, to=3, by=0.5),
  labels=TRUE,
  tick=TRUE,
  outer=FALSE,
  lty="solid",
  lwd=2.5,
  lwd.ticks=2.5,
  cex.axis=2.5,
  padj=-1
);

# plot x-axis label
mtext(
  text=expression(
    paste(
      "Degrees of freedom (",
      nu,
      ")",
      sep=""
    )
  ),
  side=1, # bottom
  line=-2.5,
  outer=TRUE, # use outer margin
  cex=2.5
);

# plot y-axis label
mtext(
  text="Ratio of true variance to reported variance",
  side=2, # left
  line=-2,
  outer=TRUE, # use outer margin
  cex=2.5
);

legend(
  x="topright",
  legend=c(
    paste(rev(p.ratio.data$trials), "trials", sep=" "),
    expression(
      paste(
        "Inflation factor: ",
        nu/(nu-4),
        sep=""
      )
    )
  ),
  col=c( rep( "black", times=nrow(p.ratio.data) ), "gray"),
  lty=c(rep( NA, times=nrow(p.ratio.data) ), 1),
  lwd=2.5,
  pch=c(rev(trial.pch), NA),
  bty="n",
  cex=2.5
);

dev.off();

rm(trial.pch);

# restore original settings
par("mai"=mai);
rm(mai);

###############################################
##### Plot ratio vs trial number in color #####
###############################################

tiff(
  filename="./plots/parallel_Ratio_vs_TrialNumber.tif",
  width=1920,
  height=1080,
  units="px",
  pointsize=12,
  compression="lzw",
  bg="white",
  type="cairo"
);

#jpeg(
#  filename="./plots/parallel_Ratio_vs_TrialNumber.jpg",
#  width=1920,
#  height=1080,
#  units="px",
#  pointsize=12,
#  quality=100,
#  bg="white",
#  type="cairo"
#);

#png(
#  filename="./plots/parallel_Ratio_vs_TrialNumber.png",
#  width=1920,
#  height=1080,
#  units="px",
#  pointsize=12,
#  bg="white",
#  type="cairo"
#);

mai <- par("mai");

layout.matrix <- matrix(
  data=c(1, 2, 3, 4),
  nrow=2,
  ncol=2,
  byrow=TRUE
);

layout(mat=layout.matrix);

rm(layout.matrix);

layout.show(n=4);

rcol <- c(2, 3, 5, 7);
yaxs <- matrix(data=NA, nrow=4, ncol=3);
yaxs[1,] <- c(1, 3, 0.4);
yaxs[2,] <- c(1, 1.8, 0.2);
yaxs[3,] <- c(1, 1.4, 0.1);
yaxs[4,] <- c(1, 1.2, 0.05);
pmai <- matrix(data=NA, nrow=4, ncol=4);
pmai[1,] <- c(0, 0.45, 0, 0);
pmai[2,] <- c(0, 0, 0, 0);
pmai[3,] <- c(0.18, 0.45, 0, 0);
pmai[4,] <- c(0.18, 0, 0, 0);

for (i in 1:4) {
  # define margin dimensions
  par("mai"=mai+pmai[i,]);

  # plot ratio
  plot(
    x=p.ratio.data$trials,
    y=p.ratio.data[,rcol[i]],
    ylim=c(yaxs[i,1], yaxs[i,2]),
    type="p",
    pch=20,
    cex=2,
    xlab="",
    ylab="",
    main=mtext(
      text=parse(
        text=paste0("nu==", dfree[rcol[i]-1])
      ),
      cex=2.5
    ),
    bty="n",
    axes=FALSE
  );

  # draw whiskers
  arrows(
    x0=p.ratio.data$trials,
    y0=p.ratio.data[,rcol[i]]-1.96*sqrt(p.var.data[,rcol[i]]),
    x1=p.ratio.data$trials,
    y1=p.ratio.data[,rcol[i]]+1.96*sqrt(p.var.data[,rcol[i]]),
    length=0.05,
    angle=90,
    code=3,
    lwd=2
  );

  abline(
    h=dfree[rcol[i]-1]/(dfree[rcol[i]-1]-2),
    lty="dashed",
    lwd=2
  );

  abline(
    h=dfree[rcol[i]-1]/(dfree[rcol[i]-1]-4),
    lty="dotted",
    lwd=2
  );

  #points(
  #  x=p.ratio.data$trials,
  #  y=varRatio(
  #    df=dfree[rcol[i]-1],
  #    k=p.ratio.data$trials,
  #    f1=FALSE
  #  ),
  #  type="p",
  #  pch=4,
  #  col="red",
  #  cex=2
  #);

  lines(
    x=seq(from=2, to=64, by=1),
    y=varRatio(
      df=dfree[rcol[i]-1],
      k=seq(from=2, to=64, by=1),
      f1=FALSE
    ),
    type="l",
    lty=1,
    col="red",
    cex=2
  );

  # define x-axis
  axis(
    side=1,
    at=p.ratio.data$trials,
    labels=TRUE,
    tick=TRUE,
    outer=FALSE,
    lty="solid",
    lwd=2,
    lwd.ticks=2.5,
    cex.axis=1.85,
    padj=1
  );

  # define y-axis
  axis(
    side=2,
    at=seq(from=yaxs[i,1], to=yaxs[i,2], by=yaxs[i,3]),
    labels=TRUE,
    tick=TRUE,
    outer=FALSE,
    lty="solid",
    lwd=2,
    lwd.ticks=2,
    cex.axis=2.5,
    padj=-1
  );
}

rm(rcol, yaxs, pmai, i);

# plot x-axis label
mtext(
  text="Number of trials",
  side=1, # bottom
  line=-1.5,
  outer=TRUE, # use outer margin
  cex=2.5
);

# plot y-axis label
mtext(
  text="Ratio of true variance to reported variance",
  side=2, # left
  line=-2.5,
  outer=TRUE, # use outer margin
  cex=2.5
);

dev.off();

# restore original settings
par("mai"=mai);
rm(mai);

#######################################################
##### Plot ratio vs trial number in black & white #####
#######################################################

tiff(
  filename="./plots/parallel_Ratio_vs_TrialNumber_bw.tif",
  width=1920,
  height=1080,
  units="px",
  pointsize=12,
  compression="lzw",
  bg="white",
  type="cairo"
);

#jpeg(
#  filename="./plots/parallel_Ratio_vs_TrialNumber_bw.jpg",
#  width=1920,
#  height=1080,
#  units="px",
#  pointsize=12,
#  quality=100,
#  bg="white",
#  type="cairo"
#);

#png(
#  filename="./plots/parallel_Ratio_vs_TrialNumber_bw.png",
#  width=1920,
#  height=1080,
#  units="px",
#  pointsize=12,
#  bg="white",
#  type="cairo"
#);

mai <- par("mai");

layout.matrix <- matrix(
  data=c(1, 2, 3, 4),
  nrow=2,
  ncol=2,
  byrow=TRUE
);

layout(mat=layout.matrix);

rm(layout.matrix);

layout.show(n=4);

rcol <- c(2, 3, 5, 7);
yaxs <- matrix(data=NA, nrow=4, ncol=3);
yaxs[1,] <- c(1, 3, 0.4);
yaxs[2,] <- c(1, 1.8, 0.2);
yaxs[3,] <- c(1, 1.4, 0.1);
yaxs[4,] <- c(1, 1.2, 0.05);
pmai <- matrix(data=NA, nrow=4, ncol=4);
pmai[1,] <- c(0, 0.45, 0, 0);
pmai[2,] <- c(0, 0, 0, 0);
pmai[3,] <- c(0.18, 0.45, 0, 0);
pmai[4,] <- c(0.18, 0, 0, 0);

for (i in 1:4) {
  # define margin dimensions
  par("mai"=mai+pmai[i,]);

  # plot ratio
  plot(
    x=p.ratio.data$trials,
    y=p.ratio.data[,rcol[i]],
    ylim=c(yaxs[i,1], yaxs[i,2]),
    type="p",
    pch=20,
    cex=2,
    xlab="",
    ylab="",
    main=mtext(
      text=parse(
        text=paste0("nu==", dfree[rcol[i]-1])
      ),
      cex=2.5
    ),
    bty="n",
    axes=FALSE
  );

  # draw whiskers
  arrows(
    x0=p.ratio.data$trials,
    y0=p.ratio.data[,rcol[i]]-1.96*sqrt(p.var.data[,rcol[i]]),
    x1=p.ratio.data$trials,
    y1=p.ratio.data[,rcol[i]]+1.96*sqrt(p.var.data[,rcol[i]]),
    length=0.05,
    angle=90,
    code=3,
    lwd=2
  );

  abline(
    h=dfree[rcol[i]-1]/(dfree[rcol[i]-1]-2),
    lty="dashed",
    lwd=2
  );

  abline(
    h=dfree[rcol[i]-1]/(dfree[rcol[i]-1]-4),
    lty="dotted",
    lwd=2
  );

  #points(
  #  x=p.ratio.data$trials,
  #  y=varRatio(
  #    df=dfree[rcol[i]-1],
  #    k=p.ratio.data$trials,
  #    f1=FALSE
  #  ),
  #  type="p",
  #  pch=4,
  #  col="gray",
  #  cex=2
  #);

  lines(
    x=seq(from=2, to=64, by=1),
    y=varRatio(
      df=dfree[rcol[i]-1],
      k=seq(from=2, to=64, by=1),
      f1=FALSE
    ),
    type="l",
    lty=1,
    col="gray",
    cex=2
  );

  # define x-axis
  axis(
    side=1,
    at=p.ratio.data$trials,
    labels=TRUE,
    tick=TRUE,
    outer=FALSE,
    lty="solid",
    lwd=2,
    lwd.ticks=2,
    cex.axis=1.85,
    padj=1
  );

  # define y-axis
  axis(
    side=2,
    at=seq(from=yaxs[i,1], to=yaxs[i,2], by=yaxs[i,3]),
    labels=TRUE,
    tick=TRUE,
    outer=FALSE,
    lty="solid",
    lwd=2,
    lwd.ticks=2,
    cex.axis=2.5,
    padj=-1
  );
}

rm(rcol, yaxs, pmai, i);

# plot x-axis label
mtext(
  text="Number of trials",
  side=1, # bottom
  line=-1.5,
  outer=TRUE, # use outer margin
  cex=2.5
);

# plot y-axis label
mtext(
  text="Ratio of true variance to reported variance",
  side=2, # left
  line=-2.5,
  outer=TRUE, # use outer margin
  cex=2.5
);

dev.off();

# restore original settings
par("mai"=mai);
rm(mai);

rm(dfree);
