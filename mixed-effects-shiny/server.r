library(shiny)
library(ggplot2)
library(lme4)
library(lmerTest)
library(arm)

# a function for generating pitch data for children
# 500 Hz at 2
# 400 Hz at 6
# 300 Hz at 10
# 150 Hz at 20
# 150 Hz at 24
# 150 Hz at 28
# inputs: no of groups, no of measurements per group, longitudinal?, adults?

generate_data <- function (group.no, measurement.no, longitudinal=F, adult=F) {
  # depending on adult, the age range, pitch mean and pitch sd are different
  # no effect for adults
  if (adult) {
    age.range <- c(22,30)
    pitch.mean <- function (x) {rep(130, length(x))}
    pitch.sd <- 10
  } else {
    age.range <- c(2,10)
    pitch.mean <- function (x) {550-25*x}
    pitch.sd <- 30
  }
  
  # different depending on group.no:
  if (group.no == 1) {
    # groups - same for all observations
    groups <- as.factor(rep(1, measurement.no))
    # uniformly distr ages between 2 and 10
    ages <- seq(age.range[1],age.range[2],length.out=measurement.no)
    # mean pitch generated using the pitch.mean function,
    # defined differently based on the value of adult;
    # plus variance determined by pitch.sd
    pitches <- pitch.mean(ages) + rnorm(ages, 0, pitch.sd)
    d <- data.frame(group=groups, age=ages, pitch=pitches)
  } else {
    # groups - varies; measurement.no measurements for each group
    groups <- as.factor(rep(1:group.no, each=measurement.no))
    # different procedure depending on longitudinal:
    if (!longitudinal) {
      # each speaker has own age
      ages.per.speaker <- seq(age.range[1],age.range[2],length.out=group.no)
      ages <- rep(ages.per.speaker, each=measurement.no)
      # generate speaker pitch intercepts
      pitches.per.speaker <- as.list(pitch.mean(ages.per.speaker) + rnorm(ages.per.speaker, 0, pitch.sd))
      pitches <- do.call(c, lapply(pitches.per.speaker, function (x) {rnorm(measurement.no, x, 10)}))
    } else {
      # each speaker varies in age
      ages.within.speaker <- seq(age.range[1],age.range[2],length.out=measurement.no)
      ages <- rep(ages.within.speaker, times=group.no)
      # generate population level pitch avgs and slopes
      pop.slope <- ifelse(adult, 0, 25)
      pop.avg <- ifelse(adult, 130, 400) # value at middle of age range
      middle <- ifelse(adult, 26, 6) # what is the middle of the age range?
      # modified pitch function
      pitch.mean <- function (x, middle, avg, slope) {avg - slope*(x-middle)}
      # per speaker slopes, avgs
      slopes.per.speaker <- rnorm(group.no, pop.slope, 5)
      avgs.per.speaker <- rnorm(group.no, pop.avg, pitch.sd)
      # this looks complicated, but it's really just applying pitch.mean
      # separately for each speaker, using their slopes and avgs
      pitches <- do.call(c, lapply(as.list(1:group.no), function (x) {pitch.mean(ages.within.speaker, middle, avgs.per.speaker[x], slopes.per.speaker[x]) + rnorm(ages.within.speaker, 0, 10)}))
    }
  }
  d <- data.frame(group=groups, age=ages, pitch=pitches)
  return(d)
}

# a function for fitting a model to the data

fit_model <- function (d, randintr=F, randslope=F) {
  # lm if there are no random structures
  # otherwise lmer with appropriate random strucs
  if (!randintr & !randslope) {
    mod <- lm(pitch ~ age, d)
  } else if (randintr & !randslope) {
    mod <- lmer(pitch ~ age + (1 | group), d)
  } else if (randintr & randslope) {
    mod <- lmer(pitch ~ age + (1 + age | group), d)
  } else {
    mod <- lmer(pitch ~ age + (0 + age | group), d)
  }
  return(mod)
}

# this is the plotting function: it generates plots appropriate to the data & model combo

create_plot <- function (d, m, longitudinal, adult, show.model=F, show.ranef=F) {
  
  # y limits for different groups
  if (adult) {
    ylims <- c(50,200)
  } else {
    ylims <- c(200,600)
  }
  
  

  # different plots depending on longitudinal
  if (longitudinal) {
    output.plot <- ggplot(d, aes(x=age, y=pitch, group=group)) + 
                   geom_point(aes(col=group), alpha=ifelse(show.ranef, 0.2, 0.5), size=3) +
                   theme_bw() +
                   theme(legend.position="none", axis.text=element_text(size=rel(1.5)),
                         axis.title=element_text(size=rel(2))) +
                   ylim(ylims[1],ylims[2])
  } else {
    output.plot <- ggplot(d, aes(x=age, y=pitch, group=group)) + 
                   geom_point(aes(col=group), alpha=ifelse(show.ranef, 0.2, 0.5), size=3) +
                   theme_bw() +
                   theme(legend.position="none", axis.text=element_text(size=rel(1.5)),
                         axis.title=element_text(size=rel(2))) +
                   ylim(ylims[1],ylims[2])
  }
  if (show.ranef) {
    if (class(m)!="lm" & !longitudinal) {
      new.d <- d[seq(1, nrow(d), nrow(d)/length(unique(d$group))),]
      preds <- predict(m, newdata=new.d)
      new.d$rpreds <- preds
      output.plot <- output.plot + geom_point(data=new.d, aes(x=age, y=rpreds, col=group), fill="white", shape=21, size=5)
    } else if (class(m)!="lm" & longitudinal) {
      preds <- predict(m)
      d$rpreds <- preds
      output.plot <- output.plot + geom_line(data=d, aes(x=age, y=rpreds, group=group, col=group), 
                                             alpha=ifelse(show.model, 0.2, 1))
    }
  }
  if (show.model) {
    if (class(m)!="lm") {
      new.d <- expand.grid(group=d$group[1], 
                           age=seq(min(d$age), max(d$age), length.out=100),
                           pitch=0)
      mm <- model.matrix(terms(m),new.d)
      new.d$pitch <- predict(m,new.d,re.form=NA)
      pvar1 <- diag(mm %*% tcrossprod(vcov(m),mm))
      cmult <- 1.96
      new.d <- data.frame(
        new.d,
        plo = new.d$pitch-cmult*sqrt(pvar1),
        phi = new.d$pitch+cmult*sqrt(pvar1)
      )
      output.plot <- output.plot + geom_line(data=new.d, aes(x=age, y=pitch), lwd=2) +
                     geom_ribbon(data=new.d, aes(x=age, ymin=plo, ymax=phi), alpha=0.3, fill="grey")
    } else {
      new.d <- expand.grid(group=d$group[1], 
                           age=seq(min(d$age), max(d$age), length.out=100),
                           pitch=0)
      preds <- predict(m,new.d,se.fit=T)
      new.d$pitch <- preds$fit
      cmult <- 1.96
      new.d$plo <- preds$fit - cmult*preds$se.fit
      new.d$phi <- preds$fit + cmult*preds$se.fit
      output.plot <- output.plot + geom_line(data=new.d, aes(x=age, y=pitch), lwd=2) +
        geom_ribbon(data=new.d, aes(x=age, ymin=plo, ymax=phi), alpha=0.3, fill="grey")
    }
  }
  
  return(output.plot)
}

shinyServer(function(input, output, session) {
  
  # when run button is pressed ...
  observeEvent(input$run, {
    
    sigs <- rep(F, input$iterations)
    for (i in 1:input$iterations) {
      # generate fake data
      d <- generate_data(input$group.no, 
                         input$measurement.no, 
                         input$longitudinal, 
                         input$adult)
    
      # fit model
      m <- fit_model(d, 
                     randintr=input$randintr,
                     randslope=input$randslope
                     )
      if (class(m)=="lm") {
        sigs[i] <- summary(m)$coef[2,4] < 0.05
      } else {
        sigs[i] <- summary(m)$coef[2,5] < 0.05
      }
    }
    sig.text <- paste("\n", sum(sigs), "/", length(sigs), " iterations significant; ", round(100*sum(sigs)/ length(sigs), 2), "%", sep="")
    
    # create plot
    print(class(m))
    pl <- create_plot(d, m, 
                      longitudinal=input$longitudinal,
                      adult=input$adult,
                      show.model=input$showmodel,
                      show.ranef=input$showrandom)
    
    # send plot to plotting window
    output$dataPlot <- renderPlot(print(pl))
    output$summary <- renderText(sig.text)
    #output$modelSummary <- renderPrint(display(m, detail=T))
  })
})
  
