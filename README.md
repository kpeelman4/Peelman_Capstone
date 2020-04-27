# Peelman_Capstone
---

## Library 

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(ez)
library(dplyr)
```

## 1. Background, Significance & Rationale

Based on work from my undergraduate research group: de Vivo et al., 2017, Science https://science.sciencemag.org/content/sci/355/6324/507.full.pdf

Sleep takes up a third of our lives, and yet it still remains unclear why we sleep, and how our brains function in sleeping versus waking states.  While most of us are fortunate enough to sleep during the night time and be awake during the day, many of society's most critical jobs require working during the evening, forcing those nearly 15 million people to sleep during the daytime.  This alternate sleeping pattern has been shown to have negative influence on essential cognitive function such as learning and decision making, yet the underlying mechanism as to why these functions are compromised remain unclear. 

Dendritic spines of neurons recieve input from an individual axon at the synapse, and help transmit electrical communication signals to neurons.  Functionally, changes in spine strength are thought to be an anatomical mechanism for memory and learning.  The synaptic homeostasis hypothesis (SHY) states that due to energy and signaling requirements of neurons during waking states, learning should occur primarily through synaptic potentiation - leading to an increase in synaptic weight - wheras slow wave sleep is associated with synaptic downscaling.  In a seminal finding published in Science (de Vivo et al., 2017), evidence was put forth in favor of SHY, where it was shown that cortical excitatory synapse volume and axon-spine interface area increased after wake and decreased after sleep.  This finding suggests that the main function of sleep is to renormalize synaptic strength.  While the authors attempt to control for circadian timing by having a cohort of sleep-deprived mice, this does not necessarily recapitulate the behavioral state of night-shift workers.  

## 2. Unknown

It is unknown as to whether changes in axon-spine interface (ASI) area following sleep demonstrate this same pattern of downscaling when circadian timing is reversed, as in night-shift work.  

## 3. Written Hypothesis ("if/then" prediction)

If mice are exposed to a reverse-cycle circadian rhythm, ASI area will still demonstrate the same pattern of upscaling during wake and downscaling following sleep.  This would show that synaptic downscaling during sleep is independent of circadian influence.  

## 4. Variables 

The two main variables we are examining here are 1) experimental groups [3 levels: standard sleep cycle (SLC), sleep restricted (SR) and reverse light cycle (RLC)] and 2) ASI area (nm^2).  Each subject (mouse) is the experimental unit.  Here, the dependent variable would be the ASI area, while the independent variables would be the experimental groups.  The dependent variable here is continuous (a measurement with units of nm^2).  The experimental groups are nominal variables.  

## 5. Statistical Hypothesis

First, one-way ANOVA, repeated measures:

**$\H_{0} \ V_{sleep group} = V_{residuals}$** 
Null hypothesis: the variance associated with each sleep group is equal to that of the variance of the residuals.

**$\H_{1} V_{sleep group} /=/  V_{residuals}$** 
Alternate hypothesis: the variance associated with each sleep group is not equal to that of the variance of the residuals.

Then, post-hoc test:

ASI size after sleep for mice that sleep on a reverse light cycle (RLC) is larger than those that sleep on a standard light cycle (SLC), indicating less synaptic downscaling.  

**$\H_{0} \ mu_{SLC} = mu_{RLC}$**
Null hypothesis: Mean ASI size after sleep for mice that sleep on a standard light cycle (SLC) is the same as those that sleep on a reverse light cycle (RLC), indicating circadian timing has no direct influence on synaptic scaling.

**$\H_{1} mu_{sLC} /=/  mu_{RLC}$** 
Alternate hypothesis: Mean ASI size after sleep for mice that sleep on a standard light cycle (SLC) is less than those that sleep on a reverse light cycle (RLC), indicating synaptic downscaling has an influence on synaptic scaling.  

## 6. Statistical Test Choice

In this analysis, I will use a one-way ANOVA, repeated measures.  Here, there is only one factor (sleep timing) that has greater than two levels, and one independent variable that is being measured across levels.  I will also run a pairwise post-hoc adjustment to compare the mean ASI volume for each group (comparing both reverse light cycle and sleep-restricted mice to standard light cycle mice). These animals all have the same genotype (wild type) and are not being sampled from more than once each.  Additionally, the ASI volumes for each  mouse is not intrinsically linked, and to ensure this, mice from different lineages (but still wild-type background) will be utilized in order to ensure random sampling.  However, repeated measurements will be taken from the same animal (the same animal will be contributing multiple ASI measurements to the dataset).  One could instead run t-tests on each group, although this would be significantly less efficient.  I will be using a Bonferroni test because it is a strict test.  This will be acceptable because this finding may be important for future steps to take in terms of future experiments to design.  

## 7. Methods 

**Animals**: Three groups C67BL/6 mice aged p60 at day of sacrifice will be used for the study.  Each animal comes from a different litter.  

**Data Acquisition**

  **Sleep Monitoring**: Animals will be housed alone in single cages with free access to food and water.  Sleep will be monitored using infared cameras to monitor movement.  Our group has shown that video tracking of sleep is >90% accurate as compared to EEG/EMG recordings for discerning sleep vs. wake.  Because of this, in order to preserve cortical tissue for later segmentation, animals will be instead be monitored by video. 
  
  **Sleep Cycles**: Standard mice and sleep restricted mice will both be on a standard light cycle (12 h:12 h light-dark cycle; lights on at 08:00), while the reverse light cycle group will have just that: a reversed light cycle.  Mice are nocturnal, and typically sleep during lights on, and are awake and active during lights off.  In order to insure that the reverse light cycle mice are awake during lights off and sleeping during lights on, mice will be provided with enrichment (toys, running wheels etc) during waking hours.  Mice in the sleep restriction group will be provided with enrichment through the entire 24-hour cycle.  Standard light cycle mice will be provided with enrichment during lights off.  Mice will be given 7 days in each paradigm before brains will be collected for analysis, except for the sleep restricted group, which will have brains collected after 24 hours of sleep restriction.
  
  **Brain Extraction**: Brains will be collected for each group after each group (aside from the sleep restriction group) after a long final bout of sleep (> 45 min, interrupted by periods of wake of <4 min) after which spent 75% of the last 6-7 hours asleep. AFter brains are extracted, a punch will be taken in the superficial regions of primary motor cortex (M1) as in de Vivo et al., 2017.  This punch will be what is used for acquiring the electron mircroscope images for segmentation.
  
  **ASI Quantification**: After brain punches are acquired, images are acquired with a scanning electron microscope (SEM).  Images are then processed by a blind panel of expert users using Fiji to fill in and calculate the ASI for every spine encountered per image.  The axon-spine interface (ASI) is defined as the interface between the spine head and the presynaptic terminal or bouton.  

## 8. Plot

```{r}
  
b = 0.256 #expected outcome value
a = 1.18 #expected effect of SD
f = 1.05 #minimal scientifically relevant effect of RLC
sd = 0.1 #expected standard deviation
n = 130 # number of independent replicates per group 
sims = 100 

dataMaker <- function(n, b, a, f, sd) { 
  
  
  a1 <- rnorm(n, b, sd) #basal or negative ctrl
  a2 <- rnorm(n, (b*a), sd) #positive control or some other treatment
  a3 <- rnorm(n, (b*f), sd) #treatment effect
    
    Outcome <- c(a1, a2, a3)
    Predictor <- c(rep(c("SLC", "SR", "RLC"), each = n))
    ID <- as.factor(c(1:length(Predictor)))
    df <-data.frame(ID, Predictor, Outcome)
    }

dat <- dataMaker(n,b,a,f,sd)
dat <- subset(dat, Outcome > 0)   # no values below 0 should be included for area measurements


z = ggplot(dat, aes(Predictor, Outcome, color = Predictor))+
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  geom_boxplot(fill="transparent", color = "black") + 
  xlab(" ") +
  ylab("ASI Volume (um^3)") +
  ggtitle("Simulated ASI Volume per Sleep Group") + 
  theme(legend.title = element_blank())

z + stat_summary(fun.y=mean, geom="point", shape=22, size=4) 
```
<img width="690" alt="Untitled" src="https://github.gatech.edu/storage/user/34673/files/c802e800-88a0-11ea-9e5d-a76dd08501ba">

I suspect that the effect of synaptic downscaling during sleep will still be present regardless of circadian timing, but potentially to a lesser degree.  Downscaling will occur less than for those animals who did not sleep at all (SR). Values chosen here are based on the reported means and error listed in the supplement of de Vivo et al., 2017.  

## 9. Monte Carlo
```{r message=FALSE, warning=FALSE}

# ANOVA - Power
p_ANOVA <- replicate(
  sims, {
 
    sample.df <- dataMaker(n,b,a,f,sd)
    
    sim.ezaov <- ezANOVA(
            data = sample.df, 
            wid = ID,
            dv = Outcome,
            between = Predictor,
            type = 2
            )
  
  p_ANOVA <- sim.ezaov$ANOVA[1,5]
    
    }
  )

pwr.pct <- sum(p_ANOVA<0.05)/sims*100
paste(pwr.pct, sep="", "% power for one-way RM ANOVA with n=130")

```

We need a sample size of n=130 ASI measurements per treatment group in order for 94% power to be achieved.  We know that around 100 ASI measurements can be taken per animal (see de Vivo et al., 2017 - 1600 total ASI measurements across 12 mice (4 mice per treatment group), 1600/12 = 133).  Therefore, we need a minimum of 2 animals per treatment group for this experimental paradigm, meaning N=6 animals total.  N=6 total, N=2/group, n=200 ASI measurements/group, n=100 ASI measurements/animal.  

## 10. Knit & Submit!



