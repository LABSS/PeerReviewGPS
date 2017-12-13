# What is it?

This is a model of paper publication and consumption that allows to study the effect of two different evaluation mechanisms, peer review and reputation, on the quality of the manuscripts accessed by a scientific community.

# How it works

In order to compare the two filters - reputation and peer review - the model includes a common paper production mechanism and then runs two distinct sub-models for accessing papers: one driven by reputation and the other by peer review.

The simulation cycle of the model begins with paper production, generating the papers for the current time step and adding them to the papers pool. Afterwards, it applies either a simulated peer review process or a simulated reputation mechanism, both driving parallel reading processes. In the former case, scientists read papers as prioritized by journal’s quality whereas, in the latter case, they read papers as prioritized by their reputation.

The two sub-models share the same ground truth for what regards paper and scientist quality. This guarantees a clean comparison, made with exactly the same papers in the system, at the cost of ignoring the feedback from reading towards paper production.

# Installation notes (important!)

* This model needs NetLogo 5.3.1.
* The model makes use of four different NetLogo extensions:
    * `pr-utils` is a custom extension developped specifically for this model.
    * [`stats`](https://github.com/cstaelin/Stats-Extension) provides NetLogo with a tables for holding data gathered during runs, a number of statistical procedures to use on those data, and access to a number of useful distributions.
    * [`csv`](https://github.com/NetLogo/CSV-Extension) facilitates the reading and writing of CSV files.
    * [`rnd`](https://github.com/NetLogo/Rnd-Extension) provides primitive for weighted random selection.

    These extensions must be present, either under NetLogo's `extensions/` folder, or under the folder from which you run the model. (Note that `rnd` and `csv` are bundled with NetLogo version 6.0 and later but were not in NetLogo 5.3.1, thus the need to provide them here.)

* A folder called `results/` has to be present in the same location of the main NetLogo file. This directory is used to store detailed results of each simulation.

# How to use it

SETUP button — sets up the model by creating the agents.

GO button — runs the model

GO-ONCE - runs one step of the simulation

# Things to try

In order to reproduce the results presented in the paper you will find defined the different set of experiments in the BehaviorSpace. Detailed results are stored in the folder `results/` that previously must be created in the directory where the application is installed (see installation notes).

# Credits and references

Copyright 2015 Mario Paolucci, Jordi Sabater-Mir & Francisco Grimaldo under the GPL licence. Developed at LABSS (http://labss.istc.cnr.it/), ETSE-UV, IIIA-CSIC.
