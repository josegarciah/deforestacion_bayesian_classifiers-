
# A Hybrid Intelligence Approach to Predicting and Explaining Land use and Land Cover Change using Bayesian Classifiers
**Christopher Rhodes Stephens** (Centro de Ciencias Nucleares, Centro de Ciencias de la Complejidad, UNAM)

**Constantino González-Salazar** (Instituto de Ciencias de la Atmósfera y Cambio Climático, UNAM)

**José García-Hernández** (Centro de Investigaciones en Geografía Ambiental UNAM)

# Abstract
The future health of land systems depends on our ability to quantify and understand
the causes of land cover change. This requires the integration of data of different types
and spatio-temporal resolutions and the creation of accurate, explainable prediction
models. To achieve this we present a framework where all variables are converted to
binomial form at the same spatio-temporal resolution, co-occurrences between classes
of interest, $C$, and a set of predictive features, $\mathbf X$, are computed and
converted into a Bayesian classifier $P(C|\mathbf X)$, where each feature $X_{ij}$ is
quantified by its effect size, coverage, statistical significance and potential causality. As
a usecase we predict deforestation at the municipal level in Mexico in 2020 using a set
of 98 Climatic, Social, Economic and Governmental factors as putative causes,
showing that besides the impact of alternative land uses, the models infer an important
role for the *Sembrando Vida* government program.

# Classes
The data that was used to calculate the deforestation metrics we define
below in section 4.2 was the deforestation area in Mexico in 2020 considering
the loss band defined as a stand replacement disturbance (a change from a
forest to non-forest state) of Global Forest Change [9]. As our interest in this
paper is to demonstrate the generality of our approach in predicting both
extensive and intensive dependent variables, we consider both the extent
and intensity of deforestation. For the latter we considered area lost as a
percentage relative to municipal area [36] and relative to tree canopy cover for
the year 2000, where the canopy cover threshold was > 30%. The processing
data was in Google Earth Engine using the code linked here <https://code.earthengine.google.com/579168f4ae01806f665755b1d6a9f4ae>

The classes of interest were chosen to be the 10% (250) of
Mexican municipalities with the highest values of the following categories:

1. Loss of forest cover in 2020 in total number of hectares of the municipality

2. Percentage loss of forest cover in 2020 relative to the municipal area

3. Percentage loss of forest cover in 2020 relative to the municipal forest
cover in 2000.

# Predictors 
As predictors we considered 98 interval variables divided into 6 types:
39 social, 19 economic, 18 governmental, 19 climatic, 2 forest cover and
one municipal area variable.

# CHILAM LAB 
Este proyecto forma parte del laboratorio CHILAM <https://chilam.c3.unam.mx/>

Tambien se encuentra en el siguiente repositorio [GitHub] (https://github.com/chilam-lab)
