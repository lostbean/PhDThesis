Prior austenite reconstruction
==============================

Two new methods, one for determining the experimentally observed OR and
another for reconstructing prior austenite phase, are proposed. Both
methods are based on the angular deviation of the OR at the grain
boundaries. The first algorithm identifies the optimum OR using the
misorientation distribution of the entire scan i.e. without manual
selection of parent grains. The second algorithm reconstructs the parent
phase using a random walk clustering technique that identifies groups of
closely related grains based on their angular deviation of the OR.

Introduction
------------

Most of the steelmaking process occurs at elevated temperature which,
for most steel grades, implies that part of the processing occurs in the
austenitic phase. The characterization of the high-temperature
austenitic phase is of great value to understand and control the
microstructure evolution during the entire production chain. But the
direct observation of the austenitic phase is not trivial. It can only
be done at elevated temperatures, and therefore, it can only be observed
within very specialized *in-situ* equipment like EBSD with a hot
stage[@seward_high-temperature_2002]. Moreover, such sophisticated
measurements are always done under simulating laboratory conditions,
which are not necessarily representative of the real process.

Alternatively, indirect measurements are done at the room temperature.
If, at this temperature, the phase transformation has already completed
then the prior austenite microstructure can observed by: (a) optical
microscopy with special etching techniques that revel the original grain
boundaries[@bechet_new_1955], (b) reconstruction of OIM obtained with
standard EBSD
equipment[@cayron_reconstruction_2006; @miyamoto_mapping_2010; @abbasi_approach_2012; @tari_back_2013; @bernier_alternative_2014].
The latter method identifies and groups product grains derived from a
single parent (austenitic) grain. This technique depends on the presence
of a specific OR between parent and product phases, and therefore, it
only applies to diffusionless phase transformations. This work
introduces two new methods aiming to improve the
current-state-of-the-art in prior austenite reconstruction.

Orientation relationship calculation
------------------------------------

One of the main characteristics of martensitic transformations is the
presence of a specific OR between parent and product phases. Although
there are some theoretical orientation relationships proposed such as
the KS or NW correspondences for steels, a more accurate OR can only be
derived from experimental data. The available
methods[@miyamoto_accurate_2009; @humbert_refinement_2011] for OR
identification depend on the manual selection of parent grains, which is
time consuming and in some cases, depending on the microstructure,
impractical.

The method proposed here can derive the OR from an orientation scan in a
fully automated manner and it is based on the misorientation between
neighboring grains. There are two possible situations where the OR can
be observed. The first one is between a parent grain and its product
grain (when still a residual fraction of the parent phase is present in
the product microstructure), cf. ([\[eq:1\]](#eq:1){reference-type="ref"
reference="eq:1"}). The second case is between product grains sharing
the same parent and involves a double OR transformation, cf.
([\[eq:2\]](#eq:2){reference-type="ref" reference="eq:2"}). In the ideal
conditions that the proposed OR is valid, the angular deviation
($\Delta OR$) is equal to $0$.

The algorithm finds the optimized OR transformation
($T^{\alpha'\leftrightarrow\gamma}$) that minimizes the average angular
deviation for all grain boundaries or for a selected subset of them. The
number of grain boundaries exhibiting this OR should be vastly larger
than the number of prior parent grain boundaries. This is a reasonable
assumption since the martensitic phase transformation produces grain
refinement and all the new grain boundaries have misorientations that
obey the OR transformation.

$$T_{m}^{\alpha'\leftrightarrow\gamma}\left(Og^{\alpha'}\right)\left(Og^{\gamma}\right)^{-1}=\Delta OR\label{eq:1}$$

$$T_{m}^{\alpha'\leftrightarrow\gamma}T_{n}^{\gamma\leftrightarrow\alpha'}\left(Og_{a}^{\alpha'}\right)\left(Og_{b}^{\alpha'}\right)^{-1}=\Delta OR\label{eq:2}$$

![[\[fig:recons\]]{#fig:recons label="fig:recons"}Validation of
reconstruction algorithm on Fe-Ni alloy: (a) initial OIM before
reconstruction, (b) retained austenite and expected prior austenite GB,
(c) reconstruction after the first iteration and (d) second and final
step of
reconstruction.](/img/ausrec/sample1/alpha+gamma.svg){width="70%"}\
(a)

![[\[fig:recons\]]{#fig:recons label="fig:recons"}Validation of
reconstruction algorithm on Fe-Ni alloy: (a) initial OIM before
reconstruction, (b) retained austenite and expected prior austenite GB,
(c) reconstruction after the first iteration and (d) second and final
step of
reconstruction.](/img/ausrec/sample1/RA+GB.svg){width="70%"}\
(b)

![[\[fig:recons\]]{#fig:recons label="fig:recons"}Validation of
reconstruction algorithm on Fe-Ni alloy: (a) initial OIM before
reconstruction, (b) retained austenite and expected prior austenite GB,
(c) reconstruction after the first iteration and (d) second and final
step of
reconstruction.](/img/ausrec/sample1/1st+GB.svg){width="70%"}\
(c)

![[\[fig:recons\]]{#fig:recons label="fig:recons"}Validation of
reconstruction algorithm on Fe-Ni alloy: (a) initial OIM before
reconstruction, (b) retained austenite and expected prior austenite GB,
(c) reconstruction after the first iteration and (d) second and final
step of reconstruction.](/img/ausrec/sample1/2nd+GB.svg){width="70%"}\
(d)

Two strategies were employed in order to calculate the grain boundary
misorientations. One is based on the average orientation of both
neighboring grains. The other one is based on the orientation in the
immediate vicinity of the grain boundary, where only the first layer of
orientations at each side of the grain boundary is selected. Slightly
different values of OR are obtained by the two strategies due to plastic
strain accommodation during phase transformation and associated
orientation gradients in bulk of the grains[@miyamoto_accurate_2009].

This new method was validated with a low Nb steel alloy with carbon
content around $0.05\%$.
Figure [\[fig:ORhist\]](#fig:ORhist){reference-type="ref"
reference="fig:ORhist"} compares the distribution of angular deviation
using the theoretical KS OR and the optimized OR, it can be noticed that
the distribution shifts to the left, i.e. reducing the overall error,
when the optimized OR is used. Better results are obtained when
considering orientations in the immediate vicinity of the grain boundary
compared to considering grain averaged orientations.
Figure [\[fig:ORcomp\]](#fig:ORcomp){reference-type="ref"
reference="fig:ORcomp"} compares the optimized OR for the low Nb sample
with two theoretical ORs and one reference
value[@miyamoto_accurate_2009] that contains comparable amount of
carbon. The optimized OR is in good agreement with the reference value.

![Fitted OR compared with theoretical KS.](/img/tikz/ORFittingHist.svg){width="70%"}

Parent phase reconstruction
---------------------------

A new method, equally based on grain boundary misorientations, is
proposed for the parent phase reconstruction. The key concept of this
method is the use of a graph clustering algorithm to find groups of
closely related grains that originate from a single parent grain. The
Markov Clustering algorithm[@dongen_graph_2001; @schaeffer_graph_2007]
is used because of the following features:

-   It is based on the probabilities of random walks through the graph.

-   It identifies natural cuts and clusters in the graph without the
    need to specify threshold values of allowable angular deviations or
    the necessity to specify a predefined number of clusters

-   There is only one parameter that controls the cluster coarsening.

-   Excellent noise tolerance.

-   Fast calculation time.

In order to use the clustering algorithm, a graph has to be derived from
the OIM in the following way: (a) each grain, product or parent, becomes
a node in the graph, (b) each grain boundary becomes an edge connecting
two neighboring nodes (grains) and (c) each edge receives a value based
on the equation ([\[eq:1\]](#eq:1){reference-type="ref"
reference="eq:1"}) or ([\[eq:2\]](#eq:2){reference-type="ref"
reference="eq:2"}), which determines the probability of the two
neighboring grains being products of the same parent. Once the graph is
constructed, the clusters are determined using the highest level of
coarsening. Then each cluster forms a parent grain and its orientation
is calculated by an optimization algorithm that minimizes the average
angular deviation using the
equation ([\[eq:1\]](#eq:1){reference-type="ref" reference="eq:1"}).

The first iteration described above may result in some of the clusters
containing more than one parent grain because of the initial coarsening
level. These grains can be identified by its higher average angular
deviation, and therefore, they should be further refined by running an
extra iteration step with reduced coarsening level. The refinement
procedure should be repeated many times until all parent grains have low
average error.

A Fe-Ni 29% sample was used to validate the proposed reconstruction
method. The OIM after the martensitic phase transformation with still
10% retained austenite is shown in the
figure [\[fig:recons\]](#fig:recons){reference-type="ref"
reference="fig:recons"}a. The expected parent microstructure is
visualized in figure [\[fig:recons\]](#fig:recons){reference-type="ref"
reference="fig:recons"}b by the retained austenite and the traces of
high angular deviation ($>10^{\circ}$).
Figure [\[fig:recons\]](#fig:recons){reference-type="ref"
reference="fig:recons"}c shows the reconstruction after the first
iteration, showing that some reconstructed grains incorporate more than
one parent grain. The final microstructure is shown in
figure [\[fig:recons\]](#fig:recons){reference-type="ref"
reference="fig:recons"}d after an extra step of refinement.

A careful look at the
figure [\[fig:recons\]](#fig:recons){reference-type="ref"
reference="fig:recons"}d shows that some unexpected features are visible
at twin boundaries in parent grains and some grains are over refined.
The current implementation of the reconstruction software is quite
simple and does not include any special rule for treating this type of
boundaries or for re-merging over-divided clusters.

Conclusion
----------

Tolerance error reduction and noise immunity are two fundamental
strategies for the success of prior austenite reconstruction. The use of
optimized ORs reduces the overall error but the identification by
previous techniques requires manual selection of parent grains. The
method proposed here identifies an optimized OR, whereby the entire
orientation scan is considered and the result is obtained without user
intervention.

Markov clustering seems a promising technique for parent grain
reconstruction due its noise tolerance and fast calculation time. The
present results are satisfactory considering the initial stage of
development and the simple implementation. Further work on the special
treatment of twin boundaries and cluster re-merging will be done.

References {#references .unnumbered}
----------

mainmatter/bib/ausrec
