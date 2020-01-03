Texture Sampling
================

Introduction
------------

Polycrystalline metallic solids are composed of a contiguous set of
crystal grains. Such a set and its properties constitute the
microstructure and can be partially described using statistical
parameters like grain size, crystallographic orientation and
misorientation distributions. To a large extent the microstructure of a
polycrystal determines its engineering behavior by controlling *e.g.*
mechanical, corrosion and magnetic properties. In the last decades a
large number of models has been developed claiming to account for a
variety of material responses and some of these may benefit from
calculations employing 3D microstructures as an input.

During the development of a microstructurally based material model a set
of different input parameters are required for validation and
optimization. In order to avoid time-consuming and sophisticated 3D
measurements of microstructures a framework for virtual microstructure
generation is proposed here. A virtual microstructure is a model itself,
capable of generating a numerical representation of numerous
microstructural features that are able to substitute for experimental
data when used as input to microstructural models. Virtual
microstructures are very handy to produce test case samples, which can
be used for fast modeling prototyping and microstructural design since
they may cover a very large spectrum of microstructural state variables
even outside the range of practical implementation. Such capacity helps
one to find the boundary conditions and the crucial state variables in a
model. It is important to state that the aim is not to completely
replace experiments but reduce their number and improve their efficiency
by predefining which samples are better suited for experimental
validation. Once validated and with the boundary conditions checked, it
is also useful to look for optimum microstructures when one wants to
design a material.

In the past decades metallurgists and scientists have been looking at
the microstructure and defined a broad range of parameters to
characterize them. However to create a virtual microstructure the
opposite exercise has to be done, *i.e.* starting from a set of
microstructural state variables, conveniently expressed as statistical
distribution functions, a 3D set of contiguous grains needs to be
created which represents the virtual microstructure. Because
microstructures are very complex objects some assumptions are required
to simplify the framework. Therefore, it is assumed here that the
microstructure is fully characterized by a set of crystal orientations
of a single phase that are assembled in a contiguous volume, ignoring
in-grain heterogeneities. The input parameters of the microstructure
generator can be specified as a set of distribution functions and their
relations like grain morphology, orientation and grain boundary
character. Additional features such as number of neighbors, triple
junction angles or clustering of some properties might be considered as
well, but are ignored for the time being. The current paper will report
the results when the grain size and texture distribution functions are
given as input.

Method
------

In the proposed framework, surfaces are the main representative objects.
Each grain is represented by a set of surfaces forming a closed volume
rather than a set of subvolumes in a grid as *e.g.* voxels in a cubic
grid space. This choice allows for a more precise description of grain
boundaries and associated properties (such as *e.g.* local GB
curvature), and on the practical side it also allows for a compact data
description and thus a faster calculation. In the first version of the
model the boundaries are composed of flat surfaces, but in future these
flat surfaces will be converted to polynomial piecewise surfaces. If
necessary, further post-processing conversion to voxels or meshes can be
carried out for the sake of compatibility with others model formats such
as *e.g.* finite elements and phase field modeling.

The grains are first created using the concept of Voronoi cells, which
consist of a mathematical concept where a space $\mathbb{R}^{n}$ is
partitioned starting from a set of points $p\in\mathbb{R}^{n}$. Each
cell is defined by a region in the space where all distances are closer
to its central point $p$ than the central points from any other cell.
Voronoi structures bear a one-to-one relation with Delaunay
triangulation  [@aurenhammer1991voronoi]. Both concepts provide
equivalent information but stored in different ways, the so called
duality property, that allows a straightforward bijective conversion
between them. Even though there are algorithms to directly calculate
Voronoi cells, an indirect method that calculates the Delaunay
triangulation first and then converts it to Voronoi cells is used here.

The key point to generate a proper grain size distribution using Voronoi
cells is to find a proper set of central points that reproduces the user
specified grain size distribution. The user provides the grain size
distribution function $f(gs)$ as well as the lower and upper limits
$gs\in[a,b]$, thus defining the *target* distribution. If $n$ points are
randomly placed in a box of which the size depends on the number of
grains $n$ and the average grain size of the target distribution, the
resultant distribution is the so-called Poisson-Voronoi which exhibits a
lognormal distribution with a variance of $0.424$ [@xu2009topological].
Even though this distribution might be useful in some specific cases, in
more general cases the target distribution will deviate from the
lognormal one and may exhibit a different variance or even a bimodal
structure. In order to transform the initial set of points in a valid
one where the Voronoi cells follow the target distribution a RMC
algorithm is used  [@mcgreevy2001reverse]. Both target and current
distributions are discretized into histograms with volume fractions
$p_{i}$ in each bin and the error function described in equation
[\[eq:gs\_err\]](#eq:gs_err){reference-type="ref" reference="eq:gs_err"}
is employed for convergence.

The orientation assignment starts once a suitable grain size
distribution is found. The target distribution is obtained from a
discretized ODF in Euler space. To the purpose of discretization the
inverse of the CDF is sampled by a uniform distribution, very much
similar to the method proposed by Toth and Van Houtte
 [@toth1992discretization]. The CDF is constructed by numerical
integration of the ODF along its three angles, as describe in equation
[\[eq:cdf\]](#eq:cdf){reference-type="ref" reference="eq:cdf"} where
$v_{i,j,k}$ is the volume fraction in a discrete position of the Euler
space.

$$\begin{aligned}
error_{GS}=\sum_{i=1}^{N}(p_{i}-p_{i}^{target})^{2}.\label{eq:gs_err}\\
error_{ODF}=\sum_{i=1}^{N}\sum_{j=1}^{M}\sum_{k=1}^{L}(v_{i,j,k}-v_{i,j,k}^{target})^{2}.\label{eq:odf_err}\\
CDF(N,M,L)=\sum_{i=1}^{N}\sum_{j=1}^{M}\sum_{k=1}^{L}v_{i,j,k}\sin\Phi\Delta\phi_{1}\Delta\Phi\Delta\phi_{2}.\label{eq:cdf}\end{aligned}$$

Even though the method described above allows a fair ODF sampling , each
sampled orientation has implicitly the same weight or volume fraction.
It means that when the orientations are associated to the grains and the
grain set has a non-uniform volume fraction distribution, the resultant
ODF is very much likely to be distorted. On the other hand there are an
infinite set of orientations that when associated with a certain set of
grains will result in equivalent ODFs. This shows how ill-posed the
problem might be *i.e.* in its present form it is not guaranteed that a
unique solution can be found. In order to specify a unique solution the
missing state variables like misorientation or grain boundary character
distributions should be included. For the moment the model does not
include any of those parameters, therefore RMC will be applied to
enforce a *possible* association between orientations and volume
fractions using the error function described in equation
[\[eq:odf\_err\]](#eq:odf_err){reference-type="ref"
reference="eq:odf_err"}.

Example
-------

One example displaying the current capabilities of the virtual
microstructure generator will be given. A set of 1000 grains was
generated and fitted into a bimodal spectrum composed by overlapping two
normal distributions. The first distribution, that was obtained by
placing points randomly, is shown in figure
[\[fig:odf\_dist\_target\]](#fig:odf_dist_target){reference-type="ref"
reference="fig:odf_dist_target"}(a). As expected, it resembles a
lognormal distribution with a variance of approximately $0.43$ that is
typical for Poisson-Voronoi statistics. Finally, after the RMC
iterations, the desired distribution is obtained as one can observe in
figure
[\[fig:odf\_dist\_target\]](#fig:odf_dist_target){reference-type="ref"
reference="fig:odf_dist_target"}(b).

![[\[fig:odf\_dist\_target\]]{#fig:odf_dist_target
label="fig:odf_dist_target"}Generation of 1000 grains that correspond to
the target distribution composed by two overlapping normal distributions
($\mu_{1}=1,\;\sigma_{1}=0.5,\;\mu_{2}=5,\;\sigma_{2}=0.7$). (a) The
initial distribution after random positioning of points. (b) Final
distribution after RMC iterations. (c) 3D section of the resultant grain
set. ](mainmatter/img/texsamp/fig1){width="100%"}

Once the grain set is defined, the following step is to create the
texture by assigning orientations to each grain. Given a target ODF,
orientations were sampled and assigned randomly to the grains, see
figure
[\[fig:recons\_odf\_brutal\]](#fig:recons_odf_brutal){reference-type="ref"
reference="fig:recons_odf_brutal"}(b). As expected, the distribution
does not match with the target distribution, figure
[\[fig:recons\_odf\_brutal\]](#fig:recons_odf_brutal){reference-type="ref"
reference="fig:recons_odf_brutal"}(a), because of the non-uniform grain
size. After the RMC iterations a valid configuration was achieved as
shown on figure
[\[fig:recons\_odf\_brutal\]](#fig:recons_odf_brutal){reference-type="ref"
reference="fig:recons_odf_brutal"}(c).

![[\[fig:recons\_odf\_brutal\]]{#fig:recons_odf_brutal
label="fig:recons_odf_brutal"}Texture reconstruction. (a) Target ODF
with typical $\alpha-\gamma$ fibers obtained experimentally. (b) Initial
ODF after random assignment of sampled orientations to grains. (c) Final
ODF after RMC iterations. (d) Overlay of the previous three
states.](mainmatter/img/texsamp/Fig2){width="100%"}

Sampling distribution
---------------------

Sampling is generation of a set of values that follows determined
statistical descriptors. Therefore, sampling is inverse of the
statistical analysis which derive statistical descriptors for a given
set of values, also known as population. If the generated population is
large enough, both initial statistical descriptors and statistical
descriptors derived from the analysis of the generated population should
be fairly close. For example, in figure
[\[fig:SamplingNormal1D\]](#fig:SamplingNormal1D){reference-type="ref"
reference="fig:SamplingNormal1D"} a normal distribution in sampled and
the resultant points form a Gaussian distribution close to the original
distribution.

The example above is very simple and the sampled points can be obtained
even with an analytic equation from cumulative distribution function of
the normal distribution. But Nature is not always so kind and the
distributions derived from most of features in polycrystal, like grain
size distribution, may have complex shape with more than one mode (peak
with higher density of values). And in the specific case of
crystallographic orientation distributions, it is even more complex due
its higher dimension, symmetry and non-linearity of the orientation
space.

There are many techniques for sampling multidimensional distributions
but the majority are defined for Euclidean space. Some of them and a new
one adapted for orientation space will be described in the following
sections.

### Inverse Transform Sampling

This is a basic method for generating random samples from an arbitrary
probability distribution function. Let $p\left(x\right)$ be a normalized
probability distribution function defined over the interval
$\left[a,b\right]$, the method begins with the calculation of the
cumulative distribution function:

$$F\left(x\right)=\int_{a}^{x}p\left(t\right)dt$$

Since $p\left(x\right)$ is normalized, $F\left(a\right)=0$ and
$F\left(b\right)=1$. The next step is to obtain the inverse of the
cumulative distribution function, $F^{-1}\left(u\right)$ where:

$$\begin{aligned}\left\{ u\in\mathbb{R}\mid0\leq u\leq1\right\} \\
u=F\left(F^{-1}\left(u\right)\right)\\
a\leq F^{-1}\left(u\right)\leq b
\end{aligned}$$

Random values following $p\left(x\right)$ can be obtained by feeding the
inverse of the cumulative distribution function with values from an
*uniform* random distribution in the interval $\left[0,1\right]$.
Uniform random distribution is a probability distribution function with
constant probability in its interval. This methodology is illustrated in
figure [\[fig:InvSampling1D\]](#fig:InvSampling1D){reference-type="ref"
reference="fig:InvSampling1D"}.

This technique is quite general and it works with any continuous or
discrete probability functions, i.e.
$\left\{ p\left(x\right)\geq0\mid a\leq x\leq b\right\}$. If the
cumulative function or its inverse do not have an analytic description
or if them are quite difficult to be calculated, then a numerical
integration and binary search can be used instead.

The inverse transform sampling is a very well accepted method for
sampling uni-variate functions (one dimension). It can also be extended
to higher dimensions, but it demands more complex calculation steps. In
order to use it in higher dimensions it is necessary to calculate the
conditional cumulative distribution function. For instance, in the
bi-variate functions that would be written as

$$\begin{alignedat}{1}F_{XY}\left(x\mid y\right)=\dfrac{\iint p\left(x,y\right)dxdy}{\int p\left(x,y\right)dx}=\dfrac{F_{XY}\left(x,y\right)}{F_{Y}\left(y\right)}\end{alignedat}
,$$

where $F_{XY}\left(x,y\right)$ is the cumulative joint distribution and
$F_{Y}\left(y\right)$ is the cumulative marginal distribution. A random
sample $\left(x,y\right)$ can be obtained from two uniformly distributed
values, $u_{1}$ and $u_{2}$, in following manner,

$$\begin{alignedat}{1}y=F_{Y}^{-1}\left(u_{1}\right),\\
x=F_{X\mid Y}^{-1}\left(u_{2}\right).
\end{alignedat}$$

The calculation described above, for a two dimension function, can be
reasonable in terms of complexity and calculation time, but it degrades
rapidly when increasing the dimensionality. Therefore alternative
techniques are used for dimensions higher than one and some of them will
be reviewed in the following sections.

### Rejection Sampling

In order to circumvent the drawbacks of sampling probability functions
with higher dimensions, a family of sampling techniques called Monte
Carlo Sampling is used. The most basic member of this family is the so
called rejection sampling. The main concept is to draw random values
from a known probability function where a sampling technique is also
known and, preferable, fast. This function, called proposal function and
represented by $Q\left(x\right)$, has to be larger than the target
probability function $p\left(x\right)$, i.e.
$Q\left(x\right)\geq p\left(x\right)$for any $x$. Let $X$ be a proposed
point sampled from $Q\text{\ensuremath{\left(x\right)}}$, then $X$ is
accepted as a random point of $p\left(x\right)$ with probability
$p\left(x\right)/Q\left(x\right)$. If the point is rejected then a new
point is proposed. The efficiency of this algorithm is given by the
overall accepting ratio that can be derived from the ratio between the
area under the target function and the area under the proposed function,
see figure [\[fig:rejectSamp\]](#fig:rejectSamp){reference-type="ref"
reference="fig:rejectSamp"}. This implies that good accepting ratios are
obtained when both propose and target distributions are similar.

Some times, when the target distribution is too complex or there is no
*a priori* knowledge about it and, the uniform distribution can be used
as proposal distribution. Although the uniform distribution can be
efficiently sampled, it normally leads to a low efficiency in the
rejection sampling. This is exactly the case when sampling in the
orientation space.

### Monte Carlo Markov Chain Sampling

The main limitation in the rejection sampling method is the need of
similar proposal function that is also easy to sample. A subclass of
algorithms called MCMC address this limitation by eliminating the need
of a user defined proposal distribution. The proposal distribution is
implicit created using the Markov chain principle. The Markov chain is
nothing else then a sequence of states (in this case points drawn from a
proposal distribution) where the current state depends on the previous
states. This means that points using only Monte Carlo technique are
independent while points using MCMC have some degree of correlation.

A pure Monte Carlo process is memoryless because it generates an
independent sequence of points. Whereas the MCMC uses the history
(correlation) to propose points with increased likelihood of being
accepted. For instance, to estimate what is the probability that a
standard normal random variable is less than $0.5$, one could generate
thousands of independent points following a standard normal distribution
and then count up the number lower than $0.5$. That would be a Monte
Carlo process. But if it is unknown how to draw independent normal
random variables, one could use a process that proposes a point by
adding some uniform random number between $-0.25$ and $0.25$ to the
current point. If the proposed point is a good candidate then it is kept
and promoted as current point otherwise the original current point is
kept and the process starts again. That is an MCMC process. The MCMC
introduces a local effect, if the current point $X$ is a good candidate
than its immediate neighborhood may also be. A sequence of points
generated in this way is called random walk.

Many MCMC algorithms have been developed but the two most populars are
the Metropolis-Hastings and Gibbs. Most of them are designed to work the
Euclidian space $\mathbb{R}^{n}$ which may cause problems when used for
sampling ODF's.

New Method of Sampling in the Orientation Space
-----------------------------------------------

A new method of sampling distribution functions in the orientation space
is proposed here. The new method can efficiently sample arbitrary
distribution functions in the orientation space, works with any
representation of rotation (e.g. matrix or quaternion) and do not need
any specific data structure like regular grids. It is a modification of
a MCMC technique called slice sampling inspired by another MCMC
technique called hit and run sampling. Both techniques and the new
method are explained bellow.

### Hit and Run

Hit and Run sampling was initially developed for creating uniform random
distributions in arbitrary closed domain. For instance, place random
points inside a pentagon or a star. Lately, it was modified for sampling
arbitrary probability functions. The original Hit and Run is described
in algorithm
[\[alg:basic-hit=000026run\]](#alg:basic-hit=000026run){reference-type="ref"
reference="alg:basic-hit=000026run"}. Note that the random direction $D$
and the the domain of the probability function
$p\left(x\right):\mathbb{R}^{n}\rightarrow\mathbb{R}^{+}$ have the same
dimension $\mathbb{R}^{n}$, e.g. two dimensions shapes have random
directions as unit vectors from a circle. A visual description of the
algorithm of 2D shapes is illustrated in figure
[\[fig:hit=000026run\]](#fig:hit=000026run){reference-type="ref"
reference="fig:hit=000026run"}.

The analogy is given by a snipper who shots at a random direction and
distance, if he hits the target then he moves to that position otherwise
he tries again. The sequence of points clearly generates a Markov chain.
The criteria of acceptance in the original design (uniform distribution
in closed domains) is trivial. If $D$ is the closed domain then $X$ is
only accepted if $X$ is inside the $D$. The modified version, for
sampling arbitrary probability functions, has a more complex sequence
that will not explained here. The advantage of this algorithm is the
ability of explorer the entire space and sample isolated regions. This
is important in the case of ODF's because them may have multiple
isolated peaks. The disadvantage is the need of tuning the range (the
maximum distance) where $k$ is sampled from. If $k$ is to small then the
output sequence is strong correlated, the random walk is quite local and
space might not be well explored. Whereas if $k$ is too large then
efficiency is quite low.

### Slice sampling

Slice sampling is also a very popular MCMC sampling technique. It is
based on the principle that any region under any distribution function
have uniform density and, therefore, can be uniformly sampled, such a
region is a slice of the probability function at certain intensity.
Figure
[\[fig:sliceprinciple\]](#fig:sliceprinciple){reference-type="ref"
reference="fig:sliceprinciple"} illustrate this principle for a
probability function $p\left(x\right)$ in one dimension.

In order to sample the target function the slice sampler randomly
alternate the slicing level covering the entire distribution function.
Algorithm [\[alg:basic-slice\]](#alg:basic-slice){reference-type="ref"
reference="alg:basic-slice"} describes a single iteration step to obtain
one sampled point. There are two crucial parts in this algorithm. The
first one, defined in the line
[\[basic-slice:getslice\]](#basic-slice:getslice){reference-type="ref"
reference="basic-slice:getslice"}, has to find the slice at given level
$y$. A slice is nothing else then the region enclosed by the level
set[^1], for instance, a slice in one dimension function is a set of
discrete points, in two dimensions it is a isoline (or contour line) and
in three dimensions it is a isosurface. Figure
[\[fig:slicebasic1D\]](#fig:slicebasic1D){reference-type="ref"
reference="fig:slicebasic1D"} illustrates the slice sampling for one
dimensional probability function.

References {#references .unnumbered}
----------

mainmatter/bib/texsamp

[^1]: Mathematically, level set is defined as the set of points where a
    function has same predefined value. It has the form
    $L_{y}\left(p\right)=\left\{ x\mid p\left(x\right)=y\right\}$.
