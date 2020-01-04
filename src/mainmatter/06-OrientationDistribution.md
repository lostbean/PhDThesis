Orientation distribution {#ch:Orientation}
========================

Douglas Hofstadter - Gödel, Escher, Bach: An Eternal Golden Braid - 1999

ome quick intro here\...

Introduction
------------

Crystallographic Orientation
----------------------------

Crystallographic orientations, in polycrystalline materials, are
represented by a rotation between the crystal lattice and the chosen ERS
that is macroscopically fixed to the material. The ERS is normally
defined according to the conformation process used to obtain the
polycrystalline material. For example, when processed by rolling mills,
the RD (direction along the plate displacement), the ND (direction
normal to the plate's surface) and the TD (direction along the axis of
the rolls) are used as a reference system. The ERS may change according
to the analysis that has to be done or the processing steps involved.
For instance, spiral pipes use rolled plates with an angular rotation
from the pipe's axis, and therefore, a new ERS may be defined with the
pipe's axial direction as X-axis, instead.

$$K_{C}=gK_{ERS}\label{eq:ori_def}$$

The crystallographic orientation $g$ is defined as the rotation applied
to the ERS ($K_{ERS}$) that brings into coincide with the crystal
lattice reference system ($K_{C}$), see equation
[\[eq:ori\_def\]](#eq:ori_def){reference-type="ref"
reference="eq:ori_def"}. This can also be interpreted as a passive
transformation where the basis forming the ERS is transformed into a new
basis corresponding to the crystal reference system. The active
rotation, on the contrary, rotates a vector and the resulting
transformation is still represented on the original basis. Figure
[\[fig:diff-act-pas-rot\]](#fig:diff-act-pas-rot){reference-type="ref"
reference="fig:diff-act-pas-rot"} shows the difference between active
and passive rotation. The active rotation applies the transformation
directly to a vector $v_{A}$ represented in the basis $A$, resulting in
a vector $v'_{A}$ that is still represented in the basis $A$.
Conversely, the passive rotation applies the transformation to the basis
$A$ and the result is the rotated basis $B$. In this case, if the vector
$v'$ is applied in the basis $B$ then the relation $v_{A}=v'_{B}$ is
valid. Note that the rotations, on both transformations, have the same
rotation axis but opposite rotation angles.

Rotations, and therefore orientations, are fixed point transformations
i.e. they are a linear transformation where at least one point remains
invariant, so if $g:\mathbb{R}^{n}\rightarrow\mathbb{R}^{n}$ is a
rotation and $u\in\mathbb{R}^{n}$ then $u$ is called fixed point or axis
of rotation if and only if $g\left(u\right)=u$. Rotations also form a
mathematical group called *rotation group* that is a non-abelian group
under composition. Let $G$ be the set of linear transformations
$g:\mathbb{R}^{n}\rightarrow\mathbb{R}^{n}$ forming the rotation group
and $\left(\bullet\right)$ be the binary operator, then the following
properties are valid (a graphic illustration of the properties is shown
on Figure [\[fig:rot-group\]](#fig:rot-group){reference-type="ref"
reference="fig:rot-group"}):

-   *Closure:* If $g_{1}$and $g_{2}$ are two arbitrary rotation
    ($g_{1}\in G$ and $g_{2}\in G$) then there is a functional
    composition operator $\left(\bullet\right):G\times G\rightarrow G$
    which implies that $g_{1}\bullet g_{2}\in G$. It means that the
    combination of any rotations is always a valid rotation.

-   *Associativity:* For $\left\{ g_{1},g_{2},g_{3}\right\} \in G$ then
    $g_{1}\bullet\left(g_{2}\bullet g_{3}\right)=\left(g_{1}\bullet g_{2}\right)\bullet g_{3}$

-   *Identity element:* There an identity element $id$ that composed
    with any $g\in G$ changes nothing, so the equation
    $id\bullet g=g\bullet id=g$ is valid.

-   *Inverse element:* For any $g\in G$, there is an element $g^{-1}$
    such that the equation $g\bullet g^{-1}=g^{-1}\bullet g=id$ holds
    valid.

-   *Non-commutativity:* If $g_{1}$ and $g_{2}$ are two arbitrary
    rotation then the equation $g_{1}\bullet g_{2}=g_{1}\bullet g_{2}$
    may *not* be valid.

Symmetry
--------

$$\begin{alignedat}{1}\left(O_{m}g_{a}\right)\left(O_{n}g_{b}\right)^{-1}=I\\
O_{m}g_{a}O_{n}=g_{b}\\
\left(O_{m}g_{a}\right)O_{n}=g_{b}
\end{alignedat}$$

Orientation representation
--------------------------

There many ways to represent physical rotations ($\mathbb{R}^{3}$). The
most important ones are described bellow, as well as their
advantages/disadvantages and their conversion to quaternions.

### Matrix representation

Rotation matrix is a square matrix ($n\times n$) with real values
entries ($a_{ij}\in\mathbb{R}^{n}$) used to perform rotation in the
Euclidean space. Moreover, rotation matrices are orthogonal matrices
with determinate equal $1$. These two features can be used as a
validation test, for instance, if $R$ is a real square matrix then $R$
can be a valid rotation matrix only if,

$$RR^{T}=I,\det R=1.$$

Rotation matrices form a group when using matrix multiplication as
function composition. In this case, the group properties are represented
as the following:

-   *Closure:* If $R_{1}$and $R_{2}$ are two arbitrary rotation matrices
    ($R_{1}\in\mathbb{M}\left(n,n\right)$ and
    $R_{2}\in\mathbb{M}\left(n,n\right)$) and matrix multiplication is
    the functional composition then $R_{1}R_{2}=R_{3}$ and
    $R_{3}\in\mathbb{M}\left(n,n\right)$. Where $R_{3}$ is the rotation
    $R_{2}$ followed by the rotation $R_{1}$.

-   *Associativity:* For
    $\left\{ R_{1},R_{2},R_{3}\right\} \in\mathbb{M}\left(n,n\right)$
    then $R_{1}\left(R_{2}R_{3}\right)=\left(R_{1}R_{2}\right)R_{3}$

-   *Identity element:* $id=I$ is the identity matrix (when all the
    elements are $0$ but the elements of the diagonal which are $1$).
    Therefore $IR=RI=R$ is valid.

-   *Inverse element:* Due the orthogonality of rotation matrices, the
    relation $RR^{T}=I=RR^{-1}$ is valid. Therefore the inverse rotation
    is the transpose matrix, $R^{-1}=R^{T}$.

Simple rotation around the Cartesian ($\mathbb{R}^{3}$) axes are given
as an example in the equation
[\[eq:simpleRotMat\]](#eq:simpleRotMat){reference-type="ref"
reference="eq:simpleRotMat"}, where $\phi$ is the angle of rotation.

$$\begin{alignedat}{1}R_{x}\left(\phi\right)=\left[\begin{array}{ccc}
1 & 0 & 0\\
0 & \cos\phi & \sin\phi\\
0 & -\sin\phi & \cos\phi
\end{array}\right]\\
R_{y}\left(\phi\right)=\left[\begin{array}{ccc}
\cos\phi & 0 & -\sin\phi\\
0 & 1 & 0\\
\sin\phi & 0 & \cos\phi
\end{array}\right]\\
R_{z}\left(\phi\right)=\left[\begin{array}{ccc}
\cos\phi & \sin\phi & 0\\
-\sin\phi & \cos\phi & 0\\
0 & 0 & 1
\end{array}\right]
\end{alignedat}
\label{eq:simpleRotMat}$$

When rotation matrices are used to represent orientations, its rows and
columns have special meaning. The rows correspond to the Cartesian
directions of the crystal basis represented in ERS whereas the columns
correspond to the Cartesian directions of the ERS represented in the
crystal basis. For instance, the first row is the direction of the
crystallographic $x$ axis in macroscopic frame and the last row is the
normal direction to the surface's plate observed from crystallographic
perspective, as indicated in Figure
[\[fig:ori\_mat\_info\]](#fig:ori_mat_info){reference-type="ref"
reference="fig:ori_mat_info"}.

The second and last column (RD and ND in Figure
[\[fig:ori\_mat\_info\]](#fig:ori_mat_info){reference-type="ref"
reference="fig:ori_mat_info"}) can also form a simplified orientation
representation called Miller indices. Miller indices are composed of two
directions: $\left(hkl\right)$ the crystallographic direction of the
plane parallel to the surface's plate i.e.
$\left(hkl\right)\parallel ND$, and $\left[uvw\right]$ the
crystallographic direction contained in $\left(hkl\right)$ plane and
parallel to the rolling direction i.e. $\left[uvw\right]\parallel RD$
and $\left[uvw\right]\mathrel\bot RD$. The remaining column (TD) is
easily calculated from the cross product between $\left(hkl\right)$ and
$\left[uvw\right]$. Although there is an identity element for Miller
indices ($id$=$\left(001\right)\left[100\right]$), no composition
function nor inversion function exist. Therefore, Miller indices do not
form a proper orientation representation.

Although rotation matrices are a simple orientation representation due
its simplified operations for composition, identity and inversion, there
some drawbacks. In the case of rotation in $\mathbb{R}^{3}$, there are 9
elements to store for a 3 degrees freedom transformation, which means
that 6 elements are implicitly redundant. Moreover, the space of
rotation is a 9-dimension space which is practically impossible to
visualize. And the rotation composition requires 27 multiplications and
18 additions.

### Euler angles

Euler angles represent arbitrary rotations by composing a sequence of
three rotations around three distinct axes. Different conventions or
sequences of rotation exist with the Bunge convention being the most
common in the field of crystallographic texture analysis. Bunge
convention corresponds to the $ZXZ$ sequence where the first rotation is
around the $Z$ axis with $\varphi_{1}$ angle followed with a rotation
around $X'$ ($X$ axis after the first rotation) with $\Phi$ angle and
the last rotation is on $Z''$ with $\varphi_{2}$ angle. This sequence
can be visualized in figure
[\[fig:EulerZXZ\]](#fig:EulerZXZ){reference-type="ref"
reference="fig:EulerZXZ"}. The Euler angles can represented in the
matrix from using the rotations in the equation
[\[eq:simpleRotMat\]](#eq:simpleRotMat){reference-type="ref"
reference="eq:simpleRotMat"}:

$$E\left(\varphi_{1},\Phi,\varphi_{2}\right)=R_{z}\left(\varphi_{2}\right)R_{x}\left(\Phi\right)R_{z}\left(\varphi_{1}\right)\label{eq:eulerMatrix}$$

By splitting the rotation around the main axes, the Euler angles allow a
more compact representation than matrices using only three angles that
is the minimum number of parameters necessary to represent a solid
rotation with three degrees of freedom.

Although widely used in some scientific communities, the Euler angles do
not have well know and clear composition and inverse functions. And the
most critical drawback is the presence of singularities, it means that
there are some rotation with more than one representation in the Euler
space. For instance, the rotation $\left(45,0,0\right)$ is exactly the
same as the rotation $\left(0,0,45\right)$ or $\left(20,0,25\right)$.

### Axis-Angle

Axis-Angle rotation is represented by a normalized direction $\vec{v}$
and a rotation angle $\theta$ around $\vec{v}$ following the right-hand
grip rule. Like the Euler representation, it has a compact
representation $\mathbb{SO2\times\mathbb{R}}$ but it has trivial inverse
function. The use of an unique rotation axis in combination with an
rotation angle makes this representation very human readable, specially
for representing difference between rotations (i.e. misorientations at
the grain boundaries). One should be aware the singularity that exists
when $\theta=0$, in this case the representation of the normalized
direction is meaningless and any arbitrary$\vec{v}$ will represent the
identity rotation.

The inverse rotation can be easily obtained by either reversing the
direction vector or by negating the rotation angle. This also implies
that some constraint has to be applied to the domain boundaries of the
axis-angle space in order to avoid multiple equivalent representations.
For instance,
$\left(\vec{v},\theta\right)=\left(\vec{-v},2\pi-\theta\right)$ if
$0\leq\theta<2\pi$. The solution is restrict $\theta$ in the range
$0\leq\theta<\pi$ or $-\nicefrac{\pi}{2}\leq\theta<\nicefrac{\pi}{2}$ or
even, although less desirable, restrict the $\vec{v}$ to be contained in
half $\mathbb{SO2}$.

### Frank-Rodriges

The Frank-Rodriges has, as Euler angles, the most possible compact
representation of rotations.

### Quaternion

Orientation distribution {#orientation-distribution}
------------------------

### Bingham distribution

#### Normalization constant

The *generalized hypergeometric function*[]{lang="en-US"}[^1] is defined
as

$$_{p}F_{q}\left(a_{1},\ldots,a_{p};b_{1},\ldots,b_{q};z\right)=\sum_{j=0}^{\infty}{\textstyle \frac{\left(a_{1}\right)_{j}\ldots\left(a_{p}\right)_{j}}{\left(b_{1}\right)_{j}\ldots\left(b_{q}\right)_{j}}\frac{z^{j}}{j!}}\label{eq:generalized_hypergeometric}$$

Where $a_{1},\ldots,a_{p},b_{1},\ldots,b_{q},z\mathbb{\in C}$ and the
*Pochhammer symbol* $\left(x\right)_{n}$represents the rising factorial
as the following

$$(x)_{n}=x(x+1)(x+2)\cdots(x+n-1)$$

The *confluent hypergeometric function*, also known as *Kummer's
function*, is a specific case of the *generalized hypergeometric
function* where $p=q=1$, therefore the
[\[generalized\_hypergeometric\]](#generalized_hypergeometric){reference-type="eqref"
reference="generalized_hypergeometric"} becomes

$$_{1}F_{1}=\sum_{j=0}^{\infty}{\textstyle \frac{\left(a\right)_{j}}{\left(b\right)_{j}}\frac{z^{j}}{j!}}$$

Factorials are time consuming to calculate but no precomputed table can
be used in this case because *Pochhammer symbol* involves real numbers.
Alternatively, *Pochhammer symbol* for rising factorials can be
calculated using the gamma function $\Gamma$

$$(x)_{n}=\frac{\Gamma(x+n)}{\Gamma(x)}$$

#### Multivariate confluent hypergeometric function

The multi-dimension confluent hypergeometric function is given by one of
the Appell series functions

$$F_{1}(0,a_{1},\ldots,a_{n},b,x_{1},\ldots,x_{n})=\sum_{k_{1},\ldots,k_{n}}{\textstyle \frac{\left(a_{1}\right)_{k_{1}}\ldots\left(a_{n}\right)_{k_{n}}}{\left(b\right)_{k_{1}+\ldots+k_{n}}}\frac{x_{1}^{j}\ldots x_{n}^{j}}{k_{1}!\ldots k_{n}!}}$$

And its partial derivatives regarding $x$ are given by

$$\frac{\partial}{\partial x}F_{1}(a,b_{1},b_{2},c;x,y)=\frac{ab_{1}}{c}F_{1}(a+1,b_{1}+1,b_{2},c+1;x,y)$$

### Hyper Spherical Harmonics

![Active and passive rotations on the quaternion space using HSH. The original orientations are:
$s_{1}=SO^{3}\:(\nicefrac{\pi}{2},\nicefrac{\pi}{2},\nicefrac{\pi}{2})$ and
$s_{2}=SO^{3}\:(\nicefrac{\pi}{4},\nicefrac{\pi}{4},0)$. Both active and passive rotation are
$SO^{3}\:(\nicefrac{-\pi}{2},\nicefrac{\pi}{2},0)$.](/img/oridist/HSH active and passive rotations.png){width="70%"}

![Enforcing symmetry on HSH.](/img/oridist/HSH symmetryzation.png){width="70%"}

[^1]: Uno due il nomine integre, lo tote tempore anglo-romanic per, ma sed practic philologos historiettas.
