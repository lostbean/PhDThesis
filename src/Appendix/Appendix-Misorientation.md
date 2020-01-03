Misorientation definition
=========================

Rotations can be seen as functions that map one vector space into an
other.

$$g\,:\,s\rightarrow t$$

Rotations can be composed using function composition.

$$g_{sv}=g_{tv}\cdot g_{st}$$

where $g_{sv}\,:\,s\rightarrow v$; $g_{tv}\,:\,t\rightarrow v$;
$g_{st}\,:\,s\rightarrow t$.

Let $g_{A}$ and $g_{B}$ be two rotations that bring the reference frame
to the frames A and B, respectively. And $\Delta g_{AB}$ the rotation
that brings the frame A to B.

$$g_{B}=\Delta g_{AB}\cdot g_{A}$$

$$g_{B}\cdot g_{A}^{-1}=\Delta g_{AB}\cdot g_{A}\cdot g_{A}^{-1}$$

$$g_{B}\cdot g_{A}^{-1}=\Delta g_{AB}$$

It also exist an inverse rotation that brings B to A, $\Delta g_{BA}$.
In such a case,

$$g_{A}=\Delta g_{BA}\cdot g_{B}$$

$$g_{A}\cdot g_{B}^{-1}=\Delta g_{BA}\cdot g_{B}\cdot g_{B}^{-1}$$

$$g_{A}\cdot g_{B}^{-1}=\Delta g_{BA}$$

Rotation compositions are not commutative, so

$$g_{B}\cdot g_{A}^{-1}\neq g_{A}^{-1}\cdot g_{B}$$

If we enforce the first element to be the inverse

$$g_{A}^{-1}\cdot g_{B}=g_{A}^{-1}\cdot\Delta g_{AB}\cdot g_{A}$$

$$\begin{aligned}
g_{A}^{-1}\cdot g_{B} & = & \overline{\Delta g_{AB}}\end{aligned}$$

then the result will be the conjugate of the misorientation, which, if
coverted to axis-rotation pair, will result the same rotation $\omega$
but with different axis from $\Delta g_{AB}$.

> Definition 2.12 (conjugate elements): If $a$ and $b$ are elements of
> $G$, then $a$ and $b$ are conjugate elements in $G$ if there exists an
> element $g$ such that $b=gag^{-1}$ . If $a$ and $b$ are conjugate, we
> write $a\backsim b$.
>
> This notion of conjugation is of great importance to physics
> applications, since conjugation is essentially a similarity
> transformation. For example, in the rotation group, if $a$ denotes a
> rotation by $30^{\circ}$ about the $x$-axis, then any rotation by
> $30^{\circ}$ is conjugate to a since rotations about different axes
> are related by a similarity transformation.[^1]

On the other hand, the relation between both misorientations is given by

$$g_{B}\cdot g_{B}^{-1}=\Delta g_{AB}\cdot g_{A}\cdot g_{B}^{-1}$$

$$I=\Delta g_{AB}\cdot g_{A}\cdot g_{B}^{-1}=\Delta g_{AB}\cdot\Delta g_{BA}$$

$$\Delta g_{AB}=(\Delta g_{BA})^{-1}$$

[^1]: 2 Subgroups and conjugate elements.
    http://pauli.physics.lsa.umich.edu/p452/gt02.pdf\
    see, Group Theoretical Methods and Applications to Molecules and
    Crystals, page 62
