Delunay Triangulation
=====================

$SD\left(D\right)=[m\left(D\right)-m_{<A,B,C>}]\cdot\dfrac{\left(a-b\right)\times\left(a-c\right)}{\parallel\left(a-b\right)\times\left(a-c\right)\parallel}$

$\left[\begin{array}{cccc}
1 & a_{x} & a_{y} & a_{z}\\
1 & b_{x} & b_{y} & b_{z}\\
1 & c_{x} & c_{y} & c_{z}\\
1 & d_{x} & d_{y} & d_{z}
\end{array}\right]\left[\begin{array}{c}
\parallel m\left(D\right)\parallel^{2}-w_{m}\\
-2m_{x}\left(D\right)\\
-2m_{y}\left(D\right)\\
-2m_{z}\left(D\right)
\end{array}\right]=-\left[\begin{array}{c}
\parallel a\parallel^{2}-w_{a}\\
\parallel b\parallel^{2}-w_{b}\\
\parallel c\parallel^{2}-w_{c}\\
\parallel d\parallel^{2}-w_{d}
\end{array}\right]$

$\left[\begin{array}{ccc}
b_{x}-a_{x} & b_{y}-a_{x} & b_{z}-a_{x}\\
c_{x}-a_{x} & c_{y}-a_{x} & c_{z}-a_{x}\\
d_{x}-a_{x} & d_{y}-a_{x} & d_{z}-a_{x}
\end{array}\right]\left[\begin{array}{c}
-2m_{x}\left(D\right)\\
-2m_{y}\left(D\right)\\
-2m_{z}\left(D\right)
\end{array}\right]=-\left[\begin{array}{c}
\parallel b\parallel^{2}-\parallel a\parallel^{2}-w_{b}+w_{a}\\
\parallel c\parallel^{2}-\parallel a\parallel^{2}-w_{c}+w_{a}\\
\parallel d\parallel^{2}-\parallel a\parallel^{2}-w_{d}+w_{a}
\end{array}\right]$

$M^{T}[-2m\left(D\right)]=-\alpha$

where

$M=\left[\begin{array}{ccc}
b_{x}-a_{x} & c_{x}-a_{x} & d_{x}-a_{x}\\
b_{y}-a_{y} & c_{y}-a_{y} & d_{y}-a_{y}\\
b_{z}-a_{z} & c_{z}-a_{z} & d_{z}-a_{z}
\end{array}\right]$ $\alpha=\left[\begin{array}{c}
\parallel b\parallel^{2}-\parallel a\parallel^{2}-w_{b}+w_{a}\\
\parallel c\parallel^{2}-\parallel a\parallel^{2}-w_{c}+w_{a}\\
\parallel d\parallel^{2}-\parallel a\parallel^{2}-w_{d}+w_{a}
\end{array}\right]$

let $\left(Q,R\right)=qrDecomp\left(M\right)$

$M^{T}[-2m\left(D\right)]=R^{T}[-2Q^{T}m\left(D\right)]=R^{T}\mu$

$m\left(D\right)=-\frac{1}{2}Q\mu=-\frac{1}{2}\left(q_{1}\mu_{x}+q_{2}\mu_{y}+q_{3}\mu_{z}\right)$

where $\mu$ can be easily solved since it is a lower triangular matrix

$R^{T}\mu=-\alpha$

$\mu_{x}=\dfrac{-\alpha_{x}}{r_{11}}$

$\mu_{y}=\dfrac{-\alpha_{x}-\mu_{x}r_{12}}{r_{22}}$

$\mu_{z}=\dfrac{-\alpha_{z}-\mu_{x}r_{13}-\mu_{y}r_{23}}{r_{33}}$
