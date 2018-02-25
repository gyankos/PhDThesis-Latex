--- 
title: A new Nested Graph Model for Data Integration
author: Giacomo Bergami
date: 2/10/2017
autosize: false
---

Introduction
========================================================

- Graph data are a widely adopted.
- Currently, most of the No-SQL data representation is semistructured (JSON, XML).
- Graph operations allowing to integrate property graphs and semi-structured (**\alert{nested}**) data are not provided.
	- An **\alert{intermediate data representation}** allowing to represent both structured, semi-structured (nested) and non-structured data is missing.
	- A query language for this generalized data model is required. It should be able to sketch the data integration task and express both semistructured and graph queries.





Related Works: Data Integration
========================================================

- *Schema Oriented*, uniform data representation
	- A schema can be always extracted if missing (**\alert{generalization}**). 
	- **\alert{Schema alignment}** allows to find data similarities.
	- All the data source schemas shall be aligned to the final **\alert{hub schema}**.

- *Query Oriented*
	- The schema alignment can be represented as a query rewriting.
	- We can perform each query separately on the original sources first and then integrate the intermediate results (**\alert{LAV}**) or translate and align the data first and then perform the query (**\alert{GAV}**).
	- In both scenarios, part of the query has to be performed on top of the intermediate representation.





Contributions
========================================================

- An operation allowing to integrate different graphs was missing. Definition of the first class of binary operators for property graphs: **\alert{graph joins}**.
	- Its definition pointed out the inefficiency of current graph query plans and data structures.
- Definition of a data model allowing to provide data integration between graph, semistructured and structured data.
	- **\alert{Future works}**





Graph Joins: A query example
========================================================
Consider an on-line service such as \hl{ResearchGate}
where researchers can {follow} each others' work, and a \hl{citation graph}.
	\hl{Return the paper graph} where **\alert{a paper cites another one iff. the \hlcyan{first
			author  of the first paper \textit{follows} the first author of the second}}**.

* \hl{Binary operator returning just one graph}.
* \hlcyan{Vertex conditions are ($\theta$-)join conditions}.
* **\alert{A way to combine the edges is determined}**.





Graph Joins: Relational Query Plan
========================================================
![A relational representation of the graph, where the vertices are $\theta$-joined.](imgs/07join/joinexamples2/fig/reljoins.pdf)





Graph Joins: The query result
========================================================
![An example of a grpah join (conjunctive) query.](images/presentation/01conjexample.pdf)





Graph Joins: Goals
========================================================
1. The **\alert{data model}** must *enhance the serialization* of both operands and 
graph result.
2. The **\alert{join definition}** must be flexible enough to 
support further extensions (modularity, compositionality and properties preserving).
3. The **\alert{physical model}** must allow a *quick access* to the data structures.





Graph Joins: Physical Model
========================================================
![](images/presentation/02blgorithm.pdf)   &nbsp;&nbsp;&nbsp;&nbsp;  &nbsp;&nbsp;&nbsp;&nbsp;  &nbsp;&nbsp;&nbsp;&nbsp;  &nbsp;&nbsp;&nbsp;&nbsp;  &nbsp;&nbsp;&nbsp;&nbsp;  &nbsp;&nbsp;&nbsp;&nbsp;  ![](images/presentation/02clgorithm.pdf)

![Representing Graph Operators and Output](images/presentation/02dlgorithm.pdf)





Graph Joins: Benchmarks
========================================================
![Conjunctive join execution on well known graph databases. Similar result were provided on graph database libraries.](images/presentation/03Result.png)





Graph Joins: Limitations (1)
========================================================
![A data integration scenario, where the schema is already aligned](imgs/07join/discarded_leipzig/01_example.pdf)





Graph Joins: Limitations (2)
========================================================
![A data integration scenario, where the schema is already aligned](imgs/07join/discarded_leipzig/02_result.pdf)





Graph Joins: Limitations (3)
========================================================
* Suppose to generalize the graph join by extending the $\theta$-join ($\join_\theta$) to a full $\theta$-join (${\tiny \textifsym{d|><|d}}_\theta$)  over the vertices.
	* This is possible within the previous definition.
* Suppose to use as a $\theta$ some edges that remark the similarity values among the nodes.
	* After the pairwise application of the join, the $\theta$ edges must be rewritten.
* In some data cleaning scenarios, we want to preserve the original non-aggregated information, while providing an integrated view as in the previous picture.
	* **\alert{A new data model is required}**, generalizing statecharts and hypernodes.




Generalized Semistructured Model and Nested Graphs
========================================================
$$GSM=(o,O,\ell,\xi,\phi)$$

* $o$ is the **reference object**, designing the root of the GSM.
* $\phi$ is the object id containment function for each expression $e$ ($\phi(o',e)$).
* $O$ is the supset of all the objects contained by $o$ ($\bigcup_{e}\phi(o',e)$)
* $\ell$ is the function associating to each object in $O$ a list of labels.
* $\xi$ is the function associating to each object in $O$ a list of expressions.





GSM, generalizing semistructured data containment
========================================================
![Allowing multiple containments of the same object.](imgs/08data/03Overlapping.pdf){ width=30% }

![Allowing multiple nesting and nestings with recursions.](imgs/08data/02RecursiveNesting.pdf){ width=30% }






GSM, representing (nested) graphs
========================================================
![Representing a nested graph with two vertices and one edge.](imgs/08data/0405Full.pdf){ width=70% }





GSM, representing semistructured documents
========================================================
![Another graph representation from a JSON document.](imgs/03introduction/alignments/02MatchingKeyword.pdf)





GSM, aligning graphs with semistructured schemas (1)
========================================================
![Schema alignment, using the usual techniques.](imgs/03introduction/alignments/04aAlignment.pdf)





GSM, aligning graphs with semistructured schemas (2)
========================================================
![Schema alignment, using the nesting information refinement.](imgs/03introduction/alignments/04bAlignment.pdf)




GSM, aligning graphs with semistructured schemas (3)
========================================================
The previous data alignment process provides an interesting perspective for graph grammars:

* The schema (*on the right*) extracted from the original data defines the **\alert{matching graph}**. 
	* Given that the schema was extracted from the original data, we already know the result of such match.
* The hub schema (*on the left*) defines how the data on the right has to be rewritten **\alert{transformation graph}**, using the edges as transformations.

Hereby, a generalization of the graph grammars is required. The operations required to do the matching and transformations are non trivial. Thefore, an algebra expressing such operations is required.






paNGRAm Algebra
========================================================
\centering
\begin{minipage}{\textwidth}
\tiny
\begin{tabular}{cccc}
\toprule
\textbf{paNGRAm}   & \textit{COA Algebra} & \textit{3W Algebra}   & \textit{Graph Languages}\\
\midrule
\multirow{2}{*}{\alert{Selection}, $\sigma_P$} &  Selection, $\sigma_P$ &  \multirow{2}{*}{Selection, $\sigma_P$}  & Graph Traversal, $P$\\
					& Projection, $\pi_P$  & 	& \\
\midrule
\multirow{2}{*}{\alert{Map}, $\mu_{\ell',\xi',\phi'}$} & Embedding, $\varepsilon_\phi$ & Calc, $Calc_f$   & Transform,\\
					& Projection, $\pi_\phi$    & 	  & Pattern Matching\\
\midrule
{\alert{Creation}, $\kappa^\omega_{L,E}$} & Embedding new elements & Regionize, $\kappa$   & Transform,\\
\midrule
\alert{Fold}, $\textup{fold}_{S,f}$ & Expression using $\varsigma$ & Loop operator, $\lambda$  & Reduce (GrAlA) \\
\midrule
{\alert{Disjoint Union}}, $\sqcup$ & $+\mu =$  Set operations & $+\mu =$ Set operations  & Graph Binary Operators \\
\bottomrule
\end{tabular}
\end{minipage}

* An operation for creating new elements is required because this algebra manipulates ids (already existing objects).
* All the other operations (such as graph unnesting $\nu$,  splicing $\varsigma$, graph joins $\join$) are expressible through a composition of operators.




Future Works
========================================================
* In order to acomplish the data integration scenario, the alignment between the source schemas and the hub schema shall be defined.
* Algebraic expressions reproducing the single steps shall be defined.
* Check if the whole algorithmic plan can be implemented more efficiently than the single operators.
