Sources:

  [1] Lecture Notes: Programming Quantum Computers
        RWTH Aachen University
          2022
  [2] Modeling Quantum Computing in Haskell
        Indiana University
          2003
  [3] Quantum Computing as a High School Module
        Illinois Mathematics and Science Academy
          2020

Why AssocList is used instead of Map

  The original solution [2] used Map to represent quantum vectors. However, 
  Map in Haskell is based on balanced binary trees, specifically on the 
  Adams' balanced trees algorithm, which is a variation of AVL trees. 
  AVL trees maintain balance with every insertion or deletion, leading 
  to immediate strict evaluation of keys. This provides logarithmic 
  complexity for search and insertion operations but violates the laziness 
  of the program, as keys must be evaluated during each insertion to maintain 
  the balanced state of the tree.

Quantum notes

  Superposition - a combination of several possible states:

    ∣ψ⟩ = c1​∣φ1〉 + c2​∣φ2〉 + ⋯ + cn​∣φn〉

  where probability amplitudes (c1, c2, ..., cn) should follow the normalization rule:

    |c1|^2 + |c2|^2 + ... + |cn|^2 = 1

  because a probability (P) can be found like the following:

    P(i) = ∣ci​∣^2

  "Why is it squared? The short answer is that it gives the correct experimental
  predictions for this choice of representation." [3]

  An operator UU is unitary if it satisfies the following condition:

    U†U=I

  where † - the dagger, is often used in mathematics and quantum mechanics to denote
  the conjugate transpose of a matrix or operator.

  This means that applying UU to any vector should preserve the vector's norm, and
  the operator should not distort the overall geometry of the vector space.

Quantum data

  In Quantum Computing the following is accepted as a boolean type:

    α|False〉 + β|True〉

  where α and β are complex numbers representing probability
  amplitudes, each constructor c is interpreted as a unit vector
  |c〉, and + is vector addition.

  Entanglement in Quantum Computing is represented in this module as pairs of
  quantum vectors. When using the tensor product operation (&*), the resulting
  quantum vector reflects the entangled state of the combined systems, demonstrating 
  the correlations between their states.
