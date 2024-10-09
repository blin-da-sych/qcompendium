module Measurement where

import           Basis                  (Basis (basis), PA, QV, amplitude,
                                         qVector)
import           Data.Bool              (bool)
import           Data.Complex           (Complex (..), magnitude)
import           Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import           Data.Map               (elems, singleton)
import           Prelude                hiding ((*>))
import           System.Random.Stateful (Random (randomR), getStdRandom)

-- | 'normalize' takes a quantum vector and returns a normalized version of it.
--   If the vector is already normalized, it scales the amplitudes accordingly.
--   If the vector's norm is zero, it throws an error.
--
--   __Parameters:__
--
--   - A quantum vector of type @QV a@, where 'a' is the basis vector type.
--
--   __Returns:__
--
--   - A normalized quantum vector of the same type.
--
--   __Throws:__
--
--   - An 'error' with the message *"NaN encountered in normalization."*
--     if the vector's norm is zero.
normalize :: Basis a => QV a -> QV a
normalize v =
  let n = norm v
   in bool
        ((1 / n :+ 0) *> v)
        (error "NaN encountered in normalization.")
        (n == 0)

-- | 'norm' computes the norm of a given quantum vector by summing the squares
--   of the magnitudes of its probability amplitudes. This is useful for
--   determining whether a vector is normalized.
--
--   __Parameters:__
--
--   - A quantum vector of type @QV a@.
--
--   __Returns:__
--
--   - A 'Double' representing the norm of the quantum vector.
norm :: Basis a => QV a -> Double
norm v =
  let probs = map (square . magnitude) (elems v)
      square = (^ (2 :: Int))
   in sqrt (sum probs)

-- | The operator '*>' scales a quantum vector by a given probability amplitude.
--   It multiplies each amplitude in the quantum vector by the given complex number.
--
--   __Parameters:__
--
--   - A complex number representing the probability amplitude.
--   - A quantum vector of type @QV a@.
--
--   __Returns:__
--
--   - A scaled quantum vector.
(*>) :: Basis a => PA -> QV a -> QV a
c *> v = fmap (c *) v

-- | 'QR' is a newtype that wraps an 'IORef' containing a quantum vector of type 'QV a'.
--   It models a quantum register, where quantum states are stored and can be observed.
--   This allows side effects during quantum operations, such as measurement and state
--   updates.
--
--   __Parameters:__
--
--   - @a@: The type of the basis vector for the quantum vector stored in the register.
--
--   __Constructors:__
--
--   - 'QR': Takes an 'IORef' containing a 'QV a', representing a quantum register.
--
--   The use of 'IORef' ensures that the quantum vector can be modified as part of
--   a sequence of quantum operations, particularly during observation, where the
--   quantum state collapses and the register is updated with the observed state.
--
--   __Example:__
--
--   > let v = qVector [(True, 0.8 :+ 0), (False, 0.6 :+ 0)]
--   > qr <- mkQR v
--
-- 'qr' now holds a reference to a quantum register storing the vector 'v'.
newtype QR a =
  QR (IORef (QV a))

-- | 'mkQR' creates a quantum register (QR) from a quantum vector.
--   A quantum register is an 'IORef' containing a quantum vector, allowing
--   side-effect operations like quantum observation and measurement.
--   The function allocates a new reference cell and stores the provided
--   quantum vector in it, making it accessible for future operations.
--
--   __Parameters:__
--
--   - A quantum vector of type @QV a@.
--
--   __Returns:__
--
--   - An IO action that returns a quantum register containing the quantum vector.
mkQR :: QV a -> IO (QR a)
mkQR v = QR <$> newIORef v

-- | 'observeR' performs a measurement on a quantum register, observing a basis element.
--   It first normalizes the quantum vector, computes the probability distribution,
--   and selects a basis element based on a random number between 0 and 1. The
--   quantum register is then collapsed to a state where only the observed value remains.
--
--   __Parameters:__
--
--   - A quantum register of type @QR a@.
--
--   __Returns:__
--
--   - An IO action that returns the observed basis element. The quantum register
--     is updated to reflect the observed value.
observeR :: (Basis a) => QR a -> IO a
observeR (QR ptr) = do
  v <- readIORef ptr
  res <- observeV v
  writeIORef ptr (singleton res 1)
  return res

-- | 'observeV' performs a probabilistic measurement on a quantum vector,
--   returning a basis element. The vector is first normalized, and the
--   probability of each unit vector is calculated by squaring its amplitude.
--   A random number between 0 and 1 is generated, and the first unit vector
--   whose cumulative probability exceeds the random number is selected.
--
--   __Parameters:__
--
--   - A quantum vector of type @QV a@.
--
--   __Returns:__
--
--   - An IO action that returns the observed basis element based on the vector's
--     probability distribution.
observeV :: (Basis a) => QV a -> IO a
observeV v = do
  let nv = normalize v
      probs = map (square . amplitude nv) basis
      square = (^ (2 :: Int))
  r <- getStdRandom (randomR (0.0, 1.0))
  return . snd . head . dropWhile ((r >=) . fst) . zip probs $ basis

-- | 'observeLeft' measures the left component of a bipartite quantum register.
--   It builds a virtual quantum vector that sums over all right-hand components
--   to compute the marginal probability for each left-hand unit vector.
--   Once the left component is observed, the register is collapsed to a state
--   where only the observed left component and the consistent right components remain.
--
--   __Parameters:__
--
--   - A quantum register of type @QR (a, b)@.
--
--   __Returns:__
--
--   - An IO action that returns the observed left component of the quantum register,
--     and updates the register to reflect this observation.
observeLeft :: (Basis a, Basis b) => QR (a, b) -> IO a
observeLeft (QR ptr) = do
  v <- readIORef ptr
  let leftF a =
        sqrt (sum [(square . magnitude . amplitude v) (a, b) | b <- basis]) :+ 0
      square = (^ (2 :: Int))
      leftV = qVector [(a, leftF a) | a <- basis]
  aobs <- observeV leftV
  let nv = qVector [((aobs, b), amplitude v (aobs, b)) | b <- basis]
  writeIORef ptr nv
  return aobs
