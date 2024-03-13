open Types

module E(M: ALGEBRA): EALGEBRA
       with type 'a t = 'a M.t

module S(M: IALGEBRA): ISEALGEBRA'
       with type 'a u = 'a M.t
        and type 'a ru = 'a Raw.u
        and type 'a rt = 'a Raw.t
