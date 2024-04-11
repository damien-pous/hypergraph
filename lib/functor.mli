open Types

module E(M: ALGEBRA): EALGEBRA
       with type 'a t = 'a M.t

module S(M: IALGEBRA): ISEALGEBRA'
       with type 'a u = 'a M.t
        and type 'a ru = 'a Term.u
        and type 'a rt = 'a Term.t
