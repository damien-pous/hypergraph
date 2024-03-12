Require Import Reals Psatz.
Open Scope R_scope.

Module A.
Section s.
Variables x y z t: R.
Hypothesis T: 0<t<1. 


Definition B z := (1-t)^2*x + 2*(1-t)*t*z + t^2 * y.
Goal B z = z - ((1-t)^2*(z-x) + t^2*(z-y)).
Proof. unfold B. field. Qed.

Definition z_ := (z - (1-t)*(1-t)*x - t*t*y) / (2*(1-t)*t).
Goal z_ = z + ((1-t)^2*(z-x) + t^2*(z-y))/(2*t*(1-t)).
Proof. unfold z_. field. lra. Qed.

Goal B z_ = z.
Proof. unfold B, z_. field. lra. Qed.

Definition D z := 2 * ((1-t)*(z-x) - t * (z-y)).

Definition O := D z_.
Goal O = ((1-t)^2*(z-x) - t^2*(z-y)) / (t*(1-t)).
Proof. unfold O, z_, D. field. lra. Qed.

Goal -2*t*(1-t)^2-(1-t)^2*(1-2*t) = -(1-t)^2.
Proof. ring. Qed.

Goal 2*t^2*(1-t)-t^2*(1-2*t) = t^2.
Proof. ring. Qed.

Definition O' := - ((z-x)/t^2 + (z-y)/(1-t)^2). (* \dot s *)

Goal -O / O' = 0.
  unfold O', O, D, z_.
  field_simplify. 
Abort.
End s.
End A.


Module B.
Section s.
Variables z zx zy t: R.
Hypothesis T: 0<t<1. 


Definition B z := z - ((1-t)^2*zx + t^2*zy).
Definition z_ := z + ((1-t)^2*zx + t^2*zy)/(2*t*(1-t)).

(* B z_ = z *)

Definition D := 2 * ((1-t)*zx - t * zy).
Definition O := ((1-t)^2*zx - t^2*zy) / (t*(1-t)).

Goal -2*t*(1-t)^2-(1-t)^2*(1-2*t) = -(1-t)^2.
Proof. ring. Qed.

Goal 2*t^2*(1-t)-t^2*(1-2*t) = t^2.
Proof. ring. Qed.

Definition O' := - (zx/t^2 + zy/(1-t)^2). (* \dot s *)

Goal -O / O' = t*(1-t)*((1-t)^2*zx - t^2*zy)/((1-t)^2*zx + t^2*zy).
  unfold O', O, D, z_.
  field. 
Abort.

End s.
End A.





