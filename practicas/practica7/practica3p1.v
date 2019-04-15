Fixpoint par (x : nat) : Prop :=
match x with
| 0 => True
| S n => match n with
| 0 => False
| S m => par m
end
end.

(*Ejercicio 1*
Fixpoint suma (x y : nat) : nat :=
AQUI VA SU CÓDIGO
*)

(*EJEMPLO PRODUCTO DE DOS NATURALES.
  PUEDEN DES-COMENTARLO HASTA QUE HAYAN HECHO suma
Fixpoint prod (x y : nat) : nat :=
match x with
| 0 => 0
| S n => suma y (prod n y)
end.

Compute prod 2 5.
*)

(*Ejercicio 2*
Fixpoint odd (x:nat) : Prop :=
AQUI VA SU CÓDIGO
*)

(*Ejercicio 3*)
Theorem proove_false_x_x : ~False.
Admitted.

Require Import Bool.

Definition eqb (b1 b2 : bool) : bool :=
match b1, b2 with
|true, true => true
|true, false => false
|false, true => false
|false, false => true
end.

(*Ejercicio 4*
Definition ifb (b1 b2 b3 : bool) : bool :=
AQUI VA SU CÓDIGO
*)

Lemma neutro_aditivo: forall n:nat, 0+n = n.
Proof.
intros.
simpl.
reflexivity.
Qed.

(*Ejercicio 5*)
Lemma neutro_aditivo_derecha : forall n:nat, n+0 = n.
Admitted.

(*Ejercicio 6*)
Lemma neutro_mult : forall n:nat, (S 0)*n = n.

Admitted.
(*Ejercicio 7*)
Lemma neutro_mult_der : forall n:nat, n*(S 0) = n.
Admitted.

(*Ejercicio 8*)
Lemma suma_uno : forall n:nat, n + 1 = S n.
Admitted.

Lemma mult_cero : forall n : nat, 0 * n = 0.
Proof.
intros.
simpl.
reflexivity.
Qed.

(*Ejercicio 9*)
Lemma mult_cero_derecha : forall n : nat, n * 0 = 0.
Admitted.

(*Ejercicio 10*)
Lemma sucesor_suma : forall n m : nat, n + S m = S (n + m).
Admitted.

(*Ejercicio 11*)
Theorem conm_suma: forall n m: nat, n+m = m+n.
Admitted.