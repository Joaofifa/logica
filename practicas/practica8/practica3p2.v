(* Auxiliares. *)
Lemma neutro_aditivo_derecha : forall n:nat, n+0 = n.
Proof.
intros.
induction n.
simpl.
reflexivity.
simpl.
rewrite IHn.
reflexivity.
Qed.

Lemma sucesor_suma : forall n m : nat, n + S m = S (n + m).
Proof.
intros.
induction n.
simpl.
reflexivity.
simpl.
rewrite IHn.
reflexivity.
Qed.

Theorem conm_suma: forall n m: nat, n+m = m+n.
Proof.
intros.
induction n.
simpl.
rewrite -> neutro_aditivo_derecha.
reflexivity.
simpl.
rewrite IHn.
rewrite -> sucesor_suma.
reflexivity.
Qed.

(*Ejercicio 1*)
Theorem asoc_suma : forall n m p:nat, (n+m)+p = n+(m+p).
Proof.
intros.
induction n.
simpl.
reflexivity.
simpl.
rewrite IHn.
reflexivity.
Qed.

(*Ejercicio 2*)
Lemma distr_mult_y_suma : forall n m p:nat, n*(m+p) = n*m + n*p.
Proof.
intros.
induction n.
simpl.
reflexivity.
simpl.
rewrite IHn.
rewrite -> asoc_suma with m (n * m) (p + n * p).
rewrite -> asoc_suma.
rewrite <- asoc_suma with p (n * m) (n * p).
rewrite -> conm_suma with p (n * m).
rewrite <- asoc_suma with (n * m) p (n * p).
reflexivity.
Qed.

(*Ejercicio 3*)
Theorem menor_igual_cero : forall n:nat, 0 <= n.
Proof.
intros.
induction n.
constructor.
constructor.
trivial.
Qed.

Require Import List.

Notation "x :: l" := (cons x l) (at level 60, right associativity).
Notation "[]" := nil.
Notation "[ x , .. , y ]" := (cons x .. (cons y nil) .. ).

Variables A : Type.

Fixpoint elem (z:A) (l: list A) :=
  match l with
    | nil => False
    | (x::xs) => x = z \/ elem z xs
  end.

(*Ejercicio 4*)
Theorem theorem_elem : forall (a:A) (l:list A), elem a (a::l).
Proof.
intros.
simpl.
left.
reflexivity.
Qed.

(*Ejercicio 5*)
Theorem theorem_elem2 : forall a:A, ~ elem a nil.
Proof.
intros.
unfold not.
simpl.
trivial.
Qed.

(*Ejercicio 6*)
Theorem theorem_elem3 : forall (a b:A) (l:list A), elem b l -> elem b (a::l).
Proof.
intros.
simpl.
right.
exact H.
Qed.

(*Ejercicio 7*)
Lemma nil_or_not : forall l:list A, l = [] \/ l <> [].
Proof.
intros.
case l.
unfold not.
left.
reflexivity.
intros.
unfold not.
right.
intros.
discriminate H.
Qed.

(*Ejercicio 8*)
Theorem split : forall l:list A, l <> [] -> exists x:A, exists l1 l2: list A, l = l1 ++ x::l2.
Proof.
intros.
destruct l.
intuition.
exists a.
exists l.
exists (a :: l).
contradiction H.

Qed.

(*Ejercicio 9*)
Lemma equal_lists : forall (a:A) (xs ys:list A), a::xs = a::ys <-> xs = ys.
Proof.
intros.
destruct xs.
intuition.
inversion H.
reflexivity.
inversion H.
reflexivity.
intuition.
inversion H.
reflexivity.
inversion H.
reflexivity.
Qed.

Variable p : A -> bool.

(*Ejercicio 10*)
Lemma diff : forall x y, p x = true -> p y = false -> x <> y.
Proof.
intros.
unfold not.
intros.
symmetry in H1.
rewrite H1 in H0.
rewrite H in H0.
discriminate H0.
Qed.

Inductive bit: Type := 
  | zero:bit
  | one:bit.

Definition bin := list bit.

Check bit_ind.
Print bin.

(*Ejercicio 11*
Fixpoint increment_one (b:bin):bin :=
AQUI VA SU CÓDIGO
*)

(*Ejercicio 12*
Fixpoint to_binary (n:nat):bin :=
AQUI VA SU CÓDIGO
*)
