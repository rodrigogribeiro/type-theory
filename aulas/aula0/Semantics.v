Set Implicit Arguments.

Inductive Exp : Set :=
  | const : nat -> Exp
  | add    : Exp -> Exp -> Exp.


Inductive step : Exp -> nat -> Prop :=
  | sconst : forall n, step (const n) n
  | sadd : forall e1 e2 n1 n2, step e1 n1 -> step e2 n2 -> step (add e1 e2) (n1 + n2).

Theorem det : forall e e' e'', step e e' -> step e e'' -> e' = e''.
Proof.
  intros e.
  induction e ;
  intros e' e'' H H'.
  inversion H ; subst.
  inversion H' ; subst.
  reflexivity.
  inversion H ; subst.
  inversion H' ; subst.
  rewrite (IHe1 _ _ H2 H3).
  rewrite (IHe2 _ _ H4 H6).
  reflexivity.
Qed.