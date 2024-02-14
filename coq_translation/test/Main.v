Require Export Coq.Strings.Ascii.
Require Import Coq.Strings.String.
Require Import Coq.ZArith.ZArith.

Local Open Scope list_scope.
Local Open Scope string_scope.
Local Open Scope Z_scope.

Import List.ListNotations.

Parameter Rational : Set.

Module Lit.
  Inductive t : Set :=
  | Char (_ : ascii)
  | Number (_ : Z)
  | String (_ : string)
  | NullAddr
  | Rubbish
  | Float (_ : Rational)
  | Double (_ : Rational)
  | Label (_ : string).
End Lit.

Module Case.
  Inductive t (A : Set) : Set :=
  | Con (_ : string) (_ : list A -> A)
  | Lit (_ : Lit.t) (_ : A)
  | Default (_ : A).
  Arguments Con {A}.
  Arguments Lit {A}.
  Arguments Default {A}.
End Case.

Module Val.
  #[bypass_check(positivity)]
  CoInductive t : Set :=
  | Lit (_ : Lit.t)
  | Con (_ : string) (_ : list t)
  | App (_ _ : t)
  | Lam (_ : t -> t)
  | Case (_ : t) (_ : t -> list (Case.t t))
  | Impossible.
End Val.

Definition Isharp' : Val.t :=
  Val.Lam (fun z => Val.Con "I#" [z]).

Definition lbracket'rbracket' : Val.t :=
  Val.Con "[]" [].

Definition colon' : Val.t :=
  Val.Lam (fun x => Val.Lam (fun xs => Val.Con ":" [x; xs])).

Definition build : Val.t :=
  Val.Lam (fun l => Val.App (Val.App l colon') lbracket'rbracket').

Parameter plus'plus' : Val.t.

Parameter _'Module : Val.t.

Parameter TrNameS : Val.t.

Parameter eq'eq' : Val.t.

Parameter dollar'fEqNatural : Val.t.

Parameter fromInteger : Val.t.

Parameter dollar'fNumNatural : Val.t.

Parameter minus' : Val.t.

Parameter _'False : Val.t.

Parameter _'True : Val.t.

Parameter _'return : Val.t.

Parameter dollar'fMonadIO : Val.t.

Parameter lparen'rparen' : Val.t.

Parameter runMainIO : Val.t.

Definition x : Val.t := (Val.App Isharp' (Val.Lit (Lit.Number 5))).

CoFixpoint onlyOne : Val.t :=
  (Val.App (Val.App colon' (Val.App Isharp' (Val.Lit (Lit.Number 1)))) onlyOne).

Definition twoOne : Val.t :=
  (Val.App
    (Val.App
      plus'plus'
      (Val.App
        build
        (Val.Lam
          (fun (c_d1ze : Val.t) =>
            (Val.Lam
              (fun (n_d1zf : Val.t) =>
                (Val.App
                  (Val.App c_d1ze (Val.App Isharp' (Val.Lit (Lit.Number 2))))
                  (Val.App
                    (Val.App c_d1ze (Val.App Isharp' (Val.Lit (Lit.Number 2))))
                    n_d1zf))))))))
    onlyOne).

CoFixpoint f : Val.t := (Val.Lam (fun (x : Val.t) => (Val.App f x))).

CoFixpoint fixObvious : Val.t :=
  (Val.Lam (fun (f : Val.t) => (Val.App f (Val.App fixObvious f)))).

Definition fixSubtle : Val.t :=
  (Val.Lam (fun (f : Val.t) => let cofix x := (Val.App f x) in x)).

Definition emptyList : Val.t := lbracket'rbracket'.

Definition dollar'trModule : Val.t :=
  (Val.App
    (Val.App _'Module (Val.App TrNameS (Val.Lit (Lit.String "main"))))
    (Val.App TrNameS (Val.Lit (Lit.String "Main")))).

CoFixpoint odd : Val.t :=
  (Val.Lam
    (fun (ds_d1yV : Val.t) =>
      (Val.Case
        (Val.App
          (Val.App (Val.App eq'eq' dollar'fEqNatural) ds_d1yV)
          (Val.App
            (Val.App fromInteger dollar'fNumNatural)
            (Val.Lit (Lit.Number 0))))
        (fun wild_00 =>
          [
            Case.Con
              "False"
              (fun α =>
                (match α with
                | [] =>
                  (Val.App
                    even
                    (Val.App
                      (Val.App (Val.App minus' dollar'fNumNatural) ds_d1yV)
                      (Val.App
                        (Val.App fromInteger dollar'fNumNatural)
                        (Val.Lit (Lit.Number 1)))))
                | _ => Val.Impossible
                end));
            Case.Con
              "True"
              (fun α =>
                (match α with | [] => _'False | _ => Val.Impossible end))
          ]))))

with even : Val.t :=
  (Val.Lam
    (fun (ds_d1z4 : Val.t) =>
      (Val.Case
        (Val.App
          (Val.App (Val.App eq'eq' dollar'fEqNatural) ds_d1z4)
          (Val.App
            (Val.App fromInteger dollar'fNumNatural)
            (Val.Lit (Lit.Number 0))))
        (fun wild_00 =>
          [
            Case.Con
              "False"
              (fun α =>
                (match α with
                | [] =>
                  (Val.App
                    odd
                    (Val.App
                      (Val.App (Val.App minus' dollar'fNumNatural) ds_d1z4)
                      (Val.App
                        (Val.App fromInteger dollar'fNumNatural)
                        (Val.Lit (Lit.Number 1)))))
                | _ => Val.Impossible
                end));
            Case.Con
              "True"
              (fun α => (match α with | [] => _'True | _ => Val.Impossible end))
          ])))).

Definition main : Val.t :=
  (Val.App (Val.App _'return dollar'fMonadIO) lparen'rparen').

Definition main : Val.t := (Val.App runMainIO main).
