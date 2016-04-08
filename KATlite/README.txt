View this file in a fixed-width font such as Courier.

To run, type ./kat in a command window.
Type 'help' at the kat prompt for a list of commands
Type 'load basic' to load some useful theorems already proved

Syntax:
Lower case identifiers represent arbitrary Kleene elements
Upper case identifiers represent tests
= equality
< less than or equal to
+ choice, Boolean join
; sequential composition, Boolean meet (you can omit it on input)
* iteration
~ Boolean complement
-> implication

The system recognizes universal Horn formulas s1 = t1 -> ... -> sn = tn -> s = t.
Example: transitivity of < is x < y -> y < z -> x < z

Here is a tutorial session.  User commands are typed at the prompt '?'.

KAT lite version 1.0
? publish p = p B -> B p = B -> p p = p           ***The theorem to prove is: if p=p;B and B;p=B, then p;p=p.
                                                  ***'publish' enters it in the library.
================================================
Theorem L0: p = p;B -> B;p = B -> p;p = p         ***The name L0 is automatically generated.
Proof: \p,B.\A0,A1.T0                             ***The proof is represented as a lambda-term.
1 task(s)
Current task T0:                                  ***To start, there is only one task T0, to prove the
                                                  ***conclusion from the premises.
  Premises:
    A0: p = p;B                                   ***A0 and A1 are the premises.
    A1: B;p = B
  Goal: p;p = p                                   ***The conclusion p;p=p must be proved under those assumptions.
? rename do-once                                  ***First we rename the theorem to something meaningful.
================================================
Theorem do-once: p = p;B -> B;p = B -> p;p = p
Proof: \p,B.\A0,A1.T0
1 task(s)
Current task T0:
  Premises:
    A0: p = p;B
    A1: B;p = B
  Goal: p;p = p
? cite trans=                                     ***We tell the system to apply the axiom of
                                                  ***transitivity of =: if x=y and y=z, then x=z
                                                  ***with x bound to p;p and z bound to p.
                                                  ***Those bindings are determined automatically by
                                                  ***unifying the conclusion x=z of the transitivity
                                                  ***axiom with the goal p;p=p.
Please provide a binding for y in x = y -> y = z -> x = z
                                                  ***But since y does not appear in the conclusion,
                                                  ***the user must supply a binding for y.
p B p                                             ***The user specifies p;B;p.
================================================
Theorem do-once: p = p;B -> B;p = B -> p;p = p
Proof: \p,B.\A0,A1.(trans=[x=p;p, z=p, y=p;B;p] T1 T2)
                                                  ***The extracted proof is updated to reflect the application
                                                  ***of the transitivity rule with the given bindings.
2 task(s)                                         ***Now the original task is broken down into 2 tasks:
                                                  ***proving p;p=p;B;p under assumptions A0, A1 and
                                                  ***proving p;B;p=p under assumptions A0, A1.
Current task T1:
  Premises:
    A0: p = p;B
    A1: B;p = B
  Goal: p;p = p;B;p
? tasks                                           ***The 'tasks' command allows the user to view all
                                                  ***outstanding tasks.                                             
T1:
  Premises:
    A0: p = p;B
    A1: B;p = B
  Goal: p;p = p;B;p
T2:
  Premises:
    A0: p = p;B
    A1: B;p = B
  Goal: p;B;p = p
================================================
Theorem do-once: p = p;B -> B;p = B -> p;p = p
Proof: \p,B.\A0,A1.(trans=[x=p;p, z=p, y=p;B;p] T1 T2)
2 task(s)
Current task T1:                                  ***The system chooses a current task to work on (here T1).
                                                  ***The user can change it with 'get'.     
  Premises:
    A0: p = p;B
    A1: B;p = B
  Goal: p;p = p;B;p
? cite cong.R                                     ***Apply right congruence of multiplication x=y -> x;z=y;z
                                                  ***to remove the rightmost p on both sides of the equation.
                                                  ***This is a subgoaling approach.  We are specifying the last
                                                  ***step in the proof.  Here we are saying that in order to
                                                  ***prove p;p=p;B;p, it suffices to prove p=p;B.
================================================
Theorem do-once: p = p;B -> B;p = B -> p;p = p
Proof: \p,B.\A0,A1.(trans=[x=p;p, z=p, y=p;B;p] (cong.R[x=p, z=p, y=p;B] T3) T2)
2 task(s)
Current task T3:
  Premises:
    A0: p = p;B
    A1: B;p = B
  Goal: p = p;B
? use A0                                          ***The new goal p=p;B is one of our assumptions, so we
                                                  ***can just 'use' it.
================================================
Theorem do-once: p = p;B -> B;p = B -> p;p = p
Proof: \p,B.\A0,A1.(trans=[x=p;p, z=p, y=p;B;p] (cong.R[x=p, z=p, y=p;B] A0) T2)
                                                  ***The task T1 is complete.  The free task
                                                  ***variable T1 has been replaced by A0 in the proof.
1 task(s)
Current task T2:                                  ***Another outstanding task becomes the current task.
  Premises:
    A0: p = p;B
    A1: B;p = B
  Goal: p;B;p = p
? cite trans=
Please provide a binding for y in x = y -> y = z -> x = z
p B
================================================
Theorem do-once: p = p;B -> B;p = B -> p;p = p
Proof: \p,B.\A0,A1.(trans=[x=p;p, z=p, y=p;B;p] (cong.R[x=p, z=p, y=p;B] A0) (trans=[x=p;B;p, z=p, y=p;B] T4 T5))
2 task(s)
Current task T4:
  Premises:
    A0: p = p;B
    A1: B;p = B
  Goal: p;B;p = p;B
? cite cong.L                                     ***Apply left congruence of multiplication to remove
                                                  ***the leftmost p from both sides of the equation.
================================================
Theorem do-once: p = p;B -> B;p = B -> p;p = p
Proof: \p,B.\A0,A1.(trans=[x=p;p, z=p, y=p;B;p] (cong.R[x=p, z=p, y=p;B] A0) (trans=[x=p;B;p, z=p, y=p;B] (cong.L[x=p, z=B, y=B;p] T6) T5))
2 task(s)
Current task T6:
  Premises:
    A0: p = p;B
    A1: B;p = B
  Goal: B;p = B
? use A1                                          ***Use an assumption.
================================================
Theorem do-once: p = p;B -> B;p = B -> p;p = p
Proof: \p,B.\A0,A1.(trans=[x=p;p, z=p, y=p;B;p] (cong.R[x=p, z=p, y=p;B] A0) (trans=[x=p;B;p, z=p, y=p;B] (cong.L[x=p, z=B, y=B;p] A1) T5))
1 task(s)
Current task T5:
  Premises:
    A0: p = p;B
    A1: B;p = B
  Goal: p;B = p
? cite sym                                        ***Cite symmetry of = to switch left- and right-hand sides.
================================================
Theorem do-once: p = p;B -> B;p = B -> p;p = p
Proof: \p,B.\A0,A1.(trans=[x=p;p, z=p, y=p;B;p] (cong.R[x=p, z=p, y=p;B] A0) (trans=[x=p;B;p, z=p, y=p;B] (cong.L[x=p, z=B, y=B;p] A1) (sym[x=p, y=p;B] T7)))
1 task(s)
Current task T7:
  Premises:
    A0: p = p;B
    A1: B;p = B
  Goal: p = p;B
? use A0
================================================
Theorem do-once: p = p;B -> B;p = B -> p;p = p
Proof: \p,B.\A0,A1.(trans=[x=p;p, z=p, y=p;B;p] (cong.R[x=p, z=p, y=p;B] A0) (trans=[x=p;B;p, z=p, y=p;B] (cong.L[x=p, z=B, y=B;p] A1) (sym[x=p, y=p;B] A0)))
No tasks                                          ***All tasks have been discharged and the proof is complete.
                                                  ***There are no more free task variables in the
                                                  ***proof term; it is a closed lambda-term.
? quit
bye
