# Usage of Shibboleth 


OCaml and SML have *just* enough differences between them to require a complex conversion process. 
Here, by default, we attempt to create a minimal converter, which we define as a converter meeting the following requirements for a given "transformation" T:

## A "Minimal" Transformation 

1. If `P` is a well formed SML program, then `T(P)` is a well formed OCaml program 
2. If `A` and `B` refer to the same symbol in the SML source, then `T(A)` and `T(B)` refer to the same OCaml symbol 
3. Constructs and comments must be preserved 
4. The produced OCaml must be, in a direct sense, a **converted**, and **not transpiled** version of the SML.

## Names 

Because of the somewhat stringent requirements set out by the above requirements, the transformation of names from SML to OCaml can be diffucult at times, firstly, while SML has no capitilization convention, OCaml has a quite stringent one


| Name space               | Case of first letter          |
| :----------------------- | :---------------------------- |
| Values                   | lowercase                     |
| Constructors             | uppercase                     |
| Variables | lowercase |
| Labels                   | lowercase                     |
| Polymorphic variant tags | any (by convention uppercase) |
| Exceptions               | uppercase                     |
| Type constructors        | lowercase                     |
| Record fields            | lowercase                     |
| Classes                  | lowercase                     |
| Instance variables       | lowercase                     |
| Methods                  | lowercase                     |
| Modules                  | uppercase                     |
| Module types             | any (by convention uppercase) |

of these, modules and module types aren't even *in* the core SML language, so these are trivial to deal with.
Classes, instance variables, methods, and polymorphic tags are unique to OCaml, and are thus a moot point. 
Finally, labels and record fields are always used with a special syntax, and are therefore also trivial. 
This leaves only 4 types of names that are ambiguous and that can occur in SML, values, constructors, variables, and types. 

While it might seem like per [A "Minimal" Transformation](#a-minimal-transformation) that we are stuck here, we can elimanate one more of these through the simple realizations that OCaml, lacking dependent types, will always have types occuring in types, and values only in values. 
Thus, the only things that are truely ambiguos are construtors, values, and variables. 
These, there is no good way to follow the rules set out above while still changing them all. 
Thus, by default, we don't attempt to handle these cases at all. 

## Underscoring underscores 

Underscores are used a lot in this project. 
This is due to a couple reasons. 
1. Underscores are not allowed at the begining of a SML identifier. Thus, `__A` will never refer to a correct SML name.
2. `__A` is a lowercase identifier, and, given that, as per noted above, the most pervasive change possible is with types, which must be lowercase, this is quite useful. 

Altogether, `A`, where `A` is an uppercase SML identifier, will be transformed to `__A`, which is a *lowercase* OCaml identifier. 

## A Short Comment 

One of the strongest aspects of this project is that it mostly preserves comments and their positioning. 
It attempts to, and largely succeeds, in converting each and every SML comment to OCaml.
How? By adding them as attributes and then performing textual replacement on those attributes 

