= 1/22 - Formalization
Plan
- Formalize our sort specification
- Discuss computability
- Implement the specification and find a bug

Our specification for sort started as
+ $forall "xs", "ys", "sort(xs)" = "ys" => forall i, j, 0 <= i < j < "length ys" => "ys"[i] <= "ys"[j]$
+ $forall x in "xs", x in "ys" $ or $"sort xs" = "ys" => forall i, |"xs"[i]| = |"ys"[i]$ where xs and ys are multisets or 2d lists. 
We can restate the second property as $S := {P | forall i, R(i, P(i))}$ where S is the set of all programs where for all inputs a checker predicate can match them with the correct output.

The first step in this process is to 