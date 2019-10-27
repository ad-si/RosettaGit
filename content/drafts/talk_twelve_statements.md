+++
title = "Talk:Twelve statements"
description = ""
date = 2012-12-17T13:59:35Z
aliases = []
[extra]
id = 12321
[taxonomies]
categories = []
tags = []
+++

==Twelve statements?==
Looking again at the twelve statements, statement 1: ''"This is a numbered list of twelve statements"'' seems to beg the question "what does it mean for this to be false?".

If you gave conditions for less than twelve statements and assumed that the statements at the end are missing and so do not need to be evaluated  then , lets say eleven then what is the truth value when a condition is not available to be evaluated? We could state that you would also have to deal with statements that directly mention them such as statement 10 relying on the presence of statements 11 and 12. In such cases we could rule that the direct mention of a missing statement by a statement evaluates false. for the other, more indirect references to "all even statements" or "all odd statements", you could say that as long as you have 1, (2) statements then you are allowed to evaluate all odd, (even) type clauses otherwise the statement is False. (Luckily this will not be tested for).

In the Python script you could make these changes and discover that the list of 1-to-11 statements with their corresponding evaluations of truth of:
:<code>1:F, 2:F, 3:F, 4:F, 5:T, 6:F, 7:F, 8:T, 9:F, 10:F, 11:T</code>  # Missing statement 12
Is a Full match.

If you added an extra statement you could similarly assign some method of how to treat these extra 'null' statements and under that scheme work out other extended solutions to the puzzle. --[[User:Paddy3118|Paddy3118]] 04:32, 21 September 2012 (UTC)

: It seems pretty obvious what "there are 12 statements" or its opposite should mean, what's not obvious is in fact your rule on missing statements. If anything, it's unpythonic.  Suppose statements are in a list <code>stmt[]</code> with zero-based index, #10 can be expressed as <code>all(stmt[10:12])</code>, or <code>all(stmt[10:11] + stmt[11:12])</code>, and what would Python say if <code>stmt</code> is only 11 long? (Ok, it's not really "unpythonic", it's just arbitrary.)
::Maybe we could incorporate a three-state logic of true/false/indeterminate? --[[User:Paddy3118|Paddy3118]] 17:16, 21 September 2012 (UTC)

: You could choose to make nonexistent statement default to true or false, either would make a consistent rule, neither would be all that more interesting than the other. --[[User:Ledrug|Ledrug]] 08:13, 21 September 2012 (UTC)

P.S. I did enjoy this task and I do like the way Haskel encodes the statements. --[[User:Paddy3118|Paddy3118]] 04:32, 21 September 2012 (UTC)

The match when there are only eleven statements might better be thought of as a near miss as the task starts "Given the twelve statements ..." and we are already off-by-one because we are considering having other than twelve? --[[User:Paddy3118|Paddy3118]] 04:39, 21 September 2012 (UTC)

==Logical consequence==
It is arguable that the problem is not well-defined since to solve the problem you must use the technical "material implication" interpretation of statements 4 and 8, rather than the everyday "entailment" interpretation. To understand the difference, consider the false statement "If Jupiter is the largest planet, then ladybugs have spots". The consequent does not follow from the antecedent though both are true. To make (common)sense, the problem should replace the "if A then B" statements by "B or not A" ones. You might say this is nit-picking, but logic is all about being precise in what you say. [[User:TobyK|TobyK]] 13:59, 17 December 2012 (UTC)
