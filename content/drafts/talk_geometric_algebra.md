+++
title = "Talk:Geometric algebra"
description = ""
date = 2015-10-21T20:46:23Z
aliases = []
[extra]
id = 19664
[taxonomies]
categories = []
tags = []
+++

==This is maybe too big for a task==

I'm pretty sure people will say that, and maybe they're right.  But maybe not.  I don't think it is much more complicated than say [[Quaternion type]], and in any case it is, from both the programming and mathematical points of view,, quite interesting and worth featuring in Rosetta Code, imho.  Please feel free to argue about it.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 22:38, 13 October 2015 (UTC)

: It's not clear that we can meaningfully implement anything with infinite dimension - countable or not. At best, we can support a finite subset of such a thing. 

: More specifically, how would we tell whether an implementation has or has not satisfied that part of the task requirement? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 12:36, 14 October 2015 (UTC)

::It's infinite in the sense that there is no limit to the number of dimensions.  But we only consider vectors that have a finite support. I guess I could mention that, indeed.   It's also true that it's not obvious how we can check that an implementation can handle any vector size.  I welcome suggestions.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 15:56, 14 October 2015 (UTC)

::: I would describe that as an ''arbitrary number of dimensions'' rather than an ''infinite number of dimensions'' (edit: and I see that you have made exactly that change in the task description). But that brings up another issue: <math>\begin{array}{c}\forall \mathbf{x}\in\mathcal{V},\,\mathbf{x}^2\in\R
\end{array}</math> - unless we severely constrain our work, how are we going to verify that an implementation has satisfied this task in that regard? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:25, 14 October 2015 (UTC)

::::There is no way to verify this kind of things from just the output.  We have to trust the code to do something significant without cheating.  What I suggested in the task description was to pick a random vector and check that its square is a Real.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 16:28, 14 October 2015 (UTC)

::::: Ok, so I think what you are saying is roughly that the task should instead be implementing <math>\begin{array}{c}\exists \mathbf{x}\in\mathcal{V},\,\mathbf{x}^2\in\R
\end{array}</math> with the added constraint that <math>{x}</math> must be picked arbitrarily from the space? That's certainly doable... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:34, 14 October 2015 (UTC)

:::::: No, I don't say that.  We should keep the axioms as they are and try our best to verify them from a computational point of view.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 16:44, 14 October 2015 (UTC)

::After some thought I changed the task requirements and made it demand an implementation of quaternion to demonstrate the solution.  Hope it's ok.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 10:37, 17 October 2015 (UTC)

:::That might be a bit overly constrained? Though, granted, not constrained enough for the "forall" test to be implemented - which, I guess, is why you made the test for correctness be a single case? See also [[Quaternion type]] for quaternion implementations... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 12:45, 17 October 2015 (UTC)
::::I see that you posted a solution using the quaternion task.  That is obviously not what was intended.  You're supposed to implement the geometric algebra and use it to implement quaternions.  Implementing quaternions is not the purpose ''per se''.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 16:14, 17 October 2015 (UTC)
:::::I believe I have changed that implementation to satisfy the requirement of the "incorrect" notice. That said, I will also note that I believe this issue could be stated significantly more clearly in the task description with relatively little effort. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:42, 17 October 2015 (UTC)
::::::I think it is pretty clear already that the quaternion data structure is meant only as a show-case for the use of geometric algebra.  Using an implementation of quaternions that does not actually implement geometric algebra can not be satisfactory.   If you persist I guess I will have to switch back to verifying axioms.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 17:58, 17 October 2015 (UTC)
:::::::Whatever you do, please be sure you understand the requirements stated in the [[Rosetta_Code:Add_a_Task|add a task]] page. You should only expect implementations to satisfy those parts of the task description which are valid for a task description. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:03, 17 October 2015 (UTC)
::::::::You removed the incorrect banner, but didn't you write that the first element of this structure is the real component?  If so, e2 defined with 1 0 0 0 is a scalar, not a vector.  In any case it is not orthonormal with the e1 and e3.  I will add the creation of a function e(n), along with an orthonormality test, as a requirement to clarify that.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 18:12, 17 October 2015 (UTC)
:::::::::I think you are confused. 
:::::::::So... let's review: a quaternion consists of four orthogonal components. What you term the "scalar part" is a part of an orthonormal basis for quaternions. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:30, 17 October 2015 (UTC)
::::::::::A scalar is not orthogonal to a vector.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 18:40, 17 October 2015 (UTC)
:::::::::::Except we are working with quaternions, and the "scalar part of a quaternion" is indeed orthogonal to any "vector part of a quaternion". --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:57, 17 October 2015 (UTC)
::::::::::::I'm not sure what you mean by orthogonal here, because there is no scalar product in the space of quaternions.  I did define a scalar product for the purpose of this task, and with this scalar product, a scalar is not orthogonal with a vector, because it commutes with all vectors.  For instance 2i + i2 is not 0.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 19:05, 17 October 2015 (UTC)
:::::::::::::Except you did not define what you mean by "the symmetric part" of the quaternion. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:14, 17 October 2015 (UTC)
::::::::::::::I wrote "the symmetric part of the geometric product", not "the symmetric part of the quaternion".  And I gave an algebraic expression, that should be enough.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 19:17, 17 October 2015 (UTC)
:::::::::::::::If the <math>\mathbf{ab}</math> and <math>\mathbf{ba}</math> in <math>\mathbf{a}\cdot\mathbf{b} = (\mathbf{ab} + \mathbf{ba})/2</math> are quaternion products, and if <math>{a}</math> and <math>{b}</math> are arbitrary quaternions, then that result is not a scalar product. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:31, 17 October 2015 (UTC)

== J's orthonormality expression. ==

The J expression <code>0 1 2 3 dot&e"0/0 1 2 3</code> generates a table of 16 dot products. 

Breaking this down:

The left argument is 0 1 2 3, and the right argument is 0 1 2 3. each row corresponds to an item from the left argument (the first row corresponding to the first item from the left argument, the last row corresponding to the last item from the left argument - in other words, rows in the result are in the same order as items in the left argument. Likewise columns in the result correspond to items from the right argument.

In each case, the value in the result is the result of (e left_item) dot (e right_item).

As the result is an identity matrix, we can see that we have an orthonormal basis.

:while you're at it, you can clarify the function e and its output for 1, 2, 3.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 19:08, 17 October 2015 (UTC)

== A possible source of confusion ==

The wikipedia page on [[wp:Clifford algebra|Clifford algebra]] states:

::The basis elements can be identified with the quaternion basis elements ''i'', ''j'', ''k'' as
:::<math> i=  \mathbf{e}_2 \mathbf{e}_3, j=  \mathbf{e}_3 \mathbf{e}_1, k =  \mathbf{e}_1 \mathbf{e}_2,</math>

This has a different parity from the current task requirement for i, j and k. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:01, 17 October 2015 (UTC)

:Well this is not accurate, because with this definition, <math>ijk = 1</math>, not -1.

:<math>ijk=e_2e_3e_3e_1e_1e_2 = e_2(e_3e_3)(e_1e_1)e_2 = e_2e_2 = 1</math>
--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 19:12, 17 October 2015 (UTC)

:: Unless you have a suitable definition for <code>e</code>.

:: For example, consider: <math> e_2=j, e_3=k, e_1=i</math>

:: Then <math>e_2e_3e_3e_1e_1e_2 = jkkiij = j(kk)(ii)j = j(-1)(-1)j = jj = -1 </math> --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:15, 17 October 2015 (UTC)

:::There's a circularity issue, here.  You're talking about a definition of quaternions that relies on quaternions.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 19:31, 17 October 2015 (UTC)

:::Also, I just checked.  The wikipedia article uses a negative sign convention : <math>\mathbf{e}_1^2 = \mathbf{e}_2^2 = \mathbf{e}_3^2 = -1</math>.  That's a bit awkward, but that allows for a more ordered way to define i, j, k indeed.  That does not fit our task since we stated that <math>\mathbf{e}_i\mathbf{e}_i = 1</math>, not -1.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 19:39, 17 October 2015 (UTC)

::::I believe that what you have defined here is not consistent with the definition of quaternions. I'm going to take some time to think about this, and then see if I can show you a specific example which illustrates the problem. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:06, 17 October 2015 (UTC)

:::::It's necessarily consistent as long as I get the defining property <math>i^2 = j^2 = k^2 = ijk = -1</math>.  Regarding the parity difference, Richard Wareham mentions it in his docoral thesis:

::::::« It is worth comparing this method of rotation to rotation via quaternions. The three bivectors B1, B2 and B3 act identically to the three ‘imaginary’ components of quaternions, i, -j and k respectively. The sign difference between B2 and j is due to the fact that the quaternions are not derived from the usual right-handed orthogonal co-ordinate system. This handedness mismatch often leads to annoying sign errors in quaternion-based algorithms. »

:::::[http://www2.eng.cam.ac.uk/~rjw57/pdf/r_wareham_pdh_thesis.pdf Computer Graphics Using Conformal Geometric Algebra], page 13. --[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 20:16, 17 October 2015 (UTC)

::::::It's not consistent if i, j and k are not orthogonal to each other, which is the case for the javascript implementation (and I do not have a working copy of perl6, so I can't test whether that implementation is invalid or not). It might be that i, j and k are not orthogonal for any implementation which satisfies the constraints in the task description - I suspect that that is the case, but I need to think a bit more to see if I can either (a) find an interpretation of the task description which is internally consistent, or (b) find a way of showing a contradiction. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:01, 17 October 2015 (UTC)
:::::::i, j and k are not vectors in geometric algebra.  They are bivectors.  Orthogonality does not apply to them.  The formula I gave for the scalar product only stands for vectors (thus the boldface notation which often means "vectors").  Also I've never written they should be orthogonal.  It's the e basis that is, not the i, j, k.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 22:24, 17 October 2015 (UTC)
::::::::We might still have an issue here. I think I can agree that I, j, and k are not vectors from the orthogonal basis e. But claiming that they are "not vectors" without qualification seems both sloppy and misleading. Needs more exposition, I guess... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:02, 19 October 2015 (UTC)
:::::::::Strictly speaking, all elements of a geometric algebra are vectors, since the whole geometric algebra has a natural vector space structure.  However, the word ''vector'' is usually reserved to the elements of <math>\mathcal{V}</math>.  Other elements are ''multivectors''.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 19:38, 19 October 2015 (UTC)
:::::::::The task description does expose this if you look carefully.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 19:41, 19 October 2015 (UTC)
::::::::::Sure, but there's nothing in the task description which supports the idea that i, j and k are not vectors. The axioms of vector spaces hold for i, j and k -- you can add them, and you can scale them and that includes scaling them by -1 which we can think of as being an inverse. You referred to them as bivectors, but the relevant definition of that term was not included in the task description.
::::::::::If it's not in the task description, and it's relevant to the task, that's a defect of the task description. You can't just refer people to some unmanageably large external context and expect that people will be able to distinguish the parts of that context which you consider relevant from the parts you consider irrelevant.
::::::::::Put differently, I do not know whether the presence of a <math>\mathcal{V}</math> within a clifford algebra excludes the existence of a different <math>\mathcal{V'}</math> from the algebra. Maybe it does, and maybe the statement "It is a known fact that if the dimension of <math>\mathcal{V}</math> is <math>n</math>, then the dimension of the algebra is <math>2^n</math>." hints at the axioms or constraints or concepts which require that. Or maybe not. I've not studied the subject enough to say for sure.  --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:15, 19 October 2015 (UTC)
:::::::::::Notice that I did not mention bivectors in the task description, only in this discussion page.  The task description hints that there are multivectors that are not vectors.  Without any knowledge of the subject, that means that the reader can not assume anything about i, j and k because he does not know if the geometric product of two vectors is a vector or not.  He should consider them as multivectors and as such the scalar product formula can not ''a priori'' be applied to them.  I would also like to point at that you were the one who wanted to discuss the orthogonality of i, j and k (for which the concept does not apply), but there is no need to consider that for solving this task anyway.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 20:39, 19 October 2015 (UTC)
::::::::::::But "geometric product" is not a part of the definition of "vector". You do not need a "geometric product" for a vector to be a vector, and in fact in the general case vectors exist without any geometric product being defined. Meanwhile, the axioms of the of the algebra require that a scalar product can be applied to i, j and k. So the reader should indeed be able to assume that the scalar product formula can be applied to them.
::::::::::::Still, yes, you are using a particular definition of "orthogonality" - it's not what I would have expected, and I can easily imagine other definitions which might serve in other contexts. So that means that that definition of orthogonality needs to be in the task description. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:48, 19 October 2015 (UTC)
:::::::::::::No, the axioms do not require that a scalar product can be applied to i, j and k.  I think you're confusing things.  The axioms imply that the ''geometric product'' can be applied to them, not the scalar product.  In fact, the axioms do not mention a scalar product at all.  Also I don't understand where you got the idea that I was suggesting that the geometric product is required for the definition of a vector.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 20:57, 19 October 2015 (UTC)
:::::::::::::As far as the orthonormality is concerned, once a scalar product has been established, the definition of orthonormality is common knowledge enough that we don't have to remind it, imho.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 20:59, 19 October 2015 (UTC)
::::::::::::::Specifically, the axioms include:
:<math>\begin{array}{c}
a(b+c) = ab+ac
\end{array}
</math>
::::::::::::::And as I understand it, a is a scalar or can be a scalar, and i, j and k are values which can be used in the context of b and/or c. Do you really disagree? If so, why?
::::::::::::::That said, scalar product itself has two relevant meanings in this discussion - a product between vectors which produces a scalar, and a product between a scalar and a vector which produces another vector. But even there, it's my understanding that a vector space only needs to support the ability to be scaled and added. Once you have that you have enough that you can easily define a mechanism which multiplies vectors and produces a scalar. That said, you have already defined product involving i, j and k which produces a scalar - that particular product doesn't make them orthonormal, but it would be easy enough to define another product which does. <code>-mul</code>, for example. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 02:49, 20 October 2015 (UTC)
::::::::::::::After thinking about this, I am going to ask you to change your "It is known" statements in the task description. You need to spell these out in more detail. You cannot expect these details to be known by typical contributors to Rosettacode. You cannot even expect these details to be known by typical mathematicians. Only people who are well versed in the geometri/clifford algebra arcana should be expected to have apriori understanding of those details. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 03:31, 20 October 2015 (UTC)
:::::::::::::::The axioms define a so-called geometric product.  The fact that this geometric product can be applied to a scalar and something else does not make it a scalar product.  The expression ''scalar product'' in math means something specific.  IIRC it's a symmetric, bilinear, positive-definite form.  The multiplication of a scalar by a vector in a vector space for instance is never called scalar product, as this would be a very unfortunate naming collision.  IIRC it's called extern scalar multiplication or something like that.
:::::::::::::::The proof that the geometric product defines a scalar product is not too hard.  First you define the inner product of two vectors <math>\mathbf{a}\cdot\mathbf{b} = (\mathbf{a}\mathbf{b} + \mathbf{b}\mathbf{a})/2</math>.  It's straightforward to see that it is a symmetric and bilinear.  What's not so obvious is that it is a form, that is that it returns a scalar.  To see it you just notice the equality :  <math>\mathbf{a}\mathbf{b} + \mathbf{b}\mathbf{a} = \mathbf{a}^2 - \mathbf{a}^2 + \mathbf{a}\mathbf{b} + \mathbf{b}\mathbf{a} + \mathbf{b}^2 - \mathbf{b}^2 = (\mathbf{a} + \mathbf{b})^2 -\mathbf{a}^2 - \mathbf{b}^2</math>, and this is a real number because it's a linear combination of real numbers.
:::::::::::::::It's not very complicated a proof, but it's quite irrelevant to the task and putting it in the description would spam it imho.  I won't add it unless other people complain.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 11:21, 20 October 2015 (UTC)
:::::::::::::::: The axioms themselves do not specify what kind of product they use - which means that the reader should be able to determine that. Of course, other statements will constrain this, but baring considerable familiarity with the topic, we are left with the sort of trial and error (or hypothesis and test) that leads to talk pages as large as this one. Put differently, a general problem with generality is that there's so many ways to do it, and so many of the underlying assumptions are just that: assumptions. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 11:47, 20 October 2015 (UTC)
::::::::::::::::: The axioms don't specify what kind of product they use because they ''define'' it.  Any product that satisfy these axioms '''IS''' a geometric product.  That's what axioms do.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 12:01, 20 October 2015 (UTC)
::::::::::::::::::One difficulty here is that the definition uses the term "dimension". And, while we might reasonably assume that we know what a "[[Dimension_(vector_space)|dimension]]" is, you have also declared that multivectors are not vectors. This means that we should not be using the definition of dimension which applies to vectors. I hope you can see the difficulty... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:29, 20 October 2015 (UTC)
:::::::::::::::::::I've explained that already.  Strictly speaking, multivectors are vectors, but the term ''vector'' is reserved to elements of <math>\mathcal{V}</math>.  The task description also briefly mentions this.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 14:22, 20 October 2015 (UTC)

== "Orthonormal basis" ==

Ok, I'm still trying to figure out what you are asking for here.

But it looks like e(1), e(2) and e(3) are not quaternions.

Is that what you are asking for? A "quaternion multiplication" which also works for something which is not a quaternion, such that when some of those other things are multiplied you can get a quaternion basis? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:25, 17 October 2015 (UTC)

:I don't know why you thought they were quaternions.  They are orthonormal vectors from which the quaternions are made.  You make three bivectors i, j and k from these vectors and you verify that those bivectors satisfy the algebraic identity <math>i^2 = j^2 = k^3 = ijk = -1</math>, which means they are isomorphic to the quaternion field.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 22:31, 17 October 2015 (UTC)

::I thought they were quaternions because you are combining them using a mechanism which you specified as quaternion multiplication. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:39, 17 October 2015 (UTC)

:::No, I combine them with the geometric product.  Let me remind you that quaternions here are used as a special case of a more general structure, the geometric algebra.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 22:43, 17 October 2015 (UTC)

:::: I think you have enough specifics in the task page now to narrow the implementation down to the particular geometric algebra you are thinking of. Previously, there were an infinity of the things which satisfied the task description. (There still might be, but if so it's now a much more constrained infinity...). Thanks! --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:58, 19 October 2015 (UTC)

== Task still needs something... ==

Re-reading the task, and the implementation you have, I think the task needs some additional qualification.

Specifically, it's entirely possible to satisfy 

:<math>\begin{array}{c}
(ab)c = a(bc)\\
a(b+c) = ab+ac\\
(a+b)c = ac+bc\\
\forall \mathbf{x}\in\mathcal{V},\,\mathbf{x}^2\in\R
\end{array}</math>

in a fashion which does not support quaternion multiplication. For example, an algebra where <math>\forall \mathbf{x}\in\mathcal{V},\forall \mathbf{y}\in\mathcal{V},\,\mathbf{xy}\in\R</math> and <math>\forall \mathbf{x}\in\mathcal{V},\forall \mathbf{y}\in\R,\,\mathbf{xy}\in\R</math> and <math>\forall \mathbf{x}\in\R,\forall \mathbf{y}\in\mathcal{V},\,\mathbf{xy}\in\R</math> would not support quaternions but would completely satisfy the given constraints.

And, in fact, there are arbitrarily many complex rules one could apply to achieve quaternion multiplication and still satisfy those constraints. This means that the specification is incomplete. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:49, 17 October 2015 (UTC)

:Yes, there are trivial geometric algebras.  <math>\R</math> is an obvious one.  But if the vector space is of dimension at least 3, and if no vector is degenerate (<math>\mathbf{x}^2 = 0</math>), then quaternions can be made.  Since I ask for an arbitrary dimension size, there is no need to require more.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 22:56, 17 October 2015 (UTC)

:The specification of geometric algebras is very much complete.  I did not invent them.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 22:57, 17 October 2015 (UTC)

::This does not mean that your task description is adequate.

::Vectors of arbitrary dimension combined with scalar product can't support quaternions. So basically what you are asking people to do is ignore the task description and copy the implementation. And that means that the task description is inadequate. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:00, 17 October 2015 (UTC)

:::From a vector space of arbitrary dimension (but at least 3), you can use the geometric product, not the scalar product, to create three bivectors i, j, k such that the subalgebra generated by (1, i, j, k) is isomorphic to the quaternion field.  I think that's basically what the task description says.  Maybe I could be a bit more verbose, but I think that is not necessary.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 23:07, 17 October 2015 (UTC)

:::: The trick, here, seems to be that in this context we use multiple conflicting concepts of the term "dimension" and, for related terms such as "scalar" and "vector". A thorough exposition would detail each of these uses and show - ideally through concrete examples - how each of them is relevant to the implementation, as well as how each of these uses is different from the other uses of the same word, and perhaps the phrasing conventions we should be using to distinguish between these cases. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:55, 19 October 2015 (UTC)

A current problem with this task is that it only tests for symmetric results.

In other words, the javascript implementation would work even if the multiply routine were changed from


```javascript
...
	for (var i in a) {
	    if (a[i]) {
		for (var j in b) {
		    if (b[j]) {
...
```


to


```javascript
...
	for (var i in a) {
	    if (a[i]) {
                var j= i;
		    if (b[j]) {
...
```


Obviously, "quaternions" which result from this kind of implementation would not be real quaternions. But, this is an easy mistake to make. And, to underline this, one of the J implementations currently features this mistake. Fixing it is trivial, but not necessary as the task is currently written.

But also display of asymmetric results seems both ugly and difficult to interpret, so I'll just mention that this is how the task is currently written. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 10:11, 21 October 2015 (UTC)
:The only way I can understand what you've just wrote here is by assuming you have again confused the geometric product and the inner product.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 10:25, 21 October 2015 (UTC)
::Hmm... you are correct. Testing this, I see that the javascript implementation does indeed give different results with this change. But looking at that flawed J implementation, the deviation from the javascript implementation is real. To simulate it in the javascript implementation, you would need to consider the i and j values as members of a list and only combine values from a/b where the i list and the j list had the same index. Anyways,, I'll let you come up with a change to the task description to catch this issue - if it matters to you. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 12:43, 21 October 2015 (UTC)

== The J solution might be correct but too small ==

I don't understand much of it, but I suspect it may be correct, considering it uses a 32-dimensions space, which is exactly what you need for the geometric algebra out of a 5-dimensions vector space, because <math>2^5 = 32</math>.

However, the required minimal dimension for the vector space was 32, not 5.  The tests do not verify that, so I will not flag the J solution as incorrect, but still I think it's worth mentioning.  It'd be nice if the solution could be modified in the future to account for a larger vector space.

--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 19:23, 18 October 2015 (UTC)

: wat? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:20, 18 October 2015 (UTC)

:: You're supposed to be able to call e(n) for n from 0 to 31 or more.  In other words the vector space <math>\mathcal{V}</math>is of dimension at least 32.  Your implementation does not allow for n > 4.  The dimension 32 in your code seems to be the dimension of the algebra, which is <math>2^n</math>.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 20:33, 18 October 2015 (UTC)

::: The Javascript implementation has essentially the same limitation. For example, try CGA.add(e(6), e(7)). 
::: Also, I rather doubt that 2^32 dimensions could be supported efficiently, unless most of them were unused. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:19, 18 October 2015 (UTC)

:::: Oh, indeed.  That's an error in the javascript code.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 21:24, 18 October 2015 (UTC)
:::: Or not.  You're right the javascript implementation can not go beyond 5 vector dimensions either.  My bad.  I'll rewrite the task description to relax the requirement on the number of dimensions.  It's not the main point of the task, anyway.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 21:29, 18 October 2015 (UTC)

:::: Jeez changed my mind again.  The javascript implementation is definitely wrong.  It will be tedious to change it but I will.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 21:35, 18 October 2015 (UTC)
:::: Done.  It was much simpler than I thought.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 21:50, 18 October 2015 (UTC)
::::: To be correct, you should verify using .hasOwnProperty() that you are not dealing with something inherited (some javascript environments create situations where arrays inherit properties which are not indices). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:58, 18 October 2015 (UTC)
::::::I've changed add and multiply.  I don't know about this hasOwnProperty stuff.  I'll look into it later.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 22:09, 18 October 2015 (UTC)
:::::::I've added a J implementation which supports (in a 64 bit J implementation) roughly 63 orthogonal dimensions (9223372036854775807 multivector dimensions). Not so many dimensions in a 32 bit J implementation, but of course most of those dimensions will be ignored in any context. 
:::::::Meanwhile, here's the [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/hasOwnProperty official doc on .hasOwnProperty()]. As for why you might want that, consider what happens if <code>Object.prototype.foo= function(){}</code> before the implementation ran...--[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:24, 19 October 2015 (UTC)

== Notation==
The task decription MUST  be coherent with the wikipedia page. A base is (e1 e2 ... en) not (e0 e1 ...en).
e0 must be reserved to the scalar part of a multivector.
Please state the algebra dimension (2^n) , n=??  in the subtasks definition.
--[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 10:04, 19 October 2015 (UTC)

:In this Wikipedia article the indices begin with 1, as is often the case for the notation of basis in linear algebra.  However, in computing it's more traditional to begin array indices with 0, for consistency with the C programming language, I suppose.  Thus a basis <math>(\mathbf{e}_1, \mathbf{e}_2,\ldots)</math> is often implemented as <tt>e(0), e(1), e(2)</tt> etc.  It's not an inconsistency between this particular article and task, it's more a slight notation difference between mathematics and computing.
:So I disagree that we should reserve the 0 index to the scalar part.  To me a scalar should be a regular scalar somehow "promoted" to the status of a multivector.  In my library I offered a way to create it with a call to <tt>e</tt> with no argument : <tt>e()</tt>
:--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 15:41, 19 October 2015 (UTC)
:Also, a scalar does not belong to <math>\mathcal{V}</math>, so there really is no reason to use a similar notation for it as for the basis of <math>\mathcal{V}</math>.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 15:45, 19 October 2015 (UTC)

:Put differently: all of the implementations here which I have inspected use index 0 to represent the "scalar part" of the "multivector". However, the scalar part of the multivector is not orthogonal to the elements of the orthogonal basis.  Specifically, in these implementations, e(0) here corresponds to index 1 of the multivector, e(1) corresponds to index 2 of the multivector, e(2) corresponds to index 4 of the multivector, e(3) corresponds to index 8 of the multivector and e(4) corresponds to index 16 of the multivector. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:14, 19 October 2015 (UTC)

== This task is a mess ==

I'm considering going back to verifying the axioms.  I would add a verification that <math>\mathcal{V}</math> is of dimension at least 5, though.  That should prevent implementations of trivial algebras and re-use of the quaternion.
In order to verify the contraction rule, I would require the test to be done on a large number of random vectors.

Possibly also displaying the multiplication table, as in the echolisp solution.

--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 14:54, 21 October 2015 (UTC)

I'm not sure what your issues are, but it seems to me that quaternions are an algebra containing a vector space \mathcal{V} and obeying these axioms:

:<math>\begin{array}{c}
(ab)c = a(bc)\\
a(b+c) = ab+ac\\
(a+b)c = ac+bc\\
\forall \mathbf{x}\in\mathcal{V},\,\mathbf{x}^2\in\R
\end{array}
</math>

You've added other constraints onto the task (such as the calculation that generates a 19, the orthonormal basis for \mathcal{V} and the requirement for two different embedded quaternion algebras), and I guess I can agree that that aspect is something of a mess. Mathematically speaking, I suppose that this hints at the need for further axioms... but computationally speaking, usually all we can ever provide is an implementation which approximates the math.

For example, no real computer implementation can satisfy the Peano postulates, so there will be cases where simple addition fails. And multiplication is even worse. Roughly half of the result domain for addition is typically missing, but for multiplication the size of the valid argument domain approximates the square root of the size of the result domain. So that sort of thing is going to be completely valid, mathematically...

Which I think has to do with why we get into concrete examples and/or concrete requirements - those might be "unnecessary" from a mathematical point of view, but they can be critically important from an implementation point of view. I think this also has something to do with why it's often a good idea to avoid an overly-general implementation...

So, anyways, from a mathematical purist point of view, I can't imagine many tasks can be anything but a "mess".

Though, I guess we could use Boolean addition and multiplication (greatest common divisor and least common multiple) or maybe modulo arithmetic - that would work around the overflow problems with regular addition and multiplication. This probably is not what you intend - might even violate the task purpose (whatever that is) - but these sorts of implementations would be a better fit if axiomatic correctness is what you are asking for.

Anyways...  if you can figure out what it is that you want, I guess go for it... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:35, 21 October 2015 (UTC)

:There is always an othonormal basis in a vector space with a scalar product.  And the inner product always defines a scalar product.  So these are not additional constraints.  The only additional constraints I added were the vector dimension of at least five and the euclidean metric. 
:I'm not sure where you are going with your suggestion of boolean addition, multiplication or modular arithmetics.  I'm pretty sure such operations would not allow the inclusion of a vector space.
:You seem to keep questioning the pertinence of the axioms but again, I did not invent them.  They look fine to me.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 15:55, 21 October 2015 (UTC)
::Nothing in the text I quoted requires the existence of a scalar product. You did mention scalar product elsewhere, but not in the quoted axioms. See also: http://mathworld.wolfram.com/VectorSpace.html --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:37, 21 October 2015 (UTC)
:::I was referring to your mention of the orthonormal basis being a constraint to the task.  The existence of a scalar product, and thus of an orthonormal basis, is a consequence of the axioms.  It is thus not a constraint.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 19:02, 21 October 2015 (UTC)
::::Sure, all vector spaces have a basis but there's nothing in those axioms that say anything about the dimension of that basis. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:46, 21 October 2015 (UTC)
