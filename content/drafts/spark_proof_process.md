+++
title = "SPARK Proof Process"
description = ""
date = 2010-08-19T17:09:15Z
aliases = []
[extra]
id = 8028
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]
=The SPARK Proof Process=

The [[SPARK|'''SPARK''']] language supports the formal proof of program properties.

==Verification Conditions==
The proof process starts with a set of verification conditions, output by the SPARK Examiner. The verification conditions generated depend on the annotations that have been specified in the SPARK source and the properties that they specify.

Each verification condition contains:
*data that is known about the state of the program at the point in the sequence of statements for which the verification condition is generated, and
*checks that must be passed at that point for the relevant property to hold.

Verification conditions from a SPARK program are not expressed in SPARK, but in a modelling language for algorithms, called FDL, which is used by all the SPARK proof tools.

A verification condition is True if the checks (the conclusions) can be proved to follow logically from the known state (the hypotheses).

==Proving Verification Conditions==
The verification conditions created by the SPARK Examiner are processed first by the SPARK Simplifier - a tool that applies a sequence of logical deduction steps to each verification condition. This normally proves 95-99% of all verification conditions.

For unproven verification conditions remaining after simplification, the analyst has to determine how to deal with each one.

An unproven verification condition may be unprovable, or may be provable but requiring more complex proof strategies than the Simplifier has available.

An '''unprovable''' verification condition may indicate a failure of the [[Ada]] code to satisfy the required property.  If this is the case then the [[Ada]] code of the program must be modified to remove that failure.

An unprovable verification condition where there is no failure in the [[Ada]] code will be caused by the Examiner having insufficient information about the program state at the point that the verification condition is generated.  This can be resolved by adding further annotations that make the required information available to the Examiner where it is required.  Subsequent reanalysis should result in the verification conditions becoming provable.

If a verification condition is '''provable''' but not proved by the Simplifier then there are a range of possibilities open to the analyst.
*Informally document a manual proof of the verification condition. This proof will not be checked by any SPARK tool so should be validated by other analysts.
*Validate a manual proof of a verification condition by entering each step of the proof into a further SPARK tool - the Proof Checker.  This tool ensures that each step of the proof is supported by a valid logical rule.
*Add further annotations to the program that convert the unproven verification condition into a sequence of simpler verification conditions that the Simplifier does prove.
*Provide the Simplifier with additional proof strategies (called 'user rules') that allow the Simplifier to complete the proof.

These possibilities allow the analyst to complete the proofs of verification conditions by methods ranging across the full range of formality.
