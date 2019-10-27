+++
title = "Village Pump/Approximate fit solutions"
description = ""
date = 2011-08-01T18:51:40Z
aliases = []
[extra]
id = 10148
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Closr fit solutions
|summary=New tag for "limited solutions"
}}
In some instances, it may not be possible for a language to provide an exact match solution to the task, due to limitations of the language or its utilized subcomponents. It might be a good idea to have a "limited solution" marker would be a good idea. (Similar to the "incorrect solution" tag), that reads something like 'The solution provided does not meet the exact specifications of the task, due to limitations of the language or its subcomponents. The author has tried to provide an "approximate fit" or "best fit" solution based on those limitations'. I know there are tasks on the wiki, where the solutions is a close approximation, rather than an exact fit. This might be quite useful. This would give better scope for making comparisons of languages, because it would be possible to see the limitations of the language and it tools for a particular task, (rather than the implementation being simply omitted, giving us nothing to use for evaluation of the language at all). [[User:Markhobley|Markhobley]] 09:55, 24 July 2011 (UTC)

:Unfortunately, the example for which you would like to add the tag: [[Pi#GUISS]] is reason to reject the notion as one gain so little when comparing that language example with the others. That page would be better if the GUISS example were dropped altogether. In light of this, I would rather we continue our current "ad-hoc" methods as we don't currently have a need to go beyond that. --[[User:Paddy3118|Paddy3118]] 12:36, 24 July 2011 (UTC)
::There are some other examples: some of the timer related tasks, file handling tasks, I think some of the UNIX shell solutions, Unix/Windows incompatibilities, levels of support with different concepts, etc. I'll paste any that I find. [[User:Markhobley|Markhobley]] 14:23, 24 July 2011 (UTC)
:::The REXX solution for "Flatten a List", works with a string representing a list rather than a list itself. That is a close approximation (and acceptable IMHO). [[User:Markhobley|Markhobley]] 16:43, 24 July 2011 (UTC)

:I see this as a consequence of your [[Pi#GUISS]] "solution" being rejected and you are trying yet another way to weasel out of it.  I don't think there is any place for such a template.  For a task, there are correct solutions, and there are not-so-correct soutions written in good faith.  What constitutes "in good faith" is subjective, so people must be allowed to scrutinize them instead of letting them hide under some umbrella tag.  Case in point: by most people's standard, the GUISS pi digits "solution" would not be a good faithed attempt (want a bet?), yet you still want a tag there so you can point at it and say "don't remove it, it's tagged such, thus acceptible". --[[User:Ledrug|Ledrug]] 19:27, 24 July 2011 (UTC) 
::It's nothing to do with that. I am trying to make the documents as good as they can be, and I think being relaxed and inclusive is the best way forward. I don't think we need to be quite so strict with an "this is not an exact match, so it must be deleted" type of approach. After all, the solution was valid, whether it gets published here or not. I can always publish it on another website. That is not a problem. It's just convenient to work with Rosetta Code, because it has similar goals to a project that I have been developing, and the infrasture is already in place here, and I quite like how the project is evolving. FWIW, a policy would alleviate the need for the tag, or it is easy to add "If your language cannot achieve this task exactly, then make an approximate implementation, and state divergencies and limitations in the provided solution" to every task. But I think this is extreme. A policy would be sufficient (if only really for the purpose of clarification of conflict between different developers ideas). [[User:Markhobley|Markhobley]] 20:47, 24 July 2011 (UTC)
:::We do include examples which aren't exact matches. Usually they are discussed in the talk pages and annotated properly (or changed/replaced) if there is a problem. The problem with the one in [[pi]] is that it doesn't seem to match any part of the task except displaying part of the value of pi. It does not calculate, and it doesn't continuously show digits except to the extent of a stored number. It's understandable to add a solution for a task which has (for example) 5 requirements and the solution meets 4 of them as long, as the one requirement it misses isn't the real point of the task (like a display issue on a calculation task) and as long as it's noted. It is not acceptable to add a solution to a task which does not fit "the spirit of the task" (like using a completely different type of algorithm than what the task asks for). In short, the policy is implicitly in place and no template is needed. Any disputes should be taken to their talk pages, and both sides should try to keep an open mind (and please keep it nice...don't attack). --[[User:Mwn3d|Mwn3d]] 21:06, 24 July 2011 (UTC)

We do (or, rather, I do, and others tend to follow my lead) have a policy of allowing approximate-fit solutions. I do like the idea of a {{tmpl|approximate}} as a "subclass" of {{tmpl|incorrect}}, which is used to enumerate acknowledged deviations from the task description. Pages with {{tmpl|approximate}} can be listed on the "unimplemented in X" pages, to draw attention to them and allow them to be fixed, where possible. --[[User:Short Circuit|Michael Mol]] 12:02, 26 July 2011 (UTC)
:If we have this template (which I still don't think we need but I understand categorizing these examples) I would want it to be plain text rather than an infobox. And we need to make sure to differentiate it from {{tmpl|incorrect}} and {{tmpl|incomplete}}. --[[User:Mwn3d|Mwn3d]] 12:26, 26 July 2011 (UTC)

This is not a good place to continue the discussion about [[Pi#GUISS]]. --[[User:Short Circuit|Michael Mol]] 12:02, 26 July 2011 (UTC)
==Policy==

We could just have a policy that allows approximations, if an exact fit is not available, providing that the solution states the limitations and any differences in behaviour from the task requirements. [[User:Markhobley|Markhobley]] 16:49, 24 July 2011 (UTC)
:Hi Mark, we have such a policy. It is explained above. It may rely on discussion in the talk pages but has worked in the past, and should work in the future. How about listening to the emerging [[wp:Consensus decision-making|consensus]]? --[[User:Paddy3118|Paddy3118]] 19:11, 26 July 2011 (UTC)

:: I like the idea of tagging what we're so far calling "approximate" solutions: solutions to problems similar in spirit to the task, when the task itself cannot be solved by the language in question.  For example, if your language is sandboxed, you're not going to be able to solve a task that requires you to save a file in an arbitrary location.  But rather than ignoring the task, it would be useful to write a quasi-solution that puts the data into local storage, if you have that ability.
:: This is a different situation from incorrect programs or "the language can do the task, but it's difficult and I'm lazy".
:: [[User:CRGreathouse|CRGreathouse]] 18:51, 1 August 2011 (UTC)
