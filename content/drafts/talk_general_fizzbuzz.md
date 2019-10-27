+++
title = "Talk:General FizzBuzz"
description = ""
date = 2018-11-08T19:09:53Z
aliases = []
[extra]
id = 18844
[taxonomies]
categories = []
tags = []
+++

==20==
20 needs explaining in the task description I think --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:41, 8 March 2015 (UTC)

===JS ES6 - avoiding a newly-introduced run-time error===

Hi [[User:MartinBRosenberg|MartinBRosenberg]] I appreciate your taking an interest in my JS ES6 rules based version. I do like some of your edits, thank you !  

However:
# '''SyntaxError: Can't create duplicate variable: 'defaultRules'''' on second run is (in all typical JS interpreter embeddings – browsers etc) the result of your removal of the outer module bracketing, If we paste your modified code in the console of a browser, it will run the first time, but if we then repeat, the only result is an error, because you are now defining constants in the global namespace of a persistent JS interpreter instance. Apart from the perennial need to abate global namespace pollution, there is the more pressing problem that a 'const' name can not be assigned twice.  
# You will find a few hundred JS examples which I have contributed here, using the Haskell name '''enumFromTo''' for ''integer enumerations'' – the semantics of which does differ from the Python pragmatic convention, but is very far from denotationally 'incorrect' as you suggest. (The Haskell tradition is rather rigorous about denotational semantics, and the use of 'through' vs 'to' in that English sense is more familiar to US than to UK (including Oxford and Glasgow) usage. See, for example, the Haskell documentation of enumFromTo: [[http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:enumFromTo|enumFromTo]] [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 18:34, 8 November 2018 (UTC)


Remember that JS interpreters are now variously embedded – not only in browsers but also for various kinds of scripting outside the browser. Run the same JS script twice in macOS script editor for example, or from a script launch button in one of the Omni application JS embeddings, and code which worked before your edit now fails and generates an error. I hope you are not going to make a solemn pilgrimage through all my JS code examples on Rosetta Code, and dutifully introduce the same run-time error everywhere :-)  (In these scripting contexts, the default name-space is typically the global namespace, which is typically persistent). [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:02, 8 November 2018 (UTC)
