+++
title = "Talk:Word frequency"
description = ""
date = 2019-07-13T16:35:01Z
aliases = []
[extra]
id = 21562
[taxonomies]
categories = []
tags = []
+++

==Note from original author==
When it doubt assume you have the freedom to define the requirements as whatever you feel is most appropriate in your language of choice. --[[User:Kentros|Kentros]] ([[User talk:Kentros|talk]]) 01:31, 31 August 2017 (UTC)

==why entered as a ''task'' instead of ''draft task''?==
Why was this entry entered as a   ''task''   instead of a   ''draft task''?   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:08, 16 August 2017 (UTC)

... ahhh ...   I see that this ''task'' was demoted to a ''draft task'' by   [[User:Paddy3118|Paddy3118]].   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:34, 16 August 2017 (UTC)

==task clarification==
I assume we are to code programs to handle the general case, not just the file specified/mandated to be used as a test case.
:True.  Originally, I suggested a specific text file.  I've taken that off and now leave it to the example writer. --[[User:Kentros|Kentros]] ([[User talk:Kentros|talk]]) 01:32, 31 August 2017 (UTC)

What is a "word"?
:A single distinct meaningful element of speech. I speak words. How speech is written is very much language, time and individual dependent. Linguistically Speech, Speach, or even Speych have been used for the same word. Don't mention Donaudampfschifffahrtselektrizitätenhauptbetriebs. For the purpose of this task I would suggest using the concept of 'orthographic word' which works well for English. Not well for Ancient Greek and Egyptian.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:46, 17 August 2017 (UTC)
Is 1997 a word?   How about 20?   How about twenty? 

What letters can be included in a word?

There are a lot of French accented letters in the prescribed text, but are we to be limited to   ''just''   the French accented letters? 

German?     Czech?     Which dialects of Greek?     Logographic kanji?     Kana?    

What other characters can be included in a word?
:Characters not included in the alphabet are called logograms. They include numbers, foreign letters and mojos. I💖NY is 1 orthographic word. The sentence '1 orthographic word' contains 3 orthographic words'--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:58, 17 August 2017 (UTC)
Are words that are hyphenated one word or two?
:1 orthographic word--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:52, 17 August 2017 (UTC)
What about words like:     '''jack-o'-lantern'''
:1 orthographic word--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:51, 17 August 2017 (UTC)
What about split words across lines   (if there are possi-

bly present)?

Are words that contain an apostrophe to be included   (such as '''let'''&apos;'''s''')?

What about words that contain non-Latin (Roman) letters?

As it happens, those non-Latin letters don't show up in the   ''top ten''.

What '''exactly''' is the ''text''   (start and stop)   that is contained in the web-page to be used? 

Should we also use the prologue and epilogue of the   ''Project Gutenberg''   along with the book's text?

Wouldn't it be a lot simpler to have a simple (and complete) text file to download   [with no (de-)assembly, editing, or text massaging required]? 

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:08, 16 August 2017 (UTC)

-----

It seems the original task author used the regexp \w+ in the Clojure and first  Python examples. Maybe he should expand on what \w+ matches and define it as the meaning of a word for the purposes of the task? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:09, 17 August 2017 (UTC)
:\w means [A-z0-9]. This could be extended to include accented Latin characters: [A-z0-9À-ÿ]. But this would not change that the answers are wrong. There are 41082 occurrences of the word 'the', not 41036. The text contains for instance "BOOK SECOND--THE FALL". I suspect that the Python and Clojure solutions miss this.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:51, 18 August 2017 (UTC)
:They are probably missing the two of the three occurrences of 'the' in:

```txt

"The beds," pursued the director, "are very much crowded against each
other."

"That is what I observed."

"The halls are nothing but rooms, and it is with difficulty that the air
can be changed in them."

```
--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:01, 18 August 2017 (UTC)

```txt

So long as there shall exist, by virtue of law and custom, decrees of
damnation pronounced by society, artificially creating hells amid the
civilization of earth, and adding the element of human fate to divine
destiny; so long as the three great problems of the century--the
degradation of man through pauperism, the corruption of woman through
hunger, the crippling of children through lack of light--are unsolved;
so long as social asphyxia is possible in any part of the world;--in
other words, and with a still wider significance, so long as ignorance
and poverty exist on earth, books of the nature of Les Misérables cannot
fail to be of use.

```

:They also need to catch the 'the' after century--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:06, 18 August 2017 (UTC)


::What I mean is, Nigel, that what constitutes a word for the purposes of this task, needs to be defined. Without such a definition, any definition of a word becomes suspect. It is best that the tasks definition of a word is succinct and defined in such a way as to be reproducible in many languages. "Whatever wordprocessor X defines as a word in its word count" doesn't seem like a good enough description ''for the purposes of a task''. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:02, 18 August 2017 (UTC)

:::Really, the only thing at all in question to my mind is, is underscore a letter or not? On the face of it, it seems clear; no, of course not. So should the word "_The" in the text be counted as "_the" or "the"? The \w assertion in PCRE (which most languages use directly or emulate) includes underscore for historical reasons, so "_the" and "the" are counted as different words. On the other hand, does the word "Alèthe" contain any non-letter characters? It is awfully narrow-minded to insist that "if you can't fit it in 7 bits, it ain't a character." That being said, I think disregarding hyphenated words and contractions with embedded apostrophes when counting words is ridiculous too so I added a second version which accounts for them, but it doesn't meet the task requirements as written (hence it being a ''second'' version). --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 23:44, 18 August 2017 (UTC)
::::For the purposes of this task, which requires the top 10 words, defining a word as [A-z0-9À-ÿ]+ works well and gives the same answers as Perl6 (41088 for the and 14596 for a). Obviously Nigel's is one word, but it would require a lot of possession to promote the single character s into the top 10 words.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:23, 19 August 2017 (UTC)

==Using Microsoft Word 2010 to count words==
I opened this task with MS Word 2010 and asked it to count the occurrences of 'the' (all word forms English). It thinks there are 41082.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:17, 17 August 2017 (UTC)

==output (using REXX) for the top 1,000 words (including words that contain apostrophes and/or accented letters)==
When using the REXX version 1 showing the top 1,000 words   (which supports accented letters that are in the file specified,   ''and''   support for words that contain an embedded apostrophe):
{{out|output|text=  when using the following as input:     <tt> ,   1000 </tt>}}
<pre style="height:140ex">
574,122  words found  (23,414 unique)  in  67,663  records read from file:  les_mes.TXT

                                    word    rank   count
                                    ════   ══════ ═══════
                                     the      1   41,088
                                      of      2   19,949
                                     and      3   14,942
                                       a      4   14,595
                                      to      5   13,950
                                      in      6   11,214
                                      he      7    9,607
                                     was      8    8,620
                                    that      9    7,826
                                      it     10    6,535
                                     his     11    6,470
                                      is     12    6,198
                                     had     13    6,182
                                   which     14    5,148
                                    with     15    4,528
                                      on     16    4,470
                                      at     17    4,069
                                    this     18    4,008
                                     not     19    3,801
                                       i     20    3,704
                                     you     21    3,686
                                      as     22    3,255
                                     one     23    3,162
                                     for     24    2,987
                                     him     25    2,942
                                    have     26    2,795
                                     her     27    2,636
                                   there     28    2,628
                                     who     29    2,552
                                     all     30    2,485
                                      by     31    2,478
                                    from     32    2,453
                                     she     33    2,439
                                      be     34    2,389
                                     are     35    2,167
                                      an     36    2,127
                                    they     37    2,118
                                     but     38    2,048
                                      no     39    1,971
                                     man     40    1,929
                                    what     41    1,828
                                    were     42    1,824
                                    said     43    1,796
                                    been     44    1,516
                                  marius     45    1,373
                                    when     46    1,366
                                      we     47    1,291
                                   their     48    1,252
                                    jean     49    1,236
                                    will     50    1,232
                                     two     51    1,214
                                      so     52    1,191
                                      my     53    1,170
                                      me     54    1,154
                                    more     55    1,130
                                 himself     56    1,087
                                     has     57    1,080
                                    them     58    1,068
                                   would     59    1,053
                                 valjean     60    1,050
                                    then     61    1,038
                                   these     62    1,008
                                     did     63      994
                                    into     64      992
                                     out     64      992
                                    like     66      985
                                  little     67      983
                                      or     68      957
                                      do     69      932
                                    very     70      923
                                      up     71      921
                                 cosette     72      920
                                     its     73      893
                                       m     74      887
                                   other     75      886
                                     old     75      886
                                    than     77      866
                                    good     78      801
                                     day     79      798
                                    made     80      788
                                    some     80      788
                                    only     82      784
                                    time     83      762
                                    your     84      760
                                 chapter     85      744
                                      if     86      738
                                 nothing     87      736
                                   those     88      735
                                 without     89      699
                                   could     90      678
                                     rue     91      667
                                    well     92      643
                                   about     92      643
                                      de     94      642
                                   where     95      615
                                     men     96      612
                                     say     97      601
                                   first     98      582
                                    here     98      582
                                     any    100      578
                                  father    101      569
                                     now    102      568
                                  should    103      566
                                  moment    104      563
                                    over    105      560
                                    come    106      559
                                     see    107      544
                                    hand    108      543
                                    eyes    109      538
                                   after    110      531
                                 through    110      531
                                      am    112      528
                                    must    113      523
                                    know    114      520
                                      us    115      519
                                   still    116      518
                                   great    117      512
                                    even    118      511
                              thénardier    119      504
                                    same    119      504
                                    just    121      502
                                  before    122      501
                                 thought    123      496
                                    once    124      485
                                    upon    125      484
                                    door    126      483
                                   three    127      477
                                     how    128      472
                                   being    129      469
                                    head    130      468
                                   under    131      466
                                  people    132      464
                                    each    133      456
                                      go    134      455
                                   again    134      455
                                    name    136      454
                                   house    137      448
                                    sort    137      448
                                   night    139      447
                                   child    139      447
                                   light    141      436
                                  longer    142      432
                                   every    143      430
                                   place    144      429
                                     let    145      428
                                    such    146      426
                                     way    147      424
                                    back    148      423
                                    life    148      423
                                  javert    150      422
                                   right    151      421
                                   young    152      420
                                     can    153      419
                                    long    154      416
                                   paris    155      412
                                    went    155      412
                                   woman    157      409
                                   saint    158      402
                                    took    158      402
                                   never    158      402
                                  seemed    158      402
                                    seen    162      384
                                  called    162      384
                                    four    164      378
                                    take    165      372
                                    make    166      370
                                    love    167      369
                               something    168      365
                                monsieur    168      365
                                   years    170      363
                                    whom    171      358
                                     may    172      356
                                    left    172      356
                                     air    174      355
                                  though    174      355
                                     god    176      350
                                  mother    177      348
                                   point    177      348
                                   shall    179      347
                                    does    180      344
                                   whole    180      344
                                   voice    182      343
                                    last    183      339
                                   might    183      339
                                  street    185      338
                                     our    186      336
                                  turned    187      332
                                    most    187      332
                                     own    189      331
                                    down    189      331
                                    much    191      329
                                  almost    192      328
                                    face    193      323
                                   thing    194      321
                                  having    195      318
                                   order    195      318
                                 between    195      318
                              everything    195      318
                                 towards    195      318
                                  always    200      312
                                  passed    201      310
                                 replied    202      309
                                 hundred    203      306
                                     off    204      305
                                    away    205      304
                                    felt    205      304
                                 against    207      303
                                  things    207      303
                                    soul    209      302
                                    room    209      302
                                    side    211      300
                                gavroche    211      300
                                    poor    213      299
                                 because    214      298
                                    word    215      297
                                   going    216      296
                                 certain    217      295
                                   taken    217      295
                               barricade    219      293
                                  behind    220      291
                                  bishop    221      288
                                      la    221      288
                                    wall    221      288
                                 another    224      286
                                  francs    224      286
                                    five    226      285
                                     put    227      284
                                   while    227      284
                                     few    227      284
                                     too    230      283
                                    fact    231      281
                                    hour    231      281
                            fauchelevent    233      280
                                   found    233      280
                                     saw    233      280
                                   heard    236      278
                                    came    237      276
                                   black    238      272
                                 entered    239      270
                                    near    240      269
                                     end    241      267
                                   heart    242      265
                               madeleine    243      264
                                     why    244      263
                                   words    245      260
                                     bed    246      258
                                  madame    247      257
                                enjolras    248      256
                                     yes    249      253
                                 evening    250      250
                              themselves    250      250
                                    work    250      250
                                    dead    253      249
                                      ah    254      248
                                     six    255      247
                                   white    256      245
                                   death    257      240
                                   since    258      239
                                remained    258      239
                                      le    260      238
                                  garden    260      238
                                    open    262      237
                                     set    263      236
                                    many    263      236
                                    full    265      235
                                 morning    265      235
                               sometimes    267      233
                                   began    268      232
                                children    269      229
                                    half    269      229
                                   table    271      228
                                    thus    272      227
                                    done    272      227
                                   hands    272      227
                                    mind    275      226
                                    also    276      225
                                   think    277      224
                                  itself    277      224
                                terrible    279      223
                                     get    279      223
                                  become    281      222
                            gillenormand    281      222
                                  opened    283      220
                                     nor    284      219
                                 beneath    285      216
                                    girl    286      215
                                anything    287      214
                                 herself    288      213
                                   don't    289      212
                                  person    289      212
                                   large    289      212
                                   human    292      210
                                    feet    293      208
                                    book    294      207
                                  second    295      206
                                   alone    295      206
                                    both    297      204
                                   water    298      203
                                  police    299      201
                                   world    299      201
                                    arms    299      201
                                     far    302      199
                                    fell    303      197
                                    give    304      196
                                  matter    305      195
                                    idea    306      194
                                  return    306      194
                                  twenty    306      194
                                    days    309      192
                                   added    309      192
                                   whose    311      191
                                 already    312      190
                                   one's    313      187
                                thousand    313      187
                                   above    315      186
                                  corner    316      185
                               exclaimed    317      183
                                 fantine    318      182
                                returned    318      182
                                  window    318      182
                                possible    321      181
                                     sir    322      179
                                    fire    323      178
                                   earth    323      178
                                 however    323      178
                                   louis    326      176
                                    case    326      176
                                   front    326      176
                                   round    329      174
                                  france    330      173
                                    tell    330      173
                                   grave    330      173
                                   later    330      173
                                    held    334      171
                              courfeyrac    334      171
                                    true    336      170
                                    knew    336      170
                                   speak    336      170
                                    cold    336      170
                                   among    340      168
                                 resumed    340      168
                                    less    342      167
                                    part    343      166
                                  saying    344      165
                                   sewer    344      165
                                     age    344      165
                                     new    347      164
                                     arm    347      164
                                    look    347      164
                                  manner    350      163
                              revolution    350      163
                                    iron    350      163
                                     yet    350      163
                                 silence    354      162
                                  glance    355      161
                                  rather    356      160
                                     low    356      160
                                      oh    358      159
                                    lost    358      159
                                  became    358      159
                                  raised    358      159
                                    pass    362      157
                                    hair    363      156
                                 convent    363      156
                                   stone    363      156
                                     des    363      156
                                   women    363      156
                                      du    363      156
                                  sister    369      155
                                appeared    369      155
                                 o'clock    371      154
                               jondrette    371      154
                                  within    371      154
                                   forth    371      154
                                   stood    371      154
                                 reached    371      154
                                  caught    377      153
                                    read    378      152
                                   happy    379      150
                                 perhaps    380      149
                               following    380      149
                            nevertheless    380      149
                                  placed    380      149
                                   small    384      148
                                  beheld    384      148
                                    turn    386      147
                                    wine    387      146
                                    form    387      146
                                   grand    387      146
                                    coat    390      145
                                   state    390      145
                                  making    390      145
                                  myself    393      144
                                    road    393      144
                                  shadow    395      143
                                 society    395      143
                                  nature    395      143
                                     joy    395      143
                                   hours    399      142
                                presence    400      141
                                 chamber    400      141
                                    fine    400      141
                                   piece    403      140
                                  ground    403      140
                                darkness    403      140
                                  letter    403      140
                                    fall    407      139
                                question    407      139
                                  battle    407      139
                                    foot    407      139
                                   paper    407      139
                                  closed    407      139
                                   sight    413      138
                                    shop    413      138
                                suddenly    415      137
                                     law    416      136
                                     war    416      136
                                    gave    416      136
                                    live    416      136
                                    find    420      135
                                   trees    420      135
                                   times    420      135
                               beginning    423      134
                                 present    423      134
                                    told    423      134
                                   asked    423      134
                                   close    423      134
                                     eye    423      134
                                napoleon    429      133
                                     red    429      133
                                   short    429      133
                                  better    432      132
                                   money    432      132
                                  public    432      132
                                moreover    432      132
                                 brought    432      132
                                  looked    432      132
                                  seized    432      132
                                  during    439      131
                                 ancient    439      131
                                    rose    439      131
                                    want    439      131
                                  reader    443      130
                                  taking    443      130
                                 neither    443      130
                                   smile    443      130
                                  others    443      130
                                      th    448      129
                                  course    448      129
                                     lay    448      129
                                  pocket    448      129
                                 english    452      128
                                 century    452      128
                                   enter    452      128
                                   force    452      128
                                   knows    452      128
                                   sound    452      128
                               necessary    458      127
                                   given    458      127
                                    ever    458      127
                               continued    458      127
                              understand    458      127
                                    rest    463      126
                                   along    463      126
                                    thou    465      125
                                    it's    465      125
                                 quarter    467      124
                                 history    468      123
                             grandfather    468      123
                                  around    468      123
                                  seated    468      123
                                    call    472      122
                                   cried    472      122
                                  beside    472      122
                                 strange    472      122
                                    able    472      122
                                daughter    472      122
                                 streets    472      122
                               perceived    479      121
                               direction    479      121
                                 visible    479      121
                                   guard    482      120
                              mysterious    482      120
                                  formed    482      120
                                   gazed    482      120
                                    dark    486      119
                                     die    486      119
                                 convict    486      119
                              impossible    489      118
                                    past    489      118
                                     ten    489      118
                                   floor    489      118
                                  filled    489      118
                                  bottom    494      117
                                  except    495      116
                                     cut    495      116
                                   seven    495      116
                                     sun    495      116
                                    cast    495      116
                                    year    500      115
                                  broken    500      115
                                    town    500      115
                                   means    500      115
                                charming    504      114
                                    king    504      114
                                  hardly    504      114
                                 country    504      114
                                probably    504      114
                                 whether    504      114
                              melancholy    504      114
                                 galleys    504      114
                                  single    512      113
                                    sous    512      113
                                    laid    512      113
                                    drew    512      113
                                 shadows    516      111
                                waterloo    516      111
                                profound    516      111
                            mademoiselle    516      111
                                 arrived    516      111
                                    paid    516      111
                                    says    516      111
                                   comes    523      110
                                  cannot    523      110
                                  french    523      110
                                   mayor    523      110
                               beautiful    523      110
                              appearance    523      110
                                  morrow    523      110
                                   makes    530      109
                                 outside    530      109
                                carriage    530      109
                                   least    530      109
                                   blood    530      109
                                   doubt    530      109
                               happiness    536      108
                                  living    536      108
                                received    536      108
                                    post    539      107
                                  depths    539      107
                                   cross    539      107
                                 lighted    539      107
                                     bad    543      106
                                 general    543      106
                                  escape    543      106
                                followed    543      106
                                    hear    543      106
                                together    543      106
                                      ii    549      105
                               bourgeois    549      105
                                    hole    549      105
                                    step    549      105
                             disappeared    549      105
                                   eight    549      105
                               boulevard    549      105
                                   often    556      104
                                   known    556      104
                                   truth    556      104
                                   bread    556      104
                                  gloomy    556      104
                                  stones    556      104
                                   quite    562      103
                                   slang    562      103
                                    hold    562      103
                                    evil    562      103
                                movement    562      103
                                    shot    567      102
                                   lived    567      102
                                  nearly    567      102
                                    gone    567      102
                                 leblanc    567      102
                                     use    572      101
                                  thirty    572      101
                                   epoch    572      101
                                  family    572      101
                               cosette's    572      101
                                   walls    572      101
                                  fallen    572      101
                              recognized    572      101
                                 immense    572      101
                                 carried    572      101
                                   mouth    572      101
                                   horse    583      100
                                progress    583      100
                                   girls    583      100
                                  caused    583      100
                                    need    583      100
                                  really    583      100
                                 hideous    583      100
                                  effect    590       99
                                 mingled    590       99
                                  heaven    590       99
                                  pretty    590       99
                                  houses    590       99
                                    wish    590       99
                                  coming    590       99
                               certainly    590       99
                                   sword    590       99
                                  future    599       98
                                  social    599       98
                                    army    599       98
                              conscience    599       98
                                  that's    599       98
                                     pay    599       98
                                 passing    605       97
                                     ago    605       97
                                   until    605       97
                                 liberty    605       97
                                   steps    605       97
                                yourself    605       97
                                 brother    611       96
                                   third    611       96
                                   chair    611       96
                               attention    611       96
                                  struck    611       96
                                      iv    616       95
                                   ideas    616       95
                                  months    616       95
                                 flowers    616       95
                                   teeth    620       94
                                  breath    620       94
                                    duty    620       94
                               gutenberg    623       93
                                   midst    623       93
                                  remain    623       93
                                    spot    623       93
                                  candle    623       93
                                 project    628       92
                                produced    628       92
                                     iii    628       92
                                   sleep    628       92
                                   below    628       92
                                    body    628       92
                                     hat    628       92
                                    soon    628       92
                                  enough    628       92
                                 becomes    637       91
                                  mabeuf    637       91
                                   forty    637       91
                                 moments    637       91
                                    bent    637       91
                                    wife    642       90
                                    city    642       90
                                 covered    642       90
                               pontmercy    642       90
                                distance    646       89
                                  doctor    646       89
                                 fifteen    646       89
                                   loved    646       89
                               frightful    646       89
                                  served    646       89
                                 further    646       89
                                    fear    646       89
                                    sign    646       89
                                 unknown    655       88
                                    wind    655       88
                                   seems    655       88
                                  simple    655       88
                                   peace    655       88
                                   glass    660       87
                                occasion    660       87
                                 allowed    660       87
                              understood    660       87
                                  fellow    660       87
                                    gaze    660       87
                                 instant    660       87
                                    line    660       87
                                singular    668       86
                                   leave    668       86
                                  slowly    668       86
                             monseigneur    671       85
                                     bit    671       85
                                although    671       85
                                 windows    671       85
                                thoughts    671       85
                                    tone    671       85
                                    brow    671       85
                                     sad    671       85
                              formidable    671       85
                                 minutes    671       85
                                  square    681       84
                                 friends    681       84
                                  secret    681       84
                                    high    681       84
                                  forest    681       84
                                   souls    681       84
                                rendered    681       84
                            montparnasse    688       83
                               grantaire    688       83
                                    pale    688       83
                                   laugh    688       83
                                    none    688       83
                                  prison    688       83
                                enormous    688       83
                                  walked    688       83
                                 husband    688       83
                                 uttered    688       83
                                   vague    698       82
                                stranger    698       82
                                  misery    698       82
                               succeeded    698       82
                                   knees    698       82
                                  either    698       82
                                  halted    698       82
                                     top    698       82
                              combeferre    698       82
                                   power    707       81
                                 obliged    707       81
                               according    707       81
                                   reply    707       81
                                    blue    707       81
                                     sur    707       81
                                prisoner    713       80
                                   watch    713       80
                                 justice    713       80
                                 dressed    713       80
                                  killed    713       80
                                     ran    713       80
                                    june    719       79
                                    view    719       79
                                  spring    719       79
                                 éponine    719       79
                                   heads    719       79
                                    home    719       79
                               presented    719       79
                                  sombre    719       79
                                     big    719       79
                                     eat    719       79
                                      re    729       78
                                 despair    729       78
                                    tree    729       78
                                 serious    729       78
                                 existed    729       78
                                   fixed    729       78
                                horrible    729       78
                                  middle    729       78
                                   doing    729       78
                                     i'm    729       78
                                  porter    729       78
                                  number    740       77
                                    rich    740       77
                                 service    740       77
                                  asleep    740       77
                                     son    740       77
                                 destiny    740       77
                                  church    740       77
                                   spoke    740       77
                               perfectly    740       77
                                 several    740       77
                                 chimney    740       77
                                    sure    740       77
                                     gun    740       77
                             montfermeil    753       76
                                entering    753       76
                                 fashion    753       76
                                  whence    753       76
                                    deal    753       76
                                  terror    753       76
                                   noise    753       76
                                magloire    753       76
                                   crime    753       76
                                    care    753       76
                                     sky    753       76
                                pavement    753       76
                                  cannon    753       76
                                   forms    766       75
                                   petit    766       75
                                   begun    766       75
                               concealed    766       75
                                 touched    766       75
                                   burst    766       75
                              motionless    766       75
                                     inn    766       75
                                     cry    766       75
                                  thrust    766       75
                                 bossuet    776       74
                                   march    776       74
                                thinking    776       74
                                   man's    776       74
                                 besides    776       74
                                   fault    776       74
                                    feel    776       74
                                composed    776       74
                                standing    776       74
                                deserted    776       74
                                   names    786       73
                                    hope    786       73
                                    rain    786       73
                                    real    786       73
                                speaking    786       73
                                   aside    786       73
                                  forced    786       73
                                  honest    786       73
                                    free    786       73
                                    mass    795       72
                                    wild    795       72
                                    gate    795       72
                                    amid    795       72
                                       d    795       72
                                   hence    795       72
                                   haste    795       72
                                    neck    795       72
                                    walk    795       72
                                  paused    795       72
                                    vast    795       72
                                   honor    795       72
                                   blind    795       72
                                 shouted    795       72
                                      et    795       72
                                     got    810       71
                                   lower    810       71
                                whatever    810       71
                                 drawing    810       71
                                 letters    810       71
                                    else    810       71
                                   court    810       71
                                   shoes    810       71
                               resembled    810       71
                                     box    810       71
                                    wore    810       71
                                  coffin    810       71
                                       v    822       70
                                  master    822       70
                               situation    822       70
                                 respect    822       70
                                 address    822       70
                                 subject    822       70
                                    rope    822       70
                                peculiar    822       70
                                 written    822       70
                                  reason    822       70
                                  breast    822       70
                                  school    822       70
                               valjean's    822       70
                                  divine    835       69
                                  horses    835       69
                                  gentle    835       69
                                   ideal    835       69
                                    holy    835       69
                                   baron    835       69
                                  chance    835       69
                               gentleman    835       69
                                  winter    835       69
                                listened    835       69
                                  silver    835       69
                               traversed    835       69
                                   paces    835       69
                                    keep    835       69
                            insurrection    835       69
                                 waiting    850       68
                            civilization    850       68
                               evidently    850       68
                                   lines    850       68
                                     sou    850       68
                                 account    850       68
                                  beyond    850       68
                                   erect    850       68
                                     key    850       68
                                    race    850       68
                                  seeing    850       68
                              everywhere    850       68
                                 walking    862       67
                                  double    862       67
                               departure    862       67
                                   stars    862       67
                                  change    862       67
                                    cart    862       67
                                  priest    862       67
                                  worthy    862       67
                                creature    862       67
                                   youth    862       67
                                   fifty    862       67
                                  behold    862       67
                                    play    862       67
                                 turning    862       67
                                 wounded    862       67
                               recognize    862       67
                                   fresh    862       67
                                   bench    862       67
                                  stared    862       67
                                  beings    862       67
                                     cap    882       66
                                   wrong    882       66
                                   heavy    882       66
                                 persons    882       66
                                    kept    882       66
                                   green    882       66
                                 talking    882       66
                               addressed    882       66
                                   space    882       66
                                soldiers    882       66
                                 emperor    892       65
                                 obscure    892       65
                                   named    892       65
                                  thanks    892       65
                               condemned    892       65
                                    sent    892       65
                               possessed    892       65
                                    shut    892       65
                              approached    892       65
                                   cloud    892       65
                                  sainte    892       65
                                    dawn    903       64
                                  virtue    903       64
                                   story    903       64
                                  listen    903       64
                                 opening    903       64
                                   dream    903       64
                                    wood    903       64
                                   month    903       64
                                     run    903       64
                                complete    912       63
                                      vi    912       63
                                   takes    912       63
                                  flight    912       63
                                   gamin    912       63
                                    pity    912       63
                                    note    912       63
                                    bare    912       63
                                     ill    912       63
                                   tried    912       63
                                   wrath    912       63
                                    calm    912       63
                                 noticed    912       63
                                     ask    912       63
                              possession    912       63
                                   lofty    912       63
                                   gloom    912       63
                                  memory    912       63
                                   angle    912       63
                              wellington    912       63
                                  affair    932       62
                               existence    932       62
                             immediately    932       62
                                 believe    932       62
                                finished    932       62
                               preceding    932       62
                                 dropped    932       62
                                  effort    932       62
                                  stupid    932       62
                                  object    932       62
                                attitude    932       62
                               lightning    932       62
                                   sweet    932       62
                                     les    945       61
                                entrance    945       61
                                    tomb    945       61
                              pronounced    945       61
                                    talk    945       61
                                     etc    945       61
                                   doors    945       61
                                   crowd    945       61
                                   linen    945       61
                                 quitted    945       61
                                  attack    945       61
                               tholomyès    945       61
                                   merry    957       60
                                  picpus    957       60
                                    self    957       60
                                   abyss    957       60
                                required    957       60
                                    gold    957       60
                                instinct    957       60
                                   fatal    957       60
                                 emerged    957       60
                                    mean    957       60
                                   sense    957       60
                                   habit    957       60
                                  action    957       60
                                 england    957       60
                                 soldier    957       60
                                demanded    957       60
                                    bore    957       60
                                   flung    957       60
                                    blow    975       59
                                national    975       59
                                faubourg    975       59
                                    died    975       59
                                   cause    975       59
                                    hall    975       59
                                  spirit    975       59
                               descended    975       59
                                  horror    975       59
                                 colonel    975       59
                               conscious    975       59
                                    deep    975       59
                                    rags    975       59
                                  revery    975       59
                                     ear    975       59
                                     dog    975       59
                                    roof    975       59
                                  pistol    975       59
                                   works    993       58
                                 horizon    993       58
                               suffering    993       58
                                   field    993       58
                                observed    993       58
                                  narrow    993       58
                                building    993       58
                                  latter    993       58
                               ourselves    993       58
                              absolutely    993       58
                                happened    993       58
                                  yellow    993       58
                                 purpose    993       58
                                  exists    993       58
                                repeated    993       58
                               shoulders    993       58
                                 falling    993       58
                                   smoke    993       58
                                prioress    993       58

```
 
Note that the first word that contains an apostrophe is the word   ''' don't '''   at the rank of   '''289'''.

The first word that contains an accented letter is the word   ''' thénardier '''   at the tied rank of   '''119'''.

==Communications of the ACM June 1986 Volume 29 Number 6==
This article is cited in the task description, it is not freely available. Apparently it is 10 pages of PASCAL. McIlroy's unix response is available. I've added a reference to the task description. When run it produces:

```txt

cat 135-0.txt | tr -cs A-Za-z '\n' | tr A-Z a-z | sort | uniq -c | sort -rn | sed ${1}q 
  41089 the

```
--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 16:23, 23 August 2017 (UTC)

:FWIW, The article cited is not free on the ACM website, but it is free from the Princeton CS (Donald Knuth) site. Just type "Programming pearls: a literate program" into Google and press "I'm feeling lucky".
:Also, that UNIX shell script example is already on the task page. It was one of the first ones added. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 17:41, 23 August 2017 (UTC)

::Thanks for the reference. The Unix example on the page does not acknowledge that it is McIlroy's solution. Reading Knuth's version it is clear that it would also have given 41089 as the answer. My point is that the task description relies on these two articles, both of which return 41089 as the answer when applied to the mandated test input, and examples in Clojure and Python which both give 41036 as the answer. I think that the answer should be 41088. This can be explained:
:::The Python and Clojure examples are wrong;
:::The references were never designed to be run using Unicode, which apparently traces it's origins to 1987, but I don't think was widely used before the late 90's.
:::The task author has never run the cited references using the mandated input.
::::The task's description has been updated so as to give freedom for example writers.  McIlroy's solution has been more explicitly acknowledged as having come from the cited article in the history. --[[User:Kentros|Kentros]] ([[User talk:Kentros|talk]]) 01:42, 31 August 2017 (UTC)
::The original author does not seem to be taking any further interest in this task. Perhaps you would like to update the description and mark the Clojure and Python examples as wrong to resolve this.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:41, 25 August 2017 (UTC)
:::The original author apologies profusely for not elaborating sooner.  I've updated the description, and I think I've addressed each concern. --[[User:Kentros|Kentros]] ([[User talk:Kentros|talk]]) 01:38, 31 August 2017 (UTC)

::Knuth's paper clarifies another issue 'Let us agree that a word is a sequence of one or more contiguous letters; “Bentley” is a word, but ain't isn’t'.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:26, 25 August 2017 (UTC)

::: That's a lot of good detective work, but if the task leaves the definition of what a word is up to the example writer, then the Python  can't be wrong. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:38, 25 August 2017 (UTC)

::::The task defines a word as a sequence of contiguous letters (as in McIlroy's solution) without defining what a letter is. How about leaving it up to the sample writer what a letter is? Samples could then use the Unicode definition or the ASCII definition (or even some other character set) as convenient? --[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 18:09, 25 August 2017 (UTC)

:::::One could for a laugh but not seriously. For a laugh I asked MS Word to open the mandated input using US-ASCII. It then thinks the book is Les MisC)rables. Knuth defined the task assuming it was going to read US-ASCII, and clearly defines what a letter is in that context. It makes no sense to write a task for US-ASCII (e.g. Unix on the task page) and then run it on an example in UTF-8. Obviously an alternative is to mandate an example written in US-ASCII--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 10:32, 26 August 2017 (UTC)

::::::The task as currently defined does not specify what a letter is. I suggested a definition that would allow both the "classic" (pre-RC) solutions and the new Python etc. samples to be accepted. --[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 12:31, 26 August 2017 (UTC)

:::::::I agree that freedom to choose is the best, so I'm going with your suggestion, Tigerofdarkness.  Thanks! --[[User:Kentros|Kentros]] ([[User talk:Kentros|talk]]) 01:27, 31 August 2017 (UTC)

==Code Golf mention==
*[https://codegolf.stackexchange.com/questions/188133/bentleys-coding-challenge-k-most-frequent-words Bentley's coding challenge: k most frequent words] at Code Golf Stack Exchange mentions this task.
