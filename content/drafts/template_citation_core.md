+++
title = "Template:Citation/core"
description = ""
date = 2010-04-25T17:35:45Z
aliases = []
[extra]
id = 7130
[taxonomies]
categories = []
tags = []
+++

<span class="citation {{{Citation class|{{{Citation type|}}}}}}"
{{
  #switch:{{{Ref|}}}
  ||none =
  |#default = id="{{anchorencode:{{{Ref}}}}}"
  |harv = {{#if:{{{Surname1|}}}{{{EditorSurname1|}}}
    |id="CITEREF{{anchorencode:{{#if:{{{Surname1|}}}
      |{{{Surname1}}}{{{Surname2|}}}{{{Surname3|}}}{{{Surname4|}}}
      |{{{EditorSurname1|}}}{{{EditorSurname2|}}}{{{EditorSurname3|}}}{{{EditorSurname4|}}}
    }}{{{Year|{{{Date|}}}}}}}}"
  }}
}}>{{
<!--
### =========  Author or editor and date  =========
-->
  #if:{{{Surname1|}}}
  |{{
     #if: {{{AuthorMask|}}}
     |{{
        #iferror: {{ #expr: 1*{{{AuthorMask}}} }}
        |{{{AuthorMask}}}
        |<del>{{loop|{{{AuthorMask}}}|2=&emsp;}}</del>
      }}
     |{{
        #if: {{{Authorlink1|}}}
        |[[{{{Authorlink1}}} |{{{Surname1}}}{{
          #if: {{{Given1|}}}
          |{{{NameSep|,&#32;}}}{{{Given1}}}
         }}]]
        |{{{Surname1}}}{{
           #if: {{{Given1|}}}
           |{{{NameSep|,&#32;}}}{{{Given1}}}
         }}
      }}
   }}{{
     #if: {{{Surname2|}}}
     |{{#ifexpr:{{{Trunc|8}}}<2
       |&#32;''et al''.
       |{{
          #iferror: {{ #expr: 1*0.0{{{AuthorMask|}}} }}
          |&#32;<!-- then punctuation should be included in AuthorMask -->
          |{{
            #if: {{{Surname3|}}}
            |{{{AuthorSep|&#059;&#32;}}}
            |{{#if:{{{amp|}}}|&#32;&amp;&#32;|{{{AuthorSep|&#059;&#32;}}}}}
          }}
        }}{{
          #if: {{{Authorlink2|}}}
          |[[{{{Authorlink2}}} |{{{Surname2}}}{{
             #if: {{{Given2|}}}
             |{{{NameSep|,&#32;}}}{{{Given2}}}
           }}]]
          |{{{Surname2}}}{{
             #if: {{{Given2|}}}
             |{{{NameSep|,&#32;}}}{{{Given2}}}
           }}
        }}{{
          #if: {{{Surname3|}}}
          |{{#ifexpr:{{{Trunc|8}}}<3
            |&#32;''et al''.
            |{{
               #if: {{{Surname4|}}}
               |{{{AuthorSep|&#059;&#32;}}}
               |{{#if:{{{amp|}}}|&#32;&amp;&#32;|{{{AuthorSep|&#059;&#32;}}}}}
             }}{{
               #if: {{{Authorlink3|}}}
               |[[{{{Authorlink3}}} |{{{Surname3}}}{{
                  #if: {{{Given3|}}}
                  |{{{NameSep|,&#32;}}}{{{Given3}}}
                }}]]
               |{{{Surname3}}}{{
                  #if: {{{Given3|}}}
                  |{{{NameSep|,&#32;}}}{{{Given3}}}
                }}
             }}{{
               #if:{{{Surname4|}}}
               |{{#ifexpr:{{{Trunc|8}}}<4
                 |&#32;''et al''.
                 |{{
                    #if: {{{Surname5|}}}
                    |{{{AuthorSep|&#059;&#32;}}}
                    |{{#if:{{{amp|}}}|&#32;&amp;&#32;|{{{AuthorSep|&#059;&#32;}}}}}
                  }}{{
                    #if: {{{Authorlink4|}}}
                    |[[{{{Authorlink4}}} |{{{Surname4}}}{{
                       #if: {{{Given4|}}}
                       |{{{NameSep|,&#32;}}}{{{Given4}}}
                     }}]]
                    |{{{Surname4}}}{{
                       #if: {{{Given4|}}}
                       |{{{NameSep|,&#32;}}}{{{Given4}}}
                    }}
                  }}{{
                  #if:{{{Surname5|}}}
                  |{{#ifexpr:{{{Trunc|8}}}<5
                    |&#32;''et al''.
                    |{{
                     #if: {{{Surname6|}}}
                     |{{{AuthorSep|&#059;&#32;}}}
                     |{{#if:{{{amp|}}}|&#32;&amp;&#32;|{{{AuthorSep|&#059;&#32;}}}}}
                    }}{{
                     #if: {{{Authorlink5|}}}
                     |[[{{{Authorlink5}}} |{{{Surname5}}}{{
                        #if: {{{Given5|}}}
                        |{{{NameSep|,&#32;}}}{{{Given5}}}
                      }}]]
                     |{{{Surname5}}}{{
                        #if: {{{Given5|}}}
                        |{{{NameSep|,&#32;}}}{{{Given5}}}
                      }}
                   }}{{
                     #if:{{{Surname6|}}}
                     |{{#ifexpr:{{{Trunc|8}}}<6
                     |&#32;''et al''.
                      |{{
                        #if: {{{Surname7|}}}
                        |{{{AuthorSep|&#059;&#32;}}}
                        |{{#if:{{{amp|}}}|&#32;&amp;&#32;|{{{AuthorSep|&#059;&#32;}}}}}
                      }}{{
                        #if: {{{Authorlink6|}}}
                        |[[{{{Authorlink6}}} |{{{Surname6}}}{{
                           #if: {{{Given6|}}}
                           |{{{NameSep|,&#32;}}}{{{Given6}}}
                         }}]]
                        |{{{Surname6}}}{{
                           #if: {{{Given6|}}}
                           |{{{NameSep|,&#32;}}}{{{Given6}}}
                         }}
                      }}{{
                        #if:{{{Surname7|}}}
                         |{{#ifexpr:{{{Trunc|8}}}<7
                           |&#32;''et al''.
                        |{{
                           #if: {{{Surname8|}}}
                           |{{{AuthorSep|&#059;&#32;}}}
                           |{{#if:{{{amp|}}}|&#32;&amp;&#32;|{{{AuthorSep|&#059;&#32;}}}}}
                         }}{{
                           #if: {{{Authorlink7|}}}
                           |[[{{{Authorlink7}}} |{{{Surname7}}}{{
                              #if: {{{Given7|}}}
                              |{{{NameSep|,&#32;}}}{{{Given7}}}
                            }}]]
                           |{{{Surname7}}}{{
                              #if: {{{Given7|}}}
                              |{{{NameSep|,&#32;}}}{{{Given7}}}
                            }}
                                    }}{{
                                       #if:{{{Surname8|}}}
                                       |{{#ifexpr:{{{Trunc|8}}}<8
                                           |&#32;''et al''.
                                           |{{
                                              #if: {{{Surname9|}}}
                                              |{{{AuthorSep|&#059;&#32;}}}
                                              |{{#if:{{{amp|}}}|&#32;&amp;&#32;|{{{AuthorSep|&#059;&#32;}}}}}
                                            }}{{
                                              #if: {{{Authorlink8|}}}
                                              |[[{{{Authorlink8}}} |{{{Surname8}}}{{
                                                 #if: {{{Given8|}}}
                                                 |{{{NameSep|,&#32;}}}{{{Given8}}}
                                               }}]]
                                              |{{{Surname8}}}{{
                                                 #if: {{{Given8|}}}
                                                 |{{{NameSep|,&#32;}}}{{{Given8}}}
                                               }}
                                            }}{{
                                              #if:{{{Surname9|}}}
                                              |&#32;''et al''.
                                            }}
                                         }}
                                      }}
                                   }}
                                }}
                             }}
                          }}
                       }}
                     }}
                  }}
               }}
            }}
         }}
      }}

   }}{{
     #if: {{{Coauthors|}}}
     |{{{AuthorSep|&#059;&#32;}}}{{{Coauthors|}}}|
   }}{{
     #if: {{{Date|}}}
     |&#32;({{{Date}}}){{
       #if:{{{YearNote|}}}
     |&#32;[{{{YearNote}}}]
     }}
   }}
  |{{<!-- 
### ========
 No author: display editors first == -->
     #if: {{{EditorSurname1|}}}
     |{{
        #if: {{{Editorlink1|}}}
        |[[{{{Editorlink1}}} |{{{EditorSurname1}}}{{
           #if: {{{EditorGiven1|}}}
           |, {{{EditorGiven1}}}
         }}]]
        |{{{EditorSurname1}}}{{
           #if: {{{EditorGiven1|}}}
           |, {{{EditorGiven1}}}
         }}
      }}{{
        #if: {{{EditorSurname2|}}}
        |{{
          #if: {{{EditorSurname3|}}}
          |{{{AuthorSep|&#059;&#32;}}}
          |{{#if:{{{amp|}}}|&#32;&amp;&#32;|{{{AuthorSep|&#059;&#32;}}}}}
        }}{{
           #if: {{{Editorlink2|}}}
           |[[{{{Editorlink2}}} |{{{EditorSurname2}}}{{
              #if: {{{EditorGiven2|}}}
              |, {{{EditorGiven2}}}
            }}]]
           |{{{EditorSurname2}}}{{
              #if: {{{EditorGiven2|}}}
              |, {{{EditorGiven2}}}
            }}
         }}{{
           #if: {{{EditorSurname3|}}}
           |{{
              #if: {{{EditorSurname4|}}}
              |{{{AuthorSep|&#059;&#32;}}}
              |{{#if:{{{amp|}}}|&#32;&amp;&#32;|{{{AuthorSep|&#059;&#32;}}}}}
            }}{{
              #if: {{{Editorlink3|}}}
              |[[{{{Editorlink3}}} |{{{EditorSurname3}}}{{
                 #if: {{{EditorGiven3|}}}
                 |, {{{EditorGiven3}}}
               }}]]
              |{{{EditorSurname3}}}{{
                 #if: {{{EditorGiven3|}}}
                 |, {{{EditorGiven3}}}
               }}
            }}{{
              #if:{{{EditorSurname4|}}}
              |&#32;et al.
            }}
         }}
       }}, ed{{#if:{{{EditorSurname2|}}}|s}}{{#ifeq:{{{Sep|,}}}|.||.}}{{
        #if: {{{Date|}}}
        |&#32;({{{Date}}}){{
         #if:{{{YearNote|}}}
       |&#32;[{{{YearNote}}}]
       }}
      }}
   }}
}}{{
<!--
### =========  Title of included work  =========
-->
  #if: {{{IncludedWorkTitle|}}}{{#if:{{{Periodical|}}}||{{#if:{{{TransItalic|}}}||{{{TransTitle|}}}}}}}
  |{{
     #if:{{{Surname1|}}}{{{EditorSurname1|}}}
     |{{{Sep|,}}}&#32;
   }}{{Citation/make link
     | 1={{
           #if: {{{IncludedWorkURL|}}}
           |{{{IncludedWorkURL}}}
           |{{
              #if: {{{URL|}}}
              |{{{URL}}}
<!-- Only link URL if to a free full text - as at PubMedCentral (PMC)-->
              |{{#ifexpr:{{#time: U}} > {{#time: U | {{{Embargo|2001-10-10}}} }}
                |{{
                   #if: {{{PMC|}}}
                   |http://www.pubmedcentral.nih.gov/articlerender.fcgi?tool=pmcentrez&artid={{{PMC}}}
                 }}
               }}
            }}
         }}
     | 2={{
           #if: {{{Periodical|}}}
           |''<nowiki />{{{IncludedWorkTitle}}}<nowiki />''
           |"{{{IncludedWorkTitle|}}}{{
             #if: {{{TransTitle|}}}
             |{{
                #if: {{{IncludedWorkTitle|}}}
                |&#32;
              }}&#91;{{{TransTitle}}}&#93;
           }}"
         }}
   }}
}}{{
<!--
### ======
  Place (if different than PublicationPlace) 
### ======
-->
  #if: {{{Place|}}}
  |{{
     #ifeq: {{{Place|}}} | {{{PublicationPlace|}}}
     |
     |{{
        #if: {{{Surname1|}}}{{{EditorSurname1|}}}{{{IncludedWorkTitle|}}}
        |{{{Sep|,}}}&#32;written at {{{Place}}}
      }}
   }}
}}{{
<!--
### =========  Editor of compilation  =========
-->
  #if: {{{EditorSurname1|}}}
  |{{
     #if: {{{Surname1|}}}
     |{{{Sep|,}}}&#32;{{
        #if: {{{IncludedWorkTitle|}}}
        |in&#32;
      }}{{
        #if: {{{Editorlink1|}}}
        |[[{{{Editorlink1}}} |{{{EditorSurname1}}}{{
           #if: {{{EditorGiven1|}}}
           |, {{{EditorGiven1}}}
         }}]]
        |{{{EditorSurname1}}}{{
           #if: {{{EditorGiven1|}}}
           |, {{{EditorGiven1}}}
         }}}}{{
        #if: {{{EditorSurname2|}}}
        |{{
          #if: {{{EditorSurname3|}}}
            |{{{AuthorSep|&#059;&#32;}}}
            |{{#if:{{{amp|}}}|&#32;&amp;&#32;|{{{AuthorSep|&#059;&#32;}}}}}
          }}{{
           #if: {{{Editorlink2|}}}
           |[[{{{Editorlink2}}}|{{{EditorSurname2}}}{{
              #if: {{{EditorGiven2|}}}
              |, {{{EditorGiven2}}}
            }}]]
           |{{{EditorSurname2}}}{{
              #if: {{{EditorGiven2|}}}
              |, {{{EditorGiven2}}}
            }}
         }}{{
           #if: {{{EditorSurname3|}}}
           |{{
              #if: {{{EditorSurname4|}}}
              |&#059;&#32;
              |{{#if:{{{amp|}}}|&#32;&amp;&#32;|{{{AuthorSep|&#059;&#32;}}}}}
            }}{{
              #if: {{{Editorlink3|}}}
              |[[{{{Editorlink3}}}|{{{EditorSurname3}}}{{
                 #if: {{{EditorGiven3|}}}
                 |, {{{EditorGiven3}}}
               }}]]
              |{{{EditorSurname3}}}{{
                 #if: {{{EditorGiven3|}}}
                 |, {{{EditorGiven3}}}
               }}
            }}{{
              #if:{{{EditorSurname4|}}}
              |&#32;et al.
            }}
         }}
      }}{{
        #if: {{{IncludedWorkTitle|}}}
        |
        |{{{Sep|,}}}&#32;ed{{#if:{{{EditorSurname2|}}}|s}}{{#ifeq:{{{Sep|,}}}|.||.}}
      }}
   }}
}}{{
  <!--
### =========  Periodicals  =========
-->
  #if: {{{Periodical|}}}
  |{{
     #if: {{{Other|}}}
     |{{{Sep|,}}}&#32;{{{Other|}}}
   }}{{
     #if: {{{Surname1|}}}{{{EditorSurname1|}}}{{{IncludedWorkTitle|}}}
     |{{{Sep|,}}}&#32;}}{{
     #if: {{{Title|}}}{{{TransTitle|}}}
     |{{Citation/make link
        | 1={{
              #if: {{{IncludedWorkTitle|}}}
              |{{
                 #if: {{{IncludedWorkURL|}}}
                 |{{
                    #if: {{{URL|}}}
                    |{{{URL}}}
                    |{{
                       #ifexpr: {{#time: U}} > {{#time: U | {{{Embargo|2001-10-10}}} }} | {{
                         #if: {{{PMC|}}}
                         |  http://www.pubmedcentral.nih.gov/articlerender.fcgi?tool=pmcentrez&artid={{{PMC}}}
                       }}
                     }}
                  }}
               }}
              |{{
                 #if: {{{URL|}}}
                 |{{{URL}}}
                 |{{#ifexpr:{{#time: U}} > {{#time: U | {{{Embargo|2001-10-10}}} }} |{{
                       #if: {{{PMC|}}}
                       |  http://www.pubmedcentral.nih.gov/articlerender.fcgi?tool=pmcentrez&artid={{{PMC}}}
                    }}
                 }}
               }}
            }}
        | 2="{{{Title}}}{{
          #if: {{{TransTitle|}}}
          |{{
             #if: {{{Title|}}}
             |&#32;
           }}&#91;{{{TransTitle}}}&#93;
        }}"
      }}{{
        #if: {{{TitleNote|}}}
        |{{{Sep|,}}}&#32;{{{TitleNote}}}
      }}
   }}
}}{{
  #if: {{{language|}}}
  |&#32;(in {{{language}}})
}}{{
  #if: {{{format|}}}
  |&#32;({{{format}}})
}}{{
   #if: {{{Periodical|}}}
   |{{
      #if:{{{IncludedWorkTitle|}}}{{{Title|}}}{{{TransTitle|}}}
      |{{{Sep|,}}}&#32;
    }}''<nowiki />{{{Periodical}}}<nowiki />''{{
      #if: {{{Series|}}}
      |{{{Sep|,}}}&#32;{{{Series}}}
    }}{{
      #if: {{{PublicationPlace|}}}
      |{{
         #if: {{{Publisher|}}}
         |&#32;({{{PublicationPlace}}}<nowiki>: </nowiki>{{{Publisher}}})
         |&#32;({{{PublicationPlace}}})
       }}
      |{{
         #if: {{{Publisher|}}}
         |&#32;({{{Publisher}}})
       }}
    }}{{
      #if: {{{Volume|}}}
      |&#32;'''<nowiki />{{{Volume}}}<nowiki />'''{{
         #if: {{{Issue|}}}
         |&#32;({{{Issue}}})
       }}
      |{{
         #if: {{{Issue|}}}
         |&#32;({{{Issue}}})
       }}
    }}{{
      #if: {{{At|}}}
      |<nowiki>: </nowiki> {{{At}}}
    }}
   |{{
      <!--
### ======
 Anything else with a title, including books 
### ======
-->
      #if: {{{Title|}}}{{{TransItalic|}}}
      |{{
         #if: {{{Surname1|}}}{{{EditorSurname1|}}}{{{IncludedWorkTitle|}}}{{{Periodical|}}}
         |{{{Sep|,}}}
       }}&#32;{{Citation/make link
         | 1={{
               #if: {{{IncludedWorkTitle|}}}
               |{{
                  #if: {{{IncludedWorkURL|}}}
                  |{{
                     #if: {{{URL|}}}
                     |{{{URL}}}
                     |{{#ifexpr:{{#time: U}} > {{#time: U | {{{Embargo|2001-10-10}}} }}|{{
                        #if: {{{PMC|}}}
                        |  http://www.pubmedcentral.nih.gov/articlerender.fcgi?tool=pmcentrez&artid={{{PMC}}}
                      }}}}
                   }}
                }}
               |{{
                  #if: {{{URL|}}}
                  |{{{URL}}}
                                |{{#ifexpr:{{#time: U}} > {{#time: U | {{{Embargo|2001-10-10}}} }}|{{
                     #if: {{{PMC|}}}
                     |  http://www.pubmedcentral.nih.gov/articlerender.fcgi?tool=pmcentrez&artid={{{PMC}}}
                   }}}}
                }}
             }}
         | 2=''<nowiki />{{{Title|}}}{{
            #if:{{{TransItalic|}}}|&#32;&#91;{{{TransItalic}}}&#93;
          }}<nowiki />''
       }}
    }}{{
      #if: {{{TitleType|}}}
      |&#32;({{{TitleType}}})
    }}{{
      #if: {{{Series|}}}
      |{{{Sep|,}}}&#32;{{{Series}}}
    }}{{
      #if: {{{Volume|}}}
      |{{{Sep|,}}}&#32;'''<nowiki />{{{Volume}}}<nowiki />'''
    }}{{
      #if: {{{Other|}}}
      |{{{Sep|,}}}&#32;{{{Other|}}}
    }}{{
      #if: {{{Edition|}}}
      |&#32;({{{Edition}}} ed.)
    }}{{
      #if: {{{PublicationPlace|}}}
      |{{{Sep|,}}}&#32;{{{PublicationPlace}}}
    }}{{
      #if: {{{Publisher|}}}
      |{{
         #if: {{{PublicationPlace|}}}
         |<nowiki>:</nowiki>
         |{{{Sep|,}}}
       }}&#32;{{{Publisher}}}
    }}
}}{{
<!--
### ======
 Date (if no author/editor) 
### ======
-->
  #if: {{{Surname1|}}}{{{EditorSurname1|}}}
  |
  |{{
     #if: {{{Date|}}}
     |{{{Sep|,}}}&#32;{{{Date}}}{{
       #if:{{{YearNote|}}}
     |&#32;[{{{YearNote}}}]
     }}
   }}
}}{{
<!--
### ========= Publication date =========
-->
  #if: {{{PublicationDate|}}}
  |{{
     #ifeq: {{{PublicationDate|}}} | {{{Date|}}}
     |
     |{{
        #if: {{{EditorSurname1|}}}
        |{{
           #if: {{{Surname1|}}}
           |{{{Sep|,}}}&#32;{{{PublicationDate}}}
           |&#32;(published {{{PublicationDate}}})
         }}
        |{{
           #if: {{{Periodical|}}}
           |{{{Sep|,}}}&#32;{{{PublicationDate}}}
           |&#32;(published {{{PublicationDate}}})
         }}
      }}
   }}
}}{{
<!--
### ========= Page within included work =========
-->
  #if: {{{Periodical|}}}
  |
  |{{
     #if: {{{At|}}}
     |{{{Sep|,}}}&#32;{{{At}}}
   }}
}}{{
<!--
### ============DOI=============
-->
#if:{{{DOI|}}}
  |{{{Sep|,}}}&#32;{{citation/identifier  |identifier=doi |input1={{{DOI|}}}  |input2={{{DoiBroken|}}} }}
}}{{
<!--
### =========  Misc. Identifier =========
-->
  #if: {{{ID|}}}
  |{{
     #if: {{{Surname1|}}}{{{EditorSurname1|}}}{{{IncludedWorkTitle|}}}{{{Periodical|}}}{{{Title|}}}{{{TransItalic|}}}
     |{{{Sep|,}}}&#32;{{{ID}}}
     |{{{ID}}}
   }}
}}{{
<!--
### =========  ISBN =========
-->
  #if: {{{ISBN|}}}
  |{{{Sep|,}}}&#32;{{citation/identifier  |identifier=isbn |input1={{{ISBN|}}} }}
}}{{
<!--
### =========  ISSN =========
-->
  #if: {{{ISSN|}}}
  |{{{Sep|,}}}&#32;{{citation/identifier  |identifier=issn |input1={{{ISSN|}}} }}
}}{{
<!--
### =========  OCLC =========
-->
  #if: {{{OCLC|}}}
  |{{{Sep|,}}}&#32;{{citation/identifier  |identifier=oclc |input1={{{OCLC|}}} }}
}}{{
<!--
### =========  PMID =========
-->
  #if: {{{PMID|}}}
  |{{{Sep|,}}}&#32;{{citation/identifier  |identifier=pmid |input1={{{PMID|}}} }}
}}{{
<!--
### =========  PMC =========
-->
  #if: {{{PMC|}}}
  |{{
     #if: {{{URL|}}}
     |{{{Sep|,}}}&#32;{{citation/identifier  |identifier=pmc |input1={{{PMC|}}} }}
     |{{only in  print|{{{Sep|,}}}&#32;{{citation/identifier  |identifier=pmc |input1={{{PMC|}}} }} }}<!--Should  only display by default in print-->
   }}
}}{{
<!--
### ========= BIBCODE =========
-->
  #if: {{{Bibcode|}}}
  |{{{Sep|,}}}&#32;{{citation/identifier  |identifier=bibcode |input1={{{Bibcode|}}} }}
}}{{
<!--
### ======
  Archive data, etc 
### =====
-->
#if: {{{Archive|}}}
|{{{Sep|,}}}&#32;{{{Archive}}}[[Category:Pages using deprecated citation archive parameters]]
|{{
  #if:{{{ArchiveURL|}}}{{{ArchiveDate|}}}
  |{{{Sep|,}}}&#32;{{#ifeq:{{{Sep}}}|.|A|a}}rchived{{
    #if:{{{OriginalURL|}}}{{{IncludedWorkURL|}}}
    |{{#if:{{{ArchiveURL|}}}|&#32;from {{Citation/make link|{{{OriginalURL|{{{IncludedWorkURL|}}}}}}|the original}}}}
    }}{{
    #if:{{{ArchiveDate|}}}
    |&#32;on {{{ArchiveDate}}}
    }}{{
    #if:{{#if:{{{ArchiveURL|}}}||A}}{{#if:{{{OriginalURL|}}}{{{IncludedWorkURL|}}}||B}}{{#if:{{{ArchiveDate|}}}||C}}
    |. {{citation error
       |If you specify <code>&#124;{{#if:{{{ArchiveURL|}}}|archiveurl|archivedate}}&#61;</code>, you must {{#if:{{{OriginalURL|}}}{{{IncludedWorkURL|}}}| also specify <code>&#124;{{#if:{{{ArchiveURL|}}}|archivedate|archiveurl}}&#61;</code>|first specify <code>&#124;url&#61;</code>}}}}
    }}
  }}
}}{{
<!--
### ========= URL and AccessDate =========
-->
  #if: {{{URL|}}}{{{IncludedWorkURL|}}}
  |{{
     #if: {{{Title|}}}{{{IncludedWorkTitle|}}}{{{TransTitle|}}}
     |<span class="printonly">{{{Sep|,}}}&#32;{{
                                      #if: {{{IncludedWorkURL|}}}
                                      |{{{IncludedWorkURL}}}
                                      |{{{URL}}}
                                    }}</span>
     |{{{Sep|,}}}&#32;{{
              #if: {{{IncludedWorkURL|}}}
              |{{{IncludedWorkURL}}}
              |{{{URL}}}
            }}
   }}{{
     #if: {{{AccessDate|}}}
     | <span class="reference-accessdate">{{#ifeq:{{{Sep|,}}}|,|,&#32;r|.&#32;R}}etrieved {{{AccessDate}}}</span>
     }}
}}{{#if:{{{laysummary|}}}
  |{{{Sep|,}}}&#32;[{{{laysummary}}} Lay summary]{{#if: {{{laysource|}}}| &ndash; ''<nowiki />{{{laysource}}}<nowiki />''}}
}}{{#if:{{{laydate|}}}
  | &#32;({{{laydate}}})
}}{{#if:{{{quote|}}}
  |{{{Sep|,}}}&#32;"{{{quote}}}"
}}{{{PS|}}}</span><!--

=== This is a COinS tag (http://ocoins.info), which allows automated tools to parse the citation information: ===

--><span
    class="Z3988"
    title="ctx_ver=Z39.88-2004&rft_val_fmt={{urlencode:info:ofi/fmt:kev:mtx:}}{{
      #if: {{{Periodical|}}}
      |journal&rft.genre=article&rft.atitle={{urlencode:{{{Title|}}}}}&rft.jtitle={{urlencode:{{{Periodical|}}}}}
      |book{{
         #if: {{{IncludedWorkTitle|}}}
         |&rft.genre=bookitem&rft.btitle={{urlencode:{{{IncludedWorkTitle|}}}}}&rft.atitle={{urlencode:{{{Title|}}}}}
         |&rft.genre=book&rft.btitle={{urlencode:{{{Title|}}}}}
       }}
    }}{{
     #if: {{{Surname1|}}} |&rft.aulast={{urlencode:{{{Surname1}}}}}{{
       #if: {{{Given1|}}} |&rft.aufirst={{urlencode:{{{Given1}}}}}
     }}
   }}{{
     #if: {{{Surname1|}}} |&rft.au={{urlencode:{{{Surname1}}}}}{{
       #if: {{{Given1|}}} |{{urlencode:{{{NameSep|,&#32;}}}{{{Given1}}}}}
     }}
   }}{{
     #if: {{{Surname2|}}} |&rft.au={{urlencode:{{{Surname2}}}}}{{
       #if: {{{Given2|}}} |{{urlencode:{{{NameSep|,&#32;}}}{{{Given2}}}}}
     }}
   }}{{
     #if: {{{Surname3|}}} |&rft.au={{urlencode:{{{Surname3}}}}}{{
       #if: {{{Given3|}}} |{{urlencode:{{{NameSep|,&#32;}}}{{{Given3}}}}}
     }}
   }}{{
     #if: {{{Surname4|}}} |&rft.au={{urlencode:{{{Surname4}}}}}{{
       #if: {{{Given4|}}} |{{urlencode:{{{NameSep|,&#32;}}}{{{Given4}}}}}
     }}
   }}{{
     #if: {{{Surname5|}}} |&rft.au={{urlencode:{{{Surname5}}}}}{{
       #if: {{{Given5|}}} |{{urlencode:{{{NameSep|,&#32;}}}{{{Given5}}}}}
     }}
   }}{{
     #if: {{{Surname6|}}} |&rft.au={{urlencode:{{{Surname6}}}}}{{
       #if: {{{Given6|}}} |{{urlencode:{{{NameSep|,&#32;}}}{{{Given6}}}}}
     }}
   }}{{
     #if: {{{Surname7|}}} |&rft.au={{urlencode:{{{Surname7}}}}}{{
       #if: {{{Given7|}}} |{{urlencode:{{{NameSep|,&#32;}}}{{{Given7}}}}}
     }}
   }}{{
     #if: {{{Surname8|}}} |&rft.au={{urlencode:{{{Surname8}}}}}{{
       #if: {{{Given8|}}} |{{urlencode:{{{NameSep|,&#32;}}}{{{Given8}}}}}
     }}
   }}{{
     #if: {{{Surname9|}}} |&rft.au={{urlencode:{{{Surname9}}}}}{{
       #if: {{{Given9|}}} |{{urlencode:{{{NameSep|,&#32;}}}{{{Given9}}}}}
     }}
    }}{{
      #if: {{{Date|}}} |&rft.date={{urlencode:{{{Date}}}}}
    }}{{
      #if: {{{Series|}}} |&rft.series={{urlencode:{{{Series}}}}}
    }}{{
      #if: {{{Volume|}}} |&rft.volume={{urlencode:{{{Volume}}}}}
    }}{{
      #if: {{{Issue|}}} |&rft.issue={{urlencode:{{{Issue}}}}}
    }}{{
      #if: {{{At|}}} |&rft.pages={{urlencode:{{{At}}}}}
    }}{{
      #if: {{{Edition|}}} |&rft.edition={{urlencode:{{{Edition}}}}}
    }}{{
      #if: {{{PublicationPlace|}}}{{{Place|}}} |&rft.place={{urlencode:{{{PublicationPlace|{{{Place}}}}}}}}
    }}{{
      #if: {{{Publisher|}}} |&rft.pub={{urlencode:{{{Publisher}}}}}
    }}{{
      #if: {{{DOI|}}} |&rft_id=info:doi/{{urlencode:{{{DOI}}}}}
    }}{{
      #if: {{{PMID|}}} |&rft_id=info:pmid/{{urlencode:{{{PMID}}}}}
    }}{{
      #if: {{{Bibcode|}}} |&rft_id=info:bibcode/{{urlencode:{{{Bibcode}}}}}
    }}{{
      #if: {{{OCLC|}}} |&rft_id=info:oclcnum/{{urlencode:{{{OCLC}}}}}
    }}{{
      #if: {{{ISBN|}}} |&rft.isbn={{urlencode:{{{ISBN}}}}}
    }}{{
      #if: {{{ISSN|}}} |&rft.issn={{urlencode:{{{ISSN}}}}}
    }}{{
      #if: {{{URL|}}}{{{IncludedWorkURL|}}} |&rft_id={{urlencode:{{{URL|{{{IncludedWorkURL|}}}}}}}}
    }}&rfr_id=info:sid/en.wikipedia.org:{{FULLPAGENAMEE}}"><span style="display: none;"> </span></span><noinclude>

Copied from WP for reflist
{{Pp-template|small=yes}}
{{Documentation}}
</noinclude>
