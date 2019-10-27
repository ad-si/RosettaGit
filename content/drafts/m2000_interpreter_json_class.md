+++
title = "M2000 Interpreter Json Class"
description = ""
date = 2018-06-26T02:33:45Z
aliases = []
[extra]
id = 21890
[taxonomies]
categories = []
tags = []
+++


## M2000 Interpreter


```M2000 Interpreter


MODULE LIB1 {
      Class ParserClass {
      Private:
            Class bStream {
                  Private:
                  cnt, Buffer A
                  Public:
                  Value (&c) {Try {c=eval(.A, .cnt) : .cnt++:=true}}
                  Class:
                  Module Final bStream (a$){
                        Buffer .A as Integer*Len(a$)
                        Return .A, 0:=a$
                  }
            }
            Func=Lambda->false
            char=0
            obj=Stack
            Function Final IsId {
                  If .char=34 Then =.IsString(false)
            }
            Function Final IsTrue {
                  If .char=0x74 Then If .func() Then If .char=0x72 Then If .func() Then If .char=0x75 Then If .func() Then If .char=0x65 Then PushIt() : =True
                  Sub PushIt()
                        Stack .obj {
                              Push .Boolean(True)
                        }
                  End Sub
            }
            Function Final IsFalse {
                  If .char=0x66 Then If .func() Then If .char=0x61 Then If .func() Then If .char=0x6c Then If .func() Then If .char=0x73 Then If .func() Then If .char=0x65 Then PushIt() : =True
                  Sub PushIt()
                        Stack .obj {
                              Push .Boolean(False)
                        }
                  End Sub
            }
            Function Final IsNull {
                  If .char=0x6e Then If .func() Then If .char=0x75 Then If .func() Then If .char=0x6c Then If .func() Then If .char=0x6c Then PushIt() : =True
                  Sub PushIt()
                        Stack .obj {
                              Push .Null()
                        }
                  End Sub
            }
            Function Final IsSemiCol {
                    If .char=0x3a Then =true
            }
            Function Final IsComma {
                    If .char=0x2c Then =true
            }
            Function Final IsObject {
                  If .char=123 Else exit
                  inventory objinv
                 Stack .obj { Push .Object(objinv)}
                 .Trim
                 While .IsId() {
                       .Trim
                       If .IsSemiCol() Then {
                             .Trim
                             If .IsValue() Then {
                                   Stack .obj {
                                          Shift 2: Append objinv, Letter$:=Group
                                    }
                              }
                       } Else Exit
                       .Trim
                        If not .IsComma() Then exit
                       .Trim
                  }
                  If .char=125 Then { =true } Else .obj<=Stack : .func<=lambda->0
            }
            Function Final IsValue {
                  If .IsString(True) Then {
                         =True
                  } Else.if .IsNumber() Then {
                        =True
                  } Else.If .IsTrue() Then {
                        =True
                  }  Else.If .IsFalse() Then {
                        =True
                  } Else.If .IsNull() Then {
                        =True
                  } Else.if .IsArray() Then {
                        =True
                  } Else.if .IsObject() Then {
                        =True
                  } Else {
                        Print "what", .char
                        Stack .obj { Stack}
                        .func<=lambda->0
                  }
            }
            Function Final Digits (private_stack){
                  While .func() {
                        Select Case .char
                        Case 48 to 57
                        {
                              =true
                             Stack private_stack { Data .char}
                        }
                        Else
                             break
                        End Select
                  }    
            }
            Function Final IsNumber {
                  a=Stack
                  Select Case .char
                  Case 45 ' -
                  {
                              oldfunc=.func
                              Stack a { Data .char}
                              If .Func() Then {
                                    Select Case .char
                                    Case 48
                                    {
                                            Stack a { Data .char}
                                            If .func() Then {
                                                If .char=46 Then {
                                                      Fraction()
                                                      Exponent()
                                                }
                                          }
                                    }
                                    Case 49 to 57
                                    {
                                          Stack a { Data .char}
                                          If .Digits(a) Then {}
                                          Fraction()
                                          Exponent()
                                    }
                                    Else
                                          a=stack
                                    End Select
                              }
                  }
                  Case 48
                  {
                        oldfunc=.func
                        Stack a { Data .char}
                        If .func() Then {
                            If .char=46 Then {
                                  Fraction()
                                  Exponent()
                            }
                      }
                  }
                  Case 49 to 57
                  {
                              oldfunc=.func
                              Stack a { Data .char}
                              If .Digits(a) Then {}
                              Fraction()
                              Exponent()
                  }
                  End Select

                  If len(a)>0 Then {
                        b=each(a)
                        Document D$
                        While b {
                              D$=chrcode$(StackItem(b))
                        }
                        .func<=oldfunc
                        If len(D$)>1 Then For i=2 to len(D$) { .Trim}
                        Stack .obj { Push .Numeric(D$) }
                        =True
                  }
                  '  here is an auto exit from function. Sub as command is an exit
                  Sub Fraction()
                        If .char=46 Then Stack a { Data .char}
                        If .Digits(a) Then { }
                  End Sub
                  Sub Exponent()
                        If .char=101 or .char=61 Then {
                              Stack a { Data .char}
                              If .func() Then {
                                    If .char=43 or .char=45 Then {
                                          Stack a { Data .char }
                                          If .Digits(a) Else {
                                                a=Stack
                                          }
                                    }  Else.If .char>47 and .char<58 Then {
                                          Stack a { Data .char}
                                          If .Digits(a) Then {}
                                    }   Else { a=Stack }
                              }
                        }
                  End Sub
            }
            Function Final IsString (as_object){
            If .char=34 Else exit
                  Document D$
                  While .func() {
                        If .char=34 Then 2000
                        If .char=92 Then {
                              ' special care
                              If .func() Then {
                                    Select Case .Char
                                    Case 117 'u
                                    GetHex()
                                    Case 114 ' r
                                    .char<=0x0d
                                    Case 110 ' n
                                    .char<=0x0a
                                    Case 116 ' t
                                    .char<=0x09
                                    Case 98 ' b
                                    .char<=0x08
                                    Case 102 ' f
                                    .char<=0x0c
                                    Case 0x22, 0x2f , 0x5c
                                    rem  ' need a line always - revision 4
                                    Else
                                    Exit   ' not normal
                                    End Select
                              }
                        }
                        D$=chrcode$(.char)
                  }
                  Exit
      2000 Stack .obj {
                  Print D$
                        If as_object Then {Push .JString$(D$)} Else Push D$
                  } : =True
                  Sub GetHex()
                        Local D$
                        Document D$="0x"
                        For i=1 to 4 {
                              If .func() Then {
                                    If Chrcode$(.char) ~ "[0123456789ABCDEFabcdef]"  Then {
                                          D$=Chrcode$(.char)
                                    } Else 3000
                              }
                        }
                        If i<>5 Then 3000
                        .Char<=Eval(D$)
      3000 End Sub
            }
            Function Final IsArray {

                  If .char=91 Else exit
                  Dim Gr()
                  .Trim
                  If .char=93 Then =true : Stack .obj { Push .Arr(Gr())} : exit
                        While .IsValue() {
                              Stack .obj {
                                    Dim Gr(Len(Gr())+1)
                                    Gr(len(Gr())-1)=Group
                              }
                              .Trim
                              If not .IsComma() Then exit
                              .Trim
                        }
                  If .char=93 Then { =true : Stack .obj { Push .Arr(Gr())} } Else .Func<=lambda->false
            }
            Module Final Trim {
                  While .func() {
                         If .char<33 or .char=160 Else exit
                  }
            }
            Function Final IsContainer {
                 .Trim
                 Select Case chrcode$(.char)
                 Case "{"
                        =.IsObject()
                 Case "["
                        =.IsArray()
                 end select
            }
            Module Final ReadArrayItem (temp, object){
                   Select Case temp.type$
                        Case "String","Boolean","Number", "Null"
                        {
                              If object Then Error "No object "+quote$(temp.type$)
                              Push temp.str$
                        }
                        Case "Object"
                        {
                              If not Empty Then {
                                 Call .ReadObject temp, object, letter$
                              } Else {
                                    If object Then Push Temp : exit
                                    Push .ser$(group(temp),0)
                              }
                        }
                        Case "Array"
                        {
                              If not Empty Then {
                                    ' recursion only with Call statement for modules
                                    Call .ReadArrayItem, Array(temp, number), object
                              } Else {
                                    If object Then Push Temp : exit
                                    Push .ser$(group(temp),0)
                              }
                        }
                        End Select
            }
            Module Final ReadObject (json, object){
                  If type$(json)="Inventory" Then {
                        If exist(json, Letter$) Then {
                              temp=eval(json)
                        } Else {
                             push "none"
                             Break  ' exit Module Final  (Break do something Else in Select End Select)
                        }
                  } Else temp=json
                        Select Case temp.type$
                        Case "String","Boolean","Number", "Null"
                        {
                              If object Then Error "No object "+quote$(temp.type$)
                              Push temp.str$
                        }
                        Case "Object"
                        {
                              If not Empty Then {
                                    Call .ReadObject temp, object
                              } Else {
                                    If object Then Push Temp : exit
                                    Push .ser$(group(temp),0)
                              }
                        }
                        Case "Array"
                        {
                              If not Empty Then {
                                    Call .ReadArrayItem array(temp, number), object
                              } Else {
                                    If object Then Push Temp : exit
                                    Push .ser$(group(temp),0)
                              }
                        }
                        End Select
            }
            Module Final Worker (object){
                         If match("IN") Or match("IS") Then {
                               Push object : ShiftBack 2
                              .ReadObject
                        } Else {
                              read Temp
                              If Type$(Temp)="mArray" Then {
                                    If not Empty Then {
                                          Call .ReadArrayItem, Array(Temp, number), object
                                    } Else {
                                          If object Then Push Temp : exit
                                          Push .ser$(Temp,0)
                                    }
                              } Else {
                                    If not Empty Then {
                                                Call .ReadObject Temp, object
                                    } Else {
                                          If not Empty Then {
                                                Call .ReadObject Temp, object
                                          } Else {
                                                If object Then Push Temp : exit
                                                If Type$(Temp)="Inventory" Then {
                                                      Push .ser$(.Object(Temp),0)
                                                } Else {
                                                      Push .ser$(group(Temp),0)
                                                }
                                          }
                                    }
                              }
                        }
            }
      Public:
            Class Arr {
            Private:
                  MyValue
            Public:
                  Property Type$ {Value} ="Array"
                  Value {
                        =.MyValue
                  }
            Class:
                  Module Final Arr (.MyValue) {}
            }
            Class Null {
                 Property Type$ {Value} ="Null"
                 Property Str$ {Value}="null"
                 Value { =0}
            }
            Class JString$ {
            Private:
                  MyValue$=""
            Public:
                  Property Type$ {Value} ="String"
                  Property Str$ {
                        Value{
                              Link parent MyValue$ to MyValue$
                              value$=quote$(string$(MyValue$ as json))
                        }
                  }
                  Value {
                        =.MyValue$
                  }
            Class:
                  Module Final JString (.MyValue$) {}
            }
            Class Numeric {
            Private:
                  MyValue$=""
            Public:
                  Property Type$ {Value} ="Number"
                  Property Str$ {
                        Value{
                              Link parent MyValue$ to MyValue$
                              value$=MyValue$
                        }
                  }
                  Value {
                        =Val(.MyValue$)
                  }
            Class:
                  Module Final Numeric {
                  If match("S") Then {
                        Read .MyValue$
                  } Else {
                        .Myvalue$<=trim$(str$(Number, 1033))
                  }
                  }
            }
            Class Boolean {
            Private:
                  MyValue=false
            Public:
                  Property Type$ {Value} ="Boolean"
                  Property Str$ {
                        Value{
                              Link parent MyValue to MyValue
                              If MyValue Then {
                                    value$="true"
                              } Else value$="false"
                        }
                  }
                  Value {
                        =.MyValue
                  }
            Class:
                  Module Final Boolean (.MyValue) {}
            }
            Class Object {
            Private:
                  Inventory MyValue
            Public:
                  Property Type$ {Value} ="Object"
                  Value {
                        =.MyValue
                  }
            Class:
                  Module Final Object (.MyValue) {}
            }
            Group Ser$
            Module Final SetSpace (.ser.space) {
            }
            Function Final UseDecimalPoint$ {
                  =str$(val(letter$),"")
            }
            Function Final ReadNumber$ {
                        .Worker false
                        =.UseDecimalPoint$( Letter$)
            }           
            Function Final ReadAnyString$ {
                        .Worker false
                        =Letter$
            }
            Function Final ReadAny {
                        .Worker true
                        Read A
                        =A
            }
            Function Final Eval {
                   .func<=Lambda z=.bStream(Letter$) -> {
                         link .char to c
                         =z(&c)
                   }
                  Stack .obj { Flush}
                  .char<=0
                  If .IsContainer() Then {
                        =StackItem(.obj)
                        .obj<=Stack
                  } Else {
                        inventory emptinv
                        =.Object(emptinv)
                  }
            }
            Group StringValue$ {
                  Add=false
                  Del=false
                  Set (temp) {
                        Read temp1
                        If type$(temp)<>"Group" Then error "Need a group"
                        If not valid(temp.type$="") Then error "not a proper group"
                        If not valid(temp1.type$="") Then error "not a proper group for value"
                        Link parent Null() to MyNull()
                        Null=MyNull()
                        Dim Base 1, A(1)
                        b=(,) : Link b to bb()
                        A(1)=Group(temp)
                        Do {
                              again=false
                              Select Case A(1).type$
                              Case "Array"
                              {
                                    If match("N") Then {
                                          Read where
                                          If len(A(1))<=where and Empty Then {
                                                If .add and not .del Then {
                                                cursize=Len(A(1))
                                                b=A(1) ' A(1) has a pointer so now b has the same pointer
                                                Dim bb(where+1) ' need one more because all "automatic arrays" have base 0
                                                Stock bb(cursize) sweep Len(b)-cursize, Group(Null)
                                                } Else Error "Index out of limits"+str$(where)
                                          } Else If where<0 Then Error "Index out of limits "+str$(where)
                                          If Empty Then {
                                                If .del Then {
                                                      cursize=Len(A(1))
                                                      b=A(1) ' A(1) has a pointer so now b has the same pointer
                                                      If where<cursize-1 Then {
                                                            Stock bb(where+1) Keep cursize-where, bb(where)
                                                      }
                                                      Dim bb(cursize-1) ' bb(0) is an empty array
                                                } Else Return A(1), where:=Group(temp1)
                                          } Else {
                                                A(1)=Array(A(1),where)
                                                again=True
                                          }
                                    } Else Error "No Index Found"
                              }
                              Case "Object"
                              {
                                    If match("S") Then {
                                          Read k$
                                          If Exist(A(1), k$) Then {
                                                If Empty Then {
                                                      If .del Then {
                                                           Delete A(1) , k$
                                                      } else {
                                                            Return A(1), k$:=Group(temp1)
                                                      }
                                                } Else {
                                                      A(1)=Eval(A(1))
                                                      again=True
                                                }
                                        } else.if .add and not .del Then {
                                                 If Empty Then {
                                                            Append A(1), k$:=Group(temp1)
                                                } Else Error "No such Tag "+k$
                                          } Else Error "No such Tag "+k$
                                    } Else Error "No Tag Found"
                              }
                              End Select
                        } until not again
                  }
                  Value (temp) {
                        If type$(temp)<>"Group" Then error "Need a group"
                        If not valid(temp.type$="") Then error "not a proper group"
                        Dim Base 1, A(1)
                        A(1)=Group(temp)
                        Do {
                              again=false
                              Select Case A(1).type$
                              Case "String", "Number", "Null", "Boolean"
                                    Exit
                              Case "Array"
                              {
                                    If match("N") Then {
                                          A(1)=Array(A(1), Number)
                                    } Else Error "No Index Found"
                                    again=True
                              }
                              Case "Object"
                              {
                                    If match("S") Then {
                                          If Exist(A(1), Letter$) Then {
                                                A(1)=Eval(A(1))
                                          } Else Error "No such Tag"
                                    } Else Error "No Tag Found"
                                    again=True
                              }
                              End Select
                        } until not again
                         =A(1).str$
                  }
            }
      Class:
            Class CreatSerialize$ {
            Private:
                  usen=0
                  n=0
                  nl1$={
                  }
                  Function Final Jarray$ (json1, n){
                        A=json1
                        nl$=.nl1$
                        If .usen>0 Then {
                              nl$=nl$+string$(" ", n+.space)
                        }
                        document a$
                        a$="["
                        If Len(A)>0 Then {
                              If .usen>0 Then a$=nl$
                               k=each(A)
                               M=len(A)-1
                               while k {
                                    For This {
                                          Temp=array(k)
                                          select Case temp.type$
                                          Case "Number", "Null","Boolean", "String"
                                          a$=temp.str$
                                          Case "Array"
                                          {
                                                nn=0
                                                If .usen>0 Then {
                                                      nn=n +.space
                                                }
                                                a$=.Jarray$(Temp, nn, "")
                                          }
                                          Case "Object"
                                          {
                                               nn=0
                                                If .usen>0 Then {
                                                      nn=n +.space
                                                }
                                                a$=.Jobject$(Temp, nn,"")
                                          }
                                          Else
                                                a$=" "+temp.type$
                                          end select
                                           If k^<M Then {
                                               a$=", "
                                                If .usen>0 Then a$=nl$
                                          } Else {
                                                If .usen>0 Then a$=.nl1$
                                          }
                                    }
                              }
                        }  else If .usen>0 Then a$=.nl1$
                         If .usen>0 Then a$=string$(" ", n)
                  a$="]"
                     =a$+letter$
                  }
                  Function Final Jobject$ (json1, n){
                                    json=json1
                                    nl$=.nl1$
                                    If .usen>0 Then {
                                          nl$=nl$+string$(" ", n+.space)
                                    }
                                    document a$
                                    a$="{"
                                    If .usen>0 Then a$=nl$
                                     k=each(json)
                                     M=len(json)-1
                                     while k {
                                          a$=quote$(eval$(json, k^)) +" : "
                                          select Case json(k^!).type$
                                          Case "Array"
                                          {
                                                nn=0
                                                If .usen>0 Then {
                                                      nn=n +.space
                                                }
                                                a$=.Jarray$(eval(k), nn, "")
                                          }
                                          Case  "Boolean", "Null", "Number", "String"
                                                a$=json(k^!).str$
                                          Case "Object"
                                          {
                                                nn=0
                                                If .usen>0 Then {
                                                      nn=n +.space
                                                }
                                                a$=.Jobject$(eval(k), nn, "")
                                          }
                                          Else
                                                a$=" "+json( k^!).type$
                                          end select
                                           If k^<M Then {
                                               a$=", "
                                                If .usen>0 Then a$=nl$
                                          } Else {
                                                If .usen>0 Then a$=.nl1$
                                          }
                                    }
                               If .usen>0 Then a$=string$(" ", n)
                              a$="}"
                              =a$+letter$
                  }
                  Class Object {
                  Private:
                        Inventory MyValue
                  Public:
                        Property Type$ {Value} ="Object"
                        Value {
                              =.MyValue
                        }
                  Class:
                        Module Final Object (.MyValue) {}
                  }
            Public:
                  space=10
                  Value (json, n) {
                              a$=.nl1$
                              b$=""
                              .usen<=n
                              n--
                              If n<=0 Then { a$="" : n=0 } Else b$=string$(" ", n)
                              If type$(json)<>"Group" Then {
                                    If type$(json)="Inventory" Then {
                                          =b$+.Jobject$(.Object(json),n, a$)
                                    } else.if type$(json)="mArray" Then {
                                          =b$+.Jarray$(json, n, a$)
                                    }
                              } Else {
                                    If json.type$="Object" Then {
                                          =b$+.Jobject$(json, n,a$)
                                    } else.if json.type$="Array" Then {
                                          =b$+.Jarray$(json, n, a$)
                                    }
                              }
                  }
            }
            Module Final ParserClass {
                  Let .Ser=.CreatSerialize$()
            }
      }
}

```

