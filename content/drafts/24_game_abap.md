+++
title = "24 game/ABAP"
description = ""
date = 2011-01-05T05:27:51Z
aliases = []
[extra]
id = 9107
[taxonomies]
categories = []
tags = []
+++

This is essentially a modified port of the C version.

Firstly we need to make a Reverse Polish Notation parser. To make it easier, I simply put this in the report used in the game itself. The following data declarations should be common to both for ease of use.

### Global Data


```ABAP
report z24_with_rpn
constants: c_eval_to   type i value 24,
           c_tolerance type f value '0.0001'.
data: gt_val type table of f,
      gv_val type f,
      gv_pac type p,
      gv_chk type c.
```



### RPN Code


```ABAP
" Log a message from the RPN Calculator.
form rpn_log using lv_msg type string.
  write : / 'RPN Message: ', lv_msg.
endform.

" Performs add in Reverse Polish Notation.
form rpn_add.
  data: lv_val1 type f,
        lv_val2 type f.
  " Get the last two values from the stack to add together.
  perform rpn_pop changing: lv_val1, lv_val2.
  add lv_val2 to lv_val1.
  " Add them and then add them back to the "top".
  perform rpn_push using lv_val1.
endform.

" Perform subtraction in RPN.
form rpn_sub.
  data: lv_val1 type f,
        lv_val2 type f.
  " Get the last two values, subtract them, and push them back on.
  perform rpn_pop changing: lv_val1, lv_val2.
  subtract lv_val1 from lv_val2.
  perform rpn_push using lv_val2.
endform.

" Perform multiplication in RPN.
form rpn_mul.
  data: lv_val1 type f,
        lv_val2 type f.
  " Get the last two values, multiply, and push them back.
  perform rpn_pop changing: lv_val1, lv_val2.
  multiply lv_val1 by lv_val2.
  perform rpn_push using lv_val1.
endform.

" Perform division in RPN.
form rpn_div.
  data: lv_val1 type f,
        lv_val2 type f.
  " Get the last two values, divide the first by the second
  " and then add it back to the stack.
  perform rpn_pop changing: lv_val1, lv_val2.
  divide lv_val1 by lv_val2.
  perform rpn_push using lv_val1.
endform.

" Negate a number in RPN.
form rpn_neg.
  data: lv_val type f.
  " Simply get the last number and negate it before pushing it back.
  perform rpn_pop changing lv_val.
  multiply lv_val by -1.
  perform rpn_push using lv_val.
endform.

" Swap the top two values on the RPN Stack.
form rpn_swap.
  data: lv_val1 type f,
        lv_val2 type f.
  " Get the top two values and then add them back in reverse order.
  perform rpn_pop changing: lv_val1, lv_val2.
  perform rpn_push using: lv_val2, lv_val1.
endform.

" Call the relevant RPN operation.
form rpn_call_op using iv_op type string.
  case iv_op.
    when '+'.
      perform rpn_add.
    when '-'.
      perform rpn_sub.
    when '*'.
      perform rpn_mul.
    when '/'.
      perform rpn_div.
    when 'n'.
      perform rpn_neg.
    when 's'.
      perform rpn_swap.
    when others. " Bad op-code found!
      perform rpn_log using 'Operation not found!'.
    endcase.
endform.

" Reverse_Polish_Notation Parser.
form rpn_pop changing ev_out type f.
  " Attempt to get the entry from the 'top' of the table.
  " If it's empty --> log an error and bail.
  data: lv_lines type i.

  describe table gt_val lines lv_lines.
  if lv_lines > 0.
    " After we have retrieved the value, we must remove it from the table.
    read table gt_val index lv_lines into ev_out.
    delete gt_val index lv_lines.
  else.
    perform rpn_log using 'RPN Stack is empty! Underflow!'.
    ev_out = 0.
  endif.
endform.

" Pushes the supplied value onto the RPN table / stack.
form rpn_push using iv_val type f.
  " Simple append - other languages this involves a stack of a certain size.
  append iv_val to gt_val.
endform.

" Refreshes the RPN stack / table.
form rpn_reset.
  " Clear the stack to start anew.
  refresh gt_val.
endform.

" Checks if the supplied string is numeric.
" Lazy evaluation - only checkcs for numbers without formatting.
form rpn_numeric using    iv_in  type string
                 changing ev_out type c.
  data: lv_moff type i,
        lv_len  type i.
  " Match digits with optional decimal places.
  find regex '\d+(\.\d+)*' in iv_in
    match offset lv_moff
    match length lv_len.
  " Get the offset and length of the first occurence, and work
  " out the length of the match.
  subtract lv_moff from lv_len.
  " If the length is different to the length of the whole string,
  " then it's NOT a match, else it is.
  if lv_len ne strlen( iv_in ).
    ev_out = ' '.
  else.
    ev_out = 'X'.
  endif.
endform.

" Convert input to a number. Added safety net of is_numeric.
form rpn_get_num using iv_in type string changing ev_num type f.
  data: lv_check type c.
  " Check if it's numeric - built in redundancy.
  perform rpn_numeric using iv_in changing lv_check.
  if lv_check = 'X'.
    ev_num = iv_in.
  else.
    perform rpn_log using 'Wrong call!'.
  endif.
endform.

" Evaluate the RPN expression and return true if success in eval.
form rpn_eval using in_expr type string changing ev_out type c.
  data: lv_len  type i,
        lv_off  type i value 0,
        lv_num  type c,
        lv_val  type f,
        lv_tok  type string.

  lv_len = strlen( in_expr ).
  do lv_len times.
    lv_tok = in_expr+lv_off(1).
    perform rpn_numeric using lv_tok changing lv_num.
    if lv_num = 'X'.
      perform: rpn_get_num using lv_tok changing lv_val,
               rpn_push    using lv_val.
    else.

      perform rpn_call_op using lv_tok.
    endif.
    add 1 to lv_off.
  enddo.

  ev_out = 'X'.
endform.
```



### 24 Game

We can now play the game since we have a parser.
The interface is a hacked up screen, and is a bit more clunky than even a CLI (No such option for ABAP, at least not which I'm aware of).

The supplied Random Number Generator seems to highly favour a five as the first digit as well (It does occasionally take on other values). It doesn't appear to be a seeding issue, as the other numbers appear sufficiently random.

```ABAP

selection-screen begin of block main with frame title lv_title.
  parameters:
    p_first  type i,
    p_second type i,
    p_third  type i,
    p_fourth type i,
    p_expr   type string.
selection-screen end of block main.

initialization.
  perform ranged_rand using 1 9 changing p_first.
  perform ranged_rand using 1 9 changing p_second.
  perform ranged_rand using 1 9 changing p_third.
  perform ranged_rand using 1 9 changing p_fourth.

at selection-screen output.
  " Set-up paramter texts.
  lv_title = 'Reverse Polish Notation Tester - Enter expression that evaluates to 24.'.
  %_p_first_%_app_%-text  = 'First Number: '.
  %_p_second_%_app_%-text = 'Second Number: '.
  %_p_third_%_app_%-text  = 'Third Number: '.
  %_p_fourth_%_app_%-text = 'Fourth Number: '.
  %_p_expr_%_app_%-text   = 'Expression: '.
  " Disallow modification of supplied numbers.
  loop at screen.
    if screen-name = 'P_FIRST' or  screen-name = 'P_SECOND' or
       screen-name = 'P_THIRD' or  screen-name = 'P_FOURTH'.
      screen-input = '0'.
      modify screen.
    endif.
  endloop.

start-of-selection.
  " Check the expression is valid.
  perform check_expr using p_expr changing gv_chk.
  if gv_chk <> 'X'.
    write : / 'Invalid input!'.
    stop.
  endif.
  " Check if the expression actually evalutes.
  perform rpn_eval using p_expr changing gv_chk.
  " If it doesn't, warning!.
  if gv_chk <> 'X'.
    write : / 'Invalid expression!'.
    stop.
  endif.
  " Get the evaluated value. Transform it to something that displays a bit better.
  " Then check if it's a valid answer, with a certain tolerance.
  " If they're wrong, give them instructions to on how to go back.
  perform rpn_pop changing gv_val.
  gv_pac = gv_val.
  gv_val = abs( gv_val - c_eval_to ).
  if gv_val < c_tolerance.
    write : / 'Answer correct'.
  else.
    write : / 'Your expression evalutes to ', gv_pac.
    write : / 'Press "F3" to go back and try again!'.
  endif.
  write : / 'Re-run the program to generate a new set.'.

 " Check that the input expression is valid - i.e. all supplied numbers
 " appears exactly once. This does not validate the expression itself.
form check_expr using iv_exp type string changing ev_ok type c.
  data: lv_chk  type c,
        lv_tok  type string,
        lv_val  type i value 0,
        lv_len  type i,
        lv_off  type i,
        lv_num  type i,
        lt_nums type standard table of i.
  ev_ok = 'X'.
  " Update the number count table - indexes 1-9 correspond to numbers.
  " The value stored corresponds to the number of occurences.
  do 9 times.
    if p_first = sy-index.
      add 1 to lv_val.
    endif.
    if p_second = sy-index.
      add 1 to lv_val.
    endif.
    if p_third = sy-index.
      add 1 to lv_val.
    endif.
    if p_fourth = sy-index.
      add 1 to lv_val.
    endif.

    append lv_val to lt_nums.
    lv_val = 0.
  enddo.
  " Loop through the expression parsing the numbers.
  lv_len = strlen( p_expr ).
  do lv_len times.
    lv_tok = p_expr+lv_off(1). " Check if the current token is a number.
    perform rpn_numeric using lv_tok changing lv_chk.
    if lv_chk = 'X'.
      lv_num = lv_tok. " If it's a number, it must be from 1 - 9.
      if lv_num < 1 or lv_num > 9.
        ev_ok = ' '.
        write : / 'Numbers must be between 1 and 9!'.
        return.
      else.
        " Check how many times the number was supplied. If it wasn't supplied
        " or if we have used it up, we should give an error.
        read table lt_nums index lv_num into lv_val.
        if lv_val <= 0.
          ev_ok = ' '.
          write : / 'You can not use numbers more than once'.
          return.
        endif.
        " If we have values left for this number, we decrement the remaining amount.
        subtract 1 from lv_val.
        modify lt_nums index lv_num from lv_val.
      endif.
    endif.
    add 1 to lv_off.
  enddo.
  " Loop through the table and check we have no numbers left for use.
  do 9 times.
    read table lt_nums index  sy-index into lv_val.
    if lv_val > 0.
      write : / 'You must use all numbers'.
      ev_ok = ' '.
      return.
     endif.
  enddo.
endform.

" Generate a random number within the given range.
form ranged_rand using iv_min type i iv_max type i
                 changing ev_val type i.
  call function 'QF05_RANDOM_INTEGER'
    exporting
      ran_int_max = iv_max
      ran_int_min = iv_min
    importing
      ran_int     = ev_val.
endform.
```

