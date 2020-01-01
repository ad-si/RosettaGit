+++
title = "LZW compression"
description = ""
date = 2019-10-14T10:57:45Z
aliases = []
[extra]
id = 2981
[taxonomies]
categories = []
tags = []
+++

{{task|Compression}}

The Lempel-Ziv-Welch (LZW) algorithm provides loss-less data compression.

You can read a complete description of it in the   [[wp:Lempel-Ziv-Welch|Wikipedia article]]   on the subject.   It was patented, but it entered the public domain in 2004.





## 11l

{{trans|Python}}

```11l
F compress(uncompressed)
   V dict_size = 256
   V dictionary = Dict((0 .< dict_size).map(i -> (String(Char(code' i)), i)))
   V w = ‘’
   [Int] result
   L(c) uncompressed
      V wc = w‘’c
      I wc C dictionary
         w = wc
      E
         result.append(dictionary[w])
         dictionary[wc] = dict_size
         dict_size++
         w = c

   I !w.empty
      result.append(dictionary[w])

   R result

F decompress([Int] &compressed)
   V dict_size = 256
   V dictionary = Dict((0 .< dict_size).map(i -> (i, String(Char(code' i)))))
   V result = ‘’
   V w = String(Char(code' compressed.pop(0)))
   result ‘’= w
   L(k) compressed
      V entry = ‘’
      I k C dictionary
         entry = dictionary[k]
      E I k == dict_size
         entry = w‘’w[0]
      E
         exit(‘Bad compressed k: ’k)
      result ‘’= entry
      dictionary[dict_size] = w‘’entry[0]
      dict_size++
      w = entry

   R result

V compressed = compress(‘TOBEORNOTTOBEORTOBEORNOT’)
print(compressed)
print(decompress(&compressed))
```



## Ada

{{works with|Ada 2005}}

lzw.ads:

```Ada
package LZW is

   MAX_CODE : constant := 4095;

   type Codes is new Natural range 0 .. MAX_CODE;
   type Compressed_Data is array (Positive range <>) of Codes;

   function Compress (Cleartext : in String) return Compressed_Data;
   function Decompress (Data : in Compressed_Data) return String;

end LZW;
```


lzw.adb:

```Ada
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

package body LZW is
   package UStrings renames Ada.Strings.Unbounded;
   use type UStrings.Unbounded_String;

   --------------
   -- Compress --
   --------------

   function Compress (Cleartext : in String) return Compressed_Data is
      -- translate String to Code-ID
      package String_To_Code is new Ada.Containers.Ordered_Maps (
         Key_Type => UStrings.Unbounded_String,
         Element_Type => Codes);

      Dictionary : String_To_Code.Map;
      -- Next unused Code-ID
      Next_Entry : Codes := 256;

      -- maximum same length as input, compression ratio always >=1.0
      Result : Compressed_Data (1 .. Cleartext'Length);
      -- position for next Code-ID
      Result_Index : Natural := 1;

      -- current and next input string
      Current_Word : UStrings.Unbounded_String :=
        UStrings.Null_Unbounded_String;
      Next_Word    : UStrings.Unbounded_String :=
        UStrings.Null_Unbounded_String;
   begin
      -- initialize Dictionary
      for C in Character loop
         String_To_Code.Insert
           (Dictionary,
            UStrings.Null_Unbounded_String & C,
            Character'Pos (C));
      end loop;

      for Index in Cleartext'Range loop
         -- add character to current word
         Next_Word := Current_Word & Cleartext (Index);
         if String_To_Code.Contains (Dictionary, Next_Word) then
            -- already in dictionary, continue with next character
            Current_Word := Next_Word;
         else
            -- insert code for current word to result
            Result (Result_Index) :=
               String_To_Code.Element (Dictionary, Current_Word);
            Result_Index          := Result_Index + 1;
            -- add new Code to Dictionary
            String_To_Code.Insert (Dictionary, Next_Word, Next_Entry);
            Next_Entry := Next_Entry + 1;
            -- reset current word to one character
            Current_Word := UStrings.Null_Unbounded_String &
                            Cleartext (Index);
         end if;
      end loop;
      -- Last word was not entered
      Result (Result_Index) :=
         String_To_Code.Element (Dictionary, Current_Word);
      -- return correct array size
      return Result (1 .. Result_Index);
   end Compress;

   ----------------
   -- Decompress --
   ----------------

   function Decompress (Data : in Compressed_Data) return String is
      -- translate Code-ID to String
      type Code_To_String is array (Codes) of UStrings.Unbounded_String;

      Dictionary : Code_To_String;
      -- next unused Code-ID
      Next_Entry : Codes := 256;

      -- initialize resulting string as empty string
      Result : UStrings.Unbounded_String := UStrings.Null_Unbounded_String;

      Next_Code : Codes;
      -- first code has to be in dictionary
      Last_Code : Codes := Data (1);
      -- suffix appended to last string for new dictionary entry
      Suffix : Character;
   begin
      -- initialize Dictionary
      for C in Character loop
         Dictionary (Codes (Character'Pos (C)))   :=
           UStrings.Null_Unbounded_String & C;
      end loop;

      -- output first Code-ID
      UStrings.Append (Result, Dictionary (Last_Code));
      for Index in 2 .. Data'Last loop
         Next_Code := Data (Index);
         if Next_Code <= Next_Entry then
            -- next Code-ID already in dictionary -> append first char
            Suffix := UStrings.Element (Dictionary (Next_Code), 1);
         else
            -- next Code-ID not in dictionary -> use char from last ID
            Suffix := UStrings.Element (Dictionary (Last_Code), 1);
         end if;
         -- expand the dictionary
         Dictionary (Next_Entry) := Dictionary (Last_Code) & Suffix;
         Next_Entry              := Next_Entry + 1;
         -- output the current Code-ID to result
         UStrings.Append (Result, Dictionary (Next_Code));
         Last_Code := Next_Code;
      end loop;
      -- return String
      return UStrings.To_String (Result);
   end Decompress;

end LZW;
```


test.adb:

```Ada
with LZW;
with Ada.Text_IO;

procedure Test is
   package Text_IO renames Ada.Text_IO;
   package Code_IO is new Ada.Text_IO.Integer_IO (LZW.Codes);

   Test_Data : constant LZW.Compressed_Data :=
      LZW.Compress ("TOBEORNOTTOBEORTOBEORNOT");
begin
   for Index in Test_Data'Range loop
      Code_IO.Put (Test_Data (Index), 0);
      Text_IO.Put (" ");
   end loop;
   Text_IO.New_Line;
   declare
      Cleartext : constant String := LZW.Decompress (Test_Data);
   begin
      Text_IO.Put_Line (Cleartext);
   end;
end Test;
```



## BaCon


```bacon
CONST lzw_data$ = "TOBEORNOTTOBEORTOBEORNOT"

PRINT "LZWData: ", lzw_data$
encoded$ = Encode_LZW$(lzw_data$)
PRINT "Encoded: ", encoded$
PRINT "Decoded: ", Decode_LZW$(encoded$)

'----------------------------------------------------------

FUNCTION Encode_LZW$(sample$)

    LOCAL dict ASSOC int
    LOCAL ch$, buf$, result$
    LOCAL nr, x

    FOR nr = 0 TO 255
        dict(CHR$(nr)) = nr
    NEXT

    FOR x = 1 TO LEN(sample$)

        ch$ = MID$(sample$, x, 1)

        IF dict(buf$ & ch$) THEN
            buf$ = buf$ & ch$
        ELSE
            result$ = APPEND$(result$, 0, STR$(dict(buf$)))
            dict(buf$ & ch$) = nr
            INCR nr
            buf$ = ch$
        END IF
    NEXT

    result$ = APPEND$(result$, 0, STR$(dict(buf$)))

    RETURN result$

END FUNCTION

'----------------------------------------------------------

FUNCTION Decode_LZW$(sample$)

    LOCAL list$ ASSOC STRING
    LOCAL old$, ch$, x$, out$, result$
    LOCAL nr

    FOR nr = 0 TO 255
        list$(STR$(nr)) = CHR$(nr)
    NEXT

    old$ = TOKEN$(sample$, 1)

    ch$ = list$(old$)
    result$ = ch$

    FOR x$ IN LAST$(sample$, 1)

        IF NOT(LEN(list$(x$))) THEN
            out$ = list$(old$)
            out$ = out$ & ch$
        ELSE
            out$ = list$(x$)
        END IF

        result$ = result$ & out$
        ch$ = LEFT$(out$, 1)
        list$(STR$(nr)) = list$(old$) & ch$

        INCR nr
        old$ = x$
    NEXT

    RETURN result$

END FUNCTION
```

{{out}}

```txt
LZWData: TOBEORNOTTOBEORTOBEORNOT
Encoded: 84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263
Decoded: TOBEORNOTTOBEORTOBEORNOT

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
Uses fixed bit-width (16 bits) and initial dictionary size = 256.

```bbcbasic
      plaintext$ = "TOBEORNOTTOBEORTOBEORNOT"
      encodeLZW$ = FNencodeLZW(plaintext$)
      FOR i% = 1 TO LEN(encodeLZW$) STEP 2
        PRINT ; ASCMID$(encodeLZW$,i%) + 256*ASCMID$(encodeLZW$,i%+1) " " ;
      NEXT
      PRINT ' FNdecodeLZW(encodeLZW$)
      END

      DEF FNencodeLZW(i$)
      LOCAL c%, d%, i%, l%, o$, w$, dict$()
      DIM dict$(4095)
      FOR i% = 0 TO 255 : dict$(i%) = CHR$(i%) : NEXT
      l% = i%
      i% = 1
      w$ = LEFT$(i$,1)
      REPEAT
        d% = 0
        REPEAT
          c% = d%
          IF i% > LEN(i$) EXIT REPEAT
          FOR d% = 1 TO l%-1
            IF w$ = dict$(d%) EXIT FOR
          NEXT d%
          IF d% < l% i% += 1 : w$ += MID$(i$, i%, 1)
        UNTIL d% >= l%
        dict$(l%) = w$ : l% += 1 : w$ = RIGHT$(w$)
        o$ += CHR$(c% MOD 256) + CHR$(c% DIV 256)
      UNTIL i% >= LEN(i$)
      = o$

      DEF FNdecodeLZW(i$)
      LOCAL c%, i%, l%, o$, t$, w$, dict$()
      DIM dict$(4095)
      FOR i% = 0 TO 255 : dict$(i%) = CHR$(i%) : NEXT
      l% = i%
      c% = ASC(i$) + 256*ASCMID$(i$,2)
      w$ = dict$(c%)
      o$ = w$
      IF LEN(i$) < 4 THEN = o$
      FOR i% = 3 TO LEN(i$) STEP 2
        c% = ASCMID$(i$,i%) + 256*ASCMID$(i$,i%+1)
        IF c% < l% t$ = dict$(c%) ELSE t$ = w$ + LEFT$(w$,1)
        o$ += t$
        dict$(l%) = w$ + LEFT$(t$,1)
        l% += 1
        w$ = t$
      NEXT
      = o$
```

{{out}}

```txt

84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263
TOBEORNOTTOBEORTOBEORNOT

```



## C

LZW encoder/decoder.  Using variable bit length from 9 to up to 15.
Encoder needs to know max allow bits, decoder doesn't.
Code 256 for clear table, 257 for end of data,
everything else are either byte values (<256) or code values.

'''WARNING: This code appears to have come from a GIF codec that has been modified to meet the requirements of this page, provided that the decoder works with the encoder to produce correct output. For writing GIF files the write_bits subroutine is wrong for Little Endian systems (it may be wrong for Big Endian as well.) The encoder also increases the number of bits in the variable length GIF-LZW after the N-2 code, whereas this must be done after N-1 to produce a working GIF file (just looking at the encoder, it's easy to see how this mistake could be made.)'''


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

/* -------- aux stuff ---------- */
void* mem_alloc(size_t item_size, size_t n_item)
{
  size_t *x = calloc(1, sizeof(size_t)*2 + n_item * item_size);
  x[0] = item_size;
  x[1] = n_item;
  return x + 2;
}

void* mem_extend(void *m, size_t new_n)
{
  size_t *x = (size_t*)m - 2;
  x = realloc(x, sizeof(size_t) * 2 + *x * new_n);
  if (new_n > x[1])
    memset((char*)(x + 2) + x[0] * x[1], 0, x[0] * (new_n - x[1]));
  x[1] = new_n;
  return x + 2;
}

inline void _clear(void *m)
{
  size_t *x = (size_t*)m - 2;
  memset(m, 0, x[0] * x[1]);
}

#define _new(type, n) mem_alloc(sizeof(type), n)
#define _del(m)   { free((size_t*)(m) - 2); m = 0; }
#define _len(m)   *((size_t*)m - 1)
#define _setsize(m, n)  m = mem_extend(m, n)
#define _extend(m)  m = mem_extend(m, _len(m) * 2)


/* ----------- LZW stuff -------------- */
typedef uint8_t byte;
typedef uint16_t ushort;

#define M_CLR 256 /* clear table marker */
#define M_EOD 257 /* end-of-data marker */
#define M_NEW 258 /* new code index */

/* encode and decode dictionary structures.
   for encoding, entry at code index is a list of indices that follow current one,
   i.e. if code 97 is 'a', code 387 is 'ab', and code 1022 is 'abc',
   then dict[97].next['b'] = 387, dict[387].next['c'] = 1022, etc. */
typedef struct {
  ushort next[256];
} lzw_enc_t;

/* for decoding, dictionary contains index of whatever prefix index plus trailing
   byte.  i.e. like previous example,
    dict[1022] = { c: 'c', prev: 387 },
    dict[387]  = { c: 'b', prev: 97 },
    dict[97]   = { c: 'a', prev: 0 }
   the "back" element is used for temporarily chaining indices when resolving
   a code to bytes
 */
typedef struct {
  ushort prev, back;
  byte c;
} lzw_dec_t;

byte* lzw_encode(byte *in, int max_bits)
{
  int len = _len(in), bits = 9, next_shift = 512;
  ushort code, c, nc, next_code = M_NEW;
  lzw_enc_t *d = _new(lzw_enc_t, 512);

  if (max_bits > 15) max_bits = 15;
  if (max_bits < 9 ) max_bits = 12;

  byte *out = _new(ushort, 4);
  int out_len = 0, o_bits = 0;
  uint32_t tmp = 0;

  inline void write_bits(ushort x) {
    tmp = (tmp << bits) | x;
    o_bits += bits;
    if (_len(out) <= out_len) _extend(out);
    while (o_bits >= 8) {
      o_bits -= 8;
      out[out_len++] = tmp >> o_bits;
      tmp &= (1 << o_bits) - 1;
    }
  }

  //write_bits(M_CLR);
  for (code = *(in++); --len; ) {
    c = *(in++);
    if ((nc = d[code].next[c]))
      code = nc;
    else {
      write_bits(code);
      nc = d[code].next[c] = next_code++;
      code = c;
    }

    /* next new code would be too long for current table */
    if (next_code == next_shift) {
      /* either reset table back to 9 bits */
      if (++bits > max_bits) {
        /* table clear marker must occur before bit reset */
        write_bits(M_CLR);

        bits = 9;
        next_shift = 512;
        next_code = M_NEW;
        _clear(d);
      } else  /* or extend table */
        _setsize(d, next_shift *= 2);
    }
  }

  write_bits(code);
  write_bits(M_EOD);
  if (tmp) write_bits(tmp);

  _del(d);

  _setsize(out, out_len);
  return out;
}

byte* lzw_decode(byte *in)
{
  byte *out = _new(byte, 4);
  int out_len = 0;

  inline void write_out(byte c)
  {
    while (out_len >= _len(out)) _extend(out);
    out[out_len++] = c;
  }

  lzw_dec_t *d = _new(lzw_dec_t, 512);
  int len, j, next_shift = 512, bits = 9, n_bits = 0;
  ushort code, c, t, next_code = M_NEW;

  uint32_t tmp = 0;
  inline void get_code() {
    while(n_bits < bits) {
      if (len > 0) {
        len --;
        tmp = (tmp << 8) | *(in++);
        n_bits += 8;
      } else {
        tmp = tmp << (bits - n_bits);
        n_bits = bits;
      }
    }
    n_bits -= bits;
    code = tmp >> n_bits;
    tmp &= (1 << n_bits) - 1;
  }

  inline void clear_table() {
    _clear(d);
    for (j = 0; j < 256; j++) d[j].c = j;
    next_code = M_NEW;
    next_shift = 512;
    bits = 9;
  };

  clear_table(); /* in case encoded bits didn't start with M_CLR */
  for (len = _len(in); len;) {
    get_code();
    if (code == M_EOD) break;
    if (code == M_CLR) {
      clear_table();
      continue;
    }

    if (code >= next_code) {
      fprintf(stderr, "Bad sequence\n");
      _del(out);
      goto bail;
    }

    d[next_code].prev = c = code;
    while (c > 255) {
      t = d[c].prev; d[t].back = c; c = t;
    }

    d[next_code - 1].c = c;

    while (d[c].back) {
      write_out(d[c].c);
      t = d[c].back; d[c].back = 0; c = t;
    }
    write_out(d[c].c);

    if (++next_code >= next_shift) {
      if (++bits > 16) {
        /* if input was correct, we'd have hit M_CLR before this */
        fprintf(stderr, "Too many bits\n");
        _del(out);
        goto bail;
      }
      _setsize(d, next_shift *= 2);
    }
  }

  /* might be ok, so just whine, don't be drastic */
  if (code != M_EOD) fputs("Bits did not end in EOD\n", stderr);

  _setsize(out, out_len);
bail: _del(d);
  return out;
}

int main()
{
  int i, fd = open("unixdict.txt", O_RDONLY);

  if (fd == -1) {
    fprintf(stderr, "Can't read file\n");
    return 1;
  };

  struct stat st;
  fstat(fd, &st);

  byte *in = _new(char, st.st_size);
  read(fd, in, st.st_size);
  _setsize(in, st.st_size);
  close(fd);

  printf("input size:   %d\n", _len(in));

  byte *enc = lzw_encode(in, 9);
  printf("encoded size: %d\n", _len(enc));

  byte *dec = lzw_decode(enc);
  printf("decoded size: %d\n", _len(dec));

  for (i = 0; i < _len(dec); i++)
    if (dec[i] != in[i]) {
      printf("bad decode at %d\n", i);
      break;
    }

  if (i == _len(dec)) printf("Decoded ok\n");


  _del(in);
  _del(enc);
  _del(dec);

  return 0;
}
```



## CoffeeScript

This only does the encoding step for now.


```coffeescript

lzw = (s) ->
  dct = {} # map substrings to codes between 256 and 4096
  stream = [] # array of compression results

  # initialize basic ASCII characters
  for code_num in [0..255]
    c = String.fromCharCode(code_num)
    dct[c] = code_num
  code_num = 256

  i = 0
  while i < s.length
    # Find word and new_word
    #   word = longest substr already encountered, or next character
    #   new_word = word plus next character, a new substr to encode
    word = ''
    j = i
    while j < s.length
      new_word = word + s[j]
      break if !dct[new_word]
      word = new_word
      j += 1

    # stream out the code for the substring
    stream.push dct[word]

    # build up our encoding dictionary
    if code_num < 4096
      dct[new_word] = code_num
      code_num += 1

    # advance thru the string
    i += word.length
  stream

console.log lzw "TOBEORNOTTOBEORTOBEORNOT"

```

{{out}}

```txt

> coffee lzw.coffee
[ 84,
  79,
  66,
  69,
  79,
  82,
  78,
  79,
  84,
  256,
  258,
  260,
  265,
  259,
  261,
  263 ]

```



## Common Lisp

<div class="examplemeta libheader">'''Library:''' [[SMW::off]][[:Category:Babel (library)|Babel]][[Category:Babel (library)]][[SMW::on]]{{#set:Uses library=Babel (library)}}</div>
<!--{{libheader|Babel (library)}}-->
{{trans|Perl}}
This version is based upon the Perl one. It doesn't contain mixed type data at the cost of being more consy. It includes vector operation routines, since using <code>VECTOR-PUSH-APPEND</code> reallocates the whole vector with each call.

The Babel library is required to convert octet vectors to strings.
Lisp strings can contain characters out of the ASCII/latin1 character set, including the whole Unicode range in them.
The exact encoding used is dependent upon the user's locale (<code>LC_CTYPE</code> on Unix).


```lisp
(declaim (ftype (function (vector vector &optional fixnum fixnum) vector)
                vector-append))
(defun vector-append (old new &optional (start2 0) end2)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (prog1 old
    (let* ((old-fill (fill-pointer old))
           (new-fill (+ old-fill (length new))))
      (when (> new-fill (array-dimension old 0))
        (adjust-array old (* 4 new-fill)))
      (setf (fill-pointer old) new-fill)
      (replace old new :start1 old-fill :start2 start2 :end2 end2))))

(declaim (ftype (function (vector t) vector) vector-append1))
(defun vector-append1 (old new)
  (prog1 old
    (let* ((old-fill (fill-pointer old))
           (new-fill (1+ old-fill)))
      (when (> new-fill (array-dimension old 0))
        (adjust-array old (* 4 new-fill)))
      (setf (fill-pointer old) new-fill)
      (setf (aref old old-fill) new))))

(declaim (ftype (function (&optional t) vector) make-empty-vector))
(defun make-empty-vector (&optional (element-type t))
  (make-array 0 :element-type element-type :fill-pointer 0 :adjustable t))


(declaim (ftype (function (t &optional t) vector) make-vector-with-elt))
(defun make-vector-with-elt (elt &optional (element-type t))
  (make-array 1 :element-type element-type
                :fill-pointer 1
                :adjustable t
                :initial-element elt))

(declaim (ftype (function (vector t) vector) vector-append1-new))
(defun vector-append1-new (old new)
  (vector-append1 (vector-append (make-empty-vector 'octet) old)
                  new))

(declaim (ftype (function (vector vector) vector) vector-append-new))
(defun vector-append-new (old new)
  (vector-append (vector-append (make-empty-vector 'octet) old)
                 new))

(deftype octet () '(unsigned-byte 8))

(declaim (ftype (function () hash-table) build-dictionary))
(defun build-dictionary ()
  (let ((dictionary (make-hash-table :test #'equalp)))
    (loop for i below 256
          do (let ((vec (make-vector-with-elt i 'octet)))
               (setf (gethash vec dictionary) vec)))
    dictionary))

(declaim (ftype (function ((vector octet)) (vector octet))
                lzw-compress-octets))
(defun lzw-compress-octets (octets)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop with dictionary-size of-type fixnum = 256
        with w = (make-empty-vector 'octet)
        with result = (make-empty-vector 't)
        with dictionary = (build-dictionary)
        for c across octets
        for wc = (vector-append1-new w c)
        if (gethash wc dictionary) do (setq w wc)
        else do
          (vector-append result (gethash w dictionary))
          (setf (gethash wc dictionary)
                (make-vector-with-elt dictionary-size))
          (incf dictionary-size)
          (setq w (make-vector-with-elt c 'octet))
        finally (unless (zerop (length (the (vector octet) w)))
                  (vector-append result (gethash w dictionary)))
                (return result)))

(declaim (ftype (function (vector) (vector octet)) lzw-decompress))
(defun #1=lzw-decompress (octets)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (zerop (length octets))
    (return-from #1# (make-empty-vector 'octet)))
  (loop with dictionary-size = 256
        with dictionary = (build-dictionary)
        with result = (make-vector-with-elt (aref octets 0) 'octet)
        with w = (copy-seq result)
        for i from 1 below (length octets)
        for k = (make-vector-with-elt (aref octets i) 't)
        for entry = (or (gethash k dictionary)
                        (if (equalp k dictionary-size)
                            (vector-append1-new w (aref w 0))
                            (error "bad compresed entry at pos ~S" i)))
        do (vector-append result entry)
           (setf (gethash (make-vector-with-elt dictionary-size) dictionary)
                 (vector-append1-new w (aref entry 0)))
           (incf dictionary-size)
           (setq w entry)
        finally (return result)))

(defgeneric lzw-compress (datum)
  (:method ((string string))
    (lzw-compress (babel:string-to-octets string)))
  (:method ((octets vector))
    (lzw-compress-octets octets)))

(defun lzw-decompress-to-string (octets)
  (babel:octets-to-string (lzw-decompress octets)))

(defun test (string)
  (assert (equal #2=(lzw-decompress-to-string (lzw-compress string)) string) ()
          "Can't compress ~S properly, got ~S instead" string #2#)
  t)
```


And the format used:


```lisp
CL-USER> (test "TOBEORNOTTOBEORTOBEORNOT")
T
CL-USER> (lzw-compress "TOBEORNOTTOBEORTOBEORNOT")
#(84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263)
CL-USER> (lzw-decompress-to-string *)
"TOBEORNOTTOBEORTOBEORNOT"
```



## C++

{{trans|D}}

```cpp
#include <string>
#include <map>

// Compress a string to a list of output symbols.
// The result will be written to the output iterator
// starting at "result"; the final iterator is returned.
template <typename Iterator>
Iterator compress(const std::string &uncompressed, Iterator result) {
  // Build the dictionary.
  int dictSize = 256;
  std::map<std::string,int> dictionary;
  for (int i = 0; i < 256; i++)
    dictionary[std::string(1, i)] = i;

  std::string w;
  for (std::string::const_iterator it = uncompressed.begin();
       it != uncompressed.end(); ++it) {
    char c = *it;
    std::string wc = w + c;
    if (dictionary.count(wc))
      w = wc;
    else {
      *result++ = dictionary[w];
      // Add wc to the dictionary.
      dictionary[wc] = dictSize++;
      w = std::string(1, c);
    }
  }

  // Output the code for w.
  if (!w.empty())
    *result++ = dictionary[w];
  return result;
}

// Decompress a list of output ks to a string.
// "begin" and "end" must form a valid range of ints
template <typename Iterator>
std::string decompress(Iterator begin, Iterator end) {
  // Build the dictionary.
  int dictSize = 256;
  std::map<int,std::string> dictionary;
  for (int i = 0; i < 256; i++)
    dictionary[i] = std::string(1, i);

  std::string w(1, *begin++);
  std::string result = w;
  std::string entry;
  for ( ; begin != end; begin++) {
    int k = *begin;
    if (dictionary.count(k))
      entry = dictionary[k];
    else if (k == dictSize)
      entry = w + w[0];
    else
      throw "Bad compressed k";

    result += entry;

    // Add w+entry[0] to the dictionary.
    dictionary[dictSize++] = w + entry[0];

    w = entry;
  }
  return result;
}

#include <iostream>
#include <iterator>
#include <vector>

int main() {
  std::vector<int> compressed;
  compress("TOBEORNOTTOBEORTOBEORNOT", std::back_inserter(compressed));
  copy(compressed.begin(), compressed.end(), std::ostream_iterator<int>(std::cout, ", "));
  std::cout << std::endl;
  std::string decompressed = decompress(compressed.begin(), compressed.end());
  std::cout << decompressed << std::endl;

  return 0;
}
```


## C sharp

{{trans|Java}}

```C sharp
using System;
using System.Collections.Generic;
using System.Text;

namespace LZW
{
    public class Program
    {
        public static void Main(string[] args)
        {
            List<int> compressed = Compress("TOBEORNOTTOBEORTOBEORNOT");
            Console.WriteLine(string.Join(", ", compressed));
            string decompressed = Decompress(compressed);
            Console.WriteLine(decompressed);
        }

        public static List<int> Compress(string uncompressed)
        {
            // build the dictionary
            Dictionary<string, int> dictionary = new Dictionary<string, int>();
            for (int i = 0; i < 256; i++)
                dictionary.Add(((char)i).ToString(), i);

            string w = string.Empty;
            List<int> compressed = new List<int>();

            foreach (char c in uncompressed)
            {
                string wc = w + c;
                if (dictionary.ContainsKey(wc))
                {
                    w = wc;
                }
                else
                {
                    // write w to output
                    compressed.Add(dictionary[w]);
                    // wc is a new sequence; add it to the dictionary
                    dictionary.Add(wc, dictionary.Count);
                    w = c.ToString();
                }
            }

            // write remaining output if necessary
            if (!string.IsNullOrEmpty(w))
                compressed.Add(dictionary[w]);

            return compressed;
        }

        public static string Decompress(List<int> compressed)
        {
            // build the dictionary
            Dictionary<int, string> dictionary = new Dictionary<int, string>();
            for (int i = 0; i < 256; i++)
                dictionary.Add(i, ((char)i).ToString());

            string w = dictionary[compressed[0]];
            compressed.RemoveAt(0);
            StringBuilder decompressed = new StringBuilder(w);

            foreach (int k in compressed)
            {
                string entry = null;
                if (dictionary.ContainsKey(k))
                    entry = dictionary[k];
                else if (k == dictionary.Count)
                    entry = w + w[0];

                decompressed.Append(entry);

                // new sequence; add it to the dictionary
                dictionary.Add(dictionary.Count, w + entry[0]);

                w = entry;
            }

            return decompressed.ToString();
        }
    }
}
```


{{out}}

```txt
84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263
TOBEORNOTTOBEORTOBEORNOT
```



## Clojure


```lisp
(defn make-dict []
  (let [vals (range 0 256)]
    (zipmap (map (comp #'list #'char) vals) vals)))

(defn compress [#^String text]
  (loop [t (seq text)
         r '()
         w '()
         dict (make-dict)
         s 256]
    (let [c (first t)]
      (if c
        (let [wc (cons c w)]
          (if (get dict wc)
            (recur (rest t) r wc dict s)
            (recur (rest t) (cons (get dict w) r) (list c) (assoc dict wc s) (inc s))))
        (reverse (if w (cons (get dict w) r) r))))))

(compress "TOBEORNOTTOBEORTOBEORNOT")
```

{{out}}

```lisp
(84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263)
```



## D


### Simpler Version


```d
import std.stdio, std.array;

auto compress(in string original) pure nothrow {
    int[string] dict;
    foreach (immutable char c; char.min .. char.max + 1)
        dict[[c]] = c;

    string w;
    int[] result;
    foreach (immutable ch; original)
        if (w ~ ch in dict)
            w = w ~ ch;
        else {
            result ~= dict[w];
            dict[w ~ ch] = dict.length;
            w = [ch];
        }
    return w.empty ? result : (result ~ dict[w]);
}

auto decompress(in int[] compressed) pure nothrow {
    auto dict = new string[char.max - char.min + 1];
    foreach (immutable char c; char.min .. char.max + 1)
        dict[c] = [c];

    auto w = dict[compressed[0]];
    auto result = w;
    foreach (immutable k; compressed[1 .. $]) {
        auto entry = (k < dict.length) ? dict[k] : w ~ w[0];
        result ~= entry;
        dict ~= w ~ entry[0];
        w = entry;
    }
    return result;
}

void main() {
    auto comp = "TOBEORNOTTOBEORTOBEORNOT".compress;
    writeln(comp, "\n", comp.decompress);
}
```

{{out}}

```txt
[84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263]
TOBEORNOTTOBEORTOBEORNOT
```



### More Refined Version

This longer version is a little more efficient and it uses stronger static typing.

```d
struct LZW {
    import std.array: empty;

    // T is ubyte instead of char because D strings are UTF-8.
    alias T = ubyte;
    alias Tcomp = ushort;
    static assert(Tcomp.sizeof > 1);
    alias Ta = immutable(T)[];

    enum int initDictSize = 256;
    static immutable ubyte[initDictSize] bytes;
    static this() {
        foreach (immutable T i; 0 .. initDictSize)
            bytes[i] = i;
    }

    static Tcomp[] compress(immutable scope T[] original) pure nothrow @safe
    out(result) {
        if (!original.empty)
            assert(result[0] < initDictSize);
    } body {
        if (original.empty)
            return [];
        Tcomp[Ta] dict;
        foreach (immutable b; bytes)
            dict[[b]] = b;

        // Here built-in slices give lower efficiency.
        struct Slice {
            size_t start, end;
            @property opSlice() const pure nothrow @safe @nogc {
                return original[start .. end];
            }
            alias opSlice this;
        }

        Slice w;
        Tcomp[] result;
        foreach (immutable i; 0 .. original.length) {
            auto wc = Slice(w.start, w.end + 1); // Extend slice.
            if (wc in dict) {
                w = wc;
            } else {
                result ~= dict[w];
                assert(dict.length < Tcomp.max); // Overflow guard.
                dict[wc] = cast(Tcomp)dict.length;
                w = Slice(i, i + 1);
            }
        }

        if (!w.empty)
            result ~= dict[w];
        return result;
    }

    static Ta decompress(in Tcomp[] compressed) pure @safe
    in {
        if (!compressed.empty)
            assert(compressed[0] < initDictSize, "Bad compressed");
    } body {
        if (compressed.empty)
            return [];

        auto dict = new Ta[initDictSize];
        foreach (immutable b; bytes)
            dict[b] = [b];

        auto w = dict[compressed[0]];
        auto result = w;
        foreach (immutable k; compressed[1 .. $]) {
            Ta entry;
            if (k < dict.length)
                entry = dict[k];
            else if (k == dict.length)
                entry = w ~ w[0];
            else
                throw new Exception("Bad compressed k.");
            result ~= entry;

            dict ~= w ~ entry[0];
            w = entry;
        }

        return result;
    }
}

void main() {
    import std.stdio, std.string;

    immutable txt = "TOBEORNOTTOBEORTOBEORNOT";
    immutable compressed = LZW.compress(txt.representation);
    compressed.writeln;
    LZW.decompress(compressed).assumeUTF.writeln;
}
```

{{out}}

```txt
[84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263]
TOBEORNOTTOBEORTOBEORNOT
```



### More Efficient Version

{{trans|C}}
This code retains part of the style of the original C code.

```d
enum Marker: ushort {
    CLR = 256, // Clear table marker.
    EOD = 257, // End-of-data marker.
    NEW = 258  // New code index.
}


ubyte[] lzwEncode(scope const(ubyte)[] inp, in uint maxBits) pure nothrow
in {
    assert(maxBits >= 9 && maxBits <= 16);
} body {
    // Encode dictionary array. For encoding, entry at
    // code index is a list of indices that follow current one,
    // i.e. if code 97 is 'a', code 387 is 'ab', and code 1022 is 'abc',
    // then dict[97].next['b'] = 387, dict[387].next['c'] = 1022, etc.
    alias LZWenc = ushort[256];

    auto len = inp.length;
    uint bits = 9;
    auto d = new LZWenc[512];

    auto result = new ubyte[16];
    size_t outLen = 0;
    size_t oBits = 0;
    uint tmp = 0;

    void writeBits(in ushort x) nothrow {
        tmp = (tmp << bits) | x;
        oBits += bits;
        if (result.length / 2 <= outLen)
            result.length *= 2;
        while (oBits >= 8) {
            oBits -= 8;
            assert(tmp >> oBits <= ubyte.max);
            result[outLen] = cast(ubyte)(tmp >> oBits);
            outLen++;
            tmp &= (1 << oBits) - 1;
        }
    }

    // writeBits(Marker.CLR);
    ushort nextCode = Marker.NEW;
    uint nextShift = 512;
    ushort code = inp[0];
    inp = inp[1 .. $]; // popFront.
    while (--len) {
        ushort c = inp[0];
        inp = inp[1 .. $]; // popFront.
        ushort nc = d[code][c];
        if (nc) {
            code = nc;
        } else {
            writeBits(code);
            nc = d[code][c] = nextCode;
            nextCode++;
            code = c;
        }

        // Next new code would be too long for current table.
        if (nextCode == nextShift) {
            // Either reset table back to 9 bits.
            bits++;
            if (bits > maxBits) {
                // Table clear marker must occur before bit reset.
                writeBits(Marker.CLR);

                bits = 9;
                nextShift = 512;
                nextCode = Marker.NEW;
                d[] = LZWenc.init;
            } else { // Or extend table.
                nextShift *= 2;
                d.length = nextShift;
            }
        }
    }

    writeBits(code);
    writeBits(Marker.EOD);
    if (tmp) {
        assert(tmp <= ushort.max);
        writeBits(cast(ushort)tmp);
    }

    return result[0 .. outLen];
}


ubyte[] lzwDecode(scope const(ubyte)[] inp) pure {
    // For decoding, dictionary contains index of whatever prefix
    // index plus trailing ubyte.  i.e. like previous example,
    //     dict[1022] = { c: 'c', prev: 387 },
    //     dict[387]  = { c: 'b', prev: 97 },
    //     dict[97]   = { c: 'a', prev: 0 }
    // the "back" element is used for temporarily chaining indices
    // when resolving a code to bytes.
    static struct LZWdec {
        ushort prev, back;
        ubyte c;
    }

    auto result = new ubyte[4];
    uint outLen = 0;

    void writeOut(in ubyte c) nothrow {
        while (outLen >= result.length)
            result.length *= 2;
        result[outLen] = c;
        outLen++;
    }

    auto d = new LZWdec[512];
    ushort code = 0;
    uint bits = 9;
    uint len = 0;
    uint nBits = 0;
    uint tmp = 0;

    void getCode() nothrow {
        while (nBits < bits) {
            if (len > 0) {
                len--;
                tmp = (tmp << 8) | inp[0];
                inp = inp[1 .. $]; // popFront.
                nBits += 8;
            } else {
                tmp = tmp << (bits - nBits);
                nBits = bits;
            }
        }

        nBits -= bits;
        assert(tmp >> nBits <= ushort.max);
        code = cast(ushort)(tmp >> nBits);
        tmp &= (1 << nBits) - 1;
    }

    uint nextShift = 512;
    ushort nextCode = Marker.NEW;

    void clearTable() nothrow {
        d[] = LZWdec.init;
        foreach (immutable ubyte j; 0 .. 256)
            d[j].c = j;
        nextCode = Marker.NEW;
        nextShift = 512;
        bits = 9;
    }

    clearTable(); // In case encoded bits didn't start with Marker.CLR.
    for (len = inp.length; len;) {
        getCode();
        if (code == Marker.EOD)
            break;
        if (code == Marker.CLR) {
            clearTable();
            continue;
        }

        if (code >= nextCode)
            throw new Error("Bad sequence.");

        auto c = code;
        d[nextCode].prev = c;
        while (c > 255) {
            immutable t = d[c].prev;
            d[t].back = c;
            c = t;
        }

        assert(c <= ubyte.max);
        d[nextCode - 1].c = cast(ubyte)c;

        while (d[c].back) {
            writeOut(d[c].c);
            immutable t = d[c].back;
            d[c].back = 0;
            c = t;
        }
        writeOut(d[c].c);

        nextCode++;
        if (nextCode >= nextShift) {
            bits++;
            if (bits > 16) {
                // If input was correct, we'd have hit Marker.CLR before this.
                throw new Error("Too many bits.");
            }
            nextShift *= 2;
            d.length = nextShift;
        }
    }

    // Might be OK, so throw just an exception.
    if (code != Marker.EOD)
        throw new Exception("Bits did not end in EOD");

    return result[0 .. outLen];
}


void main() {
    import std.stdio, std.file;

    const inputData = cast(ubyte[])read("unixdict.txt");
    writeln("Input size:   ", inputData.length);

    immutable encoded = lzwEncode(inputData, 12);
    writeln("Encoded size: ", encoded.length);

    immutable decoded = lzwDecode(encoded);
    writeln("Decoded size: ", decoded.length);

    if (inputData.length != decoded.length)
        return writeln("Error: decoded size differs");

    foreach (immutable i, immutable x; inputData)
        if (x != decoded[i])
            return writeln("Bad decode at ", i);

    "Decoded OK.".writeln;
}
```

{{out}}

```txt
Input size:   206403
Encoded size: 97633
Decoded size: 206403
Decoded OK.
```



## Dylan


```dylan
Module:   LZW
Synopsis: LZW implementation for Rosetta code

define method output(n :: <integer>)
  format-out("%d ", n);
end;

define method contains?(dict, var)
  let x = element(dict, var, default: #f);
  x ~= #f;
end;

define method byte->string(c)
  add("", as(<character>, c));
end;

define method compress(input :: <string>) => <vector>;
  let result = make(<vector>);
  let dict = make(<string-table>);
  for (x from 0 to 255)
    dict[byte->string(x)] := x;
  end;

  let next-code = 256;
  let cur-seq = "";
  for (c in input)
    let wc = add(cur-seq, c);
    if (contains?(dict, wc))
      cur-seq := wc;
    else
      result := add(result, dict[cur-seq]);
      dict[wc] := next-code;
      next-code := next-code + 1;
      cur-seq := add("", c);
    end
  end;
  unless (empty?(cur-seq))
    result := add(result, dict[cur-seq]);
  end;
  result
end;

format-out("%=\n", compress("TOBEORNOTTOBEORTOBEORNOT"))
```


## Eiffel


```Eiffel

class
  APPLICATION

create
  make

feature {NONE}

  make
    local
      test: LINKED_LIST [INTEGER]
    do
      create test.make
      test := compress ("TOBEORNOTTOBEORTOBEORNOT")
      across
        test as t
      loop
        io.put_string (t.item.out + " ")
      end
      io.new_line
      io.put_string (decompress (test))
    end

  decompress (compressed: LINKED_LIST [INTEGER]): STRING
      --Decompressed version of 'compressed'.
    local
      dictsize, i, k: INTEGER
      dictionary: HASH_TABLE [STRING, INTEGER]
      w, entry: STRING
      char: CHARACTER_8
    do
      dictsize := 256
      create dictionary.make (300)
      create entry.make_empty
      create Result.make_empty
      from
        i := 0
      until
        i > 256
      loop
        char := i.to_character_8
        dictionary.put (char.out, i)
        i := i + 1
      end
      w := compressed.first.to_character_8.out
      compressed.go_i_th (1)
      compressed.remove
      Result := w
      from
        k := 1
      until
        k > compressed.count
      loop
        if attached dictionary.at (compressed [k]) as ata then
          entry := ata
        elseif compressed [k] = dictsize then
          entry := w + w.at (1).out
        else
          io.put_string ("EXEPTION")
        end
        Result := Result + entry
        dictsize := dictsize + 1
        dictionary.put (w + entry.at (1).out, dictsize)
        w := entry
        k := k + 1
      end
    end

  compress (uncompressed: STRING): LINKED_LIST [INTEGER]
      -- Compressed version of 'uncompressed'.
    local
      dictsize: INTEGER
      dictionary: HASH_TABLE [INTEGER, STRING]
      i: INTEGER
      w, wc: STRING
      char: CHARACTER_8
    do
      dictsize := 256
      create dictionary.make (256)
      create w.make_empty
      from
        i := 0
      until
        i > 256
      loop
        char := i.to_character_8
        dictionary.put (i, char.out)
        i := i + 1
      end
      create Result.make
      from
        i := 1
      until
        i > uncompressed.count
      loop
        wc := w + uncompressed [i].out
        if dictionary.has (wc) then
          w := wc
        else
          Result.extend (dictionary.at (w))
          dictSize := dictSize + 1
          dictionary.put (dictSize, wc)
          w := "" + uncompressed [i].out
        end
        i := i + 1
      end
      if w.count > 0 then
        Result.extend (dictionary.at (w))
      end
    end

end

```

{{out}}

```txt

84 79 66 69 79 82 78 79 84 257 259 261 266 260 262 264
TOBEORNOTTOBEORTOBEORNOT

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule LZW do
  @encode_map  Enum.into(0..255, Map.new, &{[&1],&1})
  @decode_map  Enum.into(0..255, Map.new, &{&1,[&1]})

  def encode(str), do: encode(to_char_list(str), @encode_map, 256, [])

  defp encode([h], d, _, out), do: Enum.reverse([d[[h]] | out])
  defp encode([h|t], d, free, out) do
    val = d[[h]]
    find_match(t, [h], val, d, free, out)
  end

  defp find_match([h|t], l, lastval, d, free, out) do
    case Map.fetch(d, [h|l]) do
      {:ok, val} -> find_match(t, [h|l], val, d, free, out)
      :error     -> d1 = Map.put(d, [h|l], free)
                    encode([h|t], d1, free+1, [lastval | out])
    end
  end
  defp find_match([], _, lastval, _, _, out), do: Enum.reverse([lastval | out])

  def decode([h|t]) do
    val = @decode_map[h]
    decode(t, val, 256, @decode_map, val)
  end

  defp decode([], _, _, _, l), do: Enum.reverse(l) |> to_string
  defp decode([h|t], old, free, d, l) do
    val = if h == free, do: old ++ [List.first(old)], else: d[h]
    add = [List.last(val) | old]
    d1  = Map.put(d, free, add)
    decode(t, val, free+1, d1, val++l)
  end
end

str = "TOBEORNOTTOBEORTOBEORNOT"
IO.inspect enc = LZW.encode(str)
IO.inspect dec = LZW.decode(enc)
IO.inspect str == dec
```


{{out}}

```txt

[84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263]
"TOBEORNOTTOBEORTOBEORNOT"
true

```



## Erlang


```erlang
-module(lzw).

-export([test/0, encode/1, decode/1]).

-import(lists, [reverse/1, reverse/2]).

test() ->
    Str = "TOBEORNOTTOBEORTOBEORNOT",
    [84,79,66,69,79,82,78,79,84,256,258,260,265,259,261,263] =
  encode(Str),
    Str = decode(encode(Str)),
    ok.

encode(Str) ->
    D = init(dict:new()),
    encode(Str, D, 256, []).

encode([H], D, _, Out) ->
    Val = dict:fetch([H], D),
    reverse([Val|Out]);
encode([H|T], D, Free, Out) ->
    Val = dict:fetch([H], D),
    find_match(T, [H], Val, D, Free, Out).

find_match([H|T], L, LastVal, D, Free, Out) ->
    case dict:find([H|L], D) of
  {ok, Val} ->
      find_match(T, [H|L], Val, D, Free, Out);
  error ->
      D1 = dict:store([H|L], Free, D),
      encode([H|T], D1, Free+1, [LastVal|Out])
    end;
find_match([], _, LastVal, _, _, Out) ->
    reverse([LastVal|Out]).

decode([H|T]) ->
    D   = init1(dict:new()),
    Val = dict:fetch(H, D),
    decode(T, Val, 256, D, Val).

decode([], _, _, _, L) ->
    reverse(L);
decode([H|T], Old, Free, D, L) ->
    Val = dict:fetch(H, D),
    Add = [lists:last(Val)|Old],
    D1  = dict:store(Free, Add, D),
    decode(T, Val, Free+1, D1, Val ++ L).

init(D) -> init(255, D).

init(0, D) ->  D;
init(N, D) ->  D1 = dict:store([N],N,D),  init(N-1, D1).

init1(D) -> init1(255, D).

init1(0, D) ->  D;
init1(N, D) ->  D1 = dict:store(N,[N],D),  init1(N-1, D1).
```



## Forth

{{works with|GNU Forth|0.6.2}}

```forth
256 value next-symbol

\ current string fragment

create w 256 allot     \ counted string

: w=c ( c -- )       w 1+ c!        1 w c! ;
: w+c ( c -- )  w count + c!  w c@ 1+ w c! ;

\ Compression

\ dictionary of strings to symbols
0 value dict

: init-dict  table to dict  256 to next-symbol  dict set-current ;

: free-dict                           forth-wordlist set-current ;

: in-dict? ( key len -- ? )   \ can assume len > 1
  dict search-wordlist dup if nip then ;

: lookup-dict ( key len -- symbol )
  dup 1 = if drop c@ exit then
  dict search-wordlist if >body @ else abort" bad-dict!" then ;

: put-dict ( data key len -- )
  nextname create , ;

\ output buffer of symbols
\  in real life, these symbols would be packed into octets
variable out-size
create out 256 cells allot

: output ( symbol -- )
  dup out out-size @ cells + !  1 out-size +!
  dup 256 < if emit space else . then ;

: compress ( addr len -- )
  init-dict  0 out-size !
  over c@ w=c  1 /string
  bounds do
    i c@ w+c
    w count in-dict? 0= if
      w count 1- lookup-dict output
      next-symbol dup w count put-dict
      1+ to next-symbol
      i c@ w=c
    then
  loop
  w count lookup-dict output
  free-dict ;

\ Decompression

\ array of symbols to strings (in real code this would need to be growable)
\  next-symbol is reused for the size of this table
create symtab 256 cells allot
0 value start

: init-symtab  256 to next-symbol  here to start ;

: free-symtab  start here - allot ;

: get-symbol ( symbol -- addr len )
  dup 256 < if pad c! pad 1 exit then
  256 - cells symtab + @ count ;

: add-symbol ( addr len -- )
  here symtab next-symbol 256 - cells + !
  s,
  next-symbol 1+ to next-symbol ;

create entry 256 allot

: decompress ( addr len -- )
  init-symtab
  over @ dup emit w=c
  cells bounds cell+ do
    i @ next-symbol < if
      i @ get-symbol entry place
    else i @ next-symbol = if
      w 1+ c@ w count + c!  w count 1+ entry place
    else
      abort" bad symbol!"
    then then
    entry count type  \ output
    entry 1+ c@ w+c
    w count add-symbol
    entry count w place
  1 cells +loop
  free-symtab ;

\ Testing

s" TOBEORNOTTOBEORTOBEORNOT" compress cr
\ T O B E O R N O T 256 258 260 265 259 261 263

out out-size @ decompress cr
\ TOBEORNOTTOBEORTOBEORNOT
```


## FreeBASIC


```freebasic
' version 22-02-2019
' compile with: fbc -s console

Type dict
    prefix As Integer
    B      As String
End Type

Sub init(dictionary() As dict, ByRef last As ULong)

    For i As ULong  = 0 To 255
        dictionary(i).prefix = -1
        dictionary(i).B = Chr(i)
    Next
    last = 255

End Sub

Function encode_LZW(dictionary() As dict, last_entry As ULong, input_str As String) As String

    If Len(input_str) < 2 Then
        Print "input string is to short"
        Return ""
    End If

    Dim As String word, output_str, char
    Dim As ULong i = 1, index, j, len_input = Len(input_str)

    Do
        If i > len_input Then
            output_str = output_str + " " + Str(index)
            Return output_str ' no more chars to process. we are done
        End If
        char = Mid(Input_str, i, 1)
        i += 1
        For j = 0 To last_entry
            If dictionary(j).B = word + char Then
                word += char
                index = j
                Continue Do
            End If
        Next
        output_str = output_str + " " + Str(index)
        last_entry = last_entry +1
        dictionary(last_entry).B = word + char
        dictionary(last_entry).prefix = index
        word = char : index = Asc(char)
    Loop

End Function

Function decode_LZW(dictionary() As dict, last_entry As ULong, input_str As String) As String

    Dim As String temp, word, output_str
    Dim As ULong i, i1 = 1, j, index
    input_str = Trim(input_str)
    Dim As ULong len_input = Len(input_str)
    input_str = input_str + " "

    i = InStr(i1, input_str, " ")
    index = Val(Mid(Input_str, i1, i - i1))
    word = dictionary(index).B
    output_str = word
    i1 = i +1
    Do
        i = InStr(i1, input_str, " ")
        If i >= len_input Then
            index = Val(Mid(input_str, i1))
            output_str = output_str + dictionary(index).B
            Return output_str
        End If
        index = Val(Mid(Input_str, i1, i - i1))
        i1 = i +1
        If index <= last_entry Then
            temp = dictionary(index).B
        Else
            temp = word + Left(word, 1)
        End If
        output_str = output_str + temp
        last_entry = last_entry +1
        dictionary(last_entry).B = word + Left(temp, 1)
        word = temp
    Loop

End Function

' ------=< MAIN >=------

Dim As ULong last_entry, max_bit = 9
Dim As ULong dict_max = 1 Shl max_bit -1
Dim As String output_str, input_str = "TOBEORNOTTOBEORTOBEORNOT"
Dim As dict dictionary()

Print "  input str: ";input_str

ReDim dictionary(dict_max +1)
init(dictionary(), last_entry)
output_str = encode_LZW(dictionary(), last_entry, input_str)
Print "encoded str: ";output_str

ReDim dictionary(dict_max +1)
init(dictionary(), last_entry)
output_str = decode_LZW(dictionary(), last_entry, output_str)
Print "decoded str: "; output_str

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
  input str: TOBEORNOTTOBEORTOBEORNOT
encoded str:  84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263
decoded str: TOBEORNOTTOBEORTOBEORNOT
```



## Go

Go also has the
<code>[https://golang.org/pkg/compress/lzw compress/lzw]</code>
package in the standard library.
{{trans|Java}}

This handles any series of bytes in the input string,
not just ASCII or valid UTF8 encoding
(tested with [https://github.com/dvyukov/go-fuzz go-fuzz]).

```go
package main

import (
	"fmt"
	"log"
	"strings"
)

// Compress a string to a list of output symbols.
func compress(uncompressed string) []int {
	// Build the dictionary.
	dictSize := 256
	// We actually want a map of []byte -> int but
	// slices are not acceptable map key types.
	dictionary := make(map[string]int, dictSize)
	for i := 0; i < dictSize; i++ {
		// Ugly mess to work around not having a []byte key type.
		// Using `string(i)` would do utf8 encoding for i>127.
		dictionary[string([]byte{byte(i)})] = i
	}

	var result []int
	var w []byte
	for i := 0; i < len(uncompressed); i++ {
		c := uncompressed[i]
		wc := append(w, c)
		if _, ok := dictionary[string(wc)]; ok {
			w = wc
		} else {
			result = append(result, dictionary[string(w)])
			// Add wc to the dictionary.
			dictionary[string(wc)] = dictSize
			dictSize++
			//w = []byte{c}, but re-using wc
			wc[0] = c
			w = wc[:1]
		}
	}

	if len(w) > 0 {
		// Output the code for w.
		result = append(result, dictionary[string(w)])
	}
	return result
}

type BadSymbolError int

func (e BadSymbolError) Error() string {
	return fmt.Sprint("Bad compressed symbol ", int(e))
}

// Decompress a list of output symbols to a string.
func decompress(compressed []int) (string, error) {
	// Build the dictionary.
	dictSize := 256
	dictionary := make(map[int][]byte, dictSize)
	for i := 0; i < dictSize; i++ {
		dictionary[i] = []byte{byte(i)}
	}

	var result strings.Builder
	var w []byte
	for _, k := range compressed {
		var entry []byte
		if x, ok := dictionary[k]; ok {
			//entry = x, but ensuring any append will make a copy
			entry = x[:len(x):len(x)]
		} else if k == dictSize && len(w) > 0 {
			entry = append(w, w[0])
		} else {
			return result.String(), BadSymbolError(k)
		}
		result.Write(entry)

		if len(w) > 0 {
			// Add w+entry[0] to the dictionary.
			w = append(w, entry[0])
			dictionary[dictSize] = w
			dictSize++
		}
		w = entry
	}
	return result.String(), nil
}

func main() {
	compressed := compress("TOBEORNOTTOBEORTOBEORNOT")
	fmt.Println(compressed)
	decompressed, err := decompress(compressed)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(decompressed)
}
```

{{out}}

```txt

[84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263]
TOBEORNOTTOBEORTOBEORNOT

```



## Groovy


```groovy
def compress = { text ->
    def dictionary = (0..<256).inject([:]) { map, ch -> map."${(char)ch}" = ch; map }
    def w = '', compressed = []
    text.each { ch ->
        def wc = "$w$ch"
        if (dictionary[wc]) {
            w = wc
        } else {
            compressed << dictionary[w]
            dictionary[wc] = dictionary.size()
            w = "$ch"
        }
    }
    if (w) { compressed << dictionary[w] }
    compressed
}

def decompress = { compressed ->
    def dictionary = (0..<256).inject([:])  { map, ch -> map[ch] = "${(char)ch}"; map }
    int dictSize = 128;
    String w = "${(char)compressed[0]}"
    StringBuffer result = new StringBuffer(w)

    compressed.drop(1).each { k ->
        String entry = dictionary[k]
        if (!entry) {
            if (k != dictionary.size()) throw new IllegalArgumentException("Bad compressed k $k")
            entry = "$w${w[0]}"
        }
        result << entry

        dictionary[dictionary.size()] = "$w${entry[0]}"
        w = entry
    }

    result.toString()
}
```

Testing:

```groovy
def plaintext = 'TOBEORNOTTOBEORTOBEORNOT'
def compressed = compress(plaintext)
def result = decompress(compressed)

println """\
    Plaintext:    '$plaintext'
    Compressed:   $compressed
    Uncompressed: '$result'""".stripIndent()
```

{{out}}

```txt
Plaintext:    'TOBEORNOTTOBEORTOBEORNOT'
Compressed:   [84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263]
Uncompressed: 'TOBEORNOTTOBEORTOBEORNOT'
```



## Haskell



```Haskell
import Data.List (elemIndex, tails)
import Data.Maybe (fromJust)

doLZW :: Eq a => [a] -> [a] -> [Int]
doLZW _ [] = []
doLZW as (x:xs) = lzw (return <$> as) [x] xs
  where
    lzw a w [] = [fromJust $ elemIndex w a]
    lzw a w (x:xs)
      | w_ `elem` a = lzw a w_ xs
      | otherwise = fromJust (elemIndex w a) : lzw (a ++ [w_]) [x] xs
      where
        w_ = w ++ [x]

undoLZW :: [a] -> [Int] -> [a]
undoLZW _ [] = []
undoLZW a cs =
  cs >>=
  (!!)
    (foldl
       ((.) <$> (++) <*>
        (\x xs -> return (((++) <$> head <*> take 1 . last) ((x !!) <$> xs))))
       (return <$> a)
       (take2 cs))

take2 :: [a] -> [[a]]
take2 xs = filter ((2 ==) . length) (take 2 <$> tails xs)

main :: IO ()
main = do
  print $ doLZW ['\0' .. '\255'] "TOBEORNOTTOBEORTOBEORNOT"
  print $
    undoLZW
      ['\0' .. '\255']
      [84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263]
  print $
    ((==) <*> ((.) <$> undoLZW <*> doLZW) ['\NUL' .. '\255'])
      "TOBEORNOTTOBEORTOBEORNOT"
```

{{Out}}

```txt
[84,79,66,69,79,82,78,79,84,256,258,260,265,259,261,263]
"TOBEORNOTTOBEORTOBEORNOT"
True
```


Other (elegant) code can be found at Haskell wiki [http://www.haskell.org/haskellwiki/Toy_compression_implementations Toy compression]


## J


Straightforward implementations of encoding and decoding:

```J
encodeLZW =: 4 : 0
 d=. ;/x
 r=.0$0
 wc=.w=.{.y
 for_c. }.y do.
   wc=.w,c
   if. d e.~ <wc do. w=.wc else.
     r=. r, d i.<w
     d=.d,<wc
     w=.c
   end.
 end.
 r, d i.<w
)
```

Test:

```txt
   a. encodeLZW 'TOBEORNOTTOBEORTOBEORNOT'
84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263
```

Decoding:

```J
decodeLZW =: 4 : 0
 d=.;/x
 w=.r=. >d{~{.y
 ds=. #d
 for_c. }.y do.
   select. * c-ds
    case. _1 do. r=.r,e=.>c{d
    case.  0 do. r=.r,e=.w,{.w
    case.    do. 'error' return.
   end.
   d=.d,< w,{.e
   w=.e
   ds=.ds+1
 end.
 ;r
)
```

Test:

```txt
   a. decodeLZW 84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263
TOBEORNOTTOBEORTOBEORNOT
```

encode --> decode --> compare with original:

```txt
   a. (] -: [ decodeLZW encodeLZW) 'TOBEORNOTTOBEORTOBEORNOT'
1
```

Error test:

```txt
   a. decodeLZW 84 79 66 69 79 82 78 79 84 256 258 456 260 265 259 261 263
error
```


Tacit J expression for decoding:

```txt
decodeLZW=:[:;]{[:;[:(],<@(>@{.,{.@>@{:)@:{)&.>/<@(;/@[),~|.@(2<\])
```


```txt
   a. decodeLZW 84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263
TOBEORNOTTOBEORTOBEORNOT
```



## Java

{{works with|Java|1.5+}}

```java5
import java.util.*;

public class LZW {
    /** Compress a string to a list of output symbols. */
    public static List<Integer> compress(String uncompressed) {
        // Build the dictionary.
        int dictSize = 256;
        Map<String,Integer> dictionary = new HashMap<String,Integer>();
        for (int i = 0; i < 256; i++)
            dictionary.put("" + (char)i, i);

        String w = "";
        List<Integer> result = new ArrayList<Integer>();
        for (char c : uncompressed.toCharArray()) {
            String wc = w + c;
            if (dictionary.containsKey(wc))
                w = wc;
            else {
                result.add(dictionary.get(w));
                // Add wc to the dictionary.
                dictionary.put(wc, dictSize++);
                w = "" + c;
            }
        }

        // Output the code for w.
        if (!w.equals(""))
            result.add(dictionary.get(w));
        return result;
    }

    /** Decompress a list of output ks to a string. */
    public static String decompress(List<Integer> compressed) {
        // Build the dictionary.
        int dictSize = 256;
        Map<Integer,String> dictionary = new HashMap<Integer,String>();
        for (int i = 0; i < 256; i++)
            dictionary.put(i, "" + (char)i);

        String w = "" + (char)(int)compressed.remove(0);
        StringBuffer result = new StringBuffer(w);
        for (int k : compressed) {
            String entry;
            if (dictionary.containsKey(k))
                entry = dictionary.get(k);
            else if (k == dictSize)
                entry = w + w.charAt(0);
            else
                throw new IllegalArgumentException("Bad compressed k: " + k);

            result.append(entry);

            // Add w+entry[0] to the dictionary.
            dictionary.put(dictSize++, w + entry.charAt(0));

            w = entry;
        }
        return result.toString();
    }

    public static void main(String[] args) {
        List<Integer> compressed = compress("TOBEORNOTTOBEORTOBEORNOT");
        System.out.println(compressed);
        String decompressed = decompress(compressed);
        System.out.println(decompressed);
    }
}
```


{{out}} (Command Line direct output):

```java5
[84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263]
TOBEORNOTTOBEORTOBEORNOT
```



## JavaScript


```javascript
//LZW Compression/Decompression for Strings
var LZW = {
    compress: function (uncompressed) {
        "use strict";
        // Build the dictionary.
        var i,
            dictionary = {},
            c,
            wc,
            w = "",
            result = [],
            dictSize = 256;
        for (i = 0; i < 256; i += 1) {
            dictionary[String.fromCharCode(i)] = i;
        }

        for (i = 0; i < uncompressed.length; i += 1) {
            c = uncompressed.charAt(i);
            wc = w + c;
            //Do not use dictionary[wc] because javascript arrays
            //will return values for array['pop'], array['push'] etc
           // if (dictionary[wc]) {
            if (dictionary.hasOwnProperty(wc)) {
                w = wc;
            } else {
                result.push(dictionary[w]);
                // Add wc to the dictionary.
                dictionary[wc] = dictSize++;
                w = String(c);
            }
        }

        // Output the code for w.
        if (w !== "") {
            result.push(dictionary[w]);
        }
        return result;
    },


    decompress: function (compressed) {
        "use strict";
        // Build the dictionary.
        var i,
            dictionary = [],
            w,
            result,
            k,
            entry = "",
            dictSize = 256;
        for (i = 0; i < 256; i += 1) {
            dictionary[i] = String.fromCharCode(i);
        }

        w = String.fromCharCode(compressed[0]);
        result = w;
        for (i = 1; i < compressed.length; i += 1) {
            k = compressed[i];
            if (dictionary[k]) {
                entry = dictionary[k];
            } else {
                if (k === dictSize) {
                    entry = w + w.charAt(0);
                } else {
                    return null;
                }
            }

            result += entry;

            // Add w+entry[0] to the dictionary.
            dictionary[dictSize++] = w + entry.charAt(0);

            w = entry;
        }
        return result;
    }
}, // For Test Purposes
    comp = LZW.compress("TOBEORNOTTOBEORTOBEORNOT"),
    decomp = LZW.decompress(comp);
document.write(comp + '
' + decomp);
```




###  ES6 Version


This is the the same thing, but for ES6. The code has been refactored and cleaned up a bit to look neater.


```javascript
'use strict';
/**
    Namespace for LZW compression and decompression.
    Methods:
        LZW.compress(uncompressed)
        LZW.decompress(compressed)
*/
class LZW
{
    /**
        Perform the LZW compression
        uncompressed - String. The string on which to perform the compression.
    */
    static compress(uncompressed)
    {
        // Initialize dictionary
        let dictionary = {};
        for (let i = 0; i < 256; i++)
        {
            dictionary[String.fromCharCode(i)] = i;
        }

        let word = '';
        let result = [];
        let dictSize = 256;

        for (let i = 0, len = uncompressed.length; i < len; i++)
        {
            let curChar = uncompressed[i];
            let joinedWord = word + curChar;

            // Do not use dictionary[joinedWord] because javascript objects
            // will return values for myObject['toString']
            if (dictionary.hasOwnProperty(joinedWord))
            {
                word = joinedWord;
            }
            else
            {
                result.push(dictionary[word]);
                // Add wc to the dictionary.
                dictionary[joinedWord] = dictSize++;
                word = curChar;
            }
        }

        if (word !== '')
        {
            result.push(dictionary[word]);
        }

        return result;
    }

    /**
        Decompress LZW array generated by LZW.compress()
        compressed - Array. The array that holds LZW compressed data.
    */
    static decompress(compressed)
    {
        // Initialize Dictionary (inverse of compress)
        let dictionary = {};
        for (let i = 0; i < 256; i++)
        {
            dictionary[i] = String.fromCharCode(i);
        }

        let word = String.fromCharCode(compressed[0]);
        let result = word;
        let entry = '';
        let dictSize = 256;

        for (let i = 1, len = compressed.length; i < len; i++)
        {
            let curNumber = compressed[i];

            if (dictionary[curNumber] !== undefined)
            {
                entry = dictionary[curNumber];
            }
            else
            {
                if (curNumber === dictSize)
                {
                    entry = word + word[0];
                }
                else
                {
                    throw 'Error in processing';
                    return null;
                }
            }

            result += entry;

            // Add word + entry[0] to dictionary
            dictionary[dictSize++] = word + entry[0];

            word = entry;
        }

        return result;
    }
}

let comp = LZW.compress('TOBEORNOTTOBEORTOBEORNOT');
let decomp = LZW.decompress(comp);

console.log(`${comp}
${decomp}`);
```


{{out}}

```txt
84,79,66,69,79,82,78,79,84,256,258,260,265,259,261,263
TOBEORNOTTOBEORTOBEORNOT
```



## jq

{{ works with|jq|1.4}}
{{trans|JavaScript}}

```jq
# LZW compression/decompression for strings
def lzw_compress:
  def decode: [.] | implode;
  # Build the dictionary:
  256 as $dictSize
  | (reduce range(0; $dictSize) as $i ({}; .[ $i | decode ] = $i)) as $dictionary
  | reduce explode[] as $i
      ( [$dictionary, $dictSize, "", []];        # state: [dictionary, dictSize, w, result]
        .[0] as $dictionary
        | .[1] as $dictSize
        | .[2] as $w
        | ($i | decode) as $c
        | ($w + $c ) as $wc
        | if $dictionary[$wc] then .[2] = $wc
          else
              .[2] =  $c                         # w = c
            | .[3] += [$dictionary[$w]]          # result += dictionary[w]
            | .[0][$wc] = $dictSize              # Add wc to the dictionary
            | .[1] += 1                          # dictSize ++
          end
      )
      # Output the code for w unless w == "":
      | if .[2] == "" then .[3]
        else .[3] + [.[0][.[2]]]
        end
;

def lzw_decompress:
  def decode: [.] | implode;
  # Build the dictionary - an array of strings
  256 as $dictSize
  | (reduce range(0; $dictSize) as $i ([]; .[ $i ] = ($i|decode))) as $dictionary
  | (.[0]|decode) as $w
  | reduce .[1:][] as $k
    ( [ $dictionary, $dictSize, $w, $w];   # state: [dictionary, dictSize, w, result]
      .[0][$k] as $entry
      | (if $entry then $entry
        elif $k == .[1] then .[2] + .[2][0:1]
        else error("lzw_decompress: k=\($k)")
        end) as $entry
      | .[3] += $entry                     # result += entry
      | .[0][.[1]] = .[2] + $entry[0:1]    # dictionary[dictSize] = w + entry.charAt(0);
      | .[1] += 1                          # dictSize++
      | .[2] = $entry                      # w = entry
    ) | .[3]
;
```

'''Example''':

```jq
"TOBEORNOTTOBEORTOBEORNOT" | lzw_compress| lzw_decompress
```

{{Out}}
 $ jq -n -f LZW.jq
 "TOBEORNOTTOBEORTOBEORNOT"


## Julia

{{works with|Julia|1.1.1}}

```julia
function compressLZW(decompressed::String)
    dictsize = 256
    dict     = Dict{String,Int}(string(Char(i)) => i for i in 0:dictsize)
    result   = Vector{Int}(undef, 0)
    w        = ""
    for c in decompressed
        wc = string(w, c)
        if haskey(dict, wc)
            w = wc
        else
            push!(result, dict[w])
            dict[wc]  = dictsize
            dictsize += 1
            w        = string(c)
        end
    end
    if !isempty(w) push!(result, dict[w]) end
    return result
end

function decompressLZW(compressed::Vector{Int})
    dictsize = 256
    dict     = Dict{Int,String}(i => string('\0' + i) for i in 0:dictsize)
    result   = IOBuffer()
    w        = string(Char(popfirst!(compressed)))
    write(result, w)
    for k in compressed
        if haskey(dict, k)
            entry = dict[k]
        elseif k == dictsize
            entry = string(w, w[1])
        else
            error("bad compressed k: $k")
        end
        write(result, entry)
        dict[dictsize] = string(w, entry[1])
        dictsize += 1
        w = entry
    end
    return String(take!(result))
end

original     = ["0123456789", "TOBEORNOTTOBEORTOBEORNOT", "dudidudidudida"]
compressed   = compressLZW.(original)
decompressed = decompressLZW.(compressed)

for (word, comp, decomp) in zip(original, compressed, decompressed)
    comprate = (length(word) - length(comp)) / length(word) * 100
    println("Original: $word\n-> Compressed: $comp (compr.rate: $(round(comprate, digits=2))%)\n-> Decompressed: $decomp")
end
```


{{out}}

```txt
Original: 0123456789
-> Compressed: [49, 50, 51, 52, 53, 54, 55, 56, 57] (compr.rate: 10.0%)
-> Decompressed: 0123456789
Original: TOBEORNOTTOBEORTOBEORNOT
-> Compressed: [79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263] (compr.rate: 37.5%)
-> Decompressed: TOBEORNOTTOBEORTOBEORNOT
Original: dudidudidudida
-> Compressed: [117, 100, 105, 256, 258, 260, 259, 97] (compr.rate: 42.86%)
-> Decompressed: dudidudidudida
```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.2

object Lzw {
    /** Compress a string to a list of output symbols. */
    fun compress(uncompressed: String): MutableList<Int> {
        // Build the dictionary.
        var dictSize = 256
        val dictionary = mutableMapOf<String, Int>()
        (0 until dictSize).forEach { dictionary.put(it.toChar().toString(), it)}

        var w = ""
        val result = mutableListOf<Int>()
        for (c in uncompressed) {
            val wc = w + c
            if (dictionary.containsKey(wc))
                w = wc
            else {
                result.add(dictionary[w]!!)
                // Add wc to the dictionary.
                dictionary.put(wc, dictSize++)
                w = c.toString()
            }
        }

        // Output the code for w
        if (!w.isEmpty()) result.add(dictionary[w]!!)
        return result
    }

    /** Decompress a list of output symbols to a string. */
    fun decompress(compressed: MutableList<Int>): String {
        // Build the dictionary.
        var dictSize = 256
        val dictionary = mutableMapOf<Int, String>()
        (0 until dictSize).forEach { dictionary.put(it, it.toChar().toString())}

        var w = compressed.removeAt(0).toChar().toString()
        val result = StringBuilder(w)
        for (k in compressed) {
            var entry: String
            if (dictionary.containsKey(k))
                entry = dictionary[k]!!
            else if (k == dictSize)
                entry = w + w[0]
            else
                throw IllegalArgumentException("Bad compressed k: $k")
            result.append(entry)

            // Add w + entry[0] to the dictionary.
            dictionary.put(dictSize++, w + entry[0])
            w = entry
        }
        return result.toString()
    }
}

fun main(args: Array<String>) {
    val compressed = Lzw.compress("TOBEORNOTTOBEORTOBEORNOT")
    println(compressed)
    val decompressed = Lzw.decompress(compressed)
    println(decompressed)
}
```


{{out}}

```txt

[84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263]
TOBEORNOTTOBEORTOBEORNOT

```



## Liberty BASIC

The encoder features variable-bit output, a 12 to 21 bit rotating dictionary (that can also be set to "Static"), and an unbalanced binary search tree that assures a worst-case-scenario maximum of 256 searches to find any given index, regardless of the dictionary's size.
It uses both read and write buffers so is capable of handling files of any size, and it adds a settings-byte to the beginning of the encoded file to retain the maximum bit-width and rotating status of the dictionary.
It also has the option to write the encoding/decoding dictionaries to file so the encoder can be checked for accuracy.
This code directly follows the methodology described in an excellent web article by Juha Nieminen entitled "An efficient LZW implementation".
<lang> DIM LZW(1, 1)
 DIM JDlzw(1)
 DIM JDch$(1)
 LET maxBits = 20          ' maximum bit width of the dictionary: minimum=12; maximum=21
 LET resetDictionary = 1   ' flag to reset the dictionary when it gets full: 1=TRUE; 0=FALSE
 LET printDictionary = 0   ' output encoding and decoding dictionaries to files
 LET maxChunkSize = 2 ^ 14 ' maximum size of the data buffer
 LET dSize = 2 ^ maxBits   ' maximum dictionary size
 LET JDext$ = ".lzw"       ' file extension used for created archives
 FILEDIALOG "Select a file to test LZW...", "*.*", inputName$
 IF inputName$ = "" THEN END
 DO ' get fullPath\ and fileName.ext
    P = X
    X = INSTR(inputName$, "\", (X + 1))
 LOOP UNTIL X = 0
 filePath$ = LEFT$(inputName$, P)
 fileName$ = MID$(inputName$, (P + 1))
 DO ' get fileName and .ext
    P = X
    X = INSTR(fileName$, ".", (X + 1))
 LOOP UNTIL X = 0
 fileExt$ = MID$(fileName$, P)
 fileName$ = LEFT$(fileName$, (P - 1))

 GOSUB [lzwEncode]
 GOSUB [lzwDecode]

 END

''''''''''''''''''''''''''''''''''''''''
' Start LZW Encoder ''''''''''''''''''''
[lzwEncode]
 REDIM LZW(dSize, 4)
 LET EMPTY=-1:PREFIX=0:BYTE=1:FIRST=2:LESS=3:MORE=4:bmxCorrect=1
 LET bitsRemain=0:remainIndex=0:tagCount=0:currentBitSize=8:fileTag$=""
 FOR dNext = 0 TO 255                                       ' initialize dictionary for LZW
 '  LZW(dNext, PREFIX) = EMPTY                              ' prefix index of '<index>' <B>
 '  LZW(dNext, BYTE)   = dNext                              ' byte value of <index> '<B>'
    LZW(dNext, FIRST)  = EMPTY                              ' first index to use <index><B> as prefix
 '  LZW(dNext, LESS)   = EMPTY                              ' lesser index of binary search tree for <B>
 '  LZW(dNext, MORE)   = EMPTY                              ' greater index of binary search tree for <B>
 NEXT dNext
 OPEN inputName$ FOR INPUT AS #lzwIN
 IF LOF(#lzwIN) < 2 THEN
    CLOSE #lzwIN
    END
 END IF
 OPEN fileName$ + fileExt$ + JDext$ FOR OUTPUT AS #lzwOUT
 GOSUB [StartFileChunk]
 chnkPoint = 1
 IF maxBits < 12 THEN maxBits = 12
 IF maxBits > 21 THEN maxBits = 21
 settings = maxBits - 12                                    ' setting for dictionary size; 1st decimal +12
 IF resetDictionary THEN settings = settings + 100          ' setting for dictionary type; 2nd decimal even=static, odd=adaptive
 #lzwOUT, CHR$(settings);                                   ' save settings as 1st byte of output
 orgIndex = ASC(LEFT$(fileChunk$, 1))                       ' read 1st byte into <index>
 WHILE fileChunk$ <> ""                                     ' while the buffer is not empty
    DO                                                      ' begin the main encoder loop
        chnkPoint = chnkPoint + 1
        savIndex = FIRST                                    ' initialize the save-to index
        prvIndex = orgIndex                                 ' initialize the previous index in search
        newByte = ASC(MID$(fileChunk$, chnkPoint, 1))       ' read <B>
        dSearch = LZW(orgIndex, FIRST)                      ' first search index for this <index> in the dictionary
        WHILE (dSearch > EMPTY)                             ' while <index> is present in the dictionary
            IF LZW(dSearch, BYTE) = newByte THEN EXIT WHILE ' if <index><B> is found
            IF newByte < LZW(dSearch, BYTE) THEN            ' else if new <B> is less than <index><B>
                savIndex = LESS                             ' follow lesser binary tree
            ELSE
                savIndex = MORE                             ' else follow greater binary tree
            END IF
            prvIndex = dSearch                              ' set previous <index>
            dSearch = LZW(dSearch, savIndex)                ' read next search <index> from binary tree
        WEND
        IF dSearch = EMPTY THEN                             ' if <index><B> was not found in the dictionary
            GOSUB [WriteIndex]                              ' write <index> to the output
            IF dNext < dSize THEN                           ' save <index><B> into the dictionary
                LZW(prvIndex, savIndex) = dNext
                LZW(dNext, PREFIX) = orgIndex
                LZW(dNext, BYTE)   = newByte
                LZW(dNext, FIRST)  = EMPTY
                LZW(dNext, LESS)   = EMPTY
                LZW(dNext, MORE)   = EMPTY
                IF dNext = (2 ^ currentBitSize) THEN currentBitSize = currentBitSize + 1
                dNext = dNext + 1
            ELSE                                            ' else reset the dictionary... or maybe not
                IF resetDictionary THEN
                    GOSUB [PrintEncode]
                    REDIM LZW(dSize, 4)
                    FOR dNext = 0 TO 255
                        LZW(dNext, FIRST)  = EMPTY
                    NEXT dNext
                    currentBitSize = 8
                    bmxCorrect = 0
                END IF
            END IF
            orgIndex = newByte                              ' set <index> = <B>
        ELSE                                                ' if <index><B> was found in the dictionary,
            orgIndex = dSearch                              ' then set <index> = <index><B>
        END IF
    LOOP WHILE chnkPoint < chunk                            ' loop until the chunk has been processed
    GOSUB [GetFileChunk]                                    ' refill the buffer
 WEND                                                       ' loop until the buffer is empty
 GOSUB [WriteIndex]
 IF bitsRemain > 0 THEN #lzwOUT, CHR$(remainIndex);
 CLOSE #lzwOUT
 CLOSE #lzwIN
 IF bmxCorrect THEN ' correct the settings, if needed
    IF (currentBitSize < maxBits) OR resetDictionary THEN
        IF currentBitSize < 12 THEN currentBitSize = 12
        OPEN fileName$ + fileExt$ + JDext$ FOR BINARY AS #lzwOUT
        #lzwOUT, CHR$(currentBitSize - 12);
        CLOSE #lzwOUT
    END IF
 END IF
 GOSUB [PrintEncode]
 REDIM LZW(1, 1)
 RETURN

[WriteIndex]
 X = orgIndex                             ' add remaining bits to input
 IF bitsRemain > 0 THEN X = remainIndex + (X * (2 ^ bitsRemain))
 bitsRemain = bitsRemain + currentBitSize ' add current bit size to output stack
 WHILE bitsRemain > 7                     ' if 8 or more bits are to be written
    #lzwOUT, CHR$(X MOD 256);             ' attatch lower 8 bits to output string
    X = INT(X / 256)                      ' shift input value down by 2^8
    bitsRemain = bitsRemain - 8           ' adjust counters
 WEND
 remainIndex = X                          ' retain trailing bits for next write
 RETURN

' End LZW Encoder ''''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''

[StartFileChunk]
 sizeOfFile = LOF(#lzwIN)    ' set EOF marker
 bytesRemaining = sizeOfFile ' set EOF counter
 chunk = maxChunkSize        ' set max buffer size
[GetFileChunk]
 fileChunk$ = ""
 IF bytesRemaining < 1 THEN RETURN
 IF chunk > bytesRemaining THEN chunk = bytesRemaining
 bytesRemaining = bytesRemaining - chunk
 fileChunk$ = INPUT$(#lzwIN, chunk)
 chnkPoint = 0
 RETURN

''''''''''''''''''''''''''''''''''''''''
' Start LZW Decoder ''''''''''''''''''''
[lzwDecode]
 LET EMPTY=-1:bitsRemain=0:tagCount=0:fileTag$=""
 OPEN fileName$ + fileExt$ + JDext$ FOR INPUT AS #lzwIN
 OPEN fileName$ + ".Copy" + fileExt$ FOR OUTPUT AS #lzwOUT
 GOSUB [StartFileChunk]
 chnkPoint = 2
 settings = ASC(fileChunk$)
 maxBits  = VAL(RIGHT$(STR$(settings), 1)) + 12
 dSize = 2 ^ maxBits
 IF settings > 99 THEN resetDictionary = 1
 GOSUB [ResetLZW]
 oldIndex = orgIndex
 WHILE fileChunk$ <> ""
    ' decode current index and write to file
    GOSUB [GetIndex]
    IF JDch$(orgIndex) = "" THEN
        tmpIndex = oldIndex
        tmp$ = JDch$(tmpIndex)
        WHILE JDlzw(tmpIndex) > EMPTY
            tmpIndex = JDlzw(tmpIndex)
            tmp$ = JDch$(tmpIndex) + tmp$
        WEND
        tmp$ = tmp$ + LEFT$(tmp$, 1)
    ELSE
        tmpIndex = orgIndex
        tmp$ = JDch$(tmpIndex)
        WHILE JDlzw(tmpIndex) > EMPTY
            tmpIndex = JDlzw(tmpIndex)
            tmp$ = JDch$(tmpIndex) + tmp$
        WEND
    END IF
    #lzwOUT, tmp$;
    ' add next dictionary entry or reset dictionary
    IF dNext < dSize THEN
        JDlzw(dNext) = oldIndex
        JDch$(dNext) = LEFT$(tmp$, 1)
        dNext = dNext + 1
        IF dNext = (2 ^ currentBitSize) THEN
            IF maxBits > currentBitSize THEN
                currentBitSize = currentBitSize + 1
            ELSE
                IF resetDictionary THEN
                    GOSUB [PrintDecode]
                    GOSUB [ResetLZW]
                END IF
            END IF
        END IF
    END IF
    oldIndex = orgIndex
 WEND
 CLOSE #lzwOUT
 CLOSE #lzwIN
 GOSUB [PrintDecode]
 REDIM JDlzw(1)
 REDIM JDch$(1)
 RETURN

[GetIndex]
 byteCount = 0:orgIndex = 0
 bitsToGrab = currentBitSize - bitsRemain
 IF bitsRemain > 0 THEN
    orgIndex = lastByte
    byteCount = 1
 END IF
 WHILE bitsToGrab > 0
    lastByte = ASC(MID$(fileChunk$, chnkPoint, 1))
    orgIndex = orgIndex + (lastByte * (2 ^ (byteCount * 8)))
    IF chnkPoint = chunk THEN GOSUB [GetFileChunk]
    chnkPoint = chnkPoint + 1
    byteCount = byteCount + 1
    bitsToGrab = bitsToGrab - 8
 WEND
 IF bitsRemain > 0 THEN orgIndex = orgIndex / (2 ^ (8 - bitsRemain))
 orgIndex = orgIndex AND ((2 ^ currentBitSize) - 1)
 bitsRemain = bitsToGrab * (-1)
 RETURN

[ResetLZW]
 REDIM JDlzw(dSize)
 REDIM JDch$(dSize)
 FOR dNext = 0 TO 255
    JDlzw(dNext) = EMPTY       ' Prefix index
    JDch$(dNext) = CHR$(dNext) ' New byte value
 NEXT dNext
 currentBitSize = 8
 GOSUB [GetIndex]
 #lzwOUT, JDch$(orgIndex);
 currentBitSize = 9
 RETURN

' End LZW Decoder ''''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''

''''''''''''''''''''''''''''''''''''''''
[PrintEncode]
 IF printDictionary < 1 THEN RETURN
 OPEN "Encode_" + fileTag$ + fileName$ + ".txt" FOR OUTPUT AS #dictOUT
 FOR X = 0 TO 255
    LZW(X, PREFIX) = EMPTY
    LZW(X, BYTE)   = X
 NEXT X
 FOR X = dNext TO 0 STEP -1
    tmpIndex = X
    tmp$ = CHR$(LZW(tmpIndex, BYTE))
    WHILE LZW(tmpIndex, PREFIX) > EMPTY
        tmpIndex = LZW(tmpIndex, PREFIX)
        tmp$ = CHR$(LZW(tmpIndex, BYTE)) + tmp$
    WEND
    #dictOUT, X; ":"; tmp$
 NEXT X
 CLOSE #dictOUT
 tagCount = tagCount + 1
 fileTag$ = STR$(tagCount) + "_"
 RETURN

[PrintDecode]
 IF printDictionary < 1 THEN RETURN
 OPEN "Decode_" + fileTag$ + fileName$ + ".txt" FOR OUTPUT AS #dictOUT
 FOR X = dNext TO 0 STEP -1
    tmpIndex = X
    tmp$ = JDch$(tmpIndex)
    WHILE JDlzw(tmpIndex) > EMPTY
        tmpIndex = JDlzw(tmpIndex)
        tmp$ = JDch$(tmpIndex) + tmp$
    WEND
    #dictOUT, X; ":"; tmp$
 NEXT X
 CLOSE #dictOUT
 tagCount = tagCount + 1
 fileTag$ = STR$(tagCount) + "_"
 RETURN
''''''''''''''''''''''''''''''''''''''''
```


## Lua


```lua
local function compress(uncompressed) -- string
  local dictionary, result, dictSize, w, c = {}, {}, 255, ""
  for i = 0, 255 do
    dictionary[string.char(i)] = i
  end
  for i = 1, #uncompressed do
    c = string.sub(uncompressed, i, i)
    if dictionary[w .. c] then
      w = w .. c
    else
      table.insert(result, dictionary[w])
      dictSize = dictSize + 1
      dictionary[w .. c] = dictSize
      w = c
    end
  end
  if w ~= "" then
    table.insert(result, dictionary[w])
  end
  return result
end

local function decompress(compressed) -- table
  local dictionary, dictSize, entry, result, w, k = {}, 0, "", {}, ""
  for i = 0, 255 do
    dictionary[i] = string.char(i)
  end
  for i = 1, #compressed do
    k = compressed[i]
    if dictionary[k] then
      entry = dictionary[k]
    elseif k == dictSize then
      entry = w .. string.sub(w, 1, 1)
    else
      return nil, i
    end
    table.insert(result, entry)
    dictionary[dictSize] = w .. string.sub(entry, 1, 1)
    dictSize = dictSize + 1
    w = entry
  end
  return table.concat(result)
end

local example = "TOBEORNOTTOBEORTOBEORNOT"
local com = compress(example)
local dec = decompress(com)
print(table.concat(com, ", "))
print(dec)
```


{{Out}}

```txt

84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263
TOBEORNOTTOBEORTOBEORNOT

```




## M2000 Interpreter

{{trans|BBC BASIC}}

```M2000 Interpreter

Module BBCtrans {
      \\ LZW compression
      plaintext$="TOBEORNOTTOBEORTOBEORNOT"
      Function encodeLZW$(i$) {
                  Def long c, d, i, l, o$, w$
                  DIM dict$(0 to 4095)
                  FOR i = 0 TO 255 : dict$(i) = CHR$(i) : NEXT i
                  l = i
                  i = 1
                  w$ = LEFT$(i$,1)
                  REPEAT{
                          d = 0
                          REPEAT {
                                  c = d
                                  IF i > LEN(i$) THEN EXIT
                                  FOR d = 1 TO l-1
                                    IF w$ = dict$(d) THEN EXIT
                                  NEXT d
                                  IF d < l Then i += 1 : w$ += MID$(i$, i, 1)
                        } UNTIL d >= l
                          dict$(l) = w$ : l += 1 : w$ = RIGHT$(w$, 1)
                          o$ += CHR$(c MOD 256) + CHR$(c DIV 256)
                  } UNTIL i > LEN(i$)
                  = o$
      }
      encodeLZW$ = encodeLZW$(plaintext$)
      FOR i = 1 TO LEN(encodeLZW$) STEP 2
              PRINT ASC(MID$(encodeLZW$,i)) + 256*ASC(MID$(encodeLZW$,i+1));" ";
      NEXT i
      Print
      Function decodeLZW$(i$) {
                  Def c, i, l, o$, t$, w$
                  DIM dict$(0 to 4095)
                  FOR i = 0 TO 255 : dict$(i) = CHR$(i) : NEXT i
                  l = i
                  c = ASC(i$) + 256*ASC(MID$(i$,2))
                  w$ = dict$(c)
                  o$ = w$
                  IF LEN(i$) < 4 THEN = o$
                  FOR i = 3 TO LEN(i$) STEP 2
                    c = ASC(MID$(i$,i)) + 256*ASC(MID$(i$,i+1))
                    IF c < l Then { t$ = dict$(c) } ELSE t$ = w$ + LEFT$(w$,1)
                    o$ += t$
                    dict$(l) = w$ + LEFT$(t$,1)
                    l += 1
                    w$ = t$
                  NEXT i
                  = o$
      }
      Print decodeLZW$(encodeLZW$)
}
BBCtrans

```


And here a change for using Inventories, where we have hash function, and we find entry in O(1).

```M2000 Interpreter

Module FastM2000 {
      plaintext$="TOBEORNOTTOBEORTOBEORNOT"
      Function encodeLZW$(i$) {
            Def long c, d, i, l, o$, w$
            Inventory dict
            For i = 0 to 255 {Append dict , Chr$(i):=i}
            l = i
            i = 1
            w$ = LEFT$(i$,1)
            REPEAT{
                  d = 0
                  Repeat {
                        c = d
                        IF i > Len(i$) Then Exit
                        if exist(dict, w$) Then {
                              d=eval(dict)
                        } Else  Append dict, w$:=l: Exit
                        if d<l Then i += 1 : w$ += Mid$(i$, i, 1)
                  } Until d >= l
                  l += 1 : w$ = Right$(w$, 1)
                  o$ += Chr$(c Mod 256) + Chr$(c div 256)
            } Until i > Len(i$)
            = o$
      }
      encodeLZW$ = encodeLZW$(plaintext$)
      Document Doc$
      For i = 1 to Len(encodeLZW$) STEP 2
              Doc$= Str$(Asc(Mid$(encodeLZW$,i)) + 256*Asc(Mid$(encodeLZW$,i+1)))
      Next i
      Doc$={
      }
      Function decodeLZW$(i$) {
            Def c, i, l, o$, t$, w$
            Inventory Dict
            For i = 0 to 255 {Append dict , i:=chr$(i)}
            l = i
            c = Asc(i$) + 256*Asc(Mid$(i$,2))
            w$ = dict$(c)
            o$ = w$
            IF Len(i$) < 4 Then = o$
            For i = 3 to Len(i$) STEP 2 {
                  c = Asc(Mid$(i$,i)) + 256*Asc(Mid$(i$,i+1))
                  IF c < l Then {
                        t$ = dict$(c)
                  } Else t$ = w$ + LEFT$(w$,1)
                  o$ += t$
                  Append dict, l:=w$ + LEFT$(t$,1)
                  l += 1 : w$ = t$
            }
            = o$
      }
      Doc$=decodeLZW$(encodeLZW$)+{
      }
      Clipboard Doc$
      Report Doc$
}
FastM2000

```

{{out}}
<pre >
 84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263
TOBEORNOTTOBEORTOBEORNOT

```



## Mathematica

{{trans|Ruby}}
<lang>compress[uncompressed_] :=
  Module[{dictsize, dictionary, w, result, wc},
   dictsize = 256;
   dictionary = # -> # & /@ FromCharacterCode /@ Range@dictsize;
   w = "";
   result = {};
   Do[wc = w <> c;
    If[MemberQ[dictionary[[All, 1]], wc],
     w = wc,
     AppendTo[result, w /. dictionary];
     AppendTo[dictionary, wc -> dictsize];
     dictsize++;
     w = c],
    {c, Characters[uncompressed]}];
   AppendTo[result, w /. dictionary];
   result];
decompress::bc = "Bad compressed `1`";
decompress[compressed_] :=
  Module[{dictsize, dictionary, w, result, entry},
   dictsize = 256;
   dictionary = # -> # & /@ FromCharacterCode /@ Range@dictsize;
   w = result = compressed[[1]];
   Do[Which[MemberQ[dictionary[[All, 1]], k],
     entry = k /. dictionary,
     k == dictsize,
     entry = w <> StringTake[w, 1],
     True,
     Message[decompress::bc, k]];
    result = result <> entry;
    AppendTo[dictionary, dictsize -> w <> StringTake[entry, 1]];
    dictsize++;
    w = entry,
    {k, compressed[[2 ;;]]}];
   result];
(*How to use:*)
compress["TOBEORNOTTOBEORTOBEORNOT"]
decompress[%]
```

{{Out}}

```txt
{"T", "O", "B", "E", "O", "R", "N", "O", "T", 256, 258, 260, 265, 259, 261, 263}

"TOBEORNOTTOBEORTOBEORNOT"
```



## Nim

<lang>
import tables

proc compress*(uncompressed: string): seq[int] =
  ## build the dictionary
  var dictionary = initTable[string, int]()
  for i in 0..255:
    dictionary.add($char(i), i)

  var w: string = newString(0)
  var compressed = newSeq[int]()

  for c in uncompressed:
    var wc = w & c
    if(dictionary.hasKey(wc)):
      w = wc
    else:
      # writes w to output
      compressed.add(dictionary[w])
      # wc is a new sequence; add it to the dictionary
      dictionary.add(wc, dictionary.len)
      w = $c

  # write remaining output if necessary
  if(w != nil):
    compressed.add(dictionary[w])

  result = compressed

proc decompress*(compressed: var seq[int]): string =
  # build the dictionary
  var dictionary = initTable[int, string]()
  for i in 0..255:
    dictionary.add(i, $char(i))

  var w: string = dictionary[compressed[0]]

  compressed.delete(0)

  var decompressed = w

  for k in compressed:
    var entry: string = newString(0)
    if(dictionary.hasKey(k)):
      entry = dictionary[k]
    elif(k == dictionary.len):
      entry = w & w[0]
    else:
      raise newException(ValueError, "Bad compressed k: " & $k)

    decompressed &= entry

    # new sequence; add it to the dictionary
    dictionary.add(dictionary.len, w & entry[0])

    w = entry

  result = decompressed

when isMainModule:
  var compressed = compress("TOBEORNOTTOBEORTOBEORNOT")
  echo compressed
  var decompressed = decompress(compressed)
  echo decompressed

```

{{Out}}

```txt

@[84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263]
TOBEORNOTTOBEORTOBEORNOT

```



## Objeck

{{trans|Java}}

```objeck
use Collection;

class LZW {
  function : Main(args : String[]) ~ Nil {
    compressed := Compress("TOBEORNOTTOBEORTOBEORNOT");
    Show(compressed);
    decompressed := Decompress(compressed);
    decompressed->PrintLine();
  }

  function : native : Compress(uncompressed : String) ~ IntVector {
    # Build the dictionary.
    dictSize := 256;
      dictionary := StringMap->New();
      for (i := 0; i < 256; i+=1;) {
        key := "";
        key->Append(i->As(Char));
        dictionary->Insert(key, IntHolder->New(i));
    };

    w := "";
    result := IntVector->New();

    each (i : uncompressed) {
      c := uncompressed->Get(i);
      wc := String->New(w);
      wc->Append(c);
        if (dictionary->Has(wc)) {
          w := wc;
        }
        else {
          value := dictionary->Find(w)->As(IntHolder);
          result->AddBack(value->Get());
          # Add wc to the dictionary.
          dictionary->Insert(wc, IntHolder->New(dictSize));
          dictSize+=1;
          w := "";
          w->Append(c);
        };
      };

      # Output the code for w.
      if (w->Size() > 0) {
        value := dictionary->Find(w)->As(IntHolder);
        result->AddBack(value->Get());
      };

      return result;
  }

  function : Decompress(compressed : IntVector) ~ String {
    # Build the dictionary.
    dictSize := 256;
    dictionary := IntMap->New();
    for (i := 0; i < 256; i+=1;) {
      value := "";
      value->Append(i->As(Char));
      dictionary->Insert(i, value);
    };

    w := "";
    found := compressed->Remove(0);
    w->Append(found->As(Char));

    result := String->New(w);
    each (i : compressed) {
      k := compressed->Get(i);

      entry : String;
      if (dictionary->Has(k)) {
        entry := dictionary->Find(k);
      }
      else if (k = dictSize) {
        entry := String->New(w);
        entry->Append(w->Get(0));
      }
      else {
        return "";
      };
      result->Append(entry);

      # Add w+entry[0] to the dictionary.
      value := String->New(w);
      value->Append(entry->Get(0));
      dictionary->Insert(dictSize, value);
      dictSize+=1;

      w := entry;
    };

    return result;
  }

  function : Show(results : IntVector) ~ Nil {
    "["->Print();
    each(i : results) {
      results->Get(i)->Print();
      if(i + 1 < results->Size()) {
        ", "->Print();
      };
    };
    "]"->PrintLine();
  }
}
```



```txt
[84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263]
TOBEORNOTTOBEORTOBEORNOT
```


=={{header|Objective-C}}==

{{works with|GNUstep}}

The class for the LZW compression algorithm:


```objc>#import <Foundation/Foundation.h

#import <stdio.h>

@interface LZWCompressor : NSObject
{
  @private
    NSMutableArray *iostream;
    NSMutableDictionary *dict;
    NSUInteger codemark;
}

-(instancetype) init;
-(instancetype) initWithArray: (NSMutableArray *) stream;
-(BOOL) compressData: (NSData *) string;
-(void) setArray: (NSMutableArray *) stream;
-(NSArray *) getArray;
@end

@implementation LZWCompressor : NSObject

-(instancetype) init
{
   self = [super init];
   if ( self )
   {
      iostream = nil;
      codemark = 256;
      dict = [[NSMutableDictionary alloc] initWithCapacity: 512];
   }
   return self;
}

-(instancetype) initWithArray: (NSMutableArray *) stream
{
   self = [self init];
   if ( self )
   {
      [self setArray: stream];
   }
   return self;
}

-(void) setArray: (NSMutableArray *) stream
{
   iostream = stream;
}

-(BOOL) compressData: (NSData *) string;
{
    // prepare dict
    for(NSUInteger i=0; i < 256; i++)
    {
       unsigned char j = i;
       NSData *s = [NSData dataWithBytes: &j length: 1];
       dict[s] = @(i);
    }

    NSData *w = [NSData data];

    for(NSUInteger i=0; i < [string length]; i++)
    {
       NSMutableData *wc = [NSMutableData dataWithData: w];
       [wc appendData: [string subdataWithRange: NSMakeRange(i, 1)]];
       if ( dict[wc] != nil )
       {
          w = wc;
       } else {
          [iostream addObject: dict[w]];
          dict[wc] = @(codemark);
          codemark++;
          w = [string subdataWithRange: NSMakeRange(i, 1)];
       }
    }
    if ( [w length] != 0 )
    {
       [iostream addObject: dict[w]];
    }
    return YES;
}

-(NSArray *) getArray
{
  return iostream;
}

@end
```


Usage example:


```objc
NSString *text = @"TOBEORNOTTOBEORTOBEORNOT";

int main()
{
  @autoreleasepool {

    NSMutableArray *array = [[NSMutableArray alloc] init];
    LZWCompressor *lzw = [[LZWCompressor alloc]
                          initWithArray: array ];
    if ( lzw )
    {
       [lzw compressData: [text dataUsingEncoding: NSUTF8StringEncoding]];
       for ( id obj in array )
       {
          printf("%u\n", [obj unsignedIntValue]);
       }
    }

  }
  return EXIT_SUCCESS;
}
```


{{out}} (reformatted by hand):

```txt

 84  79  66  69  79  82  78  79
 84 256 258 260 265 259 261 263

```



## OCaml



```ocaml
#directory "+extlib"  (* or maybe "+site-lib/extlib/" *)
#load "extLib.cma"
open ExtString

(** compress a string to a list of output symbols *)
let compress ~uncompressed =
  (* build the dictionary *)
  let dict_size = 256 in
  let dictionary = Hashtbl.create 397 in
  for i=0 to 255 do
    let str = String.make 1 (char_of_int i) in
    Hashtbl.add dictionary str i
  done;

  let f = (fun (w, dict_size, result) c ->
    let c = String.make 1 c in
    let wc = w ^ c in
    if Hashtbl.mem dictionary wc then
      (wc, dict_size, result)
    else
      begin
        (* add wc to the dictionary *)
        Hashtbl.add dictionary wc dict_size;
        let this = Hashtbl.find dictionary w in
        (c, dict_size + 1, this::result)
      end
  ) in
  let w, _, result =
    String.fold_left f ("", dict_size, []) uncompressed
  in

  (* output the code for w *)
  let result =
    if w = ""
    then result
    else (Hashtbl.find dictionary w) :: result
  in

  (List.rev result)
;;

exception ValueError of string

(** decompress a list of output symbols to a string *)
let decompress ~compressed =
  (* build the dictionary *)
  let dict_size = 256 in
  let dictionary = Hashtbl.create 397 in
  for i=0 to pred dict_size do
    let str = String.make 1 (char_of_int i) in
    Hashtbl.add dictionary i str
  done;

  let w, compressed =
    match compressed with
    | hd::tl -> (String.make 1 (char_of_int hd)), tl
    | [] -> failwith "empty input"
  in

  let result = [w] in

  let result, _, _ =
    List.fold_left (fun (result, w, dict_size) k ->
      let entry =
        if Hashtbl.mem dictionary k then
          Hashtbl.find dictionary k
        else if k = Hashtbl.length dictionary then
          w ^ (String.make 1 w.[0])
        else
          raise(ValueError(Printf.sprintf "Bad compressed k: %d" k))
      in
      let result = entry :: result in

      (* add (w ^ entry.[0]) to the dictionary *)
      Hashtbl.add dictionary dict_size (w ^ (String.make 1 entry.[0]));
      (result, entry, dict_size + 1)
    ) (result, w, dict_size) compressed
  in
  (List.rev result)
;;
```


here is the interface:

```ocaml
val compress : uncompressed:string -> int list
val decompress : compressed:int list -> string list
```


How to use:<br />
The compressed datas are a list of symbols (of type int) that will require more than 8 bits to be saved.
So to know how many bits are required, you need to know how many bits are required for the greatest symbol in the list.


```ocaml
let greatest = List.fold_left max 0 ;;

(** number of bits needed to encode the integer m *)
let n_bits m =
  let m = float m in
  let rec aux n =
    let max = (2. ** n) -. 1. in
    if max >= m then int_of_float n
    else aux (n +. 1.0)
  in
  aux 1.0
;;

let write_compressed ~filename ~compressed =
  let nbits = n_bits(greatest compressed) in
  let oc = open_out filename in
  output_byte oc nbits;
  let ob = IO.output_bits(IO.output_channel oc) in
  List.iter (IO.write_bits ob nbits) compressed;
  IO.flush_bits ob;
  close_out oc;
;;

let read_compressed ~filename =
  let ic = open_in filename in
  let nbits = input_byte ic in
  let ib = IO.input_bits(IO.input_channel ic) in
  let rec loop acc =
    try
      let code = IO.read_bits ib nbits in
      loop (code::acc)
    with _ -> List.rev acc
  in
  let compressed = loop [] in
  let result = decompress ~compressed in
  let buf = Buffer.create 2048 in
  List.iter (Buffer.add_string buf) result;
  (Buffer.contents buf)
;;
```



## Ol

This version use lazy streams which is pair (symbol . function-to-get-next-symbol).

```scheme

(define (compress str)
(let loop ((dc (fold (lambda (f x) ; dictionary (simplest, not optimized), with reversed codes
                        (cons (list x) (cons x f)))
                  '() (iota 256)))
           (w '()) ; output sequence (reversed)
           (s 256) ; maximal dictionary code value + 1
           (x '()) ; current sequence
           (r (str-iter str))); input stream
   (cond
      ((null? r)
         (reverse (cons (cadr (member x dc)) w)))
      ((pair? r)
         (let ((xy (cons (car r) x)))
            (if (member xy dc)
               (loop dc w s xy (cdr r))
               (loop (cons xy (cons s dc))         ; update dictionary with xy . s
                     (cons (cadr (member x dc)) w) ; add code to output stream
                     (+ s 1) ; increase code
                     (list (car r)) ; new current sequence
                     (cdr r))))) ; next input
      (else
         (loop dc w s x (r)))))
)

(print (compress "TOBEORNOTTOBEORTOBEORNOT")) ; => (84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263)

```


And decoder (runes->string used to unify functions - both used string iterators):

```scheme

(define (decompress str)
(let loop ((dc (fold (lambda (f x) ; dictionary (simplest, not optimized), with reversed codes
                        (cons x (cons (list x) f)))
                  '() (iota 256)))
           (w '()) ; output sequence (reversed)
           (s 256) ; maximal dictionary code value + 1
           (x '()) ; current symbols sequence
           (r (str-iter str))); input stream
   (cond
      ((null? r)
         (reverse w))
      ((pair? r)
         (let*((y (cadr (member (car r) dc)))
               (xy (append y x)))
            (if (member xy dc)
               (loop dc (append y w) s xy (cdr r)) ; вряд ли такое будет...
               (loop (cons s (cons xy dc))          ; update dictionary with xy . s
                     (append y w) ; add phrase to output stream
                     (+ s 1)
                     y ; new initial code
                     (cdr r))))) ; next input
      (else
         (loop dc w s x (r))))))

(print (runes->string
   (decompress (runes->string '(84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263)))))
; => TOBEORNOTTOBEORTOBEEORNOT

```



## Perl


In this version the hashes contain mixed typed data:

```perl
# Compress a string to a list of output symbols.
sub compress {
    my $uncompressed = shift;

    # Build the dictionary.
    my $dict_size = 256;
    my %dictionary = map {chr $_ => chr $_} 0..$dict_size-1;

    my $w = "";
    my @result;
    foreach my $c (split '', $uncompressed) {
        my $wc = $w . $c;
        if (exists $dictionary{$wc}) {
            $w = $wc;
        } else {
            push @result, $dictionary{$w};
            # Add wc to the dictionary.
            $dictionary{$wc} = $dict_size;
            $dict_size++;
            $w = $c;
        }
    }

    # Output the code for w.
    if ($w) {
        push @result, $dictionary{$w};
    }
    return @result;
}

# Decompress a list of output ks to a string.
sub decompress {
    my @compressed = @_;

    # Build the dictionary.
    my $dict_size = 256;
    my %dictionary = map {chr $_ => chr $_} 0..$dict_size-1;

    my $w = shift @compressed;
    my $result = $w;
    foreach my $k (@compressed) {
        my $entry;
        if (exists $dictionary{$k}) {
            $entry = $dictionary{$k};
        } elsif ($k == $dict_size) {
            $entry = $w . substr($w,0,1);
        } else {
            die "Bad compressed k: $k";
        }
        $result .= $entry;

        # Add w+entry[0] to the dictionary.
        $dictionary{$dict_size} = $w . substr($entry,0,1);
        $dict_size++;

        $w = $entry;
    }
    return $result;
}

# How to use:
my @compressed = compress('TOBEORNOTTOBEORTOBEORNOT');
print "@compressed\n";
my $decompressed = decompress(@compressed);
print "$decompressed\n";
```


{{out}}

```txt

T O B E O R N O T 256 258 260 265 259 261 263
TOBEORNOTTOBEORTOBEORNOT

```



## Perl 6

{{trans|Perl}}

```perl6
sub compress(Str $uncompressed --> Seq)  {
    my $dict-size = 256;
    my %dictionary = (.chr => .chr for ^$dict-size);

    my $w = "";
    gather {
  for $uncompressed.comb -> $c {
      my $wc = $w ~ $c;
      if %dictionary{$wc}:exists { $w = $wc }
      else {
    take %dictionary{$w};
    %dictionary{$wc} = +%dictionary;
    $w = $c;
      }
  }

  take %dictionary{$w} if $w.chars;
    }
}

sub decompress(@compressed --> Str) {
    my $dict-size = 256;
    my %dictionary = (.chr => .chr for ^$dict-size);

    my $w = shift @compressed;
    join '', gather {
  take $w;
  for @compressed -> $k {
      my $entry;
      if %dictionary{$k}:exists { take $entry = %dictionary{$k} }
      elsif $k == $dict-size    { take $entry = $w ~ $w.substr(0,1) }
      else                      { die "Bad compressed k: $k" }

      %dictionary{$dict-size++} = $w ~ $entry.substr(0,1);
      $w = $entry;
  }
    }
}

my @compressed = compress('TOBEORNOTTOBEORTOBEORNOT');
say @compressed;
my $decompressed = decompress(@compressed);
say $decompressed;
```

{{out}}

```txt

T O B E O R N O T 256 258 260 265 259 261 263
TOBEORNOTTOBEORTOBEORNOT

```



## Phix

{{trans|Lua}}

```Phix
function compress(string uncompressed)
integer dict = new_dict()
sequence result = {}
integer dictSize = 255, c
string word = ""
    for i=0 to 255 do
        setd(""&i,i,dict)
    end for
    for i=1 to length(uncompressed) do
        c = uncompressed[i]
        if getd_index(word&c,dict) then
            word &= c
        else
            result &= getd(word,dict)
            dictSize += 1
            setd(word&c,dictSize,dict)
            word = ""&c
        end if
    end for
    if word!="" then
        result &= getd(word,dict)
    end if
    destroy_dict(dict)
    return result
end function

function decompress(sequence compressed)
integer dict = new_dict()
integer dictSize = 255, k, ki
string dent = "", result = "", word = ""
    for i=0 to 255 do
        setd(i,""&i,dict)
    end for
    for i=1 to length(compressed) do
        k = compressed[i]
        ki = getd_index(k,dict)
        if ki then
            dent = getd_by_index(ki,dict)
        elsif k=dictSize then
            dent = word&word[1]
        else
            return {NULL,i}
        end if
        result &= dent
        setd(dictSize,word&dent[1],dict)
        dictSize += 1
        word = dent
    end for
    destroy_dict(dict)
    return result
end function

constant example = "TOBEORNOTTOBEORTOBEORNOT"
sequence com = compress(example)
--?com
pp(com)
?decompress(com)

```

{{out}}

```txt

{84'T',79'O',66'B',69'E',79'O',82'R',78'N',79'O',84'T',256,258,260,265,259,261,263}
"TOBEORNOTTOBEORTOBEORNOT"

```



## PHP

{{trans|JavaScript}}

```PHP
class LZW
{
    function compress($unc) {
        $i;$c;$wc;
        $w = "";
        $dictionary = array();
        $result = array();
        $dictSize = 256;
        for ($i = 0; $i < 256; $i += 1) {
            $dictionary[chr($i)] = $i;
        }
        for ($i = 0; $i < strlen($unc); $i++) {
            $c = $unc[$i];
            $wc = $w.$c;
            if (array_key_exists($w.$c, $dictionary)) {
                $w = $w.$c;
            } else {
                array_push($result,$dictionary[$w]);
                $dictionary[$wc] = $dictSize++;
                $w = (string)$c;
            }
        }
        if ($w !== "") {
            array_push($result,$dictionary[$w]);
        }
        return implode(",",$result);
    }

    function decompress($com) {
        $com = explode(",",$com);
        $i;$w;$k;$result;
        $dictionary = array();
        $entry = "";
        $dictSize = 256;
        for ($i = 0; $i < 256; $i++) {
            $dictionary[$i] = chr($i);
        }
        $w = chr($com[0]);
        $result = $w;
        for ($i = 1; $i < count($com);$i++) {
            $k = $com[$i];
            if ($dictionary[$k]) {
                $entry = $dictionary[$k];
            } else {
                if ($k === $dictSize) {
                    $entry = $w.$w[0];
                } else {
                    return null;
                }
            }
            $result .= $entry;
            $dictionary[$dictSize++] = $w . $entry[0];
            $w = $entry;
        }
        return $result;
    }
}

//How to use
$str = 'TOBEORNOTTOBEORTOBEORNOT';
$lzw = new LZW();
$com = $lzw->compress($str);
$dec = $lzw->decompress($com);
echo $com . "
" . $dec;

```

{{out}}

```txt

84,79,66,69,79,82,78,79,84,256,258,260,265,259,261,263
TOBEORNOTTOBEORTOBEORNOT

```



## PicoLisp


```PicoLisp
(de lzwCompress (Lst)
   (let (Codes 255  Dict)
      (balance 'Dict
         (make
            (for C Codes
               (link (cons (char C) C)) ) ) )
      (make
         (let W (pop 'Lst)
            (for C Lst
               (let WC (pack W C)
                  (if (lup Dict WC)
                     (setq W WC)
                     (link (cdr (lup Dict W)))
                     (idx 'Dict (cons WC (inc 'Codes)) T)
                     (setq W C) ) ) )
            (and W (link (cdr (lup Dict W)))) ) ) ) )

(de lzwDecompress (Lst)
   (let (Codes 255  Dict)
      (balance 'Dict
         (make
            (for C Codes
               (link (list C (char C))) ) ) )
      (make
         (let W NIL
            (for N Lst
               (let WC (if (lup Dict N) (cdr @) (cons (last W) W))
                  (chain (reverse WC))
                  (when W
                     (idx 'Dict (cons (inc 'Codes) (cons (last WC) W)) T) )
                  (setq W WC) ) ) ) ) ) )
```

Test:

```txt
: (lzwCompress (chop "TOBEORNOTTOBEORTOBEORNOT"))
-> (84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263)

: (pack (lzwDecompress @))
-> "TOBEORNOTTOBEORTOBEORNOT"
```




## PL/I

{{trans|REXX}}
The interesting point is the implementation of REXX's associative array (compound variable).

```pli
*process source xref attributes or(!);
 lzwt: Proc Options(main);

 Dcl (LEFT,LENGTH,SUBSTR,TRANSLATE,TRIM,UNSPEC) Builtin;
 Dcl SYSPRINT Print;

 Dcl str Char(50) Var Init('TOBEORNOTTOBEORTOBEORNOT');
 Dcl compressed Char(80) Var;
 Dcl decompressed Char(80) Var;

 Dcl 1 dict(0:300),
      2 key Char(5) Var,
      2 inx Bin Fixed(16) Unsigned;
 Dcl dict_size Bin Fixed(31) Init(256);
 Dcl hi Bin Fixed(16) Unsigned Init(65535);

 Put Edit('str=',str)(Skip,a,a);
 compressed = compress(str);
 Put Edit(compressed)(Skip,a);
 decompressed = decompress(compressed);
 Put Edit('dec=',decompressed)(Skip,a,a);
 If decompressed=str Then
   Put Edit('decompression ok')(Skip,a);
 Else
   Put Edit('decompression not ok')(Skip,a);

 compress: Proc(s) Returns(Char(80) Var);
 Dcl s Char(*) Var;
 Dcl res Char(80) Var;
 Dcl i Bin Fixed(31);
 Dcl c  Char(1);
 Dcl w  Char(5) Var;
 Dcl wc Char(5) Var;
 dict.key='';
 Dcl ii Bin Fixed(8) Unsigned;
 Do i=0 To 255;
   ii=i;
   Unspec(c)=unspec(ii);
   dict.key(i)=c;
   dict.inx(i)=i;
   End;
 res='[';
 w='';
 Do i=1 To length(s);
   c=substr(s,i,1);
   wc=w!!c;
   If dicti(wc)^=hi Then Do;
     w=wc;
     End;
   Else Do;
     res=res!!trim(dicti(w))!!', ';
     Call dict_add(wc,dict_size);
     w=c;
     End;
   End;
 If w^='' Then
   res=res!!trim(dicti(w))!!', ';
 substr(res,length(res)-1,1)=']';
 Return(res);

 dicti: Proc(needle) Returns(Bin Fixed(31));
   Dcl needle Char(*) Var;
   Dcl i Bin Fixed(31);
   Do i=1 To dict_size;
     If dict.key(i)=needle Then
       Return(i);
     End;
   Return(hi);
   End;

 dict_add: Proc(needle,dict_size);
   Dcl needle Char(*) Var;
   Dcl dict_size Bin Fixed(31);
   dict.key(dict_size)=needle;
   dict.inx(dict_size)=dict_size;
   dict_size+=1;
   End;

 End;

 decompress: Proc(s) Returns(Char(80) Var);
 Dcl s Char(80) Var;
 Dcl ss Char(80) Var;
 Dcl words(50) Char(5) Var;
 Dcl wn Bin Fixed(31);
 Dcl ww  Bin Fixed(31);
 Dcl c  Char(1);
 Dcl entry Char(5) Var;
 Dcl w Char(5) Var;
 Dcl res Char(80) Var;
 ss=translate(s,'    ','[],');
 Call mk_words(ss,words,wn);
 dict.key='';
 dict.inx=hi;
 Dcl i Bin Fixed(31);
 Dcl ii Bin Fixed(8) Unsigned;
 Dcl dict(0:300) Char(5) Var;
 Dcl dict_size Bin Fixed(31);
 Do i=0 To 255;
   ii=i;
   Unspec(c)=unspec(ii);
   dict(i)=c;
   End;
 dict_size=256;
 ww=words(1);
 w=dict(ww);
 res=w;
 Do i=2 To wn;
   ww=words(i);
   Select;
     When(dict(ww)^='')
       entry=dict(ww);
     When(ww=dict_size)
       entry=w!!substr(w,1,1);
     Otherwise
       Put Edit('Bad compressed k: ',ww)(Skip,a,a);
     End;
   res=res!!entry;
   dict(dict_size)=w!!substr(entry,1,1);
   dict_size+=1;
   w=entry;
   End;
 Return(res);
 End;

 mk_words: Proc(st,arr,arrn);
 Dcl st Char(*) Var;
 Dcl sv Char(80) Var;
 Dcl arr(*) Char(5) Var;
 Dcl arrn Bin fixed(31);
 Dcl elem Char(5) Var;
 arrn=0;
 sv=st!!' ';
 elem='';
 Do While(length(sv)>0);
   If left(sv,1)=' ' Then Do;
     If elem>'' Then Do;
       arrn+=1;
       arr(arrn)=elem;
       elem='';
       End;
     End;
   Else
     elem=elem!!left(sv,1);
   sv=substr(sv,2);
   End;
 End;
 Return;

 End;
```

{{out}}

```txt
str=TOBEORNOTTOBEORTOBEORNOT
[84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263]
dec=TOBEORNOTTOBEORTOBEORNOT
decompression ok
```



## PureBasic

This version encodes character sequences as 16-bit values.
Because this version only encodes an input string it won't handle Null values.
This is because PureBasic uses these to terminate strings.
Only slight modifications are necessary to handle Null values that would be present for a more generic routine that could be used with a buffer containing any data type.

```PureBasic
Procedure compress(uncompressed.s, List result.u())
  ;Compress a string to a list of output symbols

  ;Build the dictionary.
  Protected  dict_size = 255, i
  newmap dict.u()
  For i = 0 To 254
    dict(Chr(i + 1)) = i
  Next

  Protected w.s, wc.s, *c.Character = @uncompressed
  w = ""
  LastElement(result())
  While *c\c <> #Null
    wc = w + Chr(*c\c)
    If FindMapElement(dict(), wc)
      w = wc
    Else
      AddElement(result())
      result() = dict(w)
      ;Add wc to the dictionary
      dict(wc) = dict_size
      dict_size + 1 ;no check is performed for overfilling the dictionary.
      w = Chr(*c\c)
    EndIf
    *c + 1
  Wend

  ;Output the code for w
  If w
    AddElement(result())
    result() = dict(w)
  EndIf
EndProcedure

Procedure.s decompress(List compressed.u())
  ;Decompress a list of encoded values to a string
  If ListSize(compressed()) = 0: ProcedureReturn "": EndIf

  ;Build the dictionary.
  Protected  dict_size = 255, i

  Dim dict.s(255)
  For i = 1 To 255
    dict(i - 1) = Chr(i)
  Next

  Protected w.s, entry.s, result.s
  FirstElement(compressed())
  w = dict(compressed())
  result = w

  i = 0
  While NextElement(compressed())
    i + 1
    If compressed() < dict_size
    entry = dict(compressed())
    ElseIf i = dict_size
      entry = w + Left(w, 1)
    Else
      MessageRequester("Error","Bad compression at [" + Str(i) + "]")
      ProcedureReturn result;abort
    EndIf
    result + entry
    ;Add w + Left(entry, 1) to the dictionary
    If ArraySize(dict()) <= dict_size
      Redim dict(dict_size + 256)
    EndIf
    dict(dict_size) = w + Left(entry, 1)
    dict_size + 1 ;no check is performed for overfilling the dictionary.

    w = entry
  Wend
  ProcedureReturn result
EndProcedure

If OpenConsole()
  ;How to use:

  Define initial.s, decompressed.s

  Print("Type something: ")
  initial = Input()
  NewList compressed.u()
  compress(initial, compressed())
  ForEach compressed()
    Print(Str(compressed()) + " ")
  Next
  PrintN("")

  decompressed = decompress(compressed())
  PrintN(decompressed)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
Type something: TOBEORNOTTOBEORTOBEORNOT
83 78 65 68 78 81 77 78 83 255 257 259 264 258 260 262
TOBEORNOTTOBEORTOBEORNOT
```



## Python


In this version the dicts contain mixed typed data:

```python
def compress(uncompressed):
    """Compress a string to a list of output symbols."""

    # Build the dictionary.
    dict_size = 256
    dictionary = dict((chr(i), i) for i in xrange(dict_size))
    # in Python 3: dictionary = {chr(i): i for i in range(dict_size)}

    w = ""
    result = []
    for c in uncompressed:
        wc = w + c
        if wc in dictionary:
            w = wc
        else:
            result.append(dictionary[w])
            # Add wc to the dictionary.
            dictionary[wc] = dict_size
            dict_size += 1
            w = c

    # Output the code for w.
    if w:
        result.append(dictionary[w])
    return result


def decompress(compressed):
    """Decompress a list of output ks to a string."""
    from cStringIO import StringIO

    # Build the dictionary.
    dict_size = 256
    dictionary = dict((i, chr(i)) for i in xrange(dict_size))
    # in Python 3: dictionary = {i: chr(i) for i in range(dict_size)}

    # use StringIO, otherwise this becomes O(N^2)
    # due to string concatenation in a loop
    result = StringIO()
    w = chr(compressed.pop(0))
    result.write(w)
    for k in compressed:
        if k in dictionary:
            entry = dictionary[k]
        elif k == dict_size:
            entry = w + w[0]
        else:
            raise ValueError('Bad compressed k: %s' % k)
        result.write(entry)

        # Add w+entry[0] to the dictionary.
        dictionary[dict_size] = w + entry[0]
        dict_size += 1

        w = entry
    return result.getvalue()


# How to use:
compressed = compress('TOBEORNOTTOBEORTOBEORNOT')
print (compressed)
decompressed = decompress(compressed)
print (decompressed)
```


Output:

```txt

[84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263]
TOBEORNOTTOBEORTOBEORNOT

```



## Racket


```racket

#lang racket
; utilities
(define-syntax def (make-rename-transformer #'define))
(define (dict-ref d w) (hash-ref d w #f))
(define (append-char w c)  (string-append w (string c)))
(define (append-first w s) (append-char w (string-ref s 0)))

;; Compress a string with LZW
(define (compress uncompressed)
  (def d (make-hash))
  (def (dict-add d w) (hash-set! d w (hash-count d)))
  ; build initial dictionary
  (for ([i (in-range 256)])
    (def s (string (integer->char i)))
    (hash-set! d s s))
  ; compress the string
  (def result '())
  (def (emit! i) (set! result (cons i result)))
  (def w "")
  (for ([c uncompressed])
    (define wc (append-char w c))
    (cond
      [(dict-ref d wc) (set! w wc)]
      [else            (emit! (dict-ref d w))
                       (dict-add d wc)
                       (set! w (string c))]))
  (emit! (dict-ref d w))
  (reverse result))

;; Decompress a LZW compressed string
(define (decompress compressed)
  (def d (make-hash))
  (def (dict-add! w) (hash-set! d (hash-count d) w))
  ; build initial dictionary
  (for ([i (in-range 256)])
    (def s (string (integer->char i)))
    (hash-set! d s s))
  ; decompress the list
  (def w (first compressed))
  (apply string-append
         w
         (for/list ([k (rest compressed)])
           (def entry
             (or (dict-ref d k)
                 (if (eqv? k (hash-count d))
                     (append-first w w)
                     (error 'lzq-decompress "faulty input"))))
           (dict-add! (append-first w entry))
           (set! w entry)
           entry)))

(def uncompressed "TOBEORNOTTOBEORTOBEORNOT")
(displayln uncompressed)
(def compressed (compress uncompressed))
(displayln compressed)
(def decompressed (decompress compressed))
(displayln decompressed)

```


Output:

```txt

TOBEORNOTTOBEORTOBEORNOT
(T O B E O R N O T 256 258 260 265 259 261 263)
TOBEORNOTTOBEORTOBEORNOT

```



## REXX


### version 1

{{trans|Java}}

```rexx
/* REXX ---------------------------------------------------------------
* 20.07.2014 Walter Pachl translated from Java
* 21.07.2014 WP allow for blanks in the string
*--------------------------------------------------------------------*/
Parse Arg str
default="TOBEORNOTTOBEORTOBEORNOT"
If str='' Then
  str=default
/* str=space(str,0) */
Say 'str='str
compressed = compress(str)
Say compressed
If str=default Then Do
  cx='[84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263]'
  If cx==compressed Then Say 'compression ok'
  End
decompressed = decompress(compressed)
Say 'dec='decompressed
If decompressed=str Then Say 'decompression ok'
Exit

compress: Procedure
Parse Arg uncompressed
dict.=''
Do i=0 To 255
  z=d2c(i)
  d.i=z
  dict.z=i
  End
dict_size=256
res='['
w=''
Do i=1 To length(uncompressed)
  c=substr(uncompressed,i,1)
  wc=w||c
  If dict.wc<>'' Then
    w=wc
  Else Do
    res=res||dict.w', '
    dict.wc=dict_size
    dict_size=dict_size+1
    w=c
    End
  End
If w<>'' Then
  res=res||dict.w', '
Return left(res,length(res)-2)']'

decompress: Procedure
Parse Arg compressed
compressed=space(translate(compressed,'','[],'))
d.=''
Do i=0 To 255
  z=d2c(i)
  d.i=z
  End
dict_size=256
Parse Var compressed w compressed
res=d.w
w=d.w
Do i=1 To words(compressed)
  k=word(compressed,i)
  Select
    When d.k<>'' | d.k==' ' then /* allow for blank */
      entry=d.k
    When k=dict_size Then
      entry=w||substr(w,1,1)
    Otherwise
      Say "Bad compressed k: "  k
    End
  res=res||entry
  d.dict_size=w||substr(entry,1,1)
  dict_size=dict_size+1
  w=entry
  End
Return res
```

'''Output:'''

```txt
str=TOBEORNOTTOBEORTOBEORNOT
[84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263]
compression ok
dec=TOBEORNOTTOBEORTOBEORNOT
decompression ok

str=To be or not to be that is the question!
[84, 111, 32, 98, 101, 32, 111, 114, 32, 110, 111, 116, 32, 116, 257, 259, 268, 104, 97, 267, 105, 115, 272, 260, 113, 117, 101, 115, 116, 105, 111, 110, 33]
dec=To be or not to be that is the question!
decompression ok

```



### version 2

(Based on the '''Java''' program.)

This REXX version can execute on   '''ASCII'''   or   '''EBCDIC'''   systems.

```rexx
/*REXX program compresses text using the  LZW  (Lempel─Ziv─Welch), and reconstitutes it.*/
parse arg x;     if x==''  then                  /*get an optional argument from the CL.*/
          x= '"There is nothing permanent except change."   ───  Heraclitus  [540-475 BC]'
       say 'original text='        x             /* [↑]  Not specified? Then use default*/
cypher= LZWc(x)                                  /*compress text using the LZW algorithm*/
       say 'reconstituted='   LZWd(cypher)       /*display the reconstituted string.    */
       say ' LZW integers='        cypher        /*   "     "  LZW  integers used.      */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
LZWc: procedure; parse arg y,,w $ @.;            #= 256        /*LZW compress algorithm.*/
                                     do j=0  for #;    _= d2c(j);    @._= j;    end  /*j*/
       do k=1  for length(y)+1;            z= w || substr(y, k, 1)
       if @.z==''  then do;  $= $ @.w;   @.z= #;    #= # + 1;    w= substr(y, k, 1);   end
                   else w= z
       end   /*k*/;                           return substr($, 2)   /*del leading blank.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
LZWd: procedure; parse arg x y,,@.;              #= 256      /*LZW decompress algorithm.*/
                                     do j=0  for #;      @.j= d2c(j);      end  /*j*/
      $= @.x;  w= $                                          /*#:  is the dictionay size*/
                      do k=1  for words(y);             z= word(y, k)
                      if @.z\=='' | @.k==" "  then ?= @.z
                                              else if z==#  then ?= w || left(w, 1)
                      $= $ || ?
                      @.#= w || left(?, 1);   #= # + 1;          w= ?
                      end   /*k*/;            return $
```

{{out|output|text=  when using the default input:}}

```txt

original text= "There is nothing permanent except change."   ───  Heraclitus  [540-475 BC]
reconstituted= "There is nothing permanent except change."   ───  Heraclitus  [540-475 BC]
 LZW integers= 34 84 104 101 114 101 32 105 115 32 110 111 116 104 105 110 103 32 112 259 109 97 110 101 110 116 32 101 120 99 101 112 281 99 104 277 103 101 46 34 32 296 196 298 296 72 259 97 99 108 105 116 117 264 32 91 53 52 48 45 52 55 53 32 66 67 93

```



## Ring


```ring

# Project : LZW compression

plaintext = "TOBEORNOTTOBEORTOBEORNOT"
result = []
encode = encodelzw(plaintext)
for i = 1 to len(encode) step 2
    add(result,ascii(substr(encode,i,1)) + 256*ascii(substr(encode,i+1,1)))
next
showarray(result)
see decodelzw(encode)

func encodelzw(text)
       o = ""
       dict = list(4096)
       for i = 1 to 255
            dict[i] = char(i)
       next
       l = i
       i = 1
       w = left(text,1)
       while i < len(text)
              d = 0
              while d < l
                      c = d
                      if i > len(text)
                         exit
                      ok
                      for d = 1 to l
                           if w = dict[d]
                             exit
                           ok
                      next
                      if d < l
                         i = i + 1
                         w = w + substr(text,i,1)
                      ok
              end
              dict[l] = w
              l = l + 1
              w = right(w,1)
              o = o + char(c % 256) + char(floor(c / 256))
       end
       return o

func decodelzw(text)
       o = ""
       dict = list(4096)
       for i = 1 to 255
            dict[i] = char(i)
       next
       l = i
       c = ascii(left(text,1)) + 256*ascii(substr(text,2,1))
       w = dict[c]
       o = w
       if len(text) < 4
          return o
       ok
       for i = 3 to len(text) step 2
            c = ascii(substr(text,i,1)) + 256*ascii(substr(text,i+1,1))
            if c < l
               t = dict[c]
            else
               t = w + left(w,1)
            ok
            o = o + t
            dict[l] = w + left(t,1)
            l = l + 1
            w = t
       next
       return o

func showarray(vect)
        svect = ""
        for n = 1 to len(vect)
              svect = svect + vect[n] + " "
        next
        svect = left(svect, len(svect) - 1)
        see svect + nl

```

Output:

```txt

84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263
TOBEORNOTTOBEORTOBEORNOT

```



## Ruby


In this version the hashes contain mixed typed data:

```ruby
# Compress a string to a list of output symbols.
def compress(uncompressed)
    # Build the dictionary.
    dict_size = 256
    dictionary = Hash[ Array.new(dict_size) {|i| [i.chr, i.chr]} ]

    w = ""
    result = []
    for c in uncompressed.split('')
        wc = w + c
        if dictionary.has_key?(wc)
            w = wc
        else
            result << dictionary[w]
            # Add wc to the dictionary.
            dictionary[wc] = dict_size
            dict_size += 1
            w = c
        end
    end

    # Output the code for w.
    result << dictionary[w] unless w.empty?
    result
end

# Decompress a list of output ks to a string.
def decompress(compressed)
    # Build the dictionary.
    dict_size = 256
    dictionary = Hash[ Array.new(dict_size) {|i| [i.chr, i.chr]} ]

    w = result = compressed.shift
    for k in compressed
        if dictionary.has_key?(k)
            entry = dictionary[k]
        elsif k == dict_size
            entry = w + w[0,1]
        else
            raise 'Bad compressed k: %s' % k
        end
        result += entry

        # Add w+entry[0] to the dictionary.
        dictionary[dict_size] = w + entry[0,1]
        dict_size += 1

        w = entry
    end
    result
end

# How to use:
compressed = compress('TOBEORNOTTOBEORTOBEORNOT')
p compressed
decompressed = decompress(compressed)
puts decompressed
```


Output:

```txt

["T", "O", "B", "E", "O", "R", "N", "O", "T", 256, 258, 260, 265, 259, 261, 263]
TOBEORNOTTOBEORTOBEORNOT

```



## Scala


```scala

def compress(tc:String) = {
    //initial dictionary
    val startDict = (1 to 255).map(a=>(""+a.toChar,a)).toMap
    val (fullDict, result, remain) = tc.foldLeft ((startDict, List[Int](), "")) {
      case ((dict,res,leftOver),nextChar) =>
        if (dict.contains(leftOver + nextChar)) // current substring already in dict
          (dict, res, leftOver+nextChar)
        else if (dict.size < 4096) // add to dictionary
          (dict + ((leftOver+nextChar, dict.size+1)), dict(leftOver) :: res, ""+nextChar)
        else // dictionary is full
          (dict, dict(leftOver) :: res, ""+nextChar)
    }
    if (remain.isEmpty) result.reverse else (fullDict(remain) :: result).reverse
}

def decompress(ns: List[Int]): String = {
  val startDict = (1 to 255).map(a=>(a,""+a.toChar)).toMap
  val (_, result, _) =
    ns.foldLeft[(Map[Int, String], List[String], Option[(Int, String)])]((startDict, Nil, None)) {
    case ((dict, result, conjecture), n) => {
      dict.get(n) match {
        case Some(output) => {
          val (newDict, newCode) = conjecture match {
            case Some((code, prefix)) => ((dict + (code -> (prefix + output.head))), code + 1)
            case None => (dict, dict.size + 1)
          }
          (newDict, output :: result, Some(newCode -> output))
        }
        case None => {
          // conjecture being None would be an encoding error
          val (code, prefix) = conjecture.get
          val output = prefix + prefix.head
          (dict + (code -> output), output :: result, Some(code + 1 -> output))
        }
      }
    }
  }
  result.reverse.mkString("")
}
// test
val text = "TOBEORNOTTOBEORTOBEORNOT"
val compressed = compress(text)
println(compressed)
val result = decompress(compressed)
println(result)

```



## Scheme


```scheme
; Get the list reference number for a member or #f if not found
(define (member-string-ref m l)
  (define r #f)
  (let loop ((i 0))
    (if (< i (length l))
        (if (not (string=? (list-ref l i) m))
            (loop (+ i 1))
            (set! r i))))
  r)

;; Compress a string with LZW
(define (lzw-compress uncompressed)
  (define dictionary '())
  (define n 0)
  (define result '())
  (set! uncompressed (string->list uncompressed))

  ;; Setup Dictionary
  (let dict-setup ((c 0))
    (if (> 256 c)
        (begin
          (set! dictionary (append dictionary
                                   (list (string (integer->char c)))))
          (set! n (+ n 1))
          (dict-setup (+ c 1)))))

  ;; Compress the string
  (let compress ((w "") (ci 0))
    (define c (string (list-ref uncompressed ci)))
    (define wc "")
    (set! wc (string-append w c))
    (if (member-string-ref wc dictionary)
        (set! w wc)
        (begin
          (set! result (append result
                               (list (member-string-ref w dictionary))))
          (set! dictionary (append dictionary (list wc)))
          (set! n (+ n 1))
          (set! w c)))
    (if (eqv? ci (- (length uncompressed) 1))
        (set! result (append result
                             (list (member-string-ref w dictionary))))
        (compress w (+ ci 1))))
  result)

;; Decompress a LZW compressed string (input should be a list of integers)
(define (lzw-decompress compressed)
  (define dictionary '())
  (define n 0)
  (define result "")

  ;; Setup Dictionary
  (let dict-setup ((c 0))
    (if (> 256 c)
        (begin
          (set! dictionary (append dictionary
                                   (list (string (integer->char c)))))
          (set! n (+ n 1))
          (dict-setup (+ c 1)))))

  ;; Decompress the list
  (let decompress ((k (list-ref compressed 0)) (ci 0))
    (define kn #f)
    ;; Add to dictionary
    (if (> (length compressed) (+ ci 1))
        (begin
          (set! kn (list-ref compressed (+ ci 1)))
          (if (< kn (length dictionary))
              (set! dictionary
                    (append dictionary
                            (list (string-append
           (list-ref dictionary k)
           (string (string-ref (list-ref dictionary kn) 0)))))))))

    ;; Build the resulting string
    (set! result (string-append result (list-ref dictionary k)))

    (if (not (eqv? ci (- (length compressed) 1)))
        (decompress kn (+ ci 1))))
  result)

(define compressed (lzw-compress "TOBEORNOTTOBEORTOBEORNOT"))
(display compressed) (newline)
(define decompressed (lzw-decompress compressed))
(display decompressed) (newline)
```

Output:
```txt
(84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263)
TOBEORNOTTOBEORTOBEORNOT
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: lzwCompress (in string: uncompressed) is func
  result
    var string: compressed is "";
  local
    var char: ch is ' ';
    var hash [string] char: mydict is (hash [string] char).value;
    var string: buffer is "";
    var string: xstr is "";
  begin
    for ch range chr(0) to chr(255) do
      mydict @:= [str(ch)] ch;
    end for;
    for ch range uncompressed do
      xstr := buffer & str(ch);
      if xstr in mydict then
        buffer &:= str(ch)
      else
        compressed &:= str(mydict[buffer]);
        mydict @:= [xstr] chr(length(mydict));
        buffer := str(ch);
      end if;
    end for;
    if buffer <> "" then
      compressed &:= str(mydict[buffer]);
    end if;
  end func;

const func string: lzwDecompress (in string: compressed) is func
  result
    var string: uncompressed is "";
  local
    var char: ch is ' ';
    var hash [char] string: mydict is (hash [char] string).value;
    var string: buffer is "";
    var string: current is "";
    var string: chain is "";
  begin
    for ch range chr(0) to chr(255) do
      mydict @:= [ch] str(ch);
    end for;
    for ch range compressed do
      if buffer = "" then
        buffer := mydict[ch];
        uncompressed &:= buffer;
      elsif ch <= chr(255) then
        current := mydict[ch];
        uncompressed &:= current;
        chain := buffer & current;
        mydict @:= [chr(length(mydict))] chain;
        buffer := current;
      else
        if ch in mydict then
          chain := mydict[ch];
        else
          chain := buffer & str(buffer[1]);
        end if;
        uncompressed &:= chain;
        mydict @:= [chr(length(mydict))] buffer & str(chain[1]);
        buffer := chain;
      end if;
    end for;
  end func;

const proc: main is func
  local
    var string: compressed is "";
    var string: uncompressed is "";
  begin
    compressed := lzwCompress("TOBEORNOTTOBEORTOBEORNOT");
    writeln(literal(compressed));
    uncompressed := lzwDecompress(compressed);
    writeln(uncompressed);
  end func;
```


Output:

```txt

"TOBEORNOT\256;\258;\260;\265;\259;\261;\263;"
TOBEORNOTTOBEORTOBEORNOT

```


Original source: [http://seed7.sourceforge.net/algorith/string.htm#lzwCompress] and [http://seed7.sourceforge.net/algorith/string.htm#lzwDecompress]


## Sidef

{{trans|Perl}}

```ruby
# Compress a string to a list of output symbols.
func compress(String uncompressed) -> Array {

    # Build the dictionary.
    var dict_size = 256
    var dictionary = Hash()

    for i in range(dict_size) {
        dictionary{i.chr} = i.chr
    }

    var w = ''
    var result = []
    uncompressed.each { |c|
        var wc = w+c
        if (dictionary.exists(wc)) {
            w = wc
        } else {
            result << dictionary{w}
            # Add wc to the dictionary.
            dictionary{wc} = dict_size
            dict_size++
            w = c
        }
    }

    # Output the code for w.
    if (w != '') {
        result << dictionary{w}
    }

    return result
}

# Decompress a list of output ks to a string.
func decompress(Array compressed) -> String {

    # Build the dictionary.
    var dict_size = 256
    var dictionary = Hash()

    for i in range(dict_size) {
        dictionary{i.chr} = i.chr
    }

    var w = compressed.shift
    var result = w
    compressed.each { |k|
        var entry = nil
        if (dictionary.exists(k)) {
            entry = dictionary{k}
        } elsif (k == dict_size) {
            entry = w+w.first
        } else {
            die "Bad compressed k: #{k}"
        }
        result += entry

        # Add w+entry[0] to the dictionary.
        dictionary{dict_size} = w+entry.first
        dict_size++

        w = entry
    }
    return result
}

# How to use:
var compressed = compress('TOBEORNOTTOBEORTOBEORNOT')
say compressed.join(' ')
var decompressed = decompress(compressed)
say decompressed
```

{{out}}

```txt
T O B E O R N O T 256 258 260 265 259 261 263
TOBEORNOTTOBEORTOBEORNOT
```



## Swift

{{trans|JavaScript}}

```swift
class LZW {
    class func compress(_ uncompressed:String) -> [Int] {
        var dict = [String : Int]()

        for i in 0 ..< 256 {
            let s = String(Unicode.Scalar(UInt8(i)))
            dict[s] = i
        }

        var dictSize = 256
        var w = ""
        var result = [Int]()
        for c in uncompressed {
            let wc = w + String(c)
            if dict[wc] != nil {
                w = wc
            } else {
                result.append(dict[w]!)
                dict[wc] = dictSize + 1
                w = String(c)
            }
        }

        if w != "" {
            result.append(dict[w]!)
        }
        return result
    }

    class func decompress(_ compressed:[Int]) -> String? {
        var dict = [Int : String]()

        for i in 0 ..< 256 {
            dict[i] = String(Unicode.Scalar(UInt8(i)))
        }

        var dictSize = 256
        var w = String(Unicode.Scalar(UInt8(compressed[0])))
        var result = w
        for k in compressed[1 ..< compressed.count] {
            let entry : String
            if let x = dict[k] {
                entry = x
            } else if k == dictSize {
                entry = w + String(w[w.startIndex])
            } else {
                return nil
            }

            result += entry
            dict[dictSize+1] = w + String(entry[entry.startIndex])
            w = entry
        }
        return result
    }
}

let comp = LZW.compress("TOBEORNOTTOBEORTOBEORNOT")
print(comp)

if let decomp = LZW.decompress(comp) {
    print(decomp)
}
```

{{out}}

```txt

[84, 79, 66, 69, 79, 82, 78, 79, 84, 257, 257, 257, 257, 257, 257, 257]
TOBEORNOTOTTOOTTTOOOTTTTOOOOTTTT

```



## Tcl


```tcl
namespace eval LZW {
    variable char2int
    variable chars
    for {set i 0} {$i < 256} {incr i} {
        set char [binary format c $i]
        set char2int($char) $i
        lappend chars $char
    }
}

proc LZW::encode {data} {
    variable char2int
    array set dict [array get char2int]

    set w ""
    set result [list]

    foreach c [split $data ""] {
        set wc $w$c
        if {[info exists dict($wc)]} {
            set w $wc
        } else {
            lappend result $dict($w)
            set dict($wc) [array size dict]
            set w $c
        }
    }
    lappend result $dict($w)
}

proc LZW::decode {cdata} {
    variable chars
    set dict $chars

    set k [lindex $cdata 0]
    set w [lindex $dict $k]
    set result $w

    foreach k [lrange $cdata 1 end] {
        set currSizeDict [llength $dict]
        if {$k < $currSizeDict} {
            set entry [lindex $dict $k]
        } elseif {$k == $currSizeDict} {
            set entry $w[string index $w 0]
        } else {
            error "invalid code ($k) in ($cdata)"
        }
        append result $entry
        lappend dict $w[string index $entry 0]
        set w $entry
    }
    return $result
}

set s TOBEORNOTTOBEORTOBEORNOT#
set e [LZW::encode $s] ;# ==> 84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263 35
set d [LZW::decode $e] ;# ==> TOBEORNOTTOBEORTOBEORNOT#

# or
if {$s eq [LZW::decode [LZW::encode $s]]} then {puts success} else {puts fail} ;# ==> success
```



## Xojo

{{trans|PHP}}

```vb

    Function compress(str as String) As String
      Dim i as integer
      Dim w as String = ""
      Dim c as String
      Dim strLen as Integer
      Dim wc as string
      Dim dic() as string
      Dim result() as string
      Dim lookup as integer
      Dim startPos as integer = 0
      Dim combined as String

      strLen = len(str)

      dic = populateDictionary

      for i = 1 to strLen
        c = str.mid(i, 1)
        wc = w + c

        startPos = getStartPos(wc)

        lookup = findArrayPos(dic, wc, startPos)
        if (lookup <> -1) then
          w = wc
        Else
          startPos = getStartPos(w)
          lookup = findArrayPos(dic, w, startPos)
          if lookup <> -1 then
            result.Append(lookup.ToText)
          end if
          dic.append(wc)
          w = c
        end if
      next i

      if (w <> "") then
        startPos = getStartPos(w)
        lookup = findArrayPos(dic, w, startPos)
        result.Append(lookup.ToText)
      end if

      return join(result, ",")
    End Function

    Function decompress(str as string) As string
      dim comStr() as string
      dim w as string
      dim result as string
      dim comStrLen as integer
      dim entry as string
      dim dic() as string
      dim i as integer

      comStr = str.Split(",")
      comStrLen = comStr.Ubound
      dic = populateDictionary

      w = chr(val(comStr(0)))
      result = w
      for i = 1 to comStrLen
        entry = dic(val(comStr(i)))
        result = result + entry
        dic.append(w + entry.mid(1,1))
        w = entry
      next i

      return result
    End Function

    Private Function findArrayPos(arr() as String, search as String, start as integer) As Integer
      dim arraySize as Integer
      dim arrayPosition as Integer = -1
      dim i as Integer

      arraySize = UBound(arr)

      for i = start to arraySize
        if (strcomp(arr(i), search, 0) = 0) then
          arrayPosition = i
          exit
        end if
      next i

      return arrayPosition
    End Function

    Private Function getStartPos(str as String) As integer
      if (len(str) = 1) then
        return 0
      else
        return 255
      end if
    End Function

    Private Function populateDictionary() As string()
      dim dic() as string
      dim i as integer

      for i = 0 to 255
        dic.append(Chr(i))
      next i

      return dic
    End Function


```


Test:

```txt

  Dim str as String = "TOBEORNOTTOBEORTOBEORNOT"
  Dim com as string = LZW.compress(str)
  print com
  print LZW.decompress(com)

```


Output:

```txt

84,79,66,69,79,82,78,79,84,256,258,260,265,259,261,263
TOBEORNOTTOBEORTOBEORNOT

```



## zkl


```zkl
fcn lzwCompress(uncompressed){ // text-->list of 12 bit ints
   dictionary:=(256).pump(Dictionary(),fcn(n){ return(n.toChar(),n) });
   w,compressed:="",List();
   foreach c in (uncompressed){
      wc:=w+c;
      if(dictionary.holds(wc)) w=wc;
      else{
   compressed.append(dictionary[w]);  // 12 bits
   dictionary[wc]=dictionary.len();
   w=c;
      }
   }
   if(w) compressed.append(dictionary[w]);
   compressed
}
fcn lzwUncompress(compressed){ // compressed data-->text
   dictionary:=(256).pump(Dictionary(),fcn(n){ return(n,n.toChar()) });
   w,decommpressed:=dictionary[compressed[0]],Data(Void,w);
   foreach k in (compressed[1,*]){
      if(dictionary.holds(k))      entry:=dictionary[k];
      else if(k==dictionary.len()) entry:=w+w[0];
      else throw(Exception.ValueError("Invalid compressed data"));
      decommpressed.append(entry);
      dictionary.add(dictionary.len(),w+entry[0]);
      w=entry;
   }
   decommpressed.text
}
```


```zkl
compressed:=lzwCompress("TOBEORNOTTOBEORTOBEORNOT");
compressed.toString(*).println();

lzwUncompress(compressed).println();
```

{{out}}

```txt

L(84,79,66,69,79,82,78,79,84,256,258,260,265,259,261,263)
TOBEORNOTTOBEORTOBEORNOT

```


{{omit from|Lilypond}}
