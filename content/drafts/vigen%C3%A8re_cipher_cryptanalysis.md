+++
title = "Vigenère cipher/Cryptanalysis"
description = ""
date = 2018-06-06T16:17:30Z
aliases = []
[extra]
id = 9837
[taxonomies]
categories = []
tags = []
+++

{{task|Encryption}}[[Category:Encryption]]
Given some text you suspect has been encrypted with a Vigenère cipher, extract the key and plaintext. There are several methods for doing this. See [[wp:Vigenère_cipher#Cryptanalysis|the Wikipedia entry]] for more information. Use the following encrypted text:

```txt

MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH
VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD
ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS
FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG
ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ
ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS
JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT
LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST
MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH
QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV
RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW
TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO
SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR
ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX
BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB
BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA
FWAML ZZRXJ EKAHV FASMU LVVUT TGK

```


Letter frequencies for English can be found [[wp:Letter_frequency|here]].

Specifics for this task:
* Take only the ciphertext as input. You can assume it's all capitalized and has no punctuation, but it might have whitespace.
* Assume the plaintext is written in English.
* Find and output the key.
* Use that key to decrypt and output the original plaintext. Maintaining the whitespace from the ciphertext is optional.
* The algorithm doesn't have to be perfect (which may not be possible) but it should work when given enough ciphertext. The example above is fairly long, and should be plenty for any algorithm.

## Ada

The program is not fully auto, but makes a small number of suggestions for the right key and plaintext.

```Ada
with Ada.Text_IO;

procedure Vignere_Cryptanalysis is

   subtype Letter is Character range 'A' .. 'Z';

   function "+"(X, Y: Letter) return Letter is
   begin
      return Character'Val( ( (Character'Pos(X)-Character'Pos('A'))
                                + (Character'Pos(Y)-Character'Pos('A')) ) mod 26
                          + Character'Pos('A'));
   end;

   function "-"(X, Y: Letter) return Letter is
   begin
      return Character'Val( ( (Character'Pos(X)-Character'Pos('A'))
                                - (Character'Pos(Y)-Character'Pos('A')) ) mod 26
                          + Character'Pos('A'));
   end;

   type Frequency_Array is array (Letter) of Float;

   English: Frequency_Array :=
     ( 0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
       0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
       0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
       0.00978, 0.02360, 0.00150, 0.01974, 0.00074 );

   function Get_Frequency(S: String) return Frequency_Array is
      Result: Frequency_Array := (others => 0.0);
      Offset: Float := 1.0/Float(S'Length);
   begin
      for I in S'Range loop
         if S(I) in Letter then
            Result(S(I)) := Result(S(I)) + Offset;
         end if;
      end loop;
      return Result;
   end Get_Frequency;

   function Remove_Whitespace(S: String) return String is
   begin
      if S="" then
         return "";
      elsif S(S'First) in Letter then
         return S(S'First) & Remove_Whitespace(S(S'First+1 .. S'Last));
      else
         return Remove_Whitespace(S(S'First+1 .. S'Last));
      end if;
   end Remove_Whitespace;

   function Distance(A, B: Frequency_Array;
                     Offset: Character := 'A') return Float is
      Result: Float := 0.0;
      Diff: Float;
   begin
      for C in A'Range loop
         Diff := A(C+Offset) - B(C);
         Result := Result + (Diff * Diff);
      end loop;
      return Result;
   end Distance;

   function Find_Key(Cryptogram: String; Key_Length: Positive) return String is

      function Find_Caesar_Key(S: String) return Letter is
         Frequency: Frequency_Array := Get_Frequency(S);
         Candidate: Letter := 'A'; -- a fake candidate
         Candidate_Dist : Float := Distance(Frequency, English, 'A');
         New_Dist: Float;

      begin

         for L in Letter range 'B' .. 'Z' loop
            New_Dist := Distance(Frequency, English, L);
            if New_Dist <= Candidate_Dist then
               Candidate_Dist := New_Dist;
               Candidate      := L;
            end if;
         end loop;
         return Candidate;
      end Find_Caesar_Key;

      function Get_Slide(S: String; Step: Positive) return String is
      begin
         if S'Length= 0 then
            return "";
         else
            return S(S'First) & Get_Slide(S(S'First+Step .. S'Last), Step);
         end if;
      end Get_Slide;

      Key: String(1 .. Key_Length);

      S: String renames Cryptogram;

   begin
      for I in Key'Range loop
         Key(I) := Find_Caesar_Key(Get_Slide(S(S'First+I-1 .. S'Last),
                                             Key_Length));
      end loop;
      return Key;
   end Find_Key;

   function Key_Char(Key: String; Index: Positive) return Letter is
   begin
      if Index > Key'Last then
         return Key_Char(Key, Index-Key'Last);
      else
         return Key(Index);
      end if;
   end Key_Char;

   Ciphertext: String := Remove_Whitespace(
     "MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH" &
     "VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD" &
     "ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS" &
     "FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG" &
     "ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ" &
     "ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS" &
     "JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT" &
     "LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST" &
     "MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH" &
     "QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV" &
     "RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW" &
     "TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO" &
     "SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR" &
     "ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX" &
     "BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB" &
     "BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA" &
     "FWAML ZZRXJ EKAHV FASMU LVVUT TGK");

   Best_Plain: String := Ciphertext;
   Best_Dist:  Float := Distance(English, Get_Frequency(Best_Plain));
   Best_Key:   String := Ciphertext;
   Best_Key_L: Natural := 0;

begin -- Vignere_Cryptanalysis
   for I in 1 .. Ciphertext'Length/10 loop
      declare
         Key: String(1 .. I) := Find_Key(Ciphertext, I);
         Plaintext: String(Ciphertext'Range);
      begin
         for I in Ciphertext'Range loop
            Plaintext(I) := Ciphertext(I) - Key_Char(Key, I);
         end loop;
         if Distance(English, Get_Frequency(Plaintext)) < Best_Dist then
            Best_Plain := Plaintext;
            Best_Dist  := Distance(English, Get_Frequency(Plaintext));
            Best_Key(1 .. I) := Key;
            Best_Key_L := I;
            if Best_dist < 0.01 then
               declare
                  use Ada.Text_IO;
               begin
                  Put_Line("Key       =" & Best_Key(1 .. Best_Key_L));
                  Put_Line("Distance = " & Float'Image(Best_Dist));
                  New_Line;
                  Put_Line("Plaintext =");
                  Put_Line(Best_Plain);
                  New_Line; New_Line;
               end;
            end if;
         end if;
      end;
   end loop;
end Vignere_Cryptanalysis;
```



## C

This finds the right key (I think, I didn't try to decode it after getting the key).  The program is not fully auto, but by its output, the result is pretty obvious.

```C>#include <stdio.h

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

const char *encoded =
    "MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH"
    "VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD"
    "ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS"
    "FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG"
    "ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ"
    "ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS"
    "JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT"
    "LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST"
    "MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH"
    "QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV"
    "RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW"
    "TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO"
    "SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR"
    "ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX"
    "BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB"
    "BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA"
    "FWAML ZZRXJ EKAHV FASMU LVVUT TGK";

const double freq[] = {
    0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
    0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
    0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
    0.00978, 0.02360, 0.00150, 0.01974, 0.00074
};

int best_match(const double *a, const double *b) {
    double sum = 0, fit, d, best_fit = 1e100;
    int i, rotate, best_rotate = 0;
    for (i = 0; i < 26; i++)
        sum += a[i];
    for (rotate = 0; rotate < 26; rotate++) {
        fit = 0;
        for (i = 0; i < 26; i++) {
            d = a[(i + rotate) % 26] / sum - b[i];
            fit += d * d / b[i];
        }

        if (fit < best_fit) {
            best_fit = fit;
            best_rotate = rotate;
        }
    }

    return best_rotate;
}

double freq_every_nth(const int *msg, int len, int interval, char *key) {
    double sum, d, ret;
    double out[26], accu[26] = {0};
    int i, j, rot;

    for (j = 0; j < interval; j++) {
        for (i = 0; i < 26; i++)
            out[i] = 0;
        for (i = j; i < len; i += interval)
            out[msg[i]]++;
        key[j] = rot = best_match(out, freq);
        key[j] += 'A';
        for (i = 0; i < 26; i++)
            accu[i] += out[(i + rot) % 26];
    }

    for (i = 0, sum = 0; i < 26; i++)
        sum += accu[i];

    for (i = 0, ret = 0; i < 26; i++) {
        d = accu[i] / sum - freq[i];
        ret += d * d / freq[i];
    }

    key[interval] = '\0';
    return ret;
}

int main() {
    int txt[strlen(encoded)];
    int len = 0, j;
    char key[100];
    double fit, best_fit = 1e100;

    for (j = 0; encoded[j] != '\0'; j++)
        if (isupper(encoded[j]))
            txt[len++] = encoded[j] - 'A';

    for (j = 1; j < 30; j++) {
        fit = freq_every_nth(txt, len, j, key);
        printf("%f, key length: %2d, %s", fit, j, key);
        if (fit < best_fit) {
            best_fit = fit;
            printf(" <--- best so far");
        }
        printf("\n");
    }

    return 0;
}
```



## C++

Not guaranteed to give a 100% correct answer, but it works here. Requires C++0x.


```cpp>#include <iostream

#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include <array>
using namespace std;

typedef array<pair<char, double>, 26> FreqArray;

class VigenereAnalyser 
{
private:
  array<double, 26> targets;
  array<double, 26> sortedTargets;
  FreqArray freq;

  // Update the freqs array
  FreqArray& frequency(const string& input) 
  {
    for (char c = 'A'; c <= 'Z'; ++c)
      freq[c - 'A'] = make_pair(c, 0);

    for (size_t i = 0; i < input.size(); ++i)
      freq[input[i] - 'A'].second++;

    return freq;
  }

  double correlation(const string& input) 
  {
    double result = 0.0;
    frequency(input);

    sort(freq.begin(), freq.end(), [](pair<char, double> u, pair<char, double> v)->bool
      { return u.second < v.second; });

    for (size_t i = 0; i < 26; ++i)
      result += freq[i].second * sortedTargets[i];

    return result;
  }

public:
  VigenereAnalyser(const array<double, 26>& targetFreqs) 
  {
    targets = targetFreqs;
    sortedTargets = targets;
    sort(sortedTargets.begin(), sortedTargets.end());
  }

  pair<string, string> analyze(string input) 
  {
    string cleaned;
    for (size_t i = 0; i < input.size(); ++i) 
    {
      if (input[i] >= 'A' && input[i] <= 'Z')
        cleaned += input[i];
      else if (input[i] >= 'a' && input[i] <= 'z')
        cleaned += input[i] + 'A' - 'a';
    }

    size_t bestLength = 0;
    double bestCorr = -100.0;

    // Assume that if there are less than 20 characters
    // per column, the key's too long to guess
    for (size_t i = 2; i < cleaned.size() / 20; ++i) 
    {
      vector<string> pieces(i);
      for (size_t j = 0; j < cleaned.size(); ++j)
        pieces[j % i] += cleaned[j];

      // The correlation increases artificially for smaller
      // pieces/longer keys, so weigh against them a little
      double corr = -0.5*i;
      for (size_t j = 0; j < i; ++j)
        corr += correlation(pieces[j]);

      if (corr > bestCorr) 
      {
        bestLength = i;
        bestCorr = corr;
      }
    }

    if (bestLength == 0)
      return make_pair("Text is too short to analyze", "");

    vector<string> pieces(bestLength);
    for (size_t i = 0; i < cleaned.size(); ++i)
      pieces[i % bestLength] += cleaned[i];

    vector<FreqArray> freqs;
    for (size_t i = 0; i < bestLength; ++i)
      freqs.push_back(frequency(pieces[i]));

    string key = "";
    for (size_t i = 0; i < bestLength; ++i) 
    {
      sort(freqs[i].begin(), freqs[i].end(), [](pair<char, double> u, pair<char, double> v)->bool
        { return u.second > v.second; });

      size_t m = 0;
      double mCorr = 0.0;
      for (size_t j = 0; j < 26; ++j) 
      {
        double corr = 0.0;
        char c = 'A' + j;
        for (size_t k = 0; k < 26; ++k) 
        {
          int d = (freqs[i][k].first - c + 26) % 26;
          corr += freqs[i][k].second * targets[d];
        }

        if (corr > mCorr) 
        {
          m = j;
          mCorr = corr;
        }
      }

      key += m + 'A';
    }

    string result = "";
    for (size_t i = 0; i < cleaned.size(); ++i)
      result += (cleaned[i] - key[i % key.length()] + 26) % 26 + 'A';

    return make_pair(result, key);
  }
};

int main() 
{
  string input =
    "MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH"
    "VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD"
    "ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS"
    "FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG"
    "ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ"
    "ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS"
    "JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT"
    "LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST"
    "MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH"
    "QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV"
    "RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW"
    "TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO"
    "SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR"
    "ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX"
    "BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB"
    "BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA"
    "FWAML ZZRXJ EKAHV FASMU LVVUT TGK";

  array<double, 26> english = {
    0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228,
    0.02015, 0.06094, 0.06966, 0.00153, 0.00772, 0.04025,
    0.02406, 0.06749, 0.07507, 0.01929, 0.00095, 0.05987,
    0.06327, 0.09056, 0.02758, 0.00978, 0.02360, 0.00150,
    0.01974, 0.00074};

  VigenereAnalyser va(english);
  pair<string, string> output = va.analyze(input);

  cout << "Key: " << output.second << endl << endl;
  cout << "Text: " << output.first << endl;
}
```



## D

{{trans|C++}}

```d
import std.stdio, std.algorithm, std.typecons, std.string,
       std.array, std.numeric, std.ascii;

string[2] vigenereDecrypt(in double[] targetFreqs, in string input) {
    enum nAlpha = std.ascii.uppercase.length;

    static double correlation(in string txt, in double[] sTargets)
    pure nothrow /*@safe*/ @nogc {
        uint[nAlpha] charCounts = 0;
        foreach (immutable c; txt)
            charCounts[c - 'A']++;
        return charCounts[].sort().release.dotProduct(sTargets);
    }

    static frequency(in string txt) pure nothrow @safe {
        auto freqs = new Tuple!(char,"c", uint,"d")[nAlpha];
        foreach (immutable i, immutable c; std.ascii.uppercase)
            freqs[i] = tuple(c, 0);
        foreach (immutable c; txt)
            freqs[c - 'A'].d++;
        return freqs;
    }

    static string[2] decode(in string cleaned, in string key)
    pure nothrow @safe {
        assert(!key.empty);
        string decoded;
        foreach (immutable i, immutable c; cleaned)
            decoded ~= (c - key[i % $] + nAlpha) % nAlpha + 'A';
        return [key, decoded];
    }

    static size_t findBestLength(in string cleaned,
                                 in double[] sTargets)
    pure nothrow /*@safe*/ {
        size_t bestLength;
        double bestCorr = -100.0;

        // Assume that if there are less than 20 characters
        // per column, the key's too long to guess
        foreach (immutable i; 2 .. cleaned.length / 20) {
            auto pieces = new Appender!string[i];
            foreach (immutable j, immutable c; cleaned)
                pieces[j % i] ~= c;

            // The correlation seems to increase for smaller
            // pieces/longer keys, so weigh against them a little
            double corr = -0.5 * i;
            foreach (const p; pieces)
                corr += correlation(p.data, sTargets);

            if (corr > bestCorr) {
                bestLength = i;
                bestCorr = corr;
            }
        }

        return bestLength;
    }

    static string findKey(in string cleaned, in size_t bestLength,
                          in double[] targetFreqs) pure nothrow @safe {
        auto pieces = new string[bestLength];
        foreach (immutable i, immutable c; cleaned)
            pieces[i % bestLength] ~= c;

        string key;
        foreach (fr; pieces.map!frequency) {
            fr.sort!q{ a.d > b.d };

            size_t m;
            double maxCorr = 0.0;
            foreach (immutable j, immutable c; uppercase) {
                double corr = 0.0;
                foreach (immutable frc; fr) {
                    immutable di = (frc.c - c + nAlpha) % nAlpha;
                    corr += frc.d * targetFreqs[di];
                }

                if (corr > maxCorr) {
                    m = j;
                    maxCorr = corr;
                }
            }

            key ~= m + 'A';
        }

        return key;
    }

    immutable cleaned = input.toUpper.removechars("^A-Z");

    //immutable sortedTargets = targetFreqs.sorted;
    immutable sortedTargets = targetFreqs.dup.sort().release.idup;

    immutable bestLength = findBestLength(cleaned, sortedTargets);
    if (bestLength == 0)
        throw new Exception("Text is too short to analyze.");

    immutable string key = findKey(cleaned, bestLength, targetFreqs);
    return decode(cleaned, key);
}


void main() {
    immutable encoded = "MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG
JSPXY ALUYM NSMYH VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF
WHTCQ KMLRD ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA
LWQIS FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG
ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ ILOVV
RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS JLAKI FHXUF
XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT LPRWM JAZPK LQUZA
ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST MTEOE PAPJH SMFNB YVQUZ
AALGA YDNMP AQOWT UHDBV TSMUE UIMVH QGVRW AEFSP EMPVE PKXZY WLKJA
GWALT VYYOB YIXOK IHPDS EVLEV RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY
IMAPX UOISK PVAGN MZHPW TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV
YOVDJ SOLXG TGRVO SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV
GJOKM SIFPR ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO
ZQDLX BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB
BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA FWAML
ZZRXJ EKAHV FASMU LVVUT TGK";

    immutable englishFrequences = [0.08167, 0.01492, 0.02782, 0.04253,
        0.12702, 0.02228, 0.02015, 0.06094, 0.06966, 0.00153, 0.00772,
        0.04025, 0.02406, 0.06749, 0.07507, 0.01929, 0.00095, 0.05987,
        0.06327, 0.09056, 0.02758, 0.00978, 0.02360, 0.00150, 0.01974,
        0.00074];

    immutable key_dec = vigenereDecrypt(englishFrequences, encoded);
    writefln("Key: %s\n\nText: %s", key_dec[0], key_dec[1]);
}
```

{{out|Output (cut)}}

```txt
Key: THECHESHIRECAT

Text: THISWASTHEPOEMTHATALICEREADJABBERWOCKYTWASBRILLIGANDTHESLITHY...
```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "strings"
)

var encoded = 
    "MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH" +
    "VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD" +
    "ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS" +
    "FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG" +
    "ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ" +
    "ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS" +
    "JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT" +
    "LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST" +
    "MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH" +
    "QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV" +
    "RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW" +
    "TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO" +
    "SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR" +
    "ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX" +
    "BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB" +
    "BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA" +
    "FWAML ZZRXJ EKAHV FASMU LVVUT TGK"

var freq = [26]float64{
    0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
    0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
    0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
    0.00978, 0.02360, 0.00150, 0.01974, 0.00074,
}

func sum(a []float64) (sum float64) {
    for _, f := range a {
        sum += f
    }
    return
}

func bestMatch(a []float64) int {
    sum := sum(a)
    bestFit, bestRotate := 1e100, 0
    for rotate := 0; rotate < 26; rotate++ {
        fit := 0.0
        for i := 0; i < 26; i++ {
            d := a[(i+rotate)%26]/sum - freq[i]
            fit += d * d / freq[i]
        }
        if fit < bestFit {
            bestFit, bestRotate = fit, rotate
        }
    }
    return bestRotate
}

func freqEveryNth(msg []int, key []byte) float64 {
    l := len(msg)
    interval := len(key)
    out := make([]float64, 26)
    accu := make([]float64, 26)
    for j := 0; j < interval; j++ {
        for k := 0; k < 26; k++ {
            out[k] = 0.0
        }
        for i := j; i < l; i += interval {
            out[msg[i]]++
        }
        rot := bestMatch(out)
        key[j] = byte(rot + 65)
        for i := 0; i < 26; i++ {
            accu[i] += out[(i+rot)%26]
        }
    }
    sum := sum(accu)
    ret := 0.0
    for i := 0; i < 26; i++ {
        d := accu[i]/sum - freq[i]
        ret += d * d / freq[i]
    }
    return ret
}

func decrypt(text, key string) string {
    var sb strings.Builder
    ki := 0
    for _, c := range text {
        if c < 'A' || c > 'Z' {
            continue
        }
        ci := (c - rune(key[ki]) + 26) % 26
        sb.WriteRune(ci + 65)
        ki = (ki + 1) % len(key)
    }
    return sb.String()
}

func main() {
    enc := strings.Replace(encoded, " ", "", -1)
    txt := make([]int, len(enc))
    for i := 0; i < len(txt); i++ {
        txt[i] = int(enc[i] - 'A')
    }
    bestFit, bestKey := 1e100, ""
    fmt.Println("  Fit     Length   Key")
    for j := 1; j <= 26; j++ {
        key := make([]byte, j)
        fit := freqEveryNth(txt, key)
        sKey := string(key)
        fmt.Printf("%f    %2d     %s", fit, j, sKey)
        if fit < bestFit {
            bestFit, bestKey = fit, sKey
            fmt.Print(" <--- best so far")
        }
        fmt.Println()
    }
    fmt.Println("\nBest key :", bestKey)
    fmt.Printf("\nDecrypted text:\n%s\n", decrypt(enc, bestKey))
}
```


{{out}}
Note: carriage returns inserted into decrypted text after every 80 characters to make it more readable.

```txt

  Fit     Length   Key
2.984348     1     E <--- best so far
2.483684     2     EC <--- best so far
2.642487     3     TEE
1.976651     4     THEC <--- best so far
2.356881     5     EEEPU
2.203129     6     TCECEC
1.051163     7     THECSAS <--- best so far
1.645763     8     TJQGAHET
2.001380     9     VEIZSEGNT
1.824476    10     ECEGAWQTDS
1.623083    11     TNLUSRXPTAJ
1.253527    12     XLECTHQGTHEC
1.399037    13     LJJTDGFNOTENR
0.152370    14     THECHESHIRECAT <--- best so far
1.533951    15     JNTOOEEXFTGQTNH
1.068182    16     TJTSAEETEXHPXHNE
1.034093    17     AZRAXUHEJLREEXIEE
1.443345    18     VNIZQPALEPTSXSEXUC
1.090977    19     FUCAITCSLVTEZDUDEHS
0.979868    20     EQXGAHWTTQECEWUGXHPI
0.789410    21     HVRCSAFTHEBDLSTAERSES
0.881380    22     TVIJTCIGKAQPELECRXPTNC
0.952456    23     KKEQXGPWTCQEELIEHXUWASV
0.715968    24     ELAIXHQTTIEDXJETTNTGAEPC
0.891258    25     OTJUUEGERDNQTUQEAGWUTIEOA
0.852784    26     IGITEGECAGAVUNLJAHASAVTETW

Best key : THECHESHIRECAT

Decrypted text:
THISWASTHEPOEMTHATALICEREADJABBERWOCKYTWASBRILLIGANDTHESLITHYTOVESDIDGYREANDGIMB
LEINTHEWABEALLMIMSYWERETHEBOROGOVESANDTHEMOMERATHSOUTGRABEBEWARETHEJABBERWOCKMYS
ONTHEJAWSTHATBITETHECLAWSTHATCATCHBEWARETHEJUBJUBBIRDANDSHUNTHEFRUMIOUSBANDERSNA
TCHHETOOKHISVORPALSWORDINHANDLONGTIMETHEMANXOMEFOEHESOUGHTSORESTEDHEBYTHETUMTUMT
REEANDSTOODAWHILEINTHOUGHTANDASINUFFISHTHOUGHTHESTOODTHEJABBERWOCKWITHEYESOFFLAM
ECAMEWHIFFLINGTHROUGHTHETULGEYWOODANDBURBLEDASITCAMEONETWOONETWOANDTHROUGHANDTHR
OUGHTHEVORPALBLADEWENTSNICKERSNACKHELEFTITDEADANDWITHITSHEADHEWENTGALUMPHINGBACK
ANDHASTTHOUSLAINTHEJABBERWOCKCOMETOMYARMSMYBEAMISHBOYOFRABJOUSDAYCALLOOHCALLAYHE
CHORTLEDINHISJOYTWASBRILLIGANDTHESLITHYTOVESDIDGYREANDGIMBLEINTHEWABEALLMIMSYWER
ETHEBOROGOVESANDTHEMOMERATHSOUTGRABEITSEEMSVERYPRETTYSHESAIDWHENSHEHADFINISHEDIT
BUTITSRATHERHARDTOUNDERSTAND

```



## Haskell


```haskell
{-# LANGUAGE TupleSections #-}
import Data.List(transpose, nub, sort, maximumBy)
import Data.Ord (comparing)
import Data.Char (ord)
import Data.Map (Map, fromListWith, toList, findWithDefault)

average :: Fractional a => [a] -> a
average as = sum as / fromIntegral (length as)

-- Create a map from each entry in list to the number of occurrences of
-- that entry in the list.
countEntries :: Ord a => [a] -> Map a Int
countEntries = fromListWith (+) . fmap (,1)

-- Break a string up into substrings of n chars.
breakup :: Int -> [a] -> [[a]]
breakup _ [] = []
breakup n as = 
    let (h, r) = splitAt n as
    in h:breakup n r

-- Dole out elements of a string over a n element distribution.
distribute :: [a] -> Int -> [[a]]
distribute as n = transpose $ breakup n as

-- The probability that members of a pair of characters taken randomly
-- from a given string are equal.
coincidence :: (Ord a, Fractional b) => [a] -> b
coincidence str = 
    let charCounts = snd <$> toList (countEntries str)
        strln = length str
        d = fromIntegral $ strln * (strln - 1)
        n = fromIntegral $ sum $ fmap (\cc -> cc * (cc-1)) charCounts
    in n / d

-- Use the average probablity of coincidence for all the members of
-- a distribution to rate the distribution - the higher the better.
-- The correlation increases artificially for smaller
-- pieces/longer keys, so weigh against them a little
rate :: (Ord a, Fractional b) => [[a]] -> b
rate d =  average (fmap coincidence d) - fromIntegral (length d) / 3000.0 

-- Multiply elements of lists together and add up the results.
dot :: Num a => [a] -> [a] -> a
dot v0 v1 = sum $ zipWith (*) v0 v1

-- Given two lists of floats, rotate one of them by the number of
-- characters indicated by letter and then 'dot' them together.
rotateAndDot :: Num a => [a] -> [a] -> Char -> a
rotateAndDot v0 v1 letter = dot v0 (drop (ord letter - ord 'A') (cycle v1))  

-- Find decoding offset that results in best match 
-- between actual char frequencies and expected frequencies.
getKeyChar :: RealFrac a => [a] -> String -> Char
getKeyChar expected sample =
    let charCounts = countEntries sample
        countInSample c = findWithDefault 0 c charCounts
        actual = fmap (fromIntegral . countInSample) ['A'..'Z']
    in maximumBy (comparing $ rotateAndDot expected actual) ['A'..'Z']

main = do
    let cr = filter (/=' ') crypt
        -- Assume that if there are less than 20 characters
        -- per column, the key's too long to guess
        distributions = fmap (distribute cr) [1..length cr `div` 20]
        bestDistribution = maximumBy (comparing rate) distributions
        key = fmap (getKeyChar englishFrequencies) bestDistribution
        alphaSum a b = ['A'..'Z'] !! ((ord b - ord a) `mod` 26)
    mapM_ putStrLn ["Key: " ++ key, "Decrypted Text: " ++ zipWith alphaSum (cycle key) cr]

englishFrequencies = 
    [ 0.08167, 0.01492, 0.02782, 0.04253, 
      0.12702, 0.02228, 0.02015, 0.06094, 
      0.06966, 0.00153, 0.00772, 0.04025, 
      0.02406, 0.06749, 0.07507, 0.01929, 
      0.00095, 0.05987, 0.06327, 0.09056, 
      0.02758, 0.00978, 0.02360, 0.00150, 
      0.01974, 0.00074 ] 

crypt = "\
    \MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH\
    \VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD\
    \ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS\
    \FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG\
    \ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ\
    \ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS\
    \JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT\
    \LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST\
    \MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH\
    \QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV\
    \RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW\
    \TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO\
    \SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR\
    \ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX\
    \BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB\
    \BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA\
    \FWAML ZZRXJ EKAHV FASMU LVVUT TGK\
    \"
```

{{out}}
<pre style="font-size:80%">
Key: THECHESHIRECAT
Decrypted Text: THISWASTHEPOEMTHATALICEREADJABBERWOCKYTWASBRILLIGANDTHESLITHYTOVESDIDGYREANDGIMBLEINTHEWABEALLMIMSYWERETHEBOROGOVESANDTHEMOMERATHSOUTGRABEBEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCHBEWARETHEJUBJUBBIRDANDSHUNTHEFRUMIOUSBANDERSNATCHHETOOKHISVORPALSWORDINHANDLONGTIMETHEMANXOMEFOEHESOUGHTSORESTEDHEBYTHETUMTUMTREEANDSTOODAWHILEINTHOUGHTANDASINUFFISHTHOUGHTHESTOODTHEJABBERWOCKWITHEYESOFFLAMECAMEWHIFFLINGTHROUGHTHETULGEYWOODANDBURBLEDASITCAMEONETWOONETWOANDTHROUGHANDTHROUGHTHEVORPALBLADEWENTSNICKERSNACKHELEFTITDEADANDWITHITSHEADHEWENTGALUMPHINGBACKANDHASTTHOUSLAINTHEJABBERWOCKCOMETOMYARMSMYBEAMISHBOYOFRABJOUSDAYCALLOOHCALLAYHECHORTLEDINHISJOYTWASBRILLIGANDTHESLITHYTOVESDIDGYREANDGIMBLEINTHEWABEALLMIMSYWERETHEBOROGOVESANDTHEMOMERATHSOUTGRABEITSEEMSVERYPRETTYSHESAIDWHENSHEHADFINISHEDITBUTITSRATHERHARDTOUNDERSTAND

```



## Java

{{trans|C}}


```Java
public class Vig{
static String encodedMessage =
    "MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA FWAML ZZRXJ EKAHV FASMU LVVUT TGK";
 
final static double freq[] = {
    0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
    0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
    0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
    0.00978, 0.02360, 0.00150, 0.01974, 0.00074
};
 

public static void main(String[] args) {
    int lenghtOfEncodedMessage = encodedMessage.length();
    char[] encoded = new char [lenghtOfEncodedMessage] ;
    char[] key =  new char [lenghtOfEncodedMessage] ;

    encodedMessage.getChars(0, lenghtOfEncodedMessage, encoded, 0);
    int txt[] = new int[lenghtOfEncodedMessage];
    int len = 0, j;

    double fit, best_fit = 1e100;
 
    for (j = 0; j < lenghtOfEncodedMessage; j++)
        if (Character.isUpperCase(encoded[j]))
            txt[len++] = encoded[j] - 'A';
 
    for (j = 1; j < 30; j++) {
        fit = freq_every_nth(txt, len, j, key);
        System.out.printf("%f, key length: %2d ", fit, j);
            System.out.print(key);
        if (fit < best_fit) {
            best_fit = fit;
            System.out.print(" <--- best so far");
        }
        System.out.print("\n");

    }
}


    static String decrypt(String text, final String key) {
        String res = "";
        text = text.toUpperCase();
        for (int i = 0, j = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (c < 'A' || c > 'Z') continue;
            res += (char)((c - key.charAt(j) + 26) % 26 + 'A');
            j = ++j % key.length();
        }
        return res;
    }

static int best_match(final double []a, final double []b) {
    double sum = 0, fit, d, best_fit = 1e100;
    int i, rotate, best_rotate = 0;
    for (i = 0; i < 26; i++)
        sum += a[i];
    for (rotate = 0; rotate < 26; rotate++) {
        fit = 0;
        for (i = 0; i < 26; i++) {
            d = a[(i + rotate) % 26] / sum - b[i];
            fit += d * d / b[i];
        }
 
        if (fit < best_fit) {
            best_fit = fit;
            best_rotate = rotate;
        }
    }
 
    return best_rotate;
}
 
static double freq_every_nth(final int []msg, int len, int interval, char[] key) {
    double sum, d, ret;
    double  [] accu = new double [26];
    double  [] out = new double [26];
    int i, j, rot;
 
    for (j = 0; j < interval; j++) {
        for (i = 0; i < 26; i++)
            out[i] = 0;
        for (i = j; i < len; i += interval)
            out[msg[i]]++;
	rot = best_match(out, freq);
	try{
            key[j] = (char)(rot + 'A');
	} catch (Exception e) {
		System.out.print(e.getMessage());
	}
        for (i = 0; i < 26; i++)
            accu[i] += out[(i + rot) % 26];
    }
 
    for (i = 0, sum = 0; i < 26; i++)
        sum += accu[i];
 
    for (i = 0, ret = 0; i < 26; i++) {
        d = accu[i] / sum - freq[i];
        ret += d * d / freq[i];
    }
 
    key[interval] = '\0';
    return ret;
}
 
}

```



## Julia



```Julia
# ciphertext block {{{1
const ciphertext = filter(isalpha, """
MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH
VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD
ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS
FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG
ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ
ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS
JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT
LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST
MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH
QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV
RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW
TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO
SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR
ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX
BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB
BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA
FWAML ZZRXJ EKAHV FASMU LVVUT TGK
""")
# }}}

# character frequencies {{{1
const letters = Dict{Char, Float32}(
    'E' => 12.702,
    'T' => 9.056,
    'A' => 8.167,
    'O' => 7.507,
    'I' => 6.966,
    'N' => 6.749,
    'S' => 6.327,
    'H' => 6.094,
    'R' => 5.987,
    'D' => 4.253,
    'L' => 4.025,
    'C' => 2.782,
    'U' => 2.758,
    'M' => 2.406,
    'W' => 2.361,
    'F' => 2.228,
    'G' => 2.015,
    'Y' => 1.974,
    'P' => 1.929,
    'B' => 1.492,
    'V' => 0.978,
    'K' => 0.772,
    'J' => 0.153,
    'X' => 0.150,
    'Q' => 0.095,
    'Z' => 0.074)
const digraphs = Dict{AbstractString, Float32}(
    "TH" => 15.2,
    "HE" => 12.8,
    "IN" => 9.4,
    "ER" => 9.4,
    "AN" => 8.2,
    "RE" => 6.8,
    "ND" => 6.3,
    "AT" => 5.9,
    "ON" => 5.7,
    "NT" => 5.6,
    "HA" => 5.6,
    "ES" => 5.6,
    "ST" => 5.5,
    "EN" => 5.5,
    "ED" => 5.3,
    "TO" => 5.2,
    "IT" => 5.0,
    "OU" => 5.0,
    "EA" => 4.7,
    "HI" => 4.6,
    "IS" => 4.6,
    "OR" => 4.3,
    "TI" => 3.4,
    "AS" => 3.3,
    "TE" => 2.7,
    "ET" => 1.9,
    "NG" => 1.8,
    "OF" => 1.6,
    "AL" => 0.9,
    "DE" => 0.9,
    "SE" => 0.8,
    "LE" => 0.8,
    "SA" => 0.6,
    "SI" => 0.5,
    "AR" => 0.4,
    "VE" => 0.4,
    "RA" => 0.4,
    "LD" => 0.2,
    "UR" => 0.2)
const trigraphs = Dict{AbstractString, Float32}(
    "THE" => 18.1,
    "AND" => 7.3,
    "ING" => 7.2,
    "ION" => 4.2,
    "ENT" => 4.2,
    "HER" => 3.6,
    "FOR" => 3.4,
    "THA" => 3.3,
    "NTH" => 3.3,
    "INT" => 3.2,
    "TIO" => 3.1,
    "ERE" => 3.1,
    "TER" => 3.0,
    "EST" => 2.8,
    "ERS" => 2.8,
    "HAT" => 2.6,
    "ATI" => 2.6,
    "ATE" => 2.5,
    "ALL" => 2.5,
    "VER" => 2.4,
    "HIS" => 2.4,
    "HES" => 2.4,
    "ETH" => 2.4,
    "OFT" => 2.2,
    "STH" => 2.1,
    "RES" => 2.1,
    "OTH" => 2.1,
    "ITH" => 2.1,
    "FTH" => 2.1,
    "ONT" => 2.0)
# 1}}}

function decrypt(enc::ASCIIString, key::ASCIIString)
    const enclen = length(enc)
    const keylen = length(key)

    if keylen < enclen
        key = (key^(div(enclen - keylen, keylen) + 2))[1:enclen]
    end

    msg = Array(Char, enclen)

    for i=1:enclen
        msg[i] = Char((Int(enc[i]) - Int(key[i]) + 26) % 26 + 65)
    end

    msg::Array{Char, 1}
end

function cryptanalyze(enc::ASCIIString; maxkeylen::Integer = 20)
    const enclen = length(enc)
    maxkey = ""
    maxdec = ""
    maxscore = 0.0

    for keylen=1:maxkeylen
        key = Array(Char, keylen)
        idx = filter(x -> x % keylen == 0, 1:enclen) - keylen + 1

        for i=1:keylen
            maxsubscore = 0.0

            for j='A':'Z'
                subscore = 0.0

                for k in decrypt(enc[idx], ascii(string(j)))
                    subscore += get(letters, k, 0.0)
                end

                if subscore > maxsubscore
                    maxsubscore = subscore
                    key[i] = j
                end
            end

            idx += 1
        end

        key = join(key)
        const dec = decrypt(enc, key)
        score = 0.0

        for i in dec
            score += get(letters, i, 0.0)
        end

        for i=1:enclen - 2
            const digraph = string(dec[i], dec[i + 1])
            const trigraph = string(dec[i], dec[i + 1], dec[i + 2])

            if haskey(digraphs, digraph)
                score += 2 * get(digraphs, digraph, 0.0)
            end

            if haskey(trigraphs, trigraph)
                score += 3 * get(trigraphs, trigraph, 0.0)
            end
        end

        if score > maxscore
            maxscore = score
            maxkey = key
            maxdec = dec
        end
    end
    
    (maxkey, join(maxdec))::Tuple{ASCIIString, ASCIIString}
end

key, dec = cryptanalyze(ciphertext)
println("key: ", key, "\n\n", dec)

# post-compilation profiling run
gc()
t = @elapsed cryptanalyze(ciphertext)
println("\nelapsed time: ", t, " seconds")
```


{{out}}


```txt
key: THECHESHIRECAT

THISWASTHEPOEMTHATALICEREADJABBERWOCKYTWASBRILLIGANDTHESLITHY...

elapsed time: 0.042894211 seconds
```



## Kotlin

{{trans|C}}
This is a reasonably faithful translation of the C entry though I've restricted the key lengths examined to 26 to automatically produce the correct key and hence decrypted text. This is because the C entry examines key lengths up to 29 and a value of 28 gives a slightly better fit even though the key produced (THECHESCIRECATTHECHESHIRECAT) and resulting text don't make as much sense and so would be rejected if one were examining the candidate keys manually.

```scala
// version 1.1.3

val encoded = 
    "MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH" +
    "VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD" +
    "ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS" +
    "FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG" +
    "ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ" +
    "ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS" +
    "JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT" +
    "LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST" +
    "MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH" +
    "QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV" +
    "RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW" +
    "TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO" +
    "SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR" +
    "ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX" +
    "BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB" +
    "BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA" +
    "FWAML ZZRXJ EKAHV FASMU LVVUT TGK"

val freq = doubleArrayOf(
    0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
    0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
    0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
    0.00978, 0.02360, 0.00150, 0.01974, 0.00074
)

fun bestMatch(a: DoubleArray): Int {
    val sum = a.sum()
    var bestFit = 1e100
    var bestRotate = 0
    for (rotate in 0..25) {
        var fit = 0.0
        for (i in 0..25) {
            val d = a[(i + rotate) % 26] / sum - freq[i]
            fit += d * d / freq[i]
        } 
        if (fit < bestFit) {
            bestFit = fit
            bestRotate = rotate
        }
    }
    return bestRotate
}

fun freqEveryNth(msg: IntArray, key: CharArray): Double {
    val len = msg.size
    val interval = key.size
    val out = DoubleArray(26)
    val accu = DoubleArray(26)
    for (j in 0 until interval) {
        out.fill(0.0)
        for (i in j until len step interval) out[msg[i]]++
        val rot = bestMatch(out)
        key[j] = (rot + 65).toChar()
        for (i in 0..25) accu[i] += out[(i + rot) % 26]
    }
    val sum = accu.sum()
    var ret = 0.0
    for (i in 0..25) {
        val d = accu[i] / sum - freq[i]
        ret += d * d / freq[i]
    }
    return ret
}

fun decrypt(text: String, key: String): String {
    val sb = StringBuilder()
    var ki = 0
    for (c in text) {
        if (c !in 'A'..'Z') continue
        val ci = (c.toInt() - key[ki].toInt() +  26) % 26
        sb.append((ci + 65).toChar())
        ki = (ki + 1) % key.length
    }
    return sb.toString()
}

fun main(args: Array<String>) {
    val enc = encoded.replace(" ", "")
    val txt = IntArray(enc.length) { enc[it] - 'A' }
    var bestFit = 1e100
    var bestKey = ""
    val f = "%f    %2d     %s"
    println("  Fit     Length   Key")
    for (j in 1..26) {
        val key = CharArray(j)
        val fit = freqEveryNth(txt, key)
        val sKey = key.joinToString("")
        print(f.format(fit, j, sKey))
        if (fit < bestFit) {
           bestFit = fit
           bestKey = sKey
           print(" <--- best so far")
        }
        println()
    }
    println()
    println("Best key : $bestKey") 
    println("\nDecrypted text:\n${decrypt(enc, bestKey)}")
}
```


{{out}}

```txt

  Fit     Length   Key
2.984348     1     E <--- best so far
2.483684     2     EC <--- best so far
2.642487     3     TEE
1.976651     4     THEC <--- best so far
2.356881     5     EEEPU
2.203129     6     TCECEC
1.051163     7     THECSAS <--- best so far
1.645763     8     TJQGAHET
2.001380     9     VEIZSEGNT
1.824476    10     ECEGAWQTDS
1.623083    11     TNLUSRXPTAJ
1.253527    12     XLECTHQGTHEC
1.399037    13     LJJTDGFNOTENR
0.152370    14     THECHESHIRECAT <--- best so far
1.533951    15     JNTOOEEXFTGQTNH
1.068182    16     TJTSAEETEXHPXHNE
1.034093    17     AZRAXUHEJLREEXIEE
1.443345    18     VNIZQPALEPTSXSEXUC
1.090977    19     FUCAITCSLVTEZDUDEHS
0.979868    20     EQXGAHWTTQECEWUGXHPI
0.789410    21     HVRCSAFTHEBDLSTAERSES
0.881380    22     TVIJTCIGKAQPELECRXPTNC
0.952456    23     KKEQXGPWTCQEELIEHXUWASV
0.715968    24     ELAIXHQTTIEDXJETTNTGAEPC
0.891258    25     OTJUUEGERDNQTUQEAGWUTIEOA
0.852784    26     IGITEGECAGAVUNLJAHASAVTETW

Best key : THECHESHIRECAT

Decrypted text:
THISWASTHEPOEMTHATALICEREADJABBERWOCKYTWASBRILLIGANDTHESLITHYTOVESDIDGYREANDGIMBLEINTHEWABEALLMIMSYWERETHEBOROGOVESANDTHEMOMERATHSOUTGRABEBEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCHBEWARETHEJUBJUBBIRDANDSHUNTHEFRUMIOUSBANDERSNATCHHETOOKHISVORPALSWORDINHANDLONGTIMETHEMANXOMEFOEHESOUGHTSORESTEDHEBYTHETUMTUMTREEANDSTOODAWHILEINTHOUGHTANDASINUFFISHTHOUGHTHESTOODTHEJABBERWOCKWITHEYESOFFLAMECAMEWHIFFLINGTHROUGHTHETULGEYWOODANDBURBLEDASITCAMEONETWOONETWOANDTHROUGHANDTHROUGHTHEVORPALBLADEWENTSNICKERSNACKHELEFTITDEADANDWITHITSHEADHEWENTGALUMPHINGBACKANDHASTTHOUSLAINTHEJABBERWOCKCOMETOMYARMSMYBEAMISHBOYOFRABJOUSDAYCALLOOHCALLAYHECHORTLEDINHISJOYTWASBRILLIGANDTHESLITHYTOVESDIDGYREANDGIMBLEINTHEWABEALLMIMSYWERETHEBOROGOVESANDTHEMOMERATHSOUTGRABEITSEEMSVERYPRETTYSHESAIDWHENSHEHADFINISHEDITBUTITSRATHERHARDTOUNDERSTAND

```



## Phix

{{trans|Julia}}

```Phix
--
-- demo\rosetta\Cryptanalysis.exw
--
atom t0 = time()
constant ciphertext = substitute_all("""
MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH
VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD
ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS
FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG
ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ
ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS
JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT
LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST
MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH
QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV
RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW
TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO
SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR
ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX
BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB
BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA
FWAML ZZRXJ EKAHV FASMU LVVUT TGK""",{" ","\n"},{"",""})

constant letters = new_dict(
   {{'E',12.702},
    {'T',9.056},
    {'A',8.167},
    {'O',7.507},
    {'I',6.966},
    {'N',6.749},
    {'S',6.327},
    {'H',6.094},
    {'R',5.987},
    {'D',4.253},
    {'L',4.025},
    {'C',2.782},
    {'U',2.758},
    {'M',2.406},
    {'W',2.361},
    {'F',2.228},
    {'G',2.015},
    {'Y',1.974},
    {'P',1.929},
    {'B',1.492},
    {'V',0.978},
    {'K',0.772},
    {'J',0.153},
    {'X',0.150},
    {'Q',0.095},
    {'Z',0.074}})
constant digraphs = new_dict(
   {{"TH",15.2},
    {"HE",12.8},
    {"IN",9.4},
    {"ER",9.4},
    {"AN",8.2},
    {"RE",6.8},
    {"ND",6.3},
    {"AT",5.9},
    {"ON",5.7},
    {"NT",5.6},
    {"HA",5.6},
    {"ES",5.6},
    {"ST",5.5},
    {"EN",5.5},
    {"ED",5.3},
    {"TO",5.2},
    {"IT",5.0},
    {"OU",5.0},
    {"EA",4.7},
    {"HI",4.6},
    {"IS",4.6},
    {"OR",4.3},
    {"TI",3.4},
    {"AS",3.3},
    {"TE",2.7},
    {"ET",1.9},
    {"NG",1.8},
    {"OF",1.6},
    {"AL",0.9},
    {"DE",0.9},
    {"SE",0.8},
    {"LE",0.8},
    {"SA",0.6},
    {"SI",0.5},
    {"AR",0.4},
    {"VE",0.4},
    {"RA",0.4},
    {"LD",0.2},
    {"UR",0.2}})
constant trigraphs = new_dict(
   {{"THE",18.1},
    {"AND",7.3},
    {"ING",7.2},
    {"ION",4.2},
    {"ENT",4.2},
    {"HER",3.6},
    {"FOR",3.4},
    {"THA",3.3},
    {"NTH",3.3},
    {"INT",3.2},
    {"TIO",3.1},
    {"ERE",3.1},
    {"TER",3.0},
    {"EST",2.8},
    {"ERS",2.8},
    {"HAT",2.6},
    {"ATI",2.6},
    {"ATE",2.5},
    {"ALL",2.5},
    {"VER",2.4},
    {"HIS",2.4},
    {"HES",2.4},
    {"ETH",2.4},
    {"OFT",2.2},
    {"STH",2.1},
    {"RES",2.1},
    {"OTH",2.1},
    {"ITH",2.1},
    {"FTH",2.1},
    {"ONT",2.0}})
 
function decrypt(string enc, string key)
integer keylen = length(key), k = 1
    string msg = repeat(' ', length(enc))
    for i=1 to length(enc) do
        msg[i] = mod(enc[i]-key[k]+26,26)+'A'
        k = mod(k,keylen)+1
    end for
    return msg
end function
 
function cryptanalyze(string enc, integer maxkeylen=20)
    integer enclen = length(enc)
    string maxkey = "",
           maxdec = "",
           k1 = " "
    atom maxscore = 0.0
 
    for keylen=1 to maxkeylen do
        string key = repeat(' ',keylen)
        sequence idx = {}
        for i=1 to enclen do
            if mod(i,keylen)=0 then
                idx &= i-keylen+1
            end if
        end for

        for i=1 to keylen do
            atom maxsubscore = 0.0
 
            for j='A' to 'Z' do
                atom subscore = 0.0
 
                k1[1] = j
                string encidx = ""
                for ii=1 to length(idx) do
                    encidx &= enc[idx[ii]]
                end for
                string dec = decrypt(encidx,k1)
                for di=1 to length(dec) do
                    subscore += getd(dec[di],letters)
                end for
 
                if subscore > maxsubscore then
                    maxsubscore = subscore
                    key[i] = j
                end if
            end for
 
            idx = sq_add(idx,1)
        end for
 
        string dec = decrypt(enc, key)
        atom score = 0.0
 
        for i=1 to length(dec) do
            score += getd(dec[i],letters)
        end for
 
        for i=1 to enclen - 2 do
            string digraph = dec[i..i+1]
            string trigraph = dec[i..i + 2]
            score += 2 * getd(digraph,digraphs)
            score += 3 * getd(trigraph,trigraphs)
        end for
 
        if score > maxscore then
            maxscore = score
            maxkey = key
            maxdec = dec
        end if
    end for
 
    return {maxkey,maxdec}
end function
 
function fold(string s, integer w)
    for i=w to length(s) by w do
        s[i..i-1] = "\n"
    end for
    return s
end function

string {key, dec} = cryptanalyze(ciphertext)
printf(1,"key: %s\n\n%s\n\n", {key, fold(dec,80)})
 
printf(1,"elapsed time: %3.2f seconds",{time()-t0})
```

{{Out}}

```txt

key: THECHESHIRECAT

THISWASTHEPOEMTHATALICEREADJABBERWOCKYTWASBRILLIGANDTHESLITHYTOVESDIDGYREANDGIM
BLEINTHEWABEALLMIMSYWERETHEBOROGOVESANDTHEMOMERATHSOUTGRABEBEWARETHEJABBERWOCKM
YSONTHEJAWSTHATBITETHECLAWSTHATCATCHBEWARETHEJUBJUBBIRDANDSHUNTHEFRUMIOUSBANDER
SNATCHHETOOKHISVORPALSWORDINHANDLONGTIMETHEMANXOMEFOEHESOUGHTSORESTEDHEBYTHETUM
TUMTREEANDSTOODAWHILEINTHOUGHTANDASINUFFISHTHOUGHTHESTOODTHEJABBERWOCKWITHEYESO
FFLAMECAMEWHIFFLINGTHROUGHTHETULGEYWOODANDBURBLEDASITCAMEONETWOONETWOANDTHROUGH
ANDTHROUGHTHEVORPALBLADEWENTSNICKERSNACKHELEFTITDEADANDWITHITSHEADHEWENTGALUMPH
INGBACKANDHASTTHOUSLAINTHEJABBERWOCKCOMETOMYARMSMYBEAMISHBOYOFRABJOUSDAYCALLOOH
CALLAYHECHORTLEDINHISJOYTWASBRILLIGANDTHESLITHYTOVESDIDGYREANDGIMBLEINTHEWABEAL
LMIMSYWERETHEBOROGOVESANDTHEMOMERATHSOUTGRABEITSEEMSVERYPRETTYSHESAIDWHENSHEHAD
FINISHEDITBUTITSRATHERHARDTOUNDERSTAND

elapsed time: 0.42 seconds

```



## Python

{{trans|D}}

```python
from string import uppercase
from operator import itemgetter

def vigenere_decrypt(target_freqs, input):
    nchars = len(uppercase)
    ordA = ord('A')
    sorted_targets = sorted(target_freqs)

    def frequency(input):
        result = [[c, 0.0] for c in uppercase]
        for c in input:
            result[c - ordA][1] += 1
        return result

    def correlation(input):
        result = 0.0
        freq = frequency(input)
        freq.sort(key=itemgetter(1))

        for i, f in enumerate(freq):
            result += f[1] * sorted_targets[i]
        return result

    cleaned = [ord(c) for c in input.upper() if c.isupper()]
    best_len = 0
    best_corr = -100.0

    # Assume that if there are less than 20 characters
    # per column, the key's too long to guess
    for i in xrange(2, len(cleaned) // 20):
        pieces = [[] for _ in xrange(i)]
        for j, c in enumerate(cleaned):
            pieces[j % i].append(c)

        # The correlation seems to increase for smaller
        # pieces/longer keys, so weigh against them a little
        corr = -0.5 * i + sum(correlation(p) for p in pieces)

        if corr > best_corr:
            best_len = i
            best_corr = corr

    if best_len == 0:
        return ("Text is too short to analyze", "")

    pieces = [[] for _ in xrange(best_len)]
    for i, c in enumerate(cleaned):
        pieces[i % best_len].append(c)

    freqs = [frequency(p) for p in pieces]

    key = ""
    for fr in freqs:
        fr.sort(key=itemgetter(1), reverse=True)

        m = 0
        max_corr = 0.0
        for j in xrange(nchars):
            corr = 0.0
            c = ordA + j
            for frc in fr:
                d = (ord(frc[0]) - c + nchars) % nchars
                corr += frc[1] * target_freqs[d]

            if corr > max_corr:
                m = j
                max_corr = corr

        key += chr(m + ordA)

    r = (chr((c - ord(key[i % best_len]) + nchars) % nchars + ordA)
         for i, c in enumerate(cleaned))
    return (key, "".join(r))


def main():
    encoded = """
        MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH
        VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD
        ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS
        FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG
        ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ
        ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS
        JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT
        LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST
        MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH
        QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV
        RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW
        TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO
        SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR
        ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX
        BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB
        BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA
        FWAML ZZRXJ EKAHV FASMU LVVUT TGK"""

    english_frequences = [
        0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
        0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
        0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
        0.00978, 0.02360, 0.00150, 0.01974, 0.00074]

    (key, decoded) = vigenere_decrypt(english_frequences, encoded)
    print "Key:", key
    print "\nText:", decoded

main()
```



## Racket



###  Simple method 

This is a simple method that just tries to find a key of any length that minimizes the difference from the expected English character distributions.


```Racket

#lang at-exp racket

(define max-keylen 30)

(define text
  @~a{MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH
      VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD
      ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS
      FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG
      ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ
      ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS
      JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT
      LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST
      MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH
      QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV
      RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW
      TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO
      SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR
      ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX
      BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB
      BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA
      FWAML ZZRXJ EKAHV FASMU LVVUT TGK})

(define first-char (char->integer #\A))
(define chars# (- (char->integer #\Z) first-char -1))

(define freqs ; english letter frequencies from wikipedia
  ((compose1 list->vector (curry map (curryr / 100000.0)))
   '(8167 1492 2782 4253 12702 2228 2015 6094 6966 153 772 4025 2406
     6749 7507 1929 95 5987 6327 9056 2758 978 2360 150 1974 74)))

(define text* (for/vector ([c (regexp-replace* #px"\\s+" text "")])
                (- (char->integer c) first-char)))
(define N (vector-length text*))

(define (col-guesses len)
  (for/list ([ofs len])
    (define text (for/list ([i (in-range ofs N len)]) (vector-ref text* i)))
    (define cN (length text))
    (define cfreqs (make-vector chars# 0))
    (for ([c (in-list text)])
      (vector-set! cfreqs c (add1 (vector-ref cfreqs c))))
    (for ([i chars#]) (vector-set! cfreqs i (/ (vector-ref cfreqs i) cN)))
    (argmin car
      (for/list ([d chars#])
        (cons (for/sum ([i chars#])
                (expt (- (vector-ref freqs i)
                         (vector-ref cfreqs (modulo (+ i d) chars#)))
                      2))
              d)))))

(define best-key
  (cdr (argmin car
         (for/list ([len (range 1 (add1 max-keylen))])
           (define guesses (col-guesses len))
           (cons (/ (apply + (map car guesses)) len) (map cdr guesses))))))

(printf "Best key found: ")
(for ([c best-key]) (display (integer->char (+ c first-char))))
(newline)

(printf "Decoded text:\n")
(define decode-num
  (let ([cur '()])
    (λ(n) (when (null? cur) (set! cur best-key))
          (begin0 (modulo (- n (car cur)) chars#) (set! cur (cdr cur))))))
(for ([c text])
  (define n (- (char->integer c) first-char))
  (if (not (< -1 n chars#)) (display c)
      (display (integer->char (+ first-char (decode-num n))))))
(newline)

```


Output:

```txt

Best key found: THECHESHIRECAT
Decoded text:
THISW ASTHE POEMT HATAL ICERE ADJAB BERWO CKYTW ASBRI LLIGA
...

```



###  An attempted more complete implementation 

This is an attempt at following the [http://en.wikipedia.org/wiki/Index_of_coincidence#Example Wikipedia] description.  However, it performs just as well as the simple version.  Most likely because I know almost nothing about cryptography...


```Racket

#lang at-exp racket

(define max-keylen 30)

(define text
  @~a{MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH
      VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD
      ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS
      FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG
      ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ
      ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS
      JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT
      LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST
      MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH
      QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV
      RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW
      TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO
      SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR
      ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX
      BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB
      BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA
      FWAML ZZRXJ EKAHV FASMU LVVUT TGK})

(define first-char (char->integer #\A))
(define chars# (- (char->integer #\Z) first-char -1))

(define freqs ; english letter frequencies from wikipedia
  ((compose1 list->vector (curry map (curryr / 100000.0)))
   '(8167 1492 2782 4253 12702 2228 2015 6094 6966 153 772 4025 2406
     6749 7507 1929 95 5987 6327 9056 2758 978 2360 150 1974 74)))

(define (n*n-1 n) (* n (sub1 n)))

(define text* (for/vector ([c (regexp-replace* #px"\\s+" text "")])
                (- (char->integer c) first-char)))
(define N (vector-length text*))
(define (get-col-length+freqs width offset)
  (define text (for/list ([i (in-range offset N width)]) (vector-ref text* i)))
  (define cN (length text))
  (define freqs (make-vector chars# 0))
  (for ([c (in-list text)]) (vector-set! freqs c (add1 (vector-ref freqs c))))
  (values cN freqs))

(define expected-IC (* chars# (for*/sum ([x freqs]) (* x x))))

;; maps key lengths to average index of coincidence
(define keylen->ICs
  (for/vector ([len (in-range 1 (add1 (* max-keylen 2)))])
    (for/sum ([ofs len])
      (define-values [cN cfreqs] (get-col-length+freqs len ofs))
      (/ (for/sum ([i chars#]) (n*n-1 (vector-ref cfreqs i)))
         (/ (n*n-1 cN) chars#) len 1.0))))

;; given a key length find the key that minimizes errors from alphabet freqs,
;; return (cons average-error key)
(define (guess-key len)
  (define guesses
    (for/list ([ofs len])
      (define-values [cN cfreqs] (get-col-length+freqs len ofs))
      (for ([i chars#]) (vector-set! cfreqs i (/ (vector-ref cfreqs i) cN)))
      (argmin car
        (for/list ([d chars#])
          (cons (for/sum ([i chars#])
                  (expt (- (vector-ref freqs i)
                           (vector-ref cfreqs (modulo (+ i d) chars#)))
                        2))
                d)))))
  (cons (/ (apply + (map car guesses)) len) (map cdr guesses)))

;; look for a key length that minimizes error from expected-IC, with some
;; stupid consideration of multiples of the length (which should also have low
;; errors), for each one guess a key, then find the one that minimizes both (in
;; a way that looks like it works, but undoubtedly is wrong in all kinds of
;; ways) and return the winner key
(define best-key
  ((compose1 cdr (curry argmin car))
   (for/list ([i (* max-keylen 2)])
     ;; get the error from the expected-IC for the length and its multiples,
     ;; with decreasing weights for the multiples
     (define with-multiples
       (for/list ([j (in-range i (* max-keylen 2) (add1 i))] [div N])
         (cons (/ (abs (- (vector-ref keylen->ICs j) expected-IC)) expected-IC)
               (/ (add1 div)))))
     (define total (/ (for/sum ([x with-multiples]) (* (car x) (cdr x)))
                      (for/sum ([x with-multiples]) (cdr x))))
     (define guess (guess-key (add1 i)))
     (define guess*total (* total (car guess) (car guess)))
     ;; (printf "~a~a: ~a ~s\n" (if (< i 9) " " "") (add1 i)
     ;;       (list total (car guess) guess*total) (cdr guess))
     (cons guess*total (cdr guess)))))

(printf "Best key found: ")
(for ([c best-key]) (display (integer->char (+ c first-char))))
(newline)

(printf "Decoded text:\n")
(define decode-num
  (let ([cur '()])
    (λ(n) (when (null? cur) (set! cur best-key))
          (begin0 (modulo (- n (car cur)) chars#) (set! cur (cdr cur))))))
(for ([c text])
  (define n (- (char->integer c) first-char))
  (if (not (< -1 n chars#)) (display c)
      (display (integer->char (+ first-char (decode-num n))))))
(newline)

```



## Tcl

{{trans|Python}}

```tcl
package require Tcl 8.6

oo::class create VigenereAnalyzer {
    variable letterFrequencies sortedTargets
    constructor {{frequencies {
 	0.08167 0.01492 0.02782 0.04253 0.12702 0.02228 0.02015
	0.06094 0.06966 0.00153 0.00772 0.04025 0.02406 0.06749
	0.07507 0.01929 0.00095 0.05987 0.06327 0.09056 0.02758
	0.00978 0.02360 0.00150 0.01974 0.00074
    }}} {
	set letterFrequencies $frequencies
	set sortedTargets [lsort -real $frequencies]
	if {[llength $frequencies] != 26} {
	    error "wrong length of frequency table"
	}
    }

    ### Utility methods
    # Find the value of $idxvar in the range [$from..$to) that maximizes the value
    # in $scorevar (which is computed by evaluating $body) 
    method Best {idxvar from to scorevar body} {
	upvar 1 $idxvar i $scorevar s
	set bestI $from
	for {set i $from} {$i < $to} {incr i} {
	    uplevel 1 $body
	    if {![info exist bestS] || $bestS < $s} {
		set bestI $i
		set bestS $s
	    }
	}
	return $bestI
    }
    # Simple list map
    method Map {var list body} {
	upvar 1 $var v
	set result {}
	foreach v $list {lappend result [uplevel 1 $body]}
	return $result
    }
    # Simple partition of $list into $groups groups; thus, the partition of
    # {a b c d e f} into 3 produces {a d} {b e} {c f}
    method Partition {list groups} {
	set i 0
	foreach val $list {
	    dict lappend result $i $val
	    if {[incr i] >= $groups} {
		set i 0
	    }
	}
	return [dict values $result]
    }

    ### Helper methods
    # Get the actual counts of different types of characters in the given list
    method Frequency cleaned {
	for {set i 0} {$i < 26} {incr i} {
	    dict set tbl $i 0
	}
	foreach ch $cleaned {
	    dict incr tbl [expr {[scan $ch %c] - 65}]
	}
	return $tbl
    }

    # Get the correlation factor of the characters in a given list with the
    # class-specified language frequency corpus
    method Correlation cleaned {
	set result 0.0
	set freq [lsort -integer [dict values [my Frequency $cleaned]]]
	foreach f $freq s $sortedTargets {
	    set result [expr {$result + $f * $s}]
	}
	return $result
    }

    # Compute an estimate for the key length
    method GetKeyLength {cleaned {required 20}} {
	# Assume that we need at least 20 characters per column to guess
	set bestLength [my Best i 2 [expr {[llength $cleaned] / $required}] corr {
	    set corr [expr {-0.5 * $i}]
	    foreach chars [my Partition $cleaned $i] {
		set corr [expr {$corr + [my Correlation $chars]}]
	    }
	}]
	if {$bestLength == 0} {
	    error "text is too short to analyze"
	}
	return $bestLength
    }

    # Compute the key from the given frequency tables and the class-specified
    # language frequency corpus
    method GetKeyFromFreqs freqs {
	foreach f $freqs {
	    set m [my Best i 0 26 corr {
		set corr 0.0
		foreach {ch count} $f {
		    set d [expr {($ch - $i) % 26}]
		    set corr [expr {$corr + $count*[lindex $letterFrequencies $d]}]
		}
	    }]
	    append key [format %c [expr {65 + $m}]]
	}
	return $key
    }

    ##### The main analyzer method #####
    method analyze input {
	# Turn the input into a clean letter sequence
	set cleaned [regexp -all -inline {[A-Z]} [string toupper $input]]
	# Get the (estimated) key length
	set bestLength [my GetKeyLength $cleaned]
	# Get the frequency mapping for the partitioned input text
	set freqs [my Map p [my Partition $cleaned $bestLength] {my Frequency $p}]
	# Get the key itself
	return [my GetKeyFromFreqs $freqs]
    }
}
```

Demonstration (that assumes that the Tcl solution to [[Vigenère cipher#Tcl|Vigenère cipher]] task is present):

```tcl
set encoded "
    MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH
    VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD
    ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS
    FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG
    ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ
    ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS
    JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT
    LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST
    MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH
    QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV
    RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW
    TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO
    SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR
    ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX
    BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB
    BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA
    FWAML ZZRXJ EKAHV FASMU LVVUT TGK
"
VigenereAnalyzer create englishVigenereAnalyzer
set key [englishVigenereAnalyzer analyze $encoded]
Vigenere create decoder $key
set decoded [decoder decrypt $encoded]
puts "Key: $key"
puts "Text: $decoded"
```



## Vedit macro language

This implementation is fully autonomous as long as the text is long enough and there are not too many non-English words in the original text.

The text to be analysed must be in current edit buffer. A new buffer is opened to display the results.

To automatically find the best key, a dictionary is used to find English words within the decrypted text.
I have used unixdict.txt, but if you do not have it available, you can use the Scribe English dictionary that comes with Vedit.
However, that is unnecessarily big. A smaller dictionary is faster and may actually give better results.
It might be good idea to use dictionary that only contains the most common English words.

This implementation finds the best and 2nd best Caesar key for each key position.
It then checks key combinations where max one char is taken from 2nd best Caesar key.
If this does not solve some encrypted text, you could increase the number of key combinations to be checked.


```vedit
// (1) Copy text into tmp buffer and remove non-alpha chars.

Chdir(PATH_ONLY)
BOF
Reg_Copy(10, ALL)                       // copy text to new buffer
Buf_Switch(Buf_Free)
Reg_Ins(10)
BOF
Replace ("|!|A", "", BEGIN+ALL+NOERR)   // remove non-alpha chars
Reg_Copy_Block(10,0,EOB_pos)            // @10 = text to be analysed

#20 = Buf_Num                           // buffer for text being analyzed
#21 = Buf_Free                          // buffer for English frequency list (A-Z)
Buf_Switch(#21)
Ins_Text("8167 1492 2782 4253 12702 2228 2015 6094 6966 153 772 4025 2406 6749 7507 1929 95 5987 6327 9056 2758 978 2360 150 1974 74")
File_Open("unixdict.txt")               // or use "|(MACRO_DIR)\scribe\english.vdf"
#23 = Buf_Num                           // buffer for dictionary
#24 = Buf_Free                          // buffer for key canditates

Buf_Switch(#24)
for (#1=0; #1<5; #1++) {                // Fill table for 5 keys of 50 chars
    Ins_Char('.', COUNT, 50)
    Ins_Newline
}
#22 = Buf_Free                          // buffer for results

#25 = Reg_Size(10)                      // number of letters in the text
#26 = 26                                // number of characters in the alphabet
#61 = min(#25/10, 50)                   // max key length to try

// (2) Check Index of coincidence (or Kp) for each key length

Buf_Switch(#22)                         // buffer for results
Ins_Text("KeyLen Kp   dist ") Ins_Newline
Ins_Text("-----------------") Ins_Newline
#13 = Cur_Pos
#7 = 0                                  // no Caesar encryption
for (#5=1; #5<=#61; #5++) {
    Buf_Switch(#20)                     // text being analyzed
    BOF
    #54 = 0;                            // sum of Kp's
    for (#6=0; #6<#5; #6++) {           // for each slide
        Goto_Pos(#6)
        Call("CHARACTER_FREQUENCIES")
        Call("INDEX_OF_COINCIDENCE")    // #51 = Kp * 10000
        #54 += #51
    }
    #54 /= #5                           // average of Kp's
    Buf_Switch(#22)
    Num_Ins(#5, COUNT, 3)               // write key length
    IT(": ")
    Num_Ins(#54, NOCR)                  // average Kp
    Num_Ins(670-#54)                    // distance to English Kp
}
Buf_Switch(#22)
Sort_Merge("5,12", #13, Cur_Pos, REVERSE)  // sort the results by Kp value
Ins_Newline

// (3) Check the best 4 key lengths to find which one gives the best decrypt result

#38 = 0                                 // max number of correct characters found
#19 = 1                                 // best key length
for (#14 = 0; #14<4; #14++) {           // try 4 best key lengths
    Buf_Switch(#22)                     // results buffer
    Goto_Pos(#13) Line(#14)
    #5 = Num_Eval(SUPPRESS)             // #5 = key length
    Call("FIND_KEYS")                   // find Caesar key for each key character
    #4 = -1                             // try best match key chars only
    Call("BUILD_KEY")
    EOF
    Ins_Text("Key length ")
    Num_Ins(#5, LEFT)
    Reg_Ins(10)                         // encrypted text
    BOL
    Call("DECRYPT_LINE")
    BOL
    Call("FIND_ENGLISH_WORDS")          // #37 = number of English chars
    EOL Ins_Newline
    Ins_Text("Correct chars: ")
    Num_Ins(#37)
    if (#37 > #38) {
        #38 = #37
        #19 = #5
    }
    Update()
}

Ins_Text("Using key length: ") Num_Ins(#19) Ins_Newline
#5 = #19
Call("FIND_KEYS")                       // find Caesar key for each key character

// (4) Decrypt with different key combinations and try to find English words.
//     Try key combinations where max one char is taken from 2nd best Caesar key.

#38 = 0                                 // max number of chars in English words found
#39 = -1                                // best key number found
for (#4 = -1; #4 < #19; #4++)
{
    Call("BUILD_KEY")
    Buf_Switch(#22)                     // results
    Reg_Ins(10)                         // encrypted text
    BOL
    Call("DECRYPT_LINE")
    BOL
    Update()
    Call("FIND_ENGLISH_WORDS")          // #37 := number of correct letters in text
    if (#37 > #38) {
        #38 = #37                       // new highest number of correct chars
        #39 = #4                        // new best key
    }

    EOL IT(" -- ")                      // display results
    Num_Ins(#4, COUNT, 3)               // key number
    Ins_Text(": ")
    for (#6=0; #6<#19; #6++) {          // display key
        #9 = 130 + #6
        Ins_Char(#@9)
    }
    Ins_Text("  correct chars =")
    Num_Ins(#37)
}
Ins_Text("Best key = ")
Num_Ins(#39, LEFT)
#4 = #39
Ins_Newline

// Display results
//
Buf_Switch(#24)                         // table for key canditates
BOF
Reg_Copy_Block(14, Cur_Pos, Cur_Pos+#19)  // best Caesar key chars
Line(1)
Reg_Copy_Block(15, Cur_Pos, Cur_Pos+#19)  // 2nd best Caesar key chars
Call("BUILD_KEY")
Buf_Switch(#22)
Ins_Text("Key 1: ") Reg_Ins(14) Ins_Newline
Ins_Text("Key 2: ") Reg_Ins(15) Ins_Newline
Ins_Text("Key:   ")
for (#6=0; #6 < #19; #6++) {
    #9 = #6+130
    Ins_Char(#@9)
}
Ins_Newline
Ins_Newline

// decrypt the text with selected key
Ins_Text("Decrypted text:") Ins_Newline
Reg_Ins(10)
BOL
Call("DECRYPT_LINE")
BOL Reg_Copy(13,1)
EOL Ins_Newline

// Find English words from the text
Reg_Ins(13)
Call("FIND_ENGLISH_WORDS")
EOL
Ins_Newline
Num_Ins(#37, NOCR) IT(" of ")
Num_Ins(#25, NOCR) IT(" characters are English words. ")
Ins_Newline

Buf_Switch(#20) Buf_Quit(OK)
Buf_Switch(#21) Buf_Quit(OK)
Buf_Switch(#23) Buf_Quit(OK)
Buf_Switch(#24) Buf_Quit(OK)

Statline_Message("Done!")
Return

/////////////////////////////////////////////////////////////////////////////
//
// Caesar decrypt current line and count character frequencies.
//   in: #5 = step size, #7 = encryption key, #26 = num of chars in alphabet
//  out: #65...#90 = frequencies, #60 = number of chars

:CHARACTER_FREQUENCIES:
    Save_Pos
    for (#8 = 'A'; #8<='Z'; #8++) {
        #@8 = 0                         // reset frequency counters
    }
    #60 = 0                             // total number of chars
    while (!At_EOL) {
        if (Cur_Char >= 'A' && Cur_Char <= 'Z') {
            #8 = (Cur_Char-'A'+#26-#7) % #26 + 'A'  // decrypted char
            #@8++
            #60++
        }
        Char(#5)
    }
    Restore_Pos
Return

// Calculate Index of Coincidence (Kp).
//   in: character frequencies in #65...#90, #60 = num of chars
//  out: #51 = IC * 10000
//
:INDEX_OF_COINCIDENCE:
    Num_Push(10,15)
    #10 = 0
    for (#11 = 'A'; #11<='Z'; #11++) {
        #10 += (#@11 * (#@11-1))        // Calculate sigma{ni * (ni-1)}
    }
    #12 = #60 * (#60-1)                 // #12 = N * (N-1)
    #51 = #10 * 10000 / #12             // #51 = Kp * 10000
    Num_Pop(10,15)
Return

// Find best and 2nd best Caesar key for each character position of Vigenère key.
//   in: #5=step size (key length)
//  out: keys in buffer #24
//
:FIND_KEYS:
    for (#6 = 0; #6 < #5; #6++) {               // for each char position in the key
        #30 = -1                                // best key char found so far
        #31 = -1                                // 2nd best key char
        #32 = MAXNUM                            // smallest error found so far
        #33 = MAXNUM                            // 2nd smallest error found so far
        for (#7 = 0; #7 < #26; #7++) {          // for each possible key value
            #35 = 0                             // total frequency error compared to English
            Buf_Switch(#20)                     // text being analyzed
            Goto_Pos(#6)
            Call("CHARACTER_FREQUENCIES")
            Buf_Switch(#21)                     // English frequency table
            BOF
            for (#8 = 'A'; #8<='Z'; #8++) {     // calculate total frequency error
                #34 = Num_Eval(SUPPRESS+ADVANCE)
                #35 += abs((#@8*100000+50000)/#60-#34)
            }

            if (#35 < #32) {                    // found better match?
                #33 = #32
                #32 = #35
                #31 = #30
                #30 = #7
            } else {
                if (#35 < #33) {                // 2nd best match?
                    #33 = #35
                    #31 = #7
                }
            }
        }
        Buf_Switch(#24)                         // table for key canditates
        BOF
        Goto_Col(#6+1)
        Ins_Char(#30+'A', OVERWRITE)            // save the best match
        Line(1)
        Goto_Col(#6+1)
        Ins_Char(#31+'A', OVERWRITE)            // save 2nd best match
    }
    Buf_Switch(#22)                             // results buffer
Return

// Combine actual key from 1st and 2nd best Caesar key characters
// Use 1st key chars and (possibly) one character from 2nd key.
//  #4 = index of the char to be picked from 2nd key, -1 = none.
//  #5 = key length
//
:BUILD_KEY:
    Buf_Switch(#24)                     // table for key canditates
    BOF
    for (#6=0; #6<#5; #6++) {           // copy 1st key
        #8 = 130 + #6
        #@8 = Cur_Char
        Char(1)
    }
    if (#4 >= 0) {
        #8 = 130 + #4                   // pick one char from 2st key
        Line(1)
        Goto_Col(#4+1)
        #@8 = Cur_Char
    }
    Buf_Switch(#22)                     // results buffer
Return

// Decrypt text on current line
//   in: #5 = key length, #130...#189 = key
//
:DECRYPT_LINE:
    Num_Push(6,9)
    #6 = 0
    While (!At_EOL) {
       #9 = #6+130
       #7 = #@9
       #8 = (Cur_Char - #7 + #26) % #26 + 'A'   // decrypted char
       Ins_Char(#8, OVERWRITE)
       #6++
       if (#6 >= #5) {
           #6 = 0
       }
    }
    Num_Pop(6,9)
Return

// Find English words from text on current line
//  out: #37 = number of chars matched
//
:FIND_ENGLISH_WORDS:
    Buf_Switch(#23)                     // dictionary
    BOF
    While (!At_EOF) {
        Reg_Copy_Block(12, Cur_Pos, EOL_Pos)
        if (Reg_Size(12) > 2) {
            Buf_Switch(#22)             // buffer for results
            BOL
            while (Search_Block(@12, Cur_Pos, EOL_Pos, NOERR)) {
                Reg_Ins(12, OVERWRITE)
            }
            Buf_Switch(#23)
        }
        Line(1, ERRBREAK)
    }

    Buf_Switch(#22)
    BOL
    #37 = Search_Block("|V", Cur_Pos, EOL_Pos, ALL+NOERR)
Return 
```



## zkl

{{trans|Python}}

```zkl
var[const] uppercase=["A".."Z"].pump(String),
   english_frequences=T( // A..Z
        0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
        0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
        0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
        0.00978, 0.02360, 0.00150, 0.01974, 0.00074);
 
fcn vigenere_decrypt(target_freqs, input){ // ( (float,...), string)
   nchars,ordA   :=uppercase.len(),"A".toAsc();
   sorted_targets:=target_freqs.sort();
 
   frequency:='wrap(input){  // (n,n,n,n,...), n is ASCII index ("A"==65)
      result:=uppercase.pump(List(),List.fp1(0)); // ( ("A",0),("B",0) ...)
      foreach c in (input){ result[c - ordA][1] += 1 }
      result   // --> mutable list of mutable lists ( ("A",Int)...("Z",Int) )
   };
   correlation:='wrap(input){  // (n,n,n,n,...), n is ASCII index ("A"==65)
      result,freq:=0.0, frequency(input);
      freq.sort(fcn([(_,a)],[(_,b)]){ a<b });  // sort letters by frequency
      foreach i,f in (freq.enumerate()){ result+=sorted_targets[i]*f[1] }
      result	// -->Float
   };
 
   cleaned:=input.toUpper().pump(List,uppercase.holds,Void.Filter,"toAsc");
 
   best_len,best_corr := 0,-100.0;
    # Assume that if there are less than 20 characters
    # per column, the key's too long to guess
   foreach i in ([2..cleaned.len()/20]){
      pieces:=(i).pump(List,List.copy);		// ( (),() ... )
      foreach c in (cleaned){ pieces[__cWalker.idx%i].append(c) }
 
        # The correlation seems to increase for smaller
        # pieces/longer keys, so weigh against them a little
      corr:=-0.5*i + pieces.apply(correlation).sum(0.0);
      if(corr>best_corr) best_len,best_corr=i,corr;
   }
   if(best_len==0) return("Text is too short to analyze", "");
 
   pieces:=best_len.pump(List,List.copy);
   foreach c in (cleaned){ pieces[__cWalker.idx%best_len].append(c) }
 
   key,freqs := "",pieces.apply(frequency);
   foreach fr in (freqs){
      fr.sort(fcn([(_,a)],[(_,b)]){ a>b });  // reverse sort by freq
      m,max_corr := 0,0.0;
      foreach j in (nchars){
         corr,c := 0.0,ordA + j;
	 foreach frc in (fr){
	    d:=(frc[0].toAsc() - c + nchars) % nchars;
 	    corr+=target_freqs[d]*frc[1];
	    if(corr>max_corr) m,max_corr=j,corr;
	 }
      }
      key+=(m + ordA).toChar();
   }
 
   cleaned.enumerate().apply('wrap([(i,c])){
      ( (c - (key[i%best_len]).toAsc() + nchars)%nchars + ordA ).toChar()
   }).concat() : 
   T(key,_);
}
```


```zkl
encryptedText:=
#<<<
"MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH
VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD
ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS
FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG
ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ
ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS
JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT
LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST
MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH
QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV
RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW
TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO
SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR
ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX
BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB
BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA
FWAML ZZRXJ EKAHV FASMU LVVUT TGK";
#<<<
key,decoded:=vigenere_decrypt(english_frequences,encryptedText);
println("Key:", key);
println("Decoded text:", decoded);
```

{{out}}

```txt

Key:THECHESHIRECAT
Decoded text:THISWASTHEPOEMTHATALICEREADJABBERWOCKYTWASBRILLIGANDTHESLITHYTOVESDIDGYREANDGIMBLEINTHEWABEALLMIMSYWERETHEBOROGOVESANDTHEMOMERATHSOUTGRABEBEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCHBEWARETHEJUBJUBBIRDANDSHUNTHEFRUMIOUSBANDERSNATCHHETOOKHISVORPALSWORDINHANDLONGTIMETHEMANXOMEFOEHESOUGHTSORESTEDHEBYTHETUMTUMTREEANDSTOODAWHILEINTHOUGHTANDASINUFFISHTHOUGHTHESTOODTHEJABBERWOCKWITHEYESOFFLAMECAMEWHIFFLINGTHROUGHTHETULGEYWOODANDBURBLEDASITCAMEONETWOONETWOANDTHROUGHANDTHROUGHTHEVORPALBLADEWENTSNICKERSNACKHELEFTITDEADANDWITHITSHEADHEWENTGALUMPHINGBACKANDHASTTHOUSLAINTHEJABBERWOCKCOMETOMYARMSMYBEAMISHBOYOFRABJOUSDAYCALLOOHCALLAYHECHORTLEDINHISJOYTWASBRILLIGANDTHESLITHYTOVESDIDGYREANDGIMBLEINTHEWABEALLMIMSYWERETHEBOROGOVESANDTHEMOMERATHSOUTGRABEITSEEMSVERYPRETTYSHESAIDWHENSHEHADFINISHEDITBUTITSRATHERHARDTOUNDERSTAND

```

