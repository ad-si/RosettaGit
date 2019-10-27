+++
title = "Substitution Cipher"
description = ""
date = 2018-11-21T11:49:57Z
aliases = []
[extra]
id = 19649
[taxonomies]
categories = []
tags = []
+++

{{draft task|Encryption}} 
[[Category:String manipulation]]

Substitution Cipher Implementation - File Encryption/Decryption


;Task:
Encrypt a input/source file by replacing every upper/lower case alphabets of the source file with another predetermined upper/lower case alphabets or symbols and save it into another output/encrypted file and then again convert that output/encrypted file into original/decrypted file.

This type of Encryption/Decryption scheme is often called a Substitution Cipher. 


;Related tasks:
* [[Caesar cipher]]
* [[Rot-13]]
* [[Vigenère Cipher/Cryptanalysis]]


;See also:
* Wikipedia article:   [https://en.wikipedia.org/wiki/Substitution_cipher Substitution cipher]






## Ada


```ada

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Sequential_IO;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Text_IO;

procedure Cipher is
   package Char_IO is new Ada.Sequential_IO (Character);
   use Char_IO;
   Alphabet: constant String := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
   Key :     constant String := "VsciBjedgrzyHalvXZKtUPumGfIwJxqOCFRApnDhQWobLkESYMTN";
   My_Map : Character_Mapping;
   Input, Output : File_Type;
   Buffer        : Character;
begin
   declare
      use Ada.Text_IO;
   begin
      if Argument_Count /= 1 then
	 Put_Line("Usage: " & Command_Name & " <encode|decode>");
      else
	 if Argument(1) = "encode" then
	    My_Map := To_Mapping(From => Alphabet, To => Key);
	 elsif Argument(1) = "decode" then
	    My_Map := To_Mapping(From => Key, To => Alphabet);
	 else
	    Put_Line("Unrecognised Argument: " & Argument(1));
	    return;
	 end if;
      end if;
   end;
   Open       (File => Input,  Mode => In_File,  Name => "input.txt");
   Create     (File => Output, Mode => Out_File, Name => "output.txt");
   loop
      Read  (File => Input,  Item => Buffer);
      Buffer := Value(Map => My_Map, Element => Buffer);
      Write (File => Output, Item => Buffer);
   end loop;
exception
   when Char_IO.End_Error =>
      if Is_Open(Input) then
         Close (Input);
      end if;
      if Is_Open(Output) then
         Close (Output);
      end if;
end Cipher;

```



## C

Takes input file name, plain and cipher keys and the action ( Encrypt or Decrypt) as inputs. Only the first character of the action string is checked, so if you are feeling really [https://www.nsa.gov/ NSA like], use whatever string you want as long as it has a d/D or e/E in front.

```C

#include<stdlib.h>
#include<stdio.h>
#include<wchar.h>

#define ENCRYPT 0
#define DECRYPT 1
#define ALPHA 33
#define OMEGA 126

int wideStrLen(wchar_t* str){
	int i = 0;
	while(str[i++]!=00);
	
	return i;
}

void processFile(char* fileName,char plainKey, char cipherKey,int flag){
	
	FILE* inpFile = fopen(fileName,"r");
	FILE* outFile;
	
	int i,len, diff = (flag==ENCRYPT)?(int)cipherKey - (int)plainKey:(int)plainKey - (int)cipherKey;
	wchar_t str[1000], *outStr;
	char* outFileName = (char*)malloc((strlen(fileName)+5)*sizeof(char));

	sprintf(outFileName,"%s_%s",fileName,(flag==ENCRYPT)?"ENC":"DEC");
	
	outFile = fopen(outFileName,"w");
	
	while(fgetws(str,1000,inpFile)!=NULL){
		len = wideStrLen(str);
		
		outStr = (wchar_t*)malloc((len + 1)*sizeof(wchar_t));
		
		for(i=0;i<len;i++){
			if((int)str[i]>=ALPHA && (int)str[i]<=OMEGA && flag == ENCRYPT)
				outStr[i] = (wchar_t)((int)str[i]+diff);
			 else if((int)str[i]-diff>=ALPHA && (int)str[i]-diff<=OMEGA && flag == DECRYPT)
				outStr[i] = (wchar_t)((int)str[i]-diff);
			else
				outStr[i] = str[i];
		}
		outStr[i]=str[i];
		
		fputws(outStr,outFile);
		
		free(outStr);
	}
	
	fclose(inpFile);
	fclose(outFile);
}

int main(int argC,char* argV[]){
	if(argC!=5)
		printf("Usage : %s <file name, plain key, cipher key, action (E)ncrypt or (D)ecrypt>",argV[0]);
	else{
		processFile(argV[1],argV[2][0],argV[3][0],(argV[4][0]=='E'||argV[4][0]=='e')?ENCRYPT:DECRYPT);
		
		printf("File %s_%s has been written to the same location as input file.",argV[1],(argV[4][0]=='E'||argV[4][0]=='e')?"ENC":"DEC");
	}
	
	return 0;
}

```

A long, long time ago ( yes, [http://www.rosettacode.org/wiki/N-body_problem#C I have said it before] ), I read Digital Fortress by Dan Brown. One thing which struck me was Ensei Tankado using the same algorithm to encrypt itself ( or it's human readable Unicode version, if you are a purist). I remembered the name : Bigelman's Safe, but I got the spelling wrong so I had to [https://archive.org/stream/LostSymbol/Dan%20Brown/Digital%20Fortress#page/n29/mode/2up/search/bigel read the copy on archive.org], it's there on the last line of page 30/31, Biggleman's Safe.

----
So here it is, a program which encrypts itself, you saw the cleartext file above, now here's the invocation and ciphertext file.

```txt

C:\rosettaCode>biggleman.exe substitutionCipher.c a e E
File substitutionCipher.c_ENC has been written to the same location as input file.

```

And here's what substitutionCipher.c_ENC looks like :

```txt

3.Eflmwlio Klswl0 55xl Sgxsfiv 645;.3

'mrgpyhi@wxhpmf2lB
'mrgpyhi@wxhms2lB
'mrgpyhi@{glev2lB

'hijmri IRGV]TX 4
'hijmri HIGV]TX 5
'hijmri EPTLE 77
'hijmri SQIKE 56:

mrx {mhiWxvPir,{glevcx. wxv-
	mrx m A 4?
	{lmpi,wxv_m//a%A44-?
	
	vixyvr m?


zsmh tvsgiwwJmpi,glev. jmpiReqi0glev tpemrOi}0 glev gmtlivOi}0mrx jpek-
	
	JMPI. mrtJmpi A jstir,jmpiReqi0&v&-?
	JMPI. syxJmpi?
	
	mrx m0pir0 hmjj A ,jpekAAIRGV]TX-C,mrx-gmtlivOi} 1 ,mrx-tpemrOi}>,mrx-tpemrOi} 1 ,mrx-gmtlivOi}?
	{glevcx wxv_5444a0 .syxWxv?
	glev. syxJmpiReqi A ,glev.-qeppsg,,wxvpir,jmpiReqi-/9-.wm~isj,glev--?

	wtvmrxj,syxJmpiReqi0&)wc)w&0jmpiReqi0,jpekAAIRGV]TX-C&IRG&>&HIG&-?
	
	syxJmpi A jstir,syxJmpiReqi0&{&-?
	
	{lmpi,jkix{w,wxv054440mrtJmpi-%ARYPP-
		pir A {mhiWxvPir,wxv-?
		
		syxWxv A ,{glevcx.-qeppsg,,pir / 5-.wm~isj,{glevcx--?
		
		jsv,mA4?m@pir?m//-
			mj,,mrx-wxv_maBAEPTLE ** ,mrx-wxv_ma@ASQIKE ** jpek AA IRGV]TX-
				syxWxv_ma A ,{glevcx-,,mrx-wxv_ma/hmjj-?
			 ipwi mj,,mrx-wxv_ma1hmjjBAEPTLE ** ,mrx-wxv_ma1hmjj@ASQIKE ** jpek AA HIGV]TX-
				syxWxv_ma A ,{glevcx-,,mrx-wxv_ma1hmjj-?
			ipwi
				syxWxv_ma A wxv_ma?
		
		syxWxv_maAwxv_ma?
		
		jtyx{w,syxWxv0syxJmpi-?
		
		jvii,syxWxv-?
	
	
	jgpswi,mrtJmpi-?
	jgpswi,syxJmpi-?


mrx qemr,mrx evkG0glev. evkZ_a-
	mj,evkG%A9-
		tvmrxj,&Yweki > )w @jmpi reqi0 tpemr oi}0 gmtliv oi}0 egxmsr ,I-rgv}tx sv ,H-igv}txB&0evkZ_4a-?
	ipwi
		tvsgiwwJmpi,evkZ_5a0evkZ_6a_4a0evkZ_7a_4a0,evkZ_8a_4aAA+I+€€evkZ_8a_4aAA+i+-CIRGV]TX>HIGV]TX-?
		
		tvmrxj,&Jmpi )wc)w lew fiir {vmxxir xs xli weqi psgexmsr ew mrtyx jmpi2&0evkZ_5a0,evkZ_8a_4aAA+I+€€evkZ_8a_4aAA+i+-C&IRG&>&HIG&-?
	
	
	vixyvr 4?


```


And to decrypt :

```txt

C:\rosettaCode>biggleman.exe substitutionCipher.c_ENC e a D
File substitutionCipher.c_ENC_DEC has been written to the same location as input file.

```

And for the cleartext, just scroll up...btw, did you know that Digital Fortress was Brown's first novel and he wrote it back in 1998 ? Wonder why nobody ever saw Snowden happening ?


## C++

The key file should look something like this: ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789SWoVR0kJLXQ8zbCd1OagTH5ie3nvYU2wfrM9yI4sKm6c7hNjtADqFPxpEZlBuG

```cpp

#include <iostream>
#include <string>
#include <fstream>

class cipher {
public:
    bool work( std::string e, std::string f, std::string k ) {
        if( e.length() < 1 ) return false;
        fileBuffer = readFile( f );
        if( "" == fileBuffer ) return false;
        keyBuffer = readFile( k );
        if( "" == keyBuffer ) return false;

        outName = f;
        outName.insert( outName.find_first_of( "." ), "_out" );

        switch( e[0] ) {
            case 'e': return encode();
            case 'd': return decode();
        }
        return false;
    }
private:
    bool encode() {
        size_t idx, len = keyBuffer.length() >> 1;
        for( std::string::iterator i = fileBuffer.begin(); i != fileBuffer.end(); i++ ) {
            idx = keyBuffer.find_first_of( *i );
            if( idx < len ) outBuffer.append( 1, keyBuffer.at( idx + len ) );
            else outBuffer.append( 1, *i );
        }
        return saveOutput();
    }
    bool decode() {
        size_t idx, l = keyBuffer.length(), len = l >> 1;
        for( std::string::iterator i = fileBuffer.begin(); i != fileBuffer.end(); i++ ) {
            idx = keyBuffer.find_last_of( *i );
            if( idx >= len && idx < l ) outBuffer.append( 1, keyBuffer.at( idx - len ) );
            else outBuffer.append( 1, *i );
        }
        return saveOutput();
    }
    bool saveOutput() {
        std::ofstream o( outName.c_str() );
        o.write( outBuffer.c_str(), outBuffer.size() );
        o.close();
        return true;
    }
    std::string readFile( std::string fl ) {
        std::string buffer = "";
        std::ifstream f( fl.c_str(), std::ios_base::in );
        if( f.good() ) {
            buffer = std::string( ( std::istreambuf_iterator<char>( f ) ), std::istreambuf_iterator<char>() );
            f.close();
        }
        return buffer;
    }
    std::string fileBuffer, keyBuffer, outBuffer, outName;
};

int main( int argc, char* argv[] ) {
    if( argc < 4 ) {
        std::cout << "<d or e>\tDecrypt or Encrypt\n<filename>\tInput file, the output file will have"
        "'_out' added to it.\n<key>\t\tfile with the key to encode/decode\n\n";
    } else {
        cipher c;
        if( c.work( argv[1], argv[2], argv[3] ) ) std::cout << "\nFile successfully saved!\n\n";
        else std::cout << "Something went wrong!\n\n";
    }
    return 0;
}

```



## C sharp



```csharp
using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SubstitutionCipherProject
{
    class SubstitutionCipher
    {
        static void Main(string[] args)
        {
            doEncDec("e:\\source.txt", "enc.txt", true);
            doEncDec("enc.txt", "dec.txt", false);
            Console.WriteLine("Done");
            Console.ReadKey();
        }
        static void doEncDec(String source, String target, bool IsEncrypt)
        {
            ITransform trans;

            if (IsEncrypt)
                trans = new Encrypt();
            else
                trans = new Decrypt();

            FileInfo sfi = new FileInfo(source);
            FileStream sstream = sfi.OpenRead();
            StreamReader sr = new StreamReader(sstream);

            FileInfo tfi = new FileInfo(target);
            FileStream tstream = tfi.OpenWrite();
            TransformWriter tw = new TransformWriter(tstream, trans);
            StreamWriter sw = new StreamWriter(tw);

            String line;
            while ((line = sr.ReadLine()) != null)
                sw.WriteLine(line);
            sw.Close();
        }
    }
    public interface ITransform
    {
        byte transform(byte ch);
    }
    public class Encrypt : ITransform
    {
        const String str = "xyfagchbimpourvnqsdewtkjzl";
        byte ITransform.transform(byte ch)
        {
            if (char.IsLower((char)ch))
                ch = (byte)str[ch - (byte)'a'];
            return ch;
        }
    }
    class Decrypt : ITransform
    {
        const String str = "xyfagchbimpourvnqsdewtkjzl";
        byte ITransform.transform(byte ch)
        {
            if (char.IsLower((char)ch))
                ch = (byte)(str.IndexOf((char)ch) + 'a');
            return ch;
        }
    }
    class TransformWriter : Stream, IDisposable
    {
        private Stream outs;
        private ITransform trans;

        public TransformWriter(Stream s, ITransform t)
        {
            this.outs = s;
            this.trans = t;
        }

        public override bool CanRead
        {
            get { return false; }
        }

        public override bool CanSeek
        {
            get { return false; }
        }

        public override bool CanWrite
        {
            get { return true; }
        }
        public override void Flush()
        {
            outs.Flush();
        }

        public override long Length
        {
            get { return outs.Length; }
        }
        public override long Position
        {
            get
            {
                return outs.Position;
            }
            set
            {
                outs.Position = value;
            }
        }
        public override long Seek(long offset, SeekOrigin origin)
        {
            return outs.Seek(offset, origin);
        }

        public override void SetLength(long value)
        {
            outs.SetLength(value);
        }

        public override void Write(byte[] buf, int off, int len)
        {
            for (int i = off; i < off + len; i++)
                buf[i] = trans.transform(buf[i]);
            outs.Write(buf, off, len);
        }

        void IDisposable.Dispose()
        {
            outs.Dispose();
        }

        public override void Close()
        {
            outs.Close();
        }

        public override int Read(byte[] cbuf, int off, int count)
        {
            return outs.Read(cbuf, off, count);
        }
    }
}
```



## D


```D
import std.stdio;
import std.string;
import std.traits;

string text =
`Here we have to do is there will be a input/source
 file in which we are going to Encrypt the file by replacing every
 upper/lower case alphabets of the source file with another
 predetermined upper/lower case alphabets or symbols and save
 it into another output/encrypted file and then again convert
 that output/encrypted file into original/decrypted file. This
 type of Encryption/Decryption scheme is often called a
 Substitution Cipher.`;

void main() {
    auto enc = encode(text);
    writeln("Encoded: ", enc);
    writeln;
    writeln("Decoded: ", decode(enc));
}

enum FORWARD = "A~B!C@D#E$F%G^H&I*J(K)L+M=N[O]P{Q}R<S>T/U?V:W;X.Y,Z a\tbcdefghijkl\nmnopqrstuvwxyz";
auto encode(string input) {
    return tr(input, FORWARD, REVERSE);
}

enum REVERSE = "VsciBjedgrzy\nHalvXZKtUP um\tGf?I/w>J<x.q,OC:F;R{A]p}n[D+h=Q)W(o*b&L^k%E$S#Y@M!T~N";
auto decode(string input) {
    return tr(input, REVERSE, FORWARD);
}
```

{{out}}

```txt
Encoded: aQSQn!Qn([MQnY%n=%no#nY(QSQn!o&&n+Qn[nokE@Y,#%@ShQLn)o&Qnokn!(oh(n!Qn[SQnW%okWnY%ngkhS~EYnY(Qn)o&Qn+~nSQE&[hokWnQMQS~Ln@EEQS,&%!QSnh[#Qn[&E([+QY#n%)nY(Qn#%@ShQn)o&Qn!oY(n[k%Y(QSLnESQ=QYQS^okQ=n@EEQS,&%!QSnh[#Qn[&E([+QY#n%Sn#~^+%&#n[k=n#[MQLnoYnokY%n[k%Y(QSn%@YE@Y,QkhS~EYQ=n)o&Qn[k=nY(Qkn[W[oknh%kMQSYLnY([Yn%@YE@Y,QkhS~EYQ=n)o&QnokY%n%SoWok[&,=QhS~EYQ=n)o&QAnq(o#LnY~EQn%)ngkhS~EYo%k,eQhS~EYo%kn#h(Q^Qno#n%)YQknh[&&Q=n[Lnx@+#YoY@Yo%knBoE(QSA

Decoded: Here we have to do is there will be a input/source
 file in which we are going to Encrypt the file by replacing every
 upper/lower case alphabets of the source file with another
 predetermined upper/lower case alphabets or symbols and save
 it into another output/encrypted file and then again convert
 that output/encrypted file into original/decrypted file. This
 type of Encryption/Decryption scheme is often called a
 Substitution Cipher.
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program substitution
  implicit none

  integer, parameter :: len_max = 256
  integer, parameter :: eof = -1
  integer :: in_unit = 9, out_unit = 10, ios
  character(len_max) :: line
  
  open(in_unit, file="plain.txt",  iostat=ios)
  if (ios /= 0) then
    write(*,*) "Error opening plain.txt file"
    stop
  end if
  
  open(out_unit, file="encrypted.txt", iostat=ios)
  if (ios /= 0) then
    write(*,*) "Error opening encrypted.txt file"
    stop
  end if

! Encryption
  do 
    read(in_unit, "(a)", iostat=ios) line
    if (ios > 0) then
      write(*,*) "Error reading plain.txt file"
      stop
    else if (ios == eof) then
      exit
    end if
            
    call cipher(trim(line))
    write(out_unit, "(a)", iostat=ios) trim(line)
    if (ios /= 0) then
      write(*,*) "Error writing encrypted.txt file"
      stop
    end if
  end do

  close(in_unit)
  close(out_unit)

  open(in_unit, file="encrypted.txt",  iostat=ios)
  if (ios /= 0) then
    write(*,*) "Error opening encrypted.txt file"
    stop
  end if
  
  open(out_unit, file="decrypted.txt", iostat=ios)
  if (ios /= 0) then
    write(*,*) "Error opening decrypted.txt file"
    stop
  end if
 
! Decryption 
  do 
    read(in_unit, "(a)", iostat=ios) line
    if (ios > 0) then
      write(*,*) "Error reading encrypted.txt file"
      stop
    else if (ios == eof) then
      exit
    end if
            
    call cipher(trim(line))
    write(out_unit, "(a)", iostat=ios) trim(line)
    if (ios /= 0) then
      write(*,*) "Error writing decrypted.txt file"
      stop
    end if
  end do  

  close(in_unit)
  close(out_unit)
  
contains

subroutine cipher(text)
  character(*), intent(in out) :: text
  integer :: i

! Substitutes A -> Z, B -> Y ... Y -> B, Z -> A and ditto for lower case
! works for both encryption and decryption

  do i = 1, len(text)
    select case(text(i:i))
      case ('A':'Z')
        text(i:i) = achar(155 - iachar(text(i:i)))
      case ('a':'z')
        text(i:i) = achar(219 - iachar(text(i:i)))
    end select
  end do
end subroutine

end program 
```

{{out}}

```txt

Encrypted file:
Sviv dv szev gl wl rh gsviv droo yv z rmkfg/hlfixv urov rm dsrxs dv ziv
tlrmt gl Vmxibkg gsv urov yb ivkozxrmt vevib fkkvi/oldvi xzhv zokszyvgh
lu gsv hlfixv urov drgs zmlgsvi kivwvgvinrmvw fkkvi/oldvi xzhv zokszyvgh
li hbnyloh zmw hzev rg rmgl zmlgsvi lfgkfg/vmxibkgvw urov zmw gsvm ztzrm
xlmevig gszg lfgkfg/vmxibkgvw urov rmgl lirtrmzo/wvxibkgvw urov.
Gsrh gbkv lu Vmxibkgrlm/Wvxibkgrlm hxsvnv rh lugvm xzoovw z Hfyhgrgfgrlm Xrksvi.

Decrypted file:
Here we have to do is there will be a input/source file in which we are
going to Encrypt the file by replacing every upper/lower case alphabets
of the source file with another predetermined upper/lower case alphabets
or symbols and save it into another output/encrypted file and then again
convert that output/encrypted file into original/decrypted file.
This type of Encryption/Decryption scheme is often called a Substitution Cipher.

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' uses same alphabet and key as Ada language example
Const string1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
Const string2 = "VsciBjedgrzyHalvXZKtUPumGfIwJxqOCFRApnDhQWobLkESYMTN"

Sub process(inputFile As String, outputFile As String, encrypt As Boolean)
   Open inputFile For Input As #1
   If err > 0 Then
     Print "Unable to open input file"
     Sleep
     End
   End If
   Dim As String alpha, key 
   If encrypt Then
     alpha = string1 : key = string2
   Else
     alpha = string2 : key = string1
   End If     
   Open outputFile For Output As #2
   Dim s As String
   Dim p As Integer
   While Not Eof(1)
     Line Input #1, s
     For i As Integer = 0 To Len(s) - 1  
       If (s[i] >= 65 AndAlso s[i] <= 90) OrElse (s[i] >= 97 AndAlso s[i] <= 122) Then
         p =  Instr(alpha, Mid(s, i + 1, 1)) - 1
         s[i] = key[p]         
       End If      
     Next 
     Print #2, s
   Wend
   Close #1 : Close #2
End Sub

process "plain.txt", "encrypted.txt", true
process "encrypted.txt", "decrypted.txt", false
Print
Print "Press any key to quit"
Sleep
```


{{out}}
Encrypted.txt :

```txt

KEwLkRkEkRQh cRWFqb gDWnqDqhkIkRQh - jRnq BhJbTWkRQh/iqJbTWkRQh

tILp - dqbq Yq FISq kQ xQ RL kFqbq YRnn wq I RhWEk/LQEbJq ORnq Rh YFRJF Yq Ibq
 CQRhC kQ BhJbTWk kFq ORnq wT bqWnIJRhC qSqbT EWWqb/nQYqb JILq InWFIwqkL QO kFq
 LQEbJq ORnq YRkF IhQkFqb WbqxqkqbDRhqx EWWqb/nQYqb JILq InWFIwqkL Qb LTDwQnL
 Ihx LISq Rk RhkQ IhQkFqb QEkWEk/qhJbTWkqx ORnq Ihx kFqh ICIRh JQhSqbk kFIk
 QEkWEk/qhJbTWkqx ORnq RhkQ QbRCRhIn/xqJbTWkqx ORnq.

tFRL kTWq QO BhJbTWkRQh/iqJbTWkRQh LJFqDq RL QOkqh JInnqx I KEwLkRkEkRQh cRWFqb.

cnRJp Fqbq kQ phQY DQbq.

```


Decrypted.txt = Plain.txt :

```txt

Substitution Cipher Implementation - File Encryption/Decryption

Task - Here we have to do is there will be a input/source file in which we are
 going to Encrypt the file by replacing every upper/lower case alphabets of the
 source file with another predetermined upper/lower case alphabets or symbols
 and save it into another output/encrypted file and then again convert that
 output/encrypted file into original/decrypted file.

This type of Encryption/Decryption scheme is often called a Substitution Cipher.

Click here to know more.

```



## Go

{{trans|Kotlin}}

```go
package main

import (    
    "fmt"
    "strings"
)

var key = "]kYV}(!7P$n5_0i R:?jOWtF/=-pe'AD&@r6%ZXs\"v*N[#wSl9zq2^+g;LoB`aGh{3.HIu4fbK)mU8|dMET><,Qc\\C1yxJ"

func encode(s string) string {
    bs := []byte(s)
    for i := 0; i < len(bs); i++ {
        bs[i] = key[int(bs[i]) - 32]
    }
    return string(bs)
}

func decode(s string) string {
    bs := []byte(s)
    for i := 0; i < len(bs); i++ {
        bs[i] = byte(strings.IndexByte(key, bs[i]) + 32)
    }
    return string(bs)
}

func main() {
    s := "The quick brown fox jumps over the lazy dog, who barks VERY loudly!"
    enc := encode(s)
    fmt.Println("Encoded: ", enc)
    fmt.Println("Decoded: ", decode(enc))
}
```


{{out}}

```txt

Encoded:  2bu]E,KHm].Tdc|]4d\]),8M>]dQuT]<bu]U31C]Idf_]cbd].3Tm>]+ZzL]Ud,IUCk
Decoded:  The quick brown fox jumps over the lazy dog, who barks VERY loudly!

```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "SuChiper.bas"
110 STRING ST$(1 TO 2)*52,K$*1
120 LET ST$(1)="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
130 LET ST$(2)="VsciBjedgrzyHalvXZKtUPumGfIwJxqOCFRApnDhQWobLkESYMTN"
140 CLEAR SCREEN:PRINT "1 - encode, 2 - decode"
150 DO
160   LET K$=INKEY$
170 LOOP UNTIL K$="1" OR K$="2"
180 IF K$="1" THEN
190   INPUT PROMPT "File name: ":NAME$
200   IF OPENFILE(NAME$) THEN CALL CHIPER(1)
210 ELSE
220   IF OPENFILE("Encrypte.txt") THEN CALL CHIPER(2)
230 END IF
240 DEF OPENFILE(N$)
250   LET OPENFILE=0
260   WHEN EXCEPTION USE OPENERROR
270     OPEN #1:N$
280   END WHEN 
290   LET OPENFILE=-1
300 END DEF
310 DEF CHIPER(FUNC)
320   LET EOF=0
330   WHEN EXCEPTION USE OPENERROR
340     IF FUNC=1 THEN
350       OPEN #2:"Encrypte.txt" ACCESS OUTPUT
360       LET OUTP=2
370     ELSE 
380       OPEN #2:"Decrypte.txt" ACCESS OUTPUT
390       LET OUTP=1
400     END IF 
410   END WHEN 
420   WHEN EXCEPTION USE IOERROR
430     DO 
440       GET #1:K$
450       IF UCASE$(K$)>="A" AND UCASE$(K$)<="Z" THEN
460         PRINT #2:ST$(OUTP)(POS(ST$(FUNC),K$));
470       ELSE 
480         PRINT #2:K$;
490       END IF 
500     LOOP UNTIL EOF
510   END WHEN 
520   HANDLER IOERROR
530     IF EXTYPE<>9228 THEN PRINT EXSTRING$(EXTYPE)
540     CLOSE #2
550     CLOSE #1
560     LET EOF=1
570   END HANDLER 
580 END DEF 
590 HANDLER OPENERROR
600   PRINT EXSTRING$(EXTYPE)
610   END
620 END HANDLER
```



## J


Example implementation:


```J
keysubst=: [`(a.i.])`(a."_)}
key=: 'Taehist' keysubst '!@#$%^&'
enc=: a. {~ key i. ]
dec=: key {~ a. i. ]

   enc 'This is a test.'
!$%^ %^ @ &#^&.
   dec '!$%^ %^ @ &#^&.'
This is a test.
```


Note that this particular implementation bakes the key itself into the implementations of <code>enc</code> and <code>dec</code>. Also note that this particular key is rather limited - letters not mentioned in the key encrypt as another identical character. That seems to be sufficient, given the current task description. But of course other approaches are also possible...


## Java


```java
public class SubstitutionCipher {

    final static String key = "]kYV}(!7P$n5_0i R:?jOWtF/=-pe'AD&@r6%ZXs\"v*N"
            + "[#wSl9zq2^+g;LoB`aGh{3.HIu4fbK)mU8|dMET><,Qc\\C1yxJ";

    static String text = "Here we have to do is there will be a input/source "
            + "file in which we are going to Encrypt the file by replacing every "
            + "upper/lower case alphabets of the source file with another "
            + "predetermined upper/lower case alphabets or symbols and save "
            + "it into another output/encrypted file and then again convert "
            + "that output/encrypted file into original/decrypted file. This "
            + "type of Encryption/Decryption scheme is often called a "
            + "Substitution Cipher.";

    public static void main(String[] args) {
        String enc = encode(text);
        System.out.println("Encoded: " + enc);
        System.out.println("\nDecoded: " + decode(enc));
    }

    static String encode(String s) {
        StringBuilder sb = new StringBuilder(s.length());

        for (char c : s.toCharArray())
            sb.append(key.charAt((int) c - 32));

        return sb.toString();
    }

    static String decode(String s) {
        StringBuilder sb = new StringBuilder(s.length());

        for (char c : s.toCharArray())
            sb.append((char) (key.indexOf((int) c) + 32));

        return sb.toString();
    }
}
```



```txt
Encoded: "uTu]cu]b3Qu]<d]Id]...><K<,<Kd|]6KMbuTi
Decoded: Here we have to do is... Substitution Cipher.
```



## Julia

{{works with|Julia|0.6}}
{{trans|Kotlin}}

'''Module''':

```julia
module SubstitutionCiphers

using Compat

const key = "]kYV}(!7P\$n5_0i R:?jOWtF/=-pe'AD&@r6%ZXs\"v*N[#wSl9zq2^+g;LoB`aGh{3.HIu4fbK)mU8|dMET><,Qc\\C1yxJ"

function encode(s::AbstractString)
    buf = IOBuffer()
    for c in s
        print(buf, key[Int(c) - 31])
    end
    return String(take!(buf))
end

function decode(s::AbstractString)
    buf = IOBuffer()
    for c in s
        print(buf, Char(findfirst(==(c), key) + 31))
    end
    return String(take!(buf))
end

end  # module SubstitutionCiphers
```


'''Main''':

```julia
let s = "The quick brown fox jumps over the lazy dog, who barks VERY loudly!"
    enc = SubstitutionCiphers.encode(s)
    dec = SubstitutionCiphers.decode(enc)
    println("Original: ", s, "\n -> Encoded: ", enc, "\n -> Decoded: ", dec)
end
```


{{out}}

```txt
Original: The quick brown fox jumps over the lazy dog, who barks VERY loudly!
 -> Encoded: 2bu]E,KHm].Tdc|]4d\]),8M>]dQuT]<bu]U31C]Idf_]cbd].3Tm>]+ZzL]Ud,IUCk
 -> Decoded: The quick brown fox jumps over the lazy dog, who barks VERY loudly!
```



## Kotlin

{{trans|Java}}

```scala
// version 1.0.6

object SubstitutionCipher {
    val key = "]kYV}(!7P\$n5_0i R:?jOWtF/=-pe'AD&@r6%ZXs\"v*N[#wSl9zq2^+g;LoB`aGh{3.HIu4fbK)mU8|dMET><,Qc\\C1yxJ"

    fun encode(s: String): String {
        val sb = StringBuilder()
        for (c in s) sb.append(key[c.toInt() - 32])
        return sb.toString()
    }

    fun decode(s: String): String {
        val sb = StringBuilder()
        for (c in s) sb.append((key.indexOf(c) + 32).toChar())
        return sb.toString()
    }
}

fun main(args: Array<String>) {
    val s = "The quick brown fox jumps over the lazy dog, who barks VERY loudly!"
    val enc = SubstitutionCipher.encode(s)
    println("Encoded:  $enc")
    println("Decoded:  ${SubstitutionCipher.decode(enc)}")
}
```


{{out}}

```txt

Encoded:  2bu]E,KHm].Tdc|]4d\]),8M>]dQuT]<bu]U31C]Idf_]cbd].3Tm>]+ZzL]Ud,IUCk
Decoded:  The quick brown fox jumps over the lazy dog, who barks VERY loudly!

```



## Lua


```Lua
-- Generate a random substitution cipher for ASCII characters 65 to 122
function randomCipher ()
    local cipher, rnd = {plain = {}, encoded = {}}
    for ascii = 65, 122 do
        table.insert(cipher.plain, string.char(ascii))
        table.insert(cipher.encoded, string.char(ascii))
    end
    for i = 1, #cipher.encoded do
        rnd = math.random(#cipher.encoded)
        cipher.encoded[i], cipher.encoded[rnd] = cipher.encoded[rnd], cipher.encoded[i]
    end
    return cipher
end

-- Encipher text using cipher.  Decipher if decode is true.
function encode (text, cipher, decode)
    local output, letter, found, source, dest = ""
    if decode then
        source, dest = cipher.encoded, cipher.plain
    else
        source, dest = cipher.plain, cipher.encoded
    end
    for pos = 1, #text do
        letter = text:sub(pos, pos)
        found = false
        for k, v in pairs(source) do
            if letter == v then
                output = output .. dest[k]
                found = true
                break
            end
        end
        if not found then output = output .. letter end
    end
    return output
end

-- Main procedure
math.randomseed(os.time())
local subCipher = randomCipher()
print("Cipher generated:")
print("\tPlain:", table.concat(subCipher.plain))
print("\tCoded:", table.concat(subCipher.encoded))
local inFile = io.open("C:\\ulua\\taskDescription.txt", "r")
local input = inFile:read("*all")
inFile:close()
local encoded = encode(input, subCipher)
print("\nEncoded file contents:")
print("\t" .. encoded)
print("\nAbove text deciphers to: ")
print("\t" .. encode(encoded, subCipher, true))
```

{{out}}

```txt
Cipher generated:
        Plain:  ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz
        Coded:  MwZ\zPaqSTNuLQpgbnG[V_xeJmlWYCI]rhFcv^UHXijR`yfKEktADsdoBO

Encoded file contents:
        qvkv dv Hrsv Af cf Xt AHvkv dXRR hv r XyKDA/tfDkFv ^XRv Xy dHXFH dv rkv
UfXyU Af zyFkBKA AHv ^XRv hB kvKRrFXyU vsvkB DKKvk/Rfdvk Frtv rRKHrhvAt f^ AHv t
fDkFv ^XRv dXAH ryfAHvk KkvcvAvk`Xyvc DKKvk/Rfdvk Frtv rRKHrhvAt fk tB`hfRt ryc
trsv XA XyAf ryfAHvk fDAKDA/vyFkBKAvc ^XRv ryc AHvy rUrXy FfysvkA AHrA fDAKDA/vy
FkBKAvc ^XRv XyAf fkXUXyrR/cvFkBKAvc ^XRv. [HXt ABKv f^ zyFkBKAXfy/\vFkBKAXfy tF
Hv`v Xt f^Avy FrRRvc r GDhtAXADAXfy ZXKHvk.

Above text deciphers to:
        Here we have to do is there will be a input/source file in which we are
going to Encrypt the file by replacing every upper/lower case alphabets of the s
ource file with another predetermined upper/lower case alphabets or symbols and
save it into another output/encrypted file and then again convert that output/en
crypted file into original/decrypted file. This type of Encryption/Decryption sc
heme is often called a Substitution Cipher.
```



## Perl

{{trans|Java}}

```perl
sub encode {
    my $source = shift;
    my $key = shift;
    my $out = q();

    @ka = split //, $key;
    foreach $ch (split //, $source) {
        $idx = ord($ch) - 32;
        $out .= $ka[$idx];
    }

    return $out;
}

sub decode {
    my $source = shift;
    my $key = shift;
    my $out = q();

    foreach $ch (split //, $source) {
        $idx = index $key, $ch;
        $val = chr($idx + 32);
        $out .= $val;
    }

    return $out;
}

my $key = q(]kYV}(!7P$n5_0i R:?jOWtF/=-pe'AD&@r6%ZXs"v*N[#wSl9zq2^+g;LoB`aGh{3.HIu4fbK)mU8|dMET><,Qc\C1yxJ);
my $text = "Here we have to do is there will be a input/source "
         . "file in which we are going to Encrypt the file by replacing every "
         . "upper/lower case alphabets of the source file with another "
         . "predetermined upper/lower case alphabets or symbols and save "
         . "it into another output/encrypted file and then again convert "
         . "that output/encrypted file into original/decrypted file. This "
         . "type of Encryption/Decryption scheme is often called a "
         . "Substitution Cipher.";

my $ct = encode($text, $key);
print "Encoded: $ct\n";

my $pt = decode($ct, $key);
print "Decoded: $pt\n";
```

{{out}}

```txt
Encoded: "uTu]cu]b3Qu]<d]Id]K>]<buTu]cKUU].u]3]K|M,< >d,THu]4KUu]K|]cbKHb]cu]3Tu]fdK|f]<d]Z|HTCM<]<bu]4KUu].C]TuMU3HK|f]uQuTC],MMuT UdcuT]H3>u]3UMb3.u<>]d4]<bu]>d,THu]4KUu]cK<b]3|d<buT]MTuIu<uT8K|uI],MMuT UdcuT]H3>u]3UMb3.u<>]dT]>C8.dU>]3|I]>3Qu]K<]K|<d]3|d<buT]d,<M,< u|HTCM<uI]4KUu]3|I]<bu|]3f3K|]Hd|QuT<]<b3<]d,<M,< u|HTCM<uI]4KUu]K|<d]dTKfK|3U IuHTCM<uI]4KUui]2bK>]<CMu]d4]Z|HTCM<Kd| %uHTCM<Kd|]>Hbu8u]K>]d4<u|]H3UUuI]3]q,.><K<,<Kd|]6KMbuTi
Decoded: Here we have to do is there will be a input/source file in which we are going to Encrypt the file by replacing every upper/lower case alphabets of the source file with another predetermined upper/lower case alphabets or symbols and save it into another output/encrypted file and then again convert that output/encrypted file into original/decrypted file. This type of Encryption/Decryption scheme is often called a Substitution Cipher.
```



## Perl 6

{{works with|Rakudo|2015-11-20}}
Feed it an action (encode, decode) and a file name at the command line and it will spit out the (en|de)coded file to STDOUT. Redirect into a file to save it. If no parameters are passed in, does the demo encode/decode.

```perl6
my $chr = (' ' .. '}').join('');
my $key = $chr.comb.pick(*).join('');

# Be very boring and use the same key every time to fit task reqs.
$key = q☃3#}^",dLs*>tPMcZR!fmC rEKhlw1v4AOgj7Q]YI+|pDB82a&XFV9yzuH<WT%N;iS.0e:`G\n['6@_{bk)=-5qx(/?$JoU☃;

sub MAIN ($action = 'encode', $file = '') {

    die 'Only options are encode or decode.' unless $action ~~ any 'encode'|'decode';

    my $text = qq:to/END/;
        Here we have to do is there will be a input/source file in which 
        we are going to Encrypt the file by replacing every upper/lower 
        case alphabets of the source file with another predetermined 
        upper/lower case alphabets or symbols and save it into another 
        output/encrypted file and then again convert that output/encrypted 
        file into original/decrypted file. This type of Encryption/Decryption
        scheme is often called a Substitution Cipher.
        END

    $text = $file.IO.slurp if $file;

    say "Key = $key\n";

    if $file {
        say &::($action)($text);
    } else {
        my $encoded;
        say "Encoded text: \n {$encoded = encode $text}";
        say "Decoded text: \n {decode $encoded}";
    }
}

sub encode ($text) { $text.trans($chr => $key) }

sub decode ($text) { $text.trans($key => $chr) }
```

{{out}} with no passed parameters:

```txt
Key = 3#}^",dLs*>tPMcZR!fmC rEKhlw1v4AOgj7Q]YI+|pDB82a&XFV9yzuH<WT%N;iS.0e:`G\n['6@_{bk)=-5qx(/?$JoU

Encoded text: 
 +`=`3(`3n.x`35b3:b3[-35n`=`3([@@30`3.3[{kq5Z-bq=e`3G[@`3[{3(n[en3
(`3.=`3\b[{\35b3]{e=?k535n`3G[@`30?3=`k@.e[{\3`x`=?3qkk`=Z@b(`=3
e.-`3.@kn.0`5-3bG35n`3-bq=e`3G[@`3([5n3.{b5n`=3k=`:`5`=_[{`:3
qkk`=Z@b(`=3e.-`3.@kn.0`5-3b=3-?_0b@-3.{:3-.x`3[53[{5b3.{b5n`=3
bq5kq5Z`{e=?k5`:3G[@`3.{:35n`{3.\.[{3eb{x`=535n.53bq5kq5Z`{e=?k5`:3
G[@`3[{5b3b=[\[{.@Z:`e=?k5`:3G[@`c39n[-35?k`3bG3]{e=?k5[b{ZQ`e=?k5[b{
-en`_`3[-3bG5`{3e.@@`:3.3Vq0-5[5q5[b{37[kn`=c

Decoded text: 
 Here we have to do is there will be a input/source file in which 
we are going to Encrypt the file by replacing every upper/lower 
case alphabets of the source file with another predetermined 
upper/lower case alphabets or symbols and save it into another 
output/encrypted file and then again convert that output/encrypted 
file into original/decrypted file. This type of Encryption/Decryption
scheme is often called a Substitution Cipher.

```



## Phix


```Phix
constant plain = tagset('Z','A')&tagset('z','a'),
         key = shuffle(plain)

function encode(string s, integer decrypt=false)
    sequence {p,k} = iff(decrypt?{plain,key}:{key,plain})
    for i=1 to length(s) do
        integer ki = find(s[i],p)
        if ki then s[i] = k[ki] end if
    end for
    return s
end function
 
string original = "A simple example.",
       encoded = encode(original),
       decoded = encode(encoded, true)

printf(1,"original: %s\nencoded: %s\ndecoded: %s\n",{original,encoded,decoded})
```

{{Out}}

```txt

original: A simple example.
encoded: j wqGuMv vsQGuMv.
decoded: A simple example.

```



## PicoLisp


```PicoLisp
(setq *A (chop "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
(setq *K (chop "VsciBjedgrzyHalvXZKtUPumGfIwJxqOCFRApnDhQWobLkESYMTN"))

(de cipher (Str D)
   (let (K *K  A *A)
      (and D (xchg 'A 'K))
      (pack
         (mapcar
            '((N)
               (or
                  (pick
                     '((A K) (and (= A N) K))
                     A
                     K )
                  N ) )
            (chop Str) ) ) ) )
(and
   (println 'encode (cipher "The quick brown fox jumped over the lazy dog's back"))
   (println 'decode (cipher @ T)) )
```

{{out}}

```txt
encode "tFq oERJp wbQYh OQM AEDWqx QSqb kFq nINT xQC'L wIJp"
decode "The quick brown fox jumped over the lazy dog's back"
```



## Python


```Python

from string import printable
import random

EXAMPLE_KEY = ''.join(sorted(printable, key=lambda _:random.random()))

def encode(plaintext, key):
    return ''.join(key[printable.index(char)] for char in plaintext)

def decode(plaintext, key):
    return ''.join(printable[key.index(char)] for char in plaintext)

original = "A simple example."
encoded = encode(original, EXAMPLE_KEY)
decoded = decode(encoded, EXAMPLE_KEY)
print("""The original is: {}
Encoding it with the key: {}
Gives: {}
Decoding it by the same key gives: {}""".format(
    original, EXAMPLE_KEY, encoded, decoded))
```


A straightforward implementation. The output is:

<lang>The original is: A simple example.
Encoding it with the key: dV1>r7:TLlJa�uY o]MjH\hI^X	cPN#!fmv[
<e=04|O'~{y$bAq@}U.WtF*)x/K?
Q%S(�RB;25&s6Z9C3+D-_8kn,`Egiwzp"G
Gives: 
iPMhX\YiYmJhX\Y5
Decoding it by the same key gives: A simple example.
```



## Racket


Uses [[#REXX]] input file (in data/substitution.in.txt).

The check-equal? tests assure us that we return the plain text after a cypher/decypher pair;
so I don't display the plaintext in the output.


```racket
#lang racket/base
(require racket/list racket/function racket/file)

(define abc "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

;; Used to generate my-key for examples
(define (random-key (alphabet abc))
  (list->string (shuffle (string->list alphabet))))

(define (cypher/decypher key (alphabet abc))
  ;; alist is fine, hashes are better over 40 chars... so alist for
  ;; abc, hash for ASCII.
  (define ab-chars (string->list alphabet))
  (define ky-chars (string->list key))
  (define cypher-alist (map cons ab-chars ky-chars))
  (define decypher-alist (map cons ky-chars ab-chars))
  (define ((subst-map alist) str)
    (list->string (map (lambda (c) (cond [(assoc c alist) => cdr] [else c]))
                       (string->list str))))
  (values (subst-map cypher-alist)
          (subst-map decypher-alist)))

(define (cypher/decypher-files key (alphabet abc))
  (define-values (cypher decypher) (cypher/decypher key alphabet))
  (define ((convert-file f) in out #:exists (exists-flag 'error))
     (curry with-output-to-file out #:exists exists-flag
            (lambda () (display (f (file->string in))))))
  (values (convert-file cypher)
          (convert-file decypher)))

(module+ test
  (require rackunit)
  (define my-key "LXRWzUrIYPJiVQyMwKudbAaDjSEefvhlqmOkGcBZCFsNpxHTgton")
  
  (define-values (cypher decypher) (cypher/decypher my-key abc))
  
  (define in-text #<<T
The quick brown fox...
.. jumped over
the lazy dog!
T
    )
  (define cypher-text (cypher in-text))
  
  (define plain-text (decypher cypher-text))
  (displayln cypher-text)
  (check-equal? plain-text in-text)
  
  (define-values (file-cypher file-decypher) (cypher/decypher-files my-key abc))
  (file-cypher "data/substitution.in.txt" "data/substitution.crypt.txt" #:exists 'replace)
  (file-decypher "data/substitution.crypt.txt" "data/substitution.plain.txt" #:exists 'replace)
  (displayln "---")
  (displayln (file->string "data/substitution.crypt.txt"))
  (check-equal? (file->string "data/substitution.in.txt")
                (file->string "data/substitution.plain.txt")))
```


{{out}}

```txt
dmh sHOfG eNCgZ lCt...
.. kHBFhv CThN
xmh cEno vCq!
---
             "zThNo  xCCc  mEp  E  pFhfOlOf  'xmNhpmCcv',
               EZv gmhZ xmh HphN htfhhvp xmh xmNhpmCcv,
           xmh xCCc ehfCBhp E mOZvNEZfh NExmhN xmEZ E mhcF.
          LZo pClxgENh Hphv ehoCZv Oxp xmNhpmCcv gOcc fEHph
              vhfNhEphp OZ FNCvHfxOTOxo, ZCx OZfNhEphp.
       LZv gmhZ OZvOTOvHEc FNCvHfxp ENh FHx xCqhxmhN FOhfhBhEc,
       xmh NhpHcx Op E popxhB xmEx mEp qCZh FEpx Oxp xmNhpmCcv
       ─── E fCHZxhNFNCvHfxOTh fCcchfxOCZ Cl BOplOx HZOxp xmEx
        gCNG EqEOZpx hEfm CxmhN NExmhN xmEZ gOxm hEfm CxmhN."
                               ─── VhNch MENGp


```



## REXX

Programming notes:   the cipher key (as used by this REXX program) is stored
in a file as two records:
::*   the 1<sup>st</sup> record is the plain-text characters to be encrypted.
::*   the 2<sup>nd</sup> record is the crypt-text characters used for encryption.
::*   the two records should be equal in the number of characters.
::*   the N<sup>th</sup> character of record   '''1'''   will be encrypted to the N<sup>th</sup> character of record   '''2'''.

```rexx
/*REXX program implements & demonstrates a substitution cipher for the records in a file*/
parse arg fid.1 fid.2 fid.3 fid.4 .              /*obtain optional arguments from the CL*/
if fid.1==''  then fid.1= "CIPHER.IN"            /*Not specified?  Then use the default.*/
if fid.2==''  then fid.2= "CIPHER.OUT"           /* "      "         "   "   "     "    */
if fid.3==''  then fid.3= "CIPHER.KEY"           /* "      "         "   "   "     "    */
if fid.4==''  then fid.4= "CIPHER.ORI"           /* "      "         "   "   "     "    */
say '    input file: '   fid.1                   /*display the fileID used for  input.  */
say '   output file: '   fid.2                   /*   "     "     "     "   "  output.  */
say '   cipher file: '   fid.3                   /*   "     "     "     "   " cipher-key*/
say 'decrypted file: '   fid.4                   /*   "     "     "     "   "  decrypted*/
call closer                                      /*close all files in case they're open.*/
say
       do c=1  while lines(fid.3)\==0            /*read (hopefully 2 records) from key. */
       @.c=space(linein(fid.3),0)                /*assign input record to an  @.  array.*/
       end   /*c*/
c=c-1                                            /*adjust the number of recores (for DO)*/
if c==0                       then call ser fid.3,  'not found or is empty.'
if c>2                        then call ser fid.3,  'has too many records  (>2).'
if c<2                        then call ser fid.3,  'has too few records   (<2).'
if length(@.1)\==length(@.2)  then call ser fid.3,  'has unequal length records.'
call encrypt  fid.1, fid.2                       /*encrypt the input file  ───►  output.*/
_=@.1;    @.1=@.2;   @.2=_                       /*switch the cipher keys for decryption*/
call encrypt  fid.2, fid.4                       /*decrypt the output file ───► decrypt.*/
call show     'cipher file ('fid.3")" , fid.3    /*display the cipher─key file.         */
call show      'input file ('fid.1")" , fid.1    /*   "     "     input     "           */
call show     'output file ('fid.2")" , fid.2    /*   "     "    output     "           */
call show ' decrypted file ('fid.4")" , fid.4    /*   "     "   decrypted   "           */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
closer:  do f=1  for 4;   call lineout fid.f;    end  /*f*/;                     return
ser:     say  '***error!***  file '     arg(1)" "    arg(2);                     exit
show:    say;   say center(arg(1),79,'═');       "TYPE"  arg(2);                 return
/*──────────────────────────────────────────────────────────────────────────────────────*/
encrypt: parse arg @in,@out;  'ERASE'  @out      /*delete the output file using  ERASE. */
                                  do j=1  while lines(@in)\==0
                                  call lineout @out, translate(linein(@in), @.2, @.1)
                                  end   /*j*/
         j=j-1                                   /*adjust the number of recores (for DO)*/
         if j==0  then call ser @in, 'is empty.' /*was the file not found or was empty? */
         say @in  ' records processed: '   j     /*show the number of records processed.*/
         call closer                             /*close all the files to be neat & safe*/
         return
```

'''output'''   when using the default input files:

```txt

    input file:  CIPHER.IN
   output file:  CIPHER.OUT
   cipher file:  CIPHER.KEY
decrypted file:  CIPHER.ORI

CIPHER.IN  records processed:  10
CIPHER.OUT  records processed:  10

═══════════════════════════cipher file (CIPHER.KEY)════════════════════════════
abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ
WXYZabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUV

════════════════════════════input file (CIPHER.IN)═════════════════════════════
             "Every  tool  has  a  specific  'threshold',
               and when the user exceeds the threshold,
           the tool becomes a hindrance rather than a help.
          Any software used beyond its threshold will cause
              decreases in productivity, not increases.
       And when individual products are put together piecemeal,
       the result is a system that has gone past its threshold
       ─── a counterproductive collection of misfit units that
        work against each other rather than with each other."
                               ─── Merle Parks

═══════════════════════════output file (CIPHER.OUT)════════════════════════════
             "Aranu  pkkh  dWo  W  olaYebeY  'pdnaodkhZ',
               WjZ sdaj pda qoan atYaaZo pda pdnaodkhZ,
           pda pkkh XaYkiao W dejZnWjYa nWpdan pdWj W dahl.
          wju okbpsWna qoaZ XaukjZ epo pdnaodkhZ sehh YWqoa
              ZaYnaWoao ej lnkZqYperepu, jkp ejYnaWoao.
       wjZ sdaj ejZereZqWh lnkZqYpo Wna lqp pkcapdan leaYaiaWh,
       pda naoqhp eo W ouopai pdWp dWo ckja lWop epo pdnaodkhZ
       ─── W YkqjpanlnkZqYpera YkhhaYpekj kb ieobep qjepo pdWp
        skng WcWejop aWYd kpdan nWpdan pdWj sepd aWYd kpdan."
                               ─── Ianha LWngo

═════════════════════════ decrypted file (CIPHER.ORI)══════════════════════════
             "Every  tool  has  a  specific  'threshold',
               and when the user exceeds the threshold,
           the tool becomes a hindrance rather than a help.
          Any software used beyond its threshold will cause
              decreases in productivity, not increases.
       And when individual products are put together piecemeal,
       the result is a system that has gone past its threshold
       ─── a counterproductive collection of misfit units that
        work against each other rather than with each other."
                               ─── Merle Parks

```



## Ring


```ring

# Project : Substitution Cipher

plaintext = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ciphertext = "ZEBRASCDFGHIJKLMNOPQTUVWXY"
test = "flee at once. we are discovered!"
encrypt = "SIAA ZQ LKBA. VA ZOA RFPBLUAOAR!"

see "Plaintext : " + plaintext + nl
see "Ciphertext : " + ciphertext + nl
see "Test : " + test + nl
see "Encoded : "
encodetext = encode(test)
see encodetext + nl
see "Decoded : "
decodetext = decode(encodetext)
see decodetext + nl

func encode(test)
        str = ""
        for n = 1 to len(test)
              pos = substr(plaintext, upper(test[n]))
              if test[n] = " "
                 str = str + " "
              elseif test[n] = "!"
                 str = str + "!"
              elseif test[n] = "."
                 str = str + "."
              else
                 str = str + substr(ciphertext, pos, 1)
              ok
        next
        return str

func decode(test)
        str = ""
        for n = 1 to len(encodetext)
              pos = substr(ciphertext, upper(encodetext[n]))
              if test[n] = " "
                 str = str + " "
              elseif test[n] = "!"
                 str = str + "!"
              elseif test[n] = "."
                 str = str + "."
              else
                 str = str + lower(substr(plaintext, pos, 1))
              ok
        next
        return str

```

Output:

```txt

Plaintext : ABCDEFGHIJKLMNOPQRSTUVWXYZ
Ciphertext : ZEBRASCDFGHIJKLMNOPQTUVWXY
Test : flee at once. we are discovered!
Encoded : SIAA ZQ LKBA. VA ZOA RFPBLUAOAR!
Decoded : flee at once. we are discovered!

```



## Scala


```Scala
object SubstitutionCipher extends App {
  private val key = "]kYV}(!7P$n5_0i R:?jOWtF/=-pe'AD&@r6%ZXs\"v*N" + "[#wSl9zq2^+g;LoB`aGh{3.HIu4fbK)mU8|dMET><,Qc\\C1yxJ"
  private val text =
    """"It was still dark, in the early morning hours of the twenty-second of December
      | 1946, on the second floor of the house at Schilderskade 66 in our town,
      | when the hero of this story, Frits van Egters, awoke."""".stripMargin

  val enc = encode(text)
  println("Encoded: " + enc)
  println("Decoded: " + decode(enc))

  private def encode(s: String) = {
    val sb = new StringBuilder(s.length)
    s.map {
      case c if (' ' to '~').contains(c) => sb.append(key(c.toInt - 32))
      case _ =>
    }
    sb.toString
  }

  private def decode(s: String) = {
    val sb = new StringBuilder(s.length)
    s.map {
      case c if (' ' to '~').contains(c) =>
        sb.append((key.indexOf(c.toInt) + 32).toChar)
      case _ =>
    }
    sb.toString
  }
}
```

{{Out}}See it running in your browser by [https://scalafiddle.io/sf/f9yNWk7/0 ScalaFiddle (JavaScript, non JVM)] or by [https://scastie.scala-lang.org/8CyCsxnnRZyn0JH0yegndA Scastie (JVM)].

## Sidef

{{trans|Julia}}

```ruby
module SubstitutionCipher {

    const key = %c"]kYV}(!7P$n5_0i R:?jOWtF/=-pe'AD&@r6%ZXs\"v*N[#wSl9zq2^+g;LoB`aGh{3.HIu4fbK)mU8|dMET><,Qc\\C1yxJ"

    func encode(String s) {
        var r = ""
        s.each {|c|
            r += key[c.ord - 32]
        }
        return r
    }

    func decode(String s) {
        var r = ""
        s.each {|c|
            r += (key.first_index { _ == c } + 32 -> chr)
        }
        return r
    }
}


with ("The quick brown fox jumps over the lazy dog, who barks VERY loudly!") { |s|
    var enc = SubstitutionCipher::encode(s)
    var dec = SubstitutionCipher::decode(enc)
    say("Original: ", s, "\n -> Encoded: ", enc, "\n -> Decoded: ", dec)
}
```

{{out}}

```txt
Original: The quick brown fox jumps over the lazy dog, who barks VERY loudly!
 -> Encoded: 2bu]E,KHm].Tdc|]4d\]),8M>]dQuT]<bu]U31C]Idf_]cbd].3Tm>]+ZzL]Ud,IUCk
 -> Decoded: The quick brown fox jumps over the lazy dog, who barks VERY loudly!
```



## Tcl


Here we implement a <tt>SubCipher</tt> class with three public methods:

* <tt>key <i>?newkey?</i></tt> -- set or return the current substitution key.  This validates the key for correspondence with the alphabet, returning an error if something is not right.
* <tt>enc <i>plaintext</i></tt> -- encrypt text with the current key
* <tt>dec <i>ciphertext</i></tt> -- decrypt text with the current key

The default alphabet is <tt>a-zA-Z</tt>, but can be overridden by providing an argument to the constructor.  A random initial key will be generated at construction time, unless that is also provided as an argument.


```Tcl
oo::class create SubCipher {
    variable Alphabet
    variable Key
    variable EncMap
    variable DecMap
    constructor {{alphabet abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ} {cipherbet ""}} {
        set Alphabet $alphabet
        if {$cipherbet eq ""} {
            my key [my RandomKey]
        } else {
            my key $cipherbet
        }
    }
    method key args {
        if {$args eq ""} {
            return $Key
        } elseif {[llength $args] > 1} {
            throw {TCL WRONGARGS} "Expected \"[self class] key\" or \"[self class]\" key keystring"
        }
        lassign $args s

        set size [string length $Alphabet]
        if {[string length $s] != $size} {
            return -code error "Key must be $size chars long!"
        }
        set encmap {}
        set decmap {}
        foreach c [split $Alphabet {}] e [split $s {}] {
            dict set encmap $c $e
            dict set decmap $e $c
        }
        if {[dict size $encmap] != $size} {
            return -code error "Alphabet has repeating characters!"
        }
        if {[dict size $decmap] != $size} {
            return -code error "Key has repeating characters!"
        }
        set Key $s
        set EncMap $encmap
        set DecMap $decmap
    }
    method RandomKey {} {
        set chars $Alphabet
        set key {}
        for {set n [string length $chars]} {$n > 0} {incr n -1} {
            set i [expr {int(rand()*$n)}]
            append key [string index $chars $i]
            set chars [string range $chars 0 $i-1][string range $chars $i+1 end]
        }
        return $key
    }

    method enc {s} {
        string map $EncMap $s
    }
    method dec {s} {
        string map $DecMap $s
    }
}
```


Testing looks like this:


```Tcl
SubCipher create sc
set text [read [open /etc/motd]]
puts "Original:\n$text\n----\n"
puts "Ciphered:\n[set cipher [sc enc $text]]\n----\n"
puts "Decrypted:\n[sc dec $cipher]\n----\n"
puts "Key:\n[sc key]\n----\n"
```


{{out}}

```txt
Original:

The programs included with the Debian GNU/Linux system are free software;
the exact distribution terms for each program are described in the
individual files in /usr/share/doc/*/copyright.

Debian GNU/Linux comes with ABSOLUTELY NO WARRANTY, to the extent
permitted by applicable law.

----

Ciphered:

eVv yzhQzGwm KnLjDFvF lKZV ZVv CvuKGn MgA/rKnDc mJmZvw Gzv Xzvv mhXZlGzv;
ZVv vcGLZ FKmZzKuDZKhn Zvzwm Xhz vGLV yzhQzGw Gzv FvmLzKuvF Kn ZVv
KnFKOKFDGj XKjvm Kn /Dmz/mVGzv/FhL/*/LhyJzKQVZ.

CvuKGn MgA/rKnDc Lhwvm lKZV TbHIrAeWrY gI ETUUTgeY, Zh ZVv vcZvnZ
yvzwKZZvF uJ GyyjKLGujv jGl.

----

Decrypted:

The programs included with the Debian GNU/Linux system are free software;
the exact distribution terms for each program are described in the
individual files in /usr/share/doc/*/copyright.

Debian GNU/Linux comes with ABSOLUTELY NO WARRANTY, to the extent
permitted by applicable law.

----

Key:
GuLFvXQVKRNjwnhyozmZDOlcJBTbfCWdMkxqprtgIasUHeAiESYP
----

```



## zkl


```zkl
class SubstitutionCipher{
   // 92 characters: " !"#$%&" ... "xyz{|}", doesn't include "~"
   const KEY="]kYV}(!7P$n5_0i R:?jOWtF/=-pe'AD&@r6%ZXs\"v*N"
             "[#wSl9zq2^+g;LoB`aGh{3.HIu4fbK)mU8|dMET><,Qc\\C1yxJ";
   fcn encode(s){ s.apply(fcn(c){ try{ KEY[c.toAsc()-32] }catch{ c } }) }
   fcn decode(s){ s.apply(fcn(c){ try{ (KEY.index(c)+32).toChar() }catch{ c } }) }
}
```


```zkl
text:="Here we have to do is there will be a input/source "
      "file in which we are going to Encrypt the file by replacing every "
      "upper/lower case alphabets of the source file with another "
      "predetermined upper/lower case alphabets or symbols and save "
      "it into another output/encrypted file and then again convert "
      "that output/encrypted file into original/decrypted file. This "
      "type of Encryption/Decryption scheme is often called a "
      "Substitution Cipher.";
encoded:=SubstitutionCipher.encode(text);
println(  "Encoded: ",encoded);
println("\nDecoded: ",SubstitutionCipher.decode(encoded));
```

{{out}}

```txt

Encoded: "uTu]cu]b3Qu]<d]Id]K>]<buTu]cKUU].u]3]K|M,< >d,THu]4KUu]K|]cbKHb]cu]3Tu]fdK|f]<d]Z|HTCM<]<bu]4KUu].C]TuMU3HK|f]uQuTC],MMuT UdcuT]H3>u]3UMb3.u<>]d4]<bu]>d,THu]4KUu]cK<b]3|d<buT]MTuIu<uT8K|uI],MMuT UdcuT]H3>u]3UMb3.u<>]dT]>C8.dU>]3|I]>3Qu]K<]K|<d]3|d<buT]d,<M,< u|HTCM<uI]4KUu]3|I]<bu|]3f3K|]Hd|QuT<]<b3<]d,<M,< u|HTCM<uI]4KUu]K|<d]dTKfK|3U IuHTCM<uI]4KUui]2bK>]<CMu]d4]Z|HTCM<Kd| %uHTCM<Kd|]>Hbu8u]K>]d4<u|]H3UUuI]3]q,.><K<,<Kd|]6KMbuTi

Decoded: Here we have to do is there will be a input/source file in which we are going to Encrypt the file by replacing every upper/lower case alphabets of the source file with another predetermined upper/lower case alphabets or symbols and save it into another output/encrypted file and then again convert that output/encrypted file into original/decrypted file. This type of Encryption/Decryption scheme is often called a Substitution Cipher.

```

