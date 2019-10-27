+++
title = "One-time pad/Phix"
description = ""
date = 2018-11-10T23:32:44Z
aliases = []
[extra]
id = 22065
[taxonomies]
categories = []
tags = []
+++


## Phix

A simple xor-based one time pad that allows any file to be encrypted and decrypted,
since it seems silly to limit this to plaintext.
The one time pad files are therefore binary rather than plain text, with comments
and used counts in separate .1te/.1td files, rather than #/- in the .1tp itself.
Uses a trivial xor rather than a Vigenere cipher, since the latter adds nothing.
Ignores the "true random numbers, eg from /dev/random" part, especially since the
wikipedia page cites /dev/random as potentially unsuitable for cryptographic use. 
Instead this just uses the builtin rand(): for better security I might suggest you
replace that, possibly with several different (pseudo) random number generators. 

The dump feature is only for testing, obviously, as is decrypt defaulting to the
last encrypted message/file/pad. Note however that .1te/.1td handling is not: if
you try to make a one time pad "bi-directional", then not only would there be some
confusion and re-alignment issues should both parties send a message at the same
time, but two messages encrypted with the same part of the one time pad severely
weakens the security of those two messages. To prevent that, delete/do not share
the .1te or .1td file, and it will not allow encryption/decryption respectively.

Note that options 4 (Encrypt plain text) and 5 (Decrypt cipher text) are also 
only suitable for testing: you cannot actually enter anything for option 5, it 
just decodes whatever was last encoded by option 4, however it would be trivial 
to store the output of option 4 in a file and send that, and the recipient would 
then decode it using option 7 (Decrypt file).

TIP: if a message gets lost, or you quit without decrypting something, then edit
the recipient's .1td file manually, so that Used matches the sender's .1te file.

```Phix
string default_enc = "",
       default_dec = ""

function size_used(string name, integer now_used=0)
    object txt = read_lines(name)
    if sequence(txt) then
        integer size, used
        for i=1 to length(txt) do
            if match("Size",txt[i])=1 then
                {{size}} = scanf(txt[i],"Size: %d")
            elsif match("Used",txt[i])=1 then
                if now_used=0 then
                    {{used}} = scanf(txt[i],"Used: %d")
                    return {size,used}
                else
                    txt[i] = sprintf("Used: %d",now_used)
                    if not write_lines(name,txt) then
                        printf(1,"error updating %s\n",{name})
                    end if
                    return 1
                end if
            end if
        end for
    end if
    return {-1,-1}
end function

procedure list_one_time_pads(bool set_default=false)
    puts(1,"\n")
    sequence d = dir("*.1t?")
    if d={} then
        printf(1,"No one time pad files found.\n")
        return
    elsif set_default then
        sequence bases = {},
                 types = {{},{},{}}     -- p/e/d
        for i=1 to length(d) do
            string name = d[i][D_NAME]
            integer dot = find('.',name)
            if dot!=0 then
                string base = name[1..dot-1],
                       ext = name[dot+1..$]
                integer e = find(ext,{"1tp","1te","1td"})
                if e!=0 then
                    integer k = find(base,bases)
                    if k=0 then
                        bases = append(bases,base)
                        k = length(bases)
                    end if
                    types[e] &= k
                end if
            end if
        end for
        default_enc = iff(length(types[2])=1?bases[types[2][1]]&".1te":"")
        default_dec = iff(length(types[3])=1?bases[types[3][1]]&".1td":"")
    else
        for i=1 to length(d) do
            sequence di = d[i]
            string name = di[D_NAME],
                   info = iff(name[$]='p'?sprintf("Size: %d",di[D_SIZE])
                         :sprintf("Size: %d, Used: %d",size_used(name)))
            printf(1,"%s  Created: %02d/%02d/%04d, %s\n",
                     {name,di[D_DAY],di[D_MONTH],di[D_YEAR],info})
        end for
    end if
end procedure

procedure dump_hex(sequence s)
    if length(s)=0 then
        printf(1,"{}\n")
    else
        for i=1 to length(s) do
            printf(1,"%02x%c",{s[i],iff(mod(i,16)=0?'\n':' ')})
        end for
    end if
end procedure

procedure dump_file(string file, bool ashex=false)
    printf(1,"The contents of %s:\n\n",{file})
    if match(".1tp",file) or ashex then
        integer fn = open(file,"rb")
        dump_hex(get_bytes(fn, 64))
        close(fn)
        puts(1,iff(get_file_size(file)>64?"...\n\n","\n"))
    else
        puts(1,join(read_lines(file),"\n")&"\n")
    end if
end procedure

procedure create_one_time_pad()
    string filename = prompt_string("OTP file name to create (without extension):")
    if find('.',filename) then
        puts(1,"extension not allowed\n")
    elsif filename!="" then
        string pfile = filename&".1tp",
               efile = filename&".1te",
               dfile = filename&".1td"
        if file_exists(pfile) then
            string del = lower(prompt_string("File already exists - delete?:"))
            if not find(del,{"y","yes"}) then return end if
            if not delete_file(pfile) then
                printf(1,"error deleting %s\n",{pfile})
                return
            end if
        end if
        string comment = prompt_string("comment (optional):")
        integer filesize
        while 1 do
            string sizestr = prompt_string("file size (1MB):")
            sequence res = iff(sizestr=""?{{1,"MB"}}:scanf(sizestr,"%d%s"))
            if length(res)=1 then
                integer k = find(upper(res[1][2]),{"","K","KB","M","MB","G","GB"})
                if k!=0 then
                    k = {1,1024,1024*1024,1024*1204}[floor(k/2)+1]
                    filesize = res[1][1]*k
                    exit
                end if
                printf(1,"unknown suffix:%s - use K|M|G or KB|MB|GB\n",{res[1][2]})
            end if
        end while
        printf(1,"Creating %s (%d bytes)\n",{pfile,filesize})
        integer fn = open(pfile,"wb")
        if fn=-1 then
            printf(1,"error opening %s\n",{pfile})
            return
        end if
        for i=1 to filesize do
            puts(fn,rand(256)-1)
        end for
        close(fn)
        fn = open(efile,"w")
        if fn=-1 then
            printf(1,"error opening %s\n",{efile})
            return
        end if
        printf(fn,"# OTP file\n# %s\nSize: %d\nUsed: 0\n",{comment,filesize})
        close(fn)
        if not copy_file(efile,dfile,true) then
            printf(1,"error copying %s to %s\n",{efile,dfile})
            return
        end if
        printf(1,"\n%s and %s and %s have been created in the current directory.\n\n",
                 {pfile,efile,dfile})
        dump_file(pfile)
        dump_file(efile)
        default_enc = efile
        default_dec = dfile
    end if
end procedure

procedure delete_one_time_pad()
    string filename = prompt_string("OTP file name to delete (without extension):")
    if find('.',filename) then
        puts(1,"extension not allowed\n")
    elsif filename!="" then
        string efile = filename&".1te",
               dfile = filename&".1td"
        filename &= ".1tp"
        if not file_exists(filename) then
            printf(1,"file %s found\n",{filename})
            return
        end if
        string del = lower(prompt_string("Delete file - are you sure?:"))
        if not find(del,{"y","yes"}) then
            printf(1,"file not deleted\n")
            return
        end if
        if not delete_file(filename) then
            printf(1,"error deleting %s\n",{filename})
            return
        end if
        integer c = 1
        c += delete_file(efile)
        c += delete_file(dfile)
        printf(1,"%d file%s successfully deleted.\n",{c,iff(c=1?"":"s")})
        if default_enc=efile then default_enc = "" end if
        if default_dec=dfile then default_dec = "" end if
    end if
end procedure

sequence msg = {}

procedure crypt_text(integer de)
    sequence text = iff(de='d'?msg:prompt_string("message:"))
    string dflt = iff(de='d'?default_dec:default_enc),
           otp = iff(length(dflt)?"OTP ("&dflt&"):":"OTP:"),
           efile = prompt_string(otp)
    if efile="" then efile=dflt end if
    if length(efile) then
        if not find('.',efile) then efile &= ".1t"&de end if
        integer {size,used} = size_used(efile)
        if size=-1 or used=-1 then
            printf(1,"File %s cannot be opened\n",{efile})
        elsif used+length(text)>size then
            printf(1,"OTP exhausted (%d>%d)\n",{used+length(text),size})
        else
            efile[$] = 'p'
            integer fn = open(efile,"rb")
            if seek(fn,used)!=SEEK_OK then
                printf(1,"Error in seek(%s,%d)\n",{efile,fn})
                text = ""
            else    
                msg = get_bytes(fn,length(text))
                msg = sq_xor_bits(msg,text)
                if de='e' then
                    printf(1,"Encrypted:\n")
                    dump_hex(msg)
                    printf(1,"\n\n")
                else
                    printf(1,"Decrypted: %s\n\n",{msg})
                end if
            end if
            close(fn)
            efile[$] = de
            if length(text)!=0 then
                {} = size_used(efile, used+length(text))
            end if
            dump_file(efile)
        end if
    end if
end procedure

string last_file = ""

procedure crypt_file(integer de)
    string file = iff(de='d' and last_file!=""?"file ("&last_file&"):":"file:"),
           filename = prompt_string(file)
    if de='d' and filename="" then filename=last_file end if
    if filename="" then return end if
    if not file_exists(filename) then
        printf(1,"The file %s does not exist\n",{filename})
        return
    end if
    atom filesize = get_file_size(filename)
    integer fn = open(filename,"rb")
    if fn=-1 then
        printf(1,"error opening %s\n",{filename})
        return
    end if
    sequence bytes = get_bytes(fn,filesize)
    close(fn)
    if length(bytes)!=filesize then ?9/0 end if -- uh?
    if de='e' then
        dump_file(filename, true)
    end if
    string outfile = prompt_string("output:")
    if length(outfile)=0 then return end if
    integer outfn = open(outfile,"wb")
    if outfn=-1 then
        printf(1,"error opening output file %s\n",{outfile})
        return
    end if
    string dflt = iff(de='d'?default_dec:default_enc),
           otp = iff(length(dflt)?"OTP ("&dflt&"):":"OTP:"),
           efile = prompt_string(otp)
    if efile="" then efile=dflt end if
    if length(efile) then
        if not find('.',efile) then efile &= ".1t"&de end if
        integer {size,used} = size_used(efile)
        if size=-1 or used=-1 then
            printf(1,"File %s cannot be opened\n",{efile})
        elsif used+filesize>size then
            printf(1,"OTP exhausted (%d>%d)\n",{used+filesize,size})
        else
            efile[$] = 'p'
            fn = open(efile,"rb")
            if seek(fn,used)!=SEEK_OK then
                printf(1,"Error in seek(%s,%d)\n",{efile,fn})
                filesize = 0
            else    
                msg = get_bytes(fn,filesize)
                msg = sq_xor_bits(msg,bytes)
                puts(outfn,msg)
            end if
            close(fn)
            close(outfn)
            efile[$] = de
            if filesize!=0 then
                {} = size_used(efile, used+filesize)
            end if
            if de='d' then
                dump_file(outfile, true)
            else
                last_file = outfile
            end if
            dump_file(efile)
        end if
    end if
end procedure

list_one_time_pads(set_default:=true)

-- note: the trailing space after the opening """ is deliberate:
constant menu_txt = """ 
1. List one time pad files.
2. Create one time pad file.
3. Delete one time pad file.
4. Encrypt plain text.
5. Decrypt cipher text.
6. Encrypt file.
7. Decrypt file.
8. Quit program.
Your choice (1 to 8) : """

while 1 do
    integer choice = prompt_number(menu_txt,{1,8})
    switch choice do
        case 1: list_one_time_pads()
        case 2: create_one_time_pad()
        case 3: delete_one_time_pad()
        case 4: crypt_text('e')
        case 5: crypt_text('d')
        case 6: crypt_file('e')
        case 7: crypt_file('d')
        case 8: exit
        default :puts(1,"not implemented...\n")
    end switch
end while
```

{{out}}
Sample session. Similar to Kotlin, menu display/selection has been shorted to <menu N. Xxxx.>


```txt

No one time pad files found.

1. List one time pad files.
2. Create one time pad file.
3. Delete one time pad file.
4. Encrypt plain text.
5. Decrypt cipher text.
6. Encrypt file.
7. Decrypt file.
8. Quit program.
Your choice (1 to 8) : 2
OTP file name to create (without extension):007
comment (optional):Pussy Galore
file size (1MB):
Creating 007.1tp (1048576 bytes)

007.1tp and 007.1te and 007.1td have been created in the current directory.

The contents of 007.1tp:

C3 A1 90 B2 70 E6 A2 DD 27 F4 3C 21 48 D1 54 1C
D7 7E AA 09 40 CC 49 83 F4 E3 96 86 1D 48 15 F6
A3 C2 5F 47 00 BE D1 6B 4E 4A EA 50 DB F8 62 2F
D5 66 D5 0F A9 63 E6 4D 65 BB 67 70 6F 51 34 73
...

The contents of 007.1te:

# OTP file
# Pussy Galore
Size: 1048576
Used: 0

<menu 1. List one time pad files.>
007.1td  Created: 10/11/2018, Size: 1048576, Used: 0
007.1te  Created: 10/11/2018, Size: 1048576, Used: 0
007.1tp  Created: 10/11/2018, Size: 1048576

<menu 4. Encrypt plain text.>
message:This is message 1234
OTP (007.1te):
Encrypted:
97 C9 F9 C1 50 8F D1 FD 4A 91 4F 52 29 B6 31 3C
E6 4C 99 3D

The contents of 007.1te:

# OTP file
# Pussy Galore
Size: 1048576
Used: 20

<menu 5. Decrypt cipher text.>
OTP (007.1td):
Decrypted: This is message 1234

The contents of 007.1td:

# OTP file
# Pussy Galore
Size: 1048576
Used: 20

<menu 8. Quit program.>

```

