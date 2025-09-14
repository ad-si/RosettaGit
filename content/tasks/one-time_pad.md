+++
title = "One-time pad"
description = ""
date = 2019-06-22T20:40:28Z
aliases = []
[extra]
id = 18243
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "go",
  "julia",
  "kotlin",
  "perl_6",
  "phix",
  "racket",
  "tcl",
]
+++

{{draft task}} [[Category:Encryption]] [[Category:File_handling]]
Implement a [[wp:One-time pad|One-time pad]], for encrypting and decrypting messages.

To keep it simple, we will be using letters only.

;Sub-Tasks:
* '''Generate''' the data for a One-time pad (user needs to specify a filename and length)
: The important part is to get "true random" numbers, e.g. from /dev/random
* '''encryption / decryption''' ( basically the same operation, much like [[Rot-13]] )
: For this step, much of [[Vigenère cipher]] could be reused,
with the key to be read from the file containing the One-time pad.
* optional: '''management''' of One-time pads: list, mark as used, delete, etc.
: Somehow, the users needs to keep track which pad to use for which partner.

To support the management of pad-files:
* Such files have a file-extension ".1tp"
* Lines starting with "#" may contain arbitary meta-data (i.e. comments)
* Lines starting with "-" count as "used"
* Whitespace within the otp-data is ignored
<!--
maybe support for 1tp-files on readonly-media,
i.e. an indexfile that stores which parts have been used
-->


For example, here is the data from [http://upload.wikimedia.org/wikipedia/commons/6/60/One-time_pad.svg Wikipedia]:

```txt

# Example data - Wikipedia - 2014-11-13
-ZDXWWW EJKAWO FECIFE WSNZIP PXPKIY URMZHI JZTLBC YLGDYJ
-HTSVTV RRYYEG EXNCGA GGQVRF FHZCIB EWLGGR BZXQDQ DGGIAK
 YHJYEQ TDLCQT HZBSIZ IRZDYS RBYJFZ AIRCWI UCVXTW YKPQMK
 CKHVEX VXYVCS WOGAAZ OUVVON GCNEVR LMBLYB SBDCDC PCGVJX
 QXAUIP PXZQIJ JIUWYH COVWMJ UZOJHL DWHPER UBSRUJ HGAAPR
 CRWVHI FRNTQW AJVWRT ACAKRD OZKIIB VIQGBK IJCWHF GTTSSE
 EXFIPJ KICASQ IOUQTP ZSGXGH YTYCTI BAZSTN JKMFXI RERYWE

```




## See also

* [https://breakingcode.wordpress.com/2010/02/17/one-time-pad-encryption-in-python/ one time pad encryption in Python]
* [https://github.com/snapfractalpop/1tp snapfractalpop] -  One-Time-Pad Command-Line-Utility (C).
* [http://search.cpan.org/~sifukurt/Crypt-OTP-2.00/OTP.pm Crypt-OTP-2.00] on CPAN (Perl)




## Go

```go
package main

import (
    "bufio"
    "crypto/rand"
    "fmt"
    "io/ioutil"
    "log"
    "math/big"
    "os"
    "strconv"
    "strings"
    "unicode"
)

const (
    charsPerLine = 48
    chunkSize    = 6
    cols         = 8
    demo         = true // would normally be set to false
)

type fileType int

const (
    otp fileType = iota
    enc
    dec
)

var scnr = bufio.NewScanner(os.Stdin)

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func toAlpha(s string) string {
    var filtered []rune
    for _, r := range s {
        if unicode.IsUpper(r) {
            filtered = append(filtered, r)
        }
    }
    return string(filtered)
}

func isOtpRelated(s string) bool {
    return strings.HasSuffix(s, ".1tp") || strings.HasSuffix(s, "1tp_cpy") ||
        strings.HasSuffix(s, ".1tp_enc") || strings.HasSuffix(s, "1tp_dec")
}

func makePad(nLines int) string {
    nChars := nLines * charsPerLine
    bytes := make([]byte, nChars)
    /* generate random upper case letters */
    max := big.NewInt(26)
    for i := 0; i < nChars; i++ {
        n, err := rand.Int(rand.Reader, max)
        check(err)
        bytes[i] = byte(65 + n.Uint64())
    }
    return inChunks(string(bytes), nLines, otp)
}

func vigenere(text, key string, encrypt bool) string {
    bytes := make([]byte, len(text))
    var ci byte
    for i, c := range text {
        if encrypt {
            ci = (byte(c) + key[i] - 130) % 26
        } else {
            ci = (byte(c) + 26 - key[i]) % 26
        }
        bytes[i] = ci + 65
    }
    temp := len(bytes) % charsPerLine
    if temp > 0 { // pad with random characters so each line is a full one
        max := big.NewInt(26)
        for i := temp; i < charsPerLine; i++ {
            n, err := rand.Int(rand.Reader, max)
            check(err)
            bytes = append(bytes, byte(65+n.Uint64()))
        }
    }
    ft := enc
    if !encrypt {
        ft = dec
    }
    return inChunks(string(bytes), len(bytes)/charsPerLine, ft)
}

func inChunks(s string, nLines int, ft fileType) string {
    nChunks := len(s) / chunkSize
    remainder := len(s) % chunkSize
    chunks := make([]string, nChunks)
    for i := 0; i < nChunks; i++ {
        chunks[i] = s[i*chunkSize : (i+1)*chunkSize]
    }
    if remainder > 0 {
        chunks = append(chunks, s[nChunks*chunkSize:])
    }
    var sb strings.Builder
    for i := 0; i < nLines; i++ {
        j := i * cols
        sb.WriteString(" " + strings.Join(chunks[j:j+cols], " ") + "\n")
    }
    ss := " file\n" + sb.String()
    switch ft {
    case otp:
        return "# OTP" + ss
    case enc:
        return "# Encrypted" + ss
    default: // case dec:
        return "# Decrypted" + ss
    }
}

func menu() int {
    fmt.Println(`
1. Create one time pad file.

2. Delete one time pad file.

3. List one time pad files.

4. Encrypt plain text.

5. Decrypt cipher text.

6. Quit program.
`)
    choice := 0
    for choice < 1 || choice > 6 {
        fmt.Print("Your choice (1 to 6) : ")
        scnr.Scan()
        choice, _ = strconv.Atoi(scnr.Text())
        check(scnr.Err())
    }
    return choice
}

func main() {
    for {
        choice := menu()
        fmt.Println()
        switch choice {
        case 1: // Create OTP
            fmt.Println("Note that encrypted lines always contain 48 characters.\n")
            fmt.Print("OTP file name to create (without extension) : ")
            scnr.Scan()
            fileName := scnr.Text() + ".1tp"
            nLines := 0
            for nLines < 1 || nLines > 1000 {
                fmt.Print("Number of lines in OTP (max 1000) : ")
                scnr.Scan()
                nLines, _ = strconv.Atoi(scnr.Text())
            }
            check(scnr.Err())
            key := makePad(nLines)
            file, err := os.Create(fileName)
            check(err)
            _, err = file.WriteString(key)
            check(err)
            file.Close()
            fmt.Printf("\n'%s' has been created in the current directory.\n", fileName)
            if demo {
                // a copy of the OTP file would normally be on a different machine
                fileName2 := fileName + "_cpy" // copy for decryption
                file, err := os.Create(fileName2)
                check(err)
                _, err = file.WriteString(key)
                check(err)
                file.Close()
                fmt.Printf("'%s' has been created in the current directory.\n", fileName2)
                fmt.Println("\nThe contents of these files are :\n")
                fmt.Println(key)
            }
        case 2: // Delete OTP
            fmt.Println("Note that this will also delete ALL associated files.\n")
            fmt.Print("OTP file name to delete (without extension) : ")
            scnr.Scan()
            toDelete1 := scnr.Text() + ".1tp"
            check(scnr.Err())
            toDelete2 := toDelete1 + "_cpy"
            toDelete3 := toDelete1 + "_enc"
            toDelete4 := toDelete1 + "_dec"
            allToDelete := []string{toDelete1, toDelete2, toDelete3, toDelete4}
            deleted := 0
            fmt.Println()
            for _, name := range allToDelete {
                if _, err := os.Stat(name); !os.IsNotExist(err) {
                    err = os.Remove(name)
                    check(err)
                    deleted++
                    fmt.Printf("'%s' has been deleted from the current directory.\n", name)
                }
            }
            if deleted == 0 {
                fmt.Println("There are no files to delete.")
            }
        case 3: // List OTPs
            fmt.Println("The OTP (and related) files in the current directory are:\n")
            files, err := ioutil.ReadDir(".") // already sorted by file name
            check(err)
            for _, fi := range files {
                name := fi.Name()
                if !fi.IsDir() && isOtpRelated(name) {
                    fmt.Println(name)
                }
            }
        case 4: // Encrypt
            fmt.Print("OTP file name to use (without extension) : ")
            scnr.Scan()
            keyFile := scnr.Text() + ".1tp"
            if _, err := os.Stat(keyFile); !os.IsNotExist(err) {
                file, err := os.Open(keyFile)
                check(err)
                bytes, err := ioutil.ReadAll(file)
                check(err)
                file.Close()
                lines := strings.Split(string(bytes), "\n")
                le := len(lines)
                first := le
                for i := 0; i < le; i++ {
                    if strings.HasPrefix(lines[i], " ") {
                        first = i
                        break
                    }
                }
                if first == le {
                    fmt.Println("\nThat file has no unused lines.")
                    continue
                }
                lines2 := lines[first:] // get rid of comments and used lines

                fmt.Println("Text to encrypt :-\n")
                scnr.Scan()
                text := toAlpha(strings.ToUpper(scnr.Text()))
                check(scnr.Err())
                tl := len(text)
                nLines := tl / charsPerLine
                if tl%charsPerLine > 0 {
                    nLines++
                }
                if len(lines2) >= nLines {
                    key := toAlpha(strings.Join(lines2[0:nLines], ""))
                    encrypted := vigenere(text, key, true)
                    encFile := keyFile + "_enc"
                    file2, err := os.Create(encFile)
                    check(err)
                    _, err = file2.WriteString(encrypted)
                    check(err)
                    file2.Close()
                    fmt.Printf("\n'%s' has been created in the current directory.\n", encFile)
                    for i := first; i < first+nLines; i++ {
                        lines[i] = "-" + lines[i][1:]
                    }
                    file3, err := os.Create(keyFile)
                    check(err)
                    _, err = file3.WriteString(strings.Join(lines, "\n"))
                    check(err)
                    file3.Close()
                    if demo {
                        fmt.Println("\nThe contents of the encrypted file are :\n")
                        fmt.Println(encrypted)
                    }
                } else {
                    fmt.Println("Not enough lines left in that file to do encryption.")
                }
            } else {
                fmt.Println("\nThat file does not exist.")
            }
        case 5: // Decrypt
            fmt.Print("OTP file name to use (without extension) : ")
            scnr.Scan()
            keyFile := scnr.Text() + ".1tp_cpy"
            check(scnr.Err())
            if _, err := os.Stat(keyFile); !os.IsNotExist(err) {
                file, err := os.Open(keyFile)
                check(err)
                bytes, err := ioutil.ReadAll(file)
                check(err)
                file.Close()
                keyLines := strings.Split(string(bytes), "\n")
                le := len(keyLines)
                first := le
                for i := 0; i < le; i++ {
                    if strings.HasPrefix(keyLines[i], " ") {
                        first = i
                        break
                    }
                }
                if first == le {
                    fmt.Println("\nThat file has no unused lines.")
                    continue
                }
                keyLines2 := keyLines[first:] // get rid of comments and used lines

                encFile := keyFile[0:len(keyFile)-3] + "enc"
                if _, err := os.Stat(encFile); !os.IsNotExist(err) {
                    file2, err := os.Open(encFile)
                    check(err)
                    bytes, err := ioutil.ReadAll(file2)
                    check(err)
                    file2.Close()
                    encLines := strings.Split(string(bytes), "\n")[1:] // exclude comment line
                    nLines := len(encLines)
                    if len(keyLines2) >= nLines {
                        encrypted := toAlpha(strings.Join(encLines, ""))
                        key := toAlpha(strings.Join(keyLines2[0:nLines], ""))
                        decrypted := vigenere(encrypted, key, false)
                        decFile := keyFile[0:len(keyFile)-3] + "dec"
                        file3, err := os.Create(decFile)
                        check(err)
                        _, err = file3.WriteString(decrypted)
                        check(err)
                        file3.Close()
                        fmt.Printf("\n'%s' has been created in the current directory.\n", decFile)
                        for i := first; i < first+nLines; i++ {
                            keyLines[i] = "-" + keyLines[i][1:]
                        }
                        file4, err := os.Create(keyFile)
                        check(err)
                        _, err = file4.WriteString(strings.Join(keyLines, "\n"))
                        check(err)
                        file4.Close()
                        if demo {
                            fmt.Println("\nThe contents of the decrypted file are :\n")
                            fmt.Println(decrypted)
                        }
                    }
                } else {
                    fmt.Println("Not enough lines left in that file to do decryption.")
                }
            } else {
                fmt.Println("\nThat file does not exist.")
            }
        case 6: // Quit program
            return
        }
    }
}
```


```txt

Similar (not exactly the same, of course) as the Kotlin sample session.

```



## Julia

See [[One-time pad/Julia]]


## Kotlin

This uses the JDK's SecureRandom class for generating cryptographically strong random numbers. For convenience all three sub-tasks are catered for by a single, menu-based, program.

```scala
// version 1.2.31

import java.io.File
import java.security.SecureRandom

const val CHARS_PER_LINE = 48
const val CHUNK_SIZE = 6
const val COLS = 8
const val DEMO = true  // would normally be set to false

enum class FileType { OTP, ENC, DEC }

fun Char.isAlpha() = this in 'A'..'Z'

fun String.toAlpha() = this.filter { it.isAlpha() }

fun String.isOtpRelated() = endsWith(".1tp") || endsWith(".1tp_cpy") ||
                            endsWith(".1tp_enc") || endsWith(".1tp_dec")

fun makePad(nLines: Int): String {
    val nChars = nLines * CHARS_PER_LINE
    val sr = SecureRandom()
    val sb = StringBuilder(nChars)
    /* generate random upper case letters */
    for (i in 0 until nChars) sb.append((sr.nextInt(26) + 65).toChar())
    return sb.toString().inChunks(nLines, FileType.OTP)
}

fun vigenere(text: String, key: String, encrypt: Boolean = true): String {
    val sb = StringBuilder(text.length)
    for ((i, c) in text.withIndex()) {
        val ci = if (encrypt)
            (c.toInt() + key[i].toInt() - 130) % 26
        else
            (c.toInt() - key[i].toInt() +  26) % 26
        sb.append((ci + 65).toChar())
    }
    val temp = sb.length % CHARS_PER_LINE
    if (temp > 0) {  // pad with random characters so each line is a full one
        val sr = SecureRandom()
        for (i in temp until CHARS_PER_LINE) sb.append((sr.nextInt(26) + 65).toChar())
    }
    val ft = if (encrypt) FileType.ENC else FileType.DEC
    return sb.toString().inChunks(sb.length / CHARS_PER_LINE, ft)
}

fun String.inChunks(nLines: Int, ft: FileType): String {
    val chunks = this.chunked(CHUNK_SIZE)
    val sb = StringBuilder(this.length + nLines * (COLS + 1))
    for (i in 0 until nLines) {
        val j = i * COLS
        sb.append(" ${chunks.subList(j, j + COLS).joinToString(" ")}\n")
    }
    val s = " file\n" + sb.toString()
    return when (ft) {
        FileType.OTP -> "# OTP" + s
        FileType.ENC -> "# Encrypted" + s
        FileType.DEC -> "# Decrypted" + s
    }
}

fun menu(): Int {
    println("""
        |
        |1. Create one time pad file.
        |
        |2. Delete one time pad file.
        |
        |3. List one time pad files.
        |
        |4. Encrypt plain text.
        |
        |5. Decrypt cipher text.
        |
        |6. Quit program.
        |
        """.trimMargin())
    var choice: Int?
    do {
        print("Your choice (1 to 6) : ")
        choice = readLine()!!.toIntOrNull()
    }
    while (choice == null || choice !in 1..6)
    return choice
}

fun main(args: Array<String>) {
    mainLoop@ while (true) {
        val choice = menu()
        println()
        when (choice) {
            1 -> {  // Create OTP
                println("Note that encrypted lines always contain 48 characters.\n")
                print("OTP file name to create (without extension) : ")
                val fileName = readLine()!! + ".1tp"
                var nLines: Int?

                do {
                    print("Number of lines in OTP (max 1000) : ")
                    nLines = readLine()!!.toIntOrNull()
                }
                while (nLines == null || nLines !in 1..1000)

                val key = makePad(nLines)
                File(fileName).writeText(key)
                println("\n'$fileName' has been created in the current directory.")
                if (DEMO) {
                    // a copy of the OTP file would normally be on a different machine
                    val fileName2 = fileName + "_cpy"  // copy for decryption
                    File(fileName2).writeText(key)
                    println("'$fileName2' has been created in the current directory.")
                    println("\nThe contents of these files are :\n")
                    println(key)
                }
            }

            2 -> {  // Delete OTP
                println("Note that this will also delete ALL associated files.\n")
                print("OTP file name to delete (without extension) : ")
                val toDelete1 = readLine()!! + ".1tp"
                val toDelete2 = toDelete1 + "_cpy"
                val toDelete3 = toDelete1 + "_enc"
                val toDelete4 = toDelete1 + "_dec"
                val allToDelete = listOf(toDelete1, toDelete2, toDelete3, toDelete4)
                var deleted = 0
                println()
                for (name in allToDelete) {
                    val f = File(name)
                    if (f.exists()) {
                        f.delete()
                        deleted++
                        println("'$name' has been deleted from the current directory.")
                    }
                }
                if (deleted == 0) println("There are no files to delete.")
            }

            3 -> {  // List OTPs
                println("The OTP (and related) files in the current directory are:\n")
                val otpFiles = File(".").listFiles().filter {
                    it.isFile() && it.name.isOtpRelated()
                }.map { it.name }.toMutableList()
                otpFiles.sort()
                println(otpFiles.joinToString("\n"))
            }

            4 -> {  // Encrypt
                print("OTP file name to use (without extension) : ")
                val keyFile = readLine()!! + ".1tp"
                val kf = File(keyFile)
                if (kf.exists()) {
                    val lines = File(keyFile).readLines().toMutableList()
                    var first = lines.size
                    for (i in 0 until lines.size) {
                        if (lines[i].startsWith(" ")) {
                            first = i
                            break
                        }
                    }
                    if (first == lines.size) {
                        println("\nThat file has no unused lines.")
                        continue@mainLoop
                    }
                    val lines2 = lines.drop(first)  // get rid of comments and used lines

                    println("Text to encrypt :-\n")
                    val text = readLine()!!.toUpperCase().toAlpha()
                    val len = text.length
                    var nLines = len / CHARS_PER_LINE
                    if (len % CHARS_PER_LINE > 0) nLines++

                    if (lines2.size >= nLines) {
                        val key = lines2.take(nLines).joinToString("").toAlpha()
                        val encrypted = vigenere(text, key)
                        val encFile = keyFile + "_enc"
                        File(encFile).writeText(encrypted)
                        println("\n'$encFile' has been created in the current directory.")
                        for (i in first until first + nLines) {
                            lines[i] = "-" + lines[i].drop(1)
                        }
                        File(keyFile).writeText(lines.joinToString("\n"))
                        if (DEMO) {
                            println("\nThe contents of the encrypted file are :\n")
                            println(encrypted)
                        }
                    }
                    else println("Not enough lines left in that file to do encryption")
                }
                else println("\nThat file does not exist.")
            }

            5 -> {  // Decrypt
                print("OTP file name to use (without extension) : ")
                val keyFile = readLine()!! + ".1tp_cpy"
                val kf = File(keyFile)
                if (kf.exists()) {
                    val keyLines = File(keyFile).readLines().toMutableList()
                    var first = keyLines.size
                    for (i in 0 until keyLines.size) {
                        if (keyLines[i].startsWith(" ")) {
                            first = i
                            break
                        }
                    }
                    if (first == keyLines.size) {
                        println("\nThat file has no unused lines.")
                        continue@mainLoop
                    }
                    val keyLines2 = keyLines.drop(first)  // get rid of comments and used lines

                    val encFile = keyFile.dropLast(3) + "enc"
                    val ef = File(encFile)
                    if (ef.exists()) {
                        val encLines = File(encFile).readLines().drop(1)  // exclude comment line
                        val nLines = encLines.size
                        if (keyLines2.size >= nLines) {
                            val encrypted = encLines.joinToString("").toAlpha()
                            val key = keyLines2.take(nLines).joinToString("").toAlpha()
                            val decrypted = vigenere(encrypted, key, false)
                            val decFile = keyFile.dropLast(3) + "dec"
                            File(decFile).writeText(decrypted)
                            println("\n'$decFile' has been created in the current directory.")
                            for (i in first until first + nLines) {
                                keyLines[i] = "-" + keyLines[i].drop(1)
                            }
                            File(keyFile).writeText(keyLines.joinToString("\n"))
                            if (DEMO) {
                                println("\nThe contents of the decrypted file are :\n")
                                println(decrypted)
                            }
                        }
                        else println("Not enough lines left in that file to do decryption")
                    }
                    else println("\n'$encFile' is missing.")
                }
                else println("\nThat file does not exist.")
            }

            else -> return  // Quit
        }
    }
}
```


Input/output for a sample session. In the interests of brevity, --<menu>-- indicates the re-display of the menu after the previous choice has been processed:

```txt

1. Create one time pad file.

2. Delete one time pad file.

3. List one time pad files.

4. Encrypt plain text.

5. Decrypt cipher text.

6. Quit program.

Your choice (1 to 6) : 1

Note that encrypted lines always contain 48 characters.

OTP file name to create (without extension) : user1
Number of lines in OTP (max 1000) : 4

'user1.1tp' has been created in the current directory.
'user1.1tp_cpy' has been created in the current directory.

The contents of these files are :

# OTP file
 MZSAVH VHJLRS YLFXZB JWAHSN AHPCSE RUJMFX ZZPMFB QTTNLM
 LDANXY LGJEBV IRKDTR NHKOPG ZDUBDC KOOWZC HEBKWC YAHOFY
 DYPCXG TGCBXC VFETFM VOTHNI ZCGKNL QONXYE IDCROZ LHLWMN
 YWSGDO YYSNNV QMOJFM AHVOGP CDFAKM HZCWEX CEAXXV INKAEO


--<menu>--

Your choice (1 to 6) : 3

The OTP (and related) files in the current directory are:

user1.1tp
user1.1tp_cpy

--<menu>--

Your choice (1 to 6) : 4

OTP file name to use (without extension) : user1
Text to encrypt :-

Beware the Jabberwock, my son! The jaws that bite, the claws that catch!

'user1.1tp_enc' has been created in the current directory.

The contents of the encrypted file are :

# Encrypted file
 NDOAML OONURT ZPWTND TIYZGA TOTLSA JNQMYY HSTFMF SETJDF
 SDTPXR NNHVAO YSCVZU UTLAFM PEWGRH OJRCTF IOVEHK ZKETBQ


--<menu>--

Your choice (1 to 6) : 5

OTP file name to use (without extension) : user1

'user1.1tp_dec' has been created in the current directory.

The contents of the decrypted file are :

# Decrypted file
 BEWARE THEJAB BERWOC KMYSON THEJAW STHATB ITETHE CLAWST
 HATCAT CHYRZT QBSSGD HMBMQG QBCFOF EVDGUD BKUULI BKXFWS


--<menu>-

Your choice (1 to 6) : 2

Note that this will also delete ALL associated files.

OTP file name to delete (without extension) : user1

'user1.1tp' has been deleted from the current directory.
'user1.1tp_cpy' has been deleted from the current directory.
'user1.1tp_enc' has been deleted from the current directory.
'user1.1tp_dec' has been deleted from the current directory.

--<menu>--

Your choice (1 to 6) : 6

```



## Perl 6

The task is somewhat under-specified, especially the third (optional) section so I'm skipping that for now. Each sub-task has it's own code.

'''Sub-task one:''' generate one-time-pad files.

This is a fairly basic otp file generator. Uses Crypt::Random for decently high quality random numbers. (Random bytes are drawn from /dev/urandom on Unix-like systems, and CryptGenRandom() on Windows.) It will ask for a file name to save to, and the number of lines you want. Each line can be used to encode up to 48 characters of data. Default is 1000 lines, Only generating 4 lines here for demonstration purposes. Saving the file to 'rosettacode.1tp'.


```perl6
sub MAIN {
    put "Generate data for one time pad encryption.\n" ~
        "File will have .1tp extension.";
    my $fn;
    loop {
        $fn = prompt 'Filename for one time pad data: ';
        if $fn !~~ /'.1tp' $/ { $fn ~= '.1tp' }
        if $fn.IO.e {
            my $ow = prompt "$fn aready exists, over-write? y/[n] ";
            last if $ow ~~ m:i/'y'/;
            redo;
        }
        last;
    }

    put 'Each line will contain 48 characters of encyption data.';
    my $lines = prompt 'How many lines of data to generate? [1000] ';
    $lines ||= 1000;
    generate($fn, $lines);
    say "One-time-pad data saved to: ", $fn.IO.absolute;

    sub generate ( $fn, $lines) {
        use Crypt::Random;
        $fn.IO.spurt: "# one-time-pad encryption data\n" ~
          ((sprintf(" %s %s %s %s %s %s %s %s\n",
          ((('A'..'Z')[crypt_random_uniform(26)] xx 6).join) xx 8))
          xx $lines).join;
    }
}
```

```txt
Generate data for one time pad encryption.
File wile have .1tp extension.
Filename for one time pad data: rosettacode
Each line will contain 48 characters of encyption data.
How many lines of data to generate? [1000] 4
One-time-pad data saved to: /home/myhome/mydirectory/rosettacode.1tp

```


Sample file generated by above code:

```txt
# one-time-pad encryption data
 DSUJOU UUWDZD VHFWRR AJDMDC ERZDGD WWKLHJ YITCML FORXCV
 BXGFCL ANGCGY VTAEUG UYAIPK FXWMNI INDLOR JIDZQL BOFFQD
 JISNOS CMLRPW TFGELQ HPTMRN SHBBDP AIVDAC CEWIFH TRLQVK
 FRBUUC GDCQHQ CEEURS RGVWVT JZIQLP NQCABF BWUPUI UDTZAF
```


'''Sub-task two:''' encrypt/decrypt text using the otp files generated by part one.

One-time-pad encryption gets it's security from the fact that the pads are used '''one time'''. As a line is used in the otp file, it needs to be marked as used, or removed so it doesn't get reused. Theoretically, you would make a copy of rosettacode.1tp and send it by secure means to the receiver of your encrypted text so that they can use it to decrypt. Since we are encrypting and decrypting on the same computer, we'll make a copy of the otp file named rosettacopy.1tp and use that for decryption so the encrypt and decrypt functions don't conflict.


```perl6
sub s2v ($s) { $s.uc.comb(/ <[ A..Z ]> /)».ord »-» 65 }
sub v2s (@v) { (@v »%» 26 »+» 65)».chr.join }

sub hide   ($secret, $otp) { v2s(s2v($secret) »+» s2v($otp)) }
sub reveal ($hidden, $otp) { v2s(s2v($hidden) »-» s2v($otp)) }

sub otp-data ($fn, $lines) {
    my $fh = $fn.IO.open :rw;
    my $data;
    my $count = 0;
    repeat {
        my $pos = $fh.tell;
        my $line = $fh.get;
        if $line.substr(0,1) ne '-'|'#' {
            $data ~= $line;
            $fh.seek($pos);
            $fh.put: '-' ~ $line.substr(1);
            $count++;
        }
    } until $count == $lines or $fh.eof;
    note "Insufficient lines of data remaining in $fn" if $count != $lines;
    $data;
}

sub otp-size (Str $string) { ceiling $string.uc.comb(/ <[ A..Z ]> /) / 48 }

sub otp-encrypt ( $secret, $fn ) {
    my $otp-size = otp-size $secret;
    my $otp-data = otp-data($fn, $otp-size);
    my $encrypted = hide $secret, $otp-data;
    # pad encryted text out to a full line with random text
    $encrypted ~= ('A'..'Z').roll while $encrypted.chars % 48;
    join "\n", $encrypted.comb(6).rotor(8, :partial).map:
      { sprintf "{ join ' ', "%s" xx $_ }", $_ };
}

sub otp-decrypt ( $secret, $fn ) {
    my $otp-size = otp-size $secret;
    my $otp-data = otp-data($fn, $otp-size);
    my $plain-text = reveal $secret, $otp-data;
    join "\n", $plain-text.comb(6).rotor(8, :partial).map:
      { sprintf "{ join ' ', "%s" xx $_ }", $_ };
}

my $otp-encrypt-fn = 'rosettacode.1tp';
my $otp-decrypt-fn = 'rosettacopy.1tp';

my $secret = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!";

say "Secret:\n$secret\n\nEncrypted:";
say my $encrypted =   otp-encrypt $secret,    $otp-encrypt-fn;
say "\nDecrypted:\n", otp-decrypt $encrypted, $otp-decrypt-fn;
```


```txt
Secret:
Beware the Jabberwock, my son! The jaws that bite, the claws that catch!

Encrypted:
EWQJFY NBAMZE WLWSFT KVBERP XYDMGZ OPRLAK GBXVTP HZRTUO
IXZHCE CUUORL THOFDI DCXRLG JQICPI ZEREHP RLOEAE PRMVJH

Decrypted:
BEWARE THEJAB BERWOC KMYSON THEJAW STHATB ITETHE CLAWST
HATCAT CHOMLN YOOBJC JEXJWW ETMQCA RROTTY IDLFKT ODHQTE
```


Contents of rosettacode.1tp after encryption / rosettacopy.1tp after decryption:

```txt
# one-time-pad encryption data
-DSUJOU UUWDZD VHFWRR AJDMDC ERZDGD WWKLHJ YITCML FORXCV
-BXGFCL ANGCGY VTAEUG UYAIPK FXWMNI INDLOR JIDZQL BOFFQD
 JISNOS CMLRPW TFGELQ HPTMRN SHBBDP AIVDAC CEWIFH TRLQVK
 FRBUUC GDCQHQ CEEURS RGVWVT JZIQLP NQCABF BWUPUI UDTZAF
```



## Phix


See [[One-time pad/Phix]]


## Racket


See [[One-time pad/Racket]]


## Tcl



### Part 1: random strings

Get true random numbers, and turn them into strings.

With "randInt" from Tcl'ers wiki [http://wiki.tcl.tk/29163 Cryptographically secure random numbers using /dev/urandom]


```Tcl
puts "# True random chars for one-time pad"

proc randInt { min max } {
    set randDev [open /dev/urandom rb]
    set random [read $randDev 8]
    binary scan $random H16 random
    set random [expr {([scan $random %x] % (($max-$min) + 1) + $min)}]
    close $randDev
    return $random
}

proc randStr { sLen grp alfa } {
  set aLen [string length $alfa]; incr aLen -1
  set rs ""
  for {set i 0} {$i < $sLen} {incr i} {
    if { [expr {$i % $grp} ] == 0} { append rs " " }
    set r [randInt 0 $aLen]
    set char [string index $alfa $r]
    append rs $char
  ##puts "$i: $r $char"
  }
  return $rs
}

set alfa "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
set len 48
set lines 4
set fn "test.1tp"

# Write file:
set fh [open $fn w]
puts $fh "# OTP"
for {set ln 0} {$ln < $lines} {incr ln} {
    set line [randStr $len 6 $alfa]
  ##puts "$ln :$line."
    puts $fh $line
}
close $fh

# Read file:
puts "# File $fn:"
set fh [open $fn]
puts [read $fh [file size $fn]]
close $fh

puts "# Done."
```


```txt

# True random chars for one-time pad
# File test.1tp:
# OTP
 OWCTEL SGDQEA UKEWCU PUTDEA XICBOL VVMJHD OHAXSE ZFAGDE
 QHDHKQ CCJBYF CMRCMC IXXPVM IOHQDA XIDTPX FGRIJC NPDOAT
 MYYQUV ZVKGDF ZLYKSX MBPLON RMQKQT QDYJVO LNKUFV DNKIQP
 NQOZKU MQOWHS VOQFWL EQWBFA HZQAMG JWNHGZ QERNNV GBKQTM
# Done.

```




### Part 2: Encrypt/Decrypt


See Tcl'ers wiki:
[http://wiki.tcl.tk/2884 vignere]
[http://wiki.tcl.tk/23095 Vigenere]
...



### Part 3: Management

* list padfiles in directory
* list lines / blocks between "comment"-lines in padfile (i.e. remaining usable data)
...

<!--
# True random chars for one-time pad
: AVSLCR PUNRMF LBNGVV XGTIAW DLWFCW IAKXWW MUSJOI OVIGOY.
: RRNCIZ ZKLYTU FBXJOG GGDZUX UWNTSB LGBXNV SDBJLT GEPIOI.
: JSFRAS SYWMEP DMMRWN KFAYYP NDZTYJ SPIFRY CIBUIN XMLWHF.
: NLYBUL VJFZDH NWXKXF VXHMZQ HNFPMK ZMJDIE VLDPWU JJDMBH.

: EADURX JSHGPH NXELJX VRBZKF AWHYMZ ZXNFET FNQLAZ XYLFSQ.
: MIIQRV TBFHTY EWRQGE SYCXRJ FAOBIS PPGQOT MCTHJQ YYKCTN.
: GAFVEP JENXPH KBPEGD SDHEZQ NCSTTQ SOGSYC MIMACU ZZVTJW.
: OJSCYX WGYSQL UMRLSB HCNIQJ KYQXAB TDKTTE ZREBLK MOVDXD.

: FYIENC SMTQCU VCFHLB MVPLXM DXHQXR QLTUHM LSDSDY FJAQNJ.
: UKXBHP IGHQTB JFUBEY BFTARM CDJHDK FRSPYR PDGWLJ GOFFTH.
: GVZJNR QNQVRJ NATFEI ONXLHE UVOVXA EIVCGD STXRXP MRTWVB.
: HDWXZM PHEMDU YQOKLH PQNRXP WPQPIK LZNRSB PDHPMO HDMMTI.
-->
