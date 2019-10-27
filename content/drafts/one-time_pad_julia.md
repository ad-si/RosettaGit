+++
title = "One-time pad/Julia"
description = ""
date = 2019-06-22T20:52:08Z
aliases = []
[extra]
id = 22388
[taxonomies]
categories = []
tags = []
+++

This is a console based app for One Time Pad file management. It uses the internet random.org as source combined with local random number generation as a backup source of random letters.

```julia
using Dates, HTTP

const configs = Dict("OTPdir" => ".")

stringtohash(s) = string(hash(s), base=16)

vencode(a::Char, b) = Char((Int(a) + Int(b) - 130) % 26 + 65)
vencode(atxt::String, btxt) = String(map((a, b) -> vencode(a, b), atxt, btxt))
vdecode(a::Char, b) = Char((Int(a) + 26 - Int(b)) % 26 + 65)
vdecode(atxt::String, btxt) = String(map((a, b) -> vdecode(a, b), atxt, btxt))

function getrandom()
    bytes, ret, source = UInt8[], Char[], "local"
    try
        hx = HTTP.get("https://www.random.org/cgi-bin/randbyte?nbytes=1024&format=hex").body
        if length(hx) < 100
            throw("not enough bytes gotten from internet")
        end
        bytes = map(s -> parse(UInt8, s, base=16), split(strip(String(hx)), r"\s+"))
        source = "random.org"
        bytes = map((a, b) -> xor(a, b), bytes, rand(UInt8, length(bytes)))
    catch y
        @warn("internet source failure: $y")
        bytes = rand(UInt8, 1568)
    end
    for b in bytes
        ch = "ABCDEFGHIJKLMNOPQRSTUVWXYZ***"[div(b, 9) + 1]
        if ch != '*'
            push!(ret, ch)
        end
    end
    source, ret
end

function newOTPfilename()
    dircontents = readdir(configs["OTPdir"])
    while true
        nam = "OTP" * replace(string(now()), ":" => "_") * ".1tp"
        if (i = findfirst(x -> x == nam, dircontents)) != nothing
            sleep(0.5)
        else
            return nam
        end
    end
end

function lines40by5(s)
    ret = ""
    for (i, ch) in enumerate(s)
        ret *= (i % 40 == 0) ? ch * "\n" :
            (i % 5 == 1) ? " " * ch : ch
    end
    ret
end

function createOTPfile(nletters, partnername="")
    newname, source = newOTPfilename(), "unknown"
    fp = open(newname, "w")
    needed = nletters + (nletters % 40 == 0 ? 0 : 40 - n % 40)
    data = Char[]
    while true
        source, randomdata = getrandom()
        println("Got ", length(randomdata), " letters from ", source)
        data = append!(data, randomdata)
        if length(data) >= needed
            data = data[1:needed]
            break
        end
    end
    s = lines40by5(data)
    write(fp, "# $source OTP ", stringtohash(partnername), "\n", s)
    close(fp)
    newname, needed, source
end

function readOTPfile(filename, n, skiplines=0, useused=false)
    fp = open(filename, "r+")
    seekpositions, ret = Vector{Int}(), ""
    while length(ret) < n
        if eof(fp)
            throw("Not enough letters in file")
        end
        if skiplines < 1
            push!(seekpositions, position(fp))
        end
        line = readline(fp)
        if skiplines > 0
            skiplines -= 1
            continue
        end
        if (useused || line[1] != '-') && line[1] != '#'
            ret *= replace(line, r"[^A-Z]" => "")
        else
            pop!(seekpositions)
        end
    end
    seekstart(fp)
    for pos in seekpositions
        seek(fp, pos)
        write(fp, '-')
    end
    close(fp)
    ret[1:n]
end

function getOTPfiles(dir=configs["OTPdir"])
    namedict = Dict{String, Pair{String,String}}()
    filenames = filter(nam -> occursin(r"\.1tp$", nam), readdir(dir))
    for (i, nam) in enumerate(filenames)
        fp = open(dir * "/" * nam, "r")
        lin = readline(fp)
        m = match(r"#\s+([\w\.]+)\s+OTP\s+([01-9a-fA-F]+)", lin)
        if m != nothing
            namedict[nam] = Pair(m.captures[1], m.captures[2])
        end
    end
    namedict
end

function listOTPfiles(partnername="")
    phash = stringtohash(partnername)
    files = Dict{Int, String}()
    println("Number            File name               partner code       source     count")
    for (i, p) in enumerate(getOTPfiles())
        if partnername == "" || phash == p[2][2]
            pathname = configs["OTPdir"] * "/" * p[1]
            files[i] = p[1]
            println(rpad(i, 8), rpad(p[1], 32), rpad(p[2][2], 20),
                rpad(p[2][1], 12), stat(pathname).size)
        end
    end
    files
end

function getconsoleinput(asInt=false, yn=false, default="")
    while true
        println("Enter choice ", asInt ? "number " : "", "or <enter> ", asInt ? "for 0:" : "to exit:")
        s = readline()
        if s == ""
            return asInt ? 0 : ""
        elseif !asInt
            if !yn || (s = string(uppercase(s)[1])) in ["Y", "N"]
                return s
            end
        else
            choice = tryparse(Int, s)
            if choice != nothing
                return choice
            end
        end
    end
end

function chooseOTPfiledialog()
    println("Enter partner name or <enter> for all.")
    pname = getconsoleinput()
    files = listOTPfiles(pname)
    println("Choose file number (0 to quit routine).")
    while true
        fnum = getconsoleinput(true)
        if fnum == 0
            return ""
        elseif haskey(files, fnum)
            return files[fnum]
        end
    end
end

function encryptdialog()
    if(fname = chooseOTPfiledialog()) == ""
        return
    end
    println("Skip lines marked as used?")
    useused = getconsoleinput(false, true) == "N"
    skiplines = 0
    println("Start file read at beginning?")
    if getconsoleinput(false, true) == "N"
        println("Enter number of lines to skip:")
        skiplines = getconsoleinput(true)
    end
    println("Enter lines of text to encrypt. Non-alphabet will be ignored. A blank line ends entry.")
    txt = ""
    while (lin = readline()) != ""
        txt *= lin
    end
    txt = replace(uppercase(txt), r"[^A-Z]" => "")
    otp = readOTPfile(fname, length(txt), skiplines, useused)
    println(lines40by5(vencode(txt, otp)))
end

function decryptdialog()
    if(fname = chooseOTPfiledialog()) == ""
        return
    end
    println("Skip lines marked as used?")
    useused = getconsoleinput(false, true) == "N"
    skiplines = 0
    println("Start file read at beginning?")
    if getconsoleinput(false, true) == "N"
        println("Enter number of lines to skip:")
        skiplines = getconsoleinput(true)
    end
    println("Enter lines of text to decrypt. A blank line ends entry.")
    txt = ""
    while (lin = readline()) != ""
        txt *= lin
    end
    txt = replace(txt, r"[^A-Z]" => "")
    otp = readOTPfile(fname, length(txt), skiplines, useused)
    println(vdecode(txt, otp))
end

function securedeletefile(filename, writes=100)
    ltrs = ["ABCDEFGHIJKLMNOPQRSTUVWXYZ"[i] for i in 1:26]
    pathname, len = configs["OTPdir"] * "/" * filename, stat(filename).size
    fp = open(pathname, "w")
    for _ in 1:writes
        write(fp, rand(ltrs, len))
        seekstart(fp)
    end
    close(fp)
    try rm(pathname); catch y; @warn(y) end
end

function createfiledialog()
    println("Enter partner name or press <enter> for none:")
    nam = getconsoleinput()
    println("Enter number of letters or <enter> for 2000:")
    n = getconsoleinput(true)
    newname, nletters, source = createOTPfile(n, nam)
    println("Created file $newname in directory $(configs["OTPdir"])",
        "\nwith $nletters letters. The source was $source.\n")
end

function deletefiledialog()
    fnam = chooseOTPfiledialog()
    if fnam != ""
        println("Confim by entering file name time string as (hh_mm_ss):")
        if occursin(getconsoleinput(), fnam)
            securedeletefile(fnam)
            println("File $fnam has been overwritten and deleted.")
        else
            println("No files deleted.")
        end
    end
end

function configure()
    println("The current subdirectory is: ", configs["OTPdir"])
    println("Enter new dirpath or <enter> to leave unchanged:")
    dirname = getconsoleinput()
    if dirname != ""
        try
            readdir(dirname)
            configs["OTPdir"] = dirname
        catch
            @warn("Invalid directory pathname: $dirname")
        end
    end
end

const mainmenu = """
Welcome to One Time Pad manager.

Select menu item:

A - Add new OTP file
D - Decrypt text
E - Encrypt text
R - Remove file (secure deletion)
C - Configure subdirectory
X - eXit
"""

function onetimepadapp()
    while true
        println(mainmenu)
        inchar = getconsoleinput()
        ltr = (inchar == "") ? "X" : string(uppercase(inchar)[1])
        if ltr == "A"
            createfiledialog()
        elseif ltr == "D"
            decryptdialog()
        elseif ltr == "E"
            encryptdialog()
        elseif ltr == "R"
            deletefiledialog()
        elseif ltr == "C"
            configure()
        elseif ltr == "X"
            break
        else
            println("Unknown choice $ltr, type $(typeof(ltr))")
        end
    end
    println("Close the console to keep past session text from being seen.")
end

onetimepadapp()

```

