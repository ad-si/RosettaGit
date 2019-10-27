+++
title = "Execute Brain****/Csharp"
description = ""
date = 2011-01-12T21:42:45Z
aliases = []
[extra]
id = 9152
[taxonomies]
categories = []
tags = []
+++


```csharp
using System;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using Microsoft.CSharp;

class Brainfuck
{
    static string Translate(string brainfuck)
    {
        var dictionary = new Dictionary<char, string>();
        dictionary.Add('>', "p++;");
        dictionary.Add('<', "p--;");
        dictionary.Add('+', "m[p]++;");
        dictionary.Add('-', "m[p]--;");
        dictionary.Add('.', "Console.Write(m[p]);");
        dictionary.Add(',', "m[p]=(char)Console.Read();");
        dictionary.Add('[', "while(m[p]!=0){");
        dictionary.Add(']', "}");
        return "using System;class Program{public static void Main(){var p = 0;var m = new char[30000];" +
            string.Join(string.Empty, brainfuck.Where(c => dictionary.ContainsKey(c)).Select(c => dictionary[c])) + "}}";
    }

    static void Run(string csharp)
    {
        var compiler = new CSharpCodeProvider();
        var compilerParameters = new CompilerParameters();
        compilerParameters.ReferencedAssemblies.Add("System.dll");
        var compilerResults = compiler.CompileAssemblyFromSource(compilerParameters, csharp);
        var assembly = compilerResults.CompiledAssembly;
        var instance = assembly.CreateInstance("Program");
        instance.GetType().InvokeMember("Main", BindingFlags.InvokeMethod, null, instance, null);
    }

    static void Main()
    {
        var standardInput = Console.OpenStandardInput();
        var reader = new StreamReader(standardInput);
        var brainfuck = reader.ReadToEnd();
        Run(Translate(brainfuck));
    }
}
```

