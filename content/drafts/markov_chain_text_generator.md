+++
title = "Markov chain text generator"
description = ""
date = 2019-09-04T12:13:15Z
aliases = []
[extra]
id = 20964
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
This task is about coding a Text Generator using Markov Chain algorithm.

A Markov chain algorithm basically determines the next most probable suffix word for a given prefix.

To do this, a Markov chain program typically breaks an input text (training text) into a series of words, 
then by sliding along them in some fixed sized window, storing the first N words as a prefix and then 
the N + 1 word as a member of a set to choose from randomly for the suffix.

As an example, take this text with N = 2:

now he is gone she said he is gone for good 

this would build the following table:

```txt

PREFIX               SUFFIX
now he               is
he is                gone, gone
is gone              she, for
gone she             said
she said             he
said he              is
gone for             good
for good             (empty) if we get at this point, the program stops generating text

```

To generate the final text choose a random PREFIX, if it has more than one SUFFIX, get one at random,
create the new PREFIX and repeat until you have completed the text.

Following our simple example, N = 2, 8 words:

```txt

random prefix: gone she
suffix: said
new prefix: she + said
new suffix: he
new prefix: said + he
new suffix: is
... and so on

gone she said he is gone she said

```

The bigger the training text, the better the results. 
You can try this text here: [http://paulo-jorente.de/text/alice_oz.txt alice_oz.txt]

Create a program that is able to handle keys of any size (I guess keys smaller than 2 words would be
pretty random text but...) and create output text also in any length.
Probably you want to call your program passing those numbers as parameters. Something like: markov( "text.txt", 3, 300 )







## C++

In this implementation there is no repeated suffixes!


```cpp

#include <ctime>
#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <map>
class markov {
public:
    void create( std::string& file, unsigned int keyLen, unsigned int words ) {
        std::ifstream f( file.c_str(), std::ios_base::in );
        fileBuffer = std::string( ( std::istreambuf_iterator<char>( f ) ), std::istreambuf_iterator<char>() );
        f.close();
        if( fileBuffer.length() < 1 ) return;
        createDictionary( keyLen );
        createText( words - keyLen );
    }
private:
    void createText( int w ) {
        std::string key, first, second;
        size_t next;
        std::map<std::string, std::vector<std::string> >::iterator it = dictionary.begin();
        std::advance( it, rand() % dictionary.size() );
        key = ( *it ).first;
        std::cout << key;
        while( true ) {
            std::vector<std::string> d = dictionary[key];
            if( d.size() < 1 ) break;
            second = d[rand() % d.size()];
            if( second.length() < 1 ) break;
            std::cout << " " << second;
            if( --w < 0 ) break;
            next = key.find_first_of( 32, 0 );
            first = key.substr( next + 1 );
            key = first + " " + second;
        }
        std::cout << "\n";
    }
    void createDictionary( unsigned int kl ) {
        std::string w1, key;
        size_t wc = 0, pos, next;
        next = fileBuffer.find_first_not_of( 32, 0 );
        if( next == std::string::npos ) return;
        while( wc < kl ) {
            pos = fileBuffer.find_first_of( ' ', next );
            w1 = fileBuffer.substr( next, pos - next );
            key += w1 + " ";
            next = fileBuffer.find_first_not_of( 32, pos + 1 );
            if( next == std::string::npos ) return;
            wc++;
        }
        key = key.substr( 0, key.size() - 1 );
        while( true ) {
            next = fileBuffer.find_first_not_of( 32, pos + 1 );
            if( next == std::string::npos ) return;
            pos = fileBuffer.find_first_of( 32, next );
            w1 = fileBuffer.substr( next, pos - next );
            if( w1.size() < 1 ) break;
            if( std::find( dictionary[key].begin(), dictionary[key].end(), w1 ) == dictionary[key].end() ) 
                dictionary[key].push_back( w1 );
            key = key.substr( key.find_first_of( 32 ) + 1 ) + " " + w1;
        }
    }
    std::string fileBuffer;
    std::map<std::string, std::vector<std::string> > dictionary;
};
int main( int argc, char* argv[] ) {
    srand( unsigned( time( 0 ) ) );
    markov m;
    m.create( std::string( "alice_oz.txt" ), 3, 200 );
    return 0;
}

```

{{out}}

```txt

March Hare had just upset the milk-jug into his plate. Alice did not dare to 
disobey, though she felt sure it would all come wrong, and she went on. Her 
listeners were perfectly quiet till she got to the part about her repeating 
'You are old, Father William,' said the Caterpillar. 'Well, I've tried to say 
How doth the little crocodile Improve his shining tail, And pour the waters of 
the Nile On every golden scale! 'How cheerfully he seems to grin, How neatly 
spread his claws, And welcome little fishes in With gently smiling jaws!' 
'I'm sure those are not the right words,' said poor Alice, and her eyes filled 
with tears again as she went slowly after it: 'I never was so small as this before, 
never! And I declare it's too bad, that it is!' As she said this she looked 
down into its face in some alarm. This time there were three gardeners at it, 
busily painting them red. Alice thought this a very difficult game indeed. 
The players all played at once without waiting for the end of me. But the 
tinsmith happened to come along, and he made me a body of tin, fastening my 
tin arms and

```


=={{header|C#|C sharp}}==

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace MarkovChainTextGenerator {
    class Program {
        static string Join(string a, string b) {
            return a + " " + b;
        }

        static string Markov(string filePath, int keySize, int outputSize) {
            if (keySize < 1) throw new ArgumentException("Key size can't be less than 1");

            string body;
            using (StreamReader sr = new StreamReader(filePath)) {
                body = sr.ReadToEnd();
            }
            var words = body.Split();
            if (outputSize < keySize || words.Length < outputSize) {
                throw new ArgumentException("Output size is out of range");
            }

            Dictionary<string, List<string>> dict = new Dictionary<string, List<string>>();
            for (int i = 0; i < words.Length - keySize; i++) {
                var key = words.Skip(i).Take(keySize).Aggregate(Join);
                string value;
                if (i + keySize < words.Length) {
                    value = words[i + keySize];
                } else {
                    value = "";
                }

                if (dict.ContainsKey(key)) {
                    dict[key].Add(value);
                } else {
                    dict.Add(key, new List<string>() { value });
                }
            }

            Random rand = new Random();
            List<string> output = new List<string>();
            int n = 0;
            int rn = rand.Next(dict.Count);
            string prefix = dict.Keys.Skip(rn).Take(1).Single();
            output.AddRange(prefix.Split());

            while (true) {
                var suffix = dict[prefix];
                if (suffix.Count == 1) {
                    if (suffix[0] == "") {
                        return output.Aggregate(Join);
                    }
                    output.Add(suffix[0]);
                } else {
                    rn = rand.Next(suffix.Count);
                    output.Add(suffix[rn]);
                }
                if (output.Count >= outputSize) {
                    return output.Take(outputSize).Aggregate(Join);
                }
                n++;
                prefix = output.Skip(n).Take(keySize).Aggregate(Join);
            }
        }

        static void Main(string[] args) {
            Console.WriteLine(Markov("alice_oz.txt", 3, 200));
        }
    }
}
```

{{out}}

```txt
axes,' said the Duchess, who seemed ready to agree to everything that Alice said; 'there's a large mustard-mine near here. And the moral of that is-Be what you would seem to be-or if you'd like it put more simply-Never imagine yourself not to be alive. Those creatures frightened me so badly that I cannot keep my promises. I think you are wrong to want a heart. It makes most people unhappy. If you only knew it, you are in luck not to have a heart. I have played Wizard for so many years that I may be as other men are. Why should I give you fair warning,' shouted the Queen, stamping on the ground as she spoke; 'either you or your head must be off, and that the pail was lying in several small pieces, while the poor milkmaid had a nick in her left elbow. There! cried the milkmaid angrily. See what you have done! she screamed. In a minute I shall melt away. I'm very sorry, returned Dorothy. Please forgive us. But the pretty milkmaid was much too wise not to swim, and he was obliged to call to her to help him up again. Why didn't
```



## Crystal


```ruby
class Markov(N)
  @dictionary = Hash(StaticArray(String, N), Array(String)).new { [] of String }

  def parse(filename : String)
    File.open(filename) do |file|
      parse(file)
    end
  end

  private def prefix_from(array)
    StaticArray(String, N).new { |i| array[-(N - i)] }
  end

  def parse(input : IO)
    sequence = [] of String
    loop do
      word = input.gets(' ', true)
      break unless word
      if sequence.size == N
        prefix = prefix_from(sequence)
        @dictionary[prefix] = (@dictionary[prefix] << word)
      end
      sequence << word
      sequence.shift if sequence.size > N
    end
  end

  def generate(count)
    prefix = @dictionary.keys.sample
    result = Array(String).new(prefix.size) { |i| prefix[i] }
    (count - N).times do
      prefix = prefix_from(result)
      values = @dictionary[prefix]
      break if values.size == 0
      result << values.sample
    end
    result.join(' ')
  end
end

chain = Markov(3).new
chain.parse("alice_oz.txt")
puts chain.generate(200)
```

{{out}}

```txt
tired. I'll tell you my story. So they sat down and listened while he told the following story: I was born in Omaha-- Why, that isn't very far from Kansas! cried Dorothy. No, but I am sure we shall sometime come to some place. But day by day passed away, and they found themselves in the midst of a strange people, who, seeing me come from the big Head; so she took courage and answered: I am Dorothy, answered the girl, if he will see you, said the soldier who had taken her message to the Wizard, although he does not like to go back to Oz, and claim his promise. Yes, said the Woodman, so get behind me and I will tell you my story. So they sat down upon the bank and gazed wistfully at the Scarecrow until a Stork flew by, who, upon seeing them, stopped to rest at the water's edge. Who are you and where are you going? My name is Dorothy, said the girl, let us go. And she handed the basket to the Scarecrow. There were no fences at all by the roadside now, and the land was rough and untilled. Toward evening
```




## D

{{trans|Kotlin}}

```D
import std.file;
import std.random;
import std.range;
import std.stdio;
import std.string;

string markov(string filePath, int keySize, int outputSize) {
    if (keySize < 1) throw new Exception("Key size can't be less than 1");
    auto words = filePath.readText().chomp.split;
    if (outputSize < keySize || words.length < outputSize) {
        throw new Exception("Output size is out of range");
    }
    string[][string] dict;

    foreach (i; 0..words.length-keySize) {
        auto key = words[i..i+keySize].join(" ");
        string value;
        if (i+keySize<words.length) {
            value = words[i+keySize];
        }
        if (key !in dict) {
            dict[key] = [value];
        } else {
            dict[key] ~= value;
        }
    }

    string[] output;
    auto n = 0;
    auto rn = uniform(0, dict.length);
    auto prefix = dict.keys[rn];
    output ~= prefix.split;

    while (true) {
        auto suffix = dict[prefix];
        if (suffix.length == 1) {
            if (suffix[0] == "") return output.join(" ");
            output ~= suffix[0];
        } else {
            rn = uniform(0, suffix.length);
            output ~= suffix[rn];
        }
        if (output.length >= outputSize) return output.take(outputSize).join(" ");
        n++;
        prefix = output[n .. n+keySize].join(" ");
    }
}

void main() {
    writeln(markov("alice_oz.txt", 3, 200));
}
```

{{out}}

```txt
neighbour to tell him. 'A nice muddle their slates'll be in before the trial's over!' thought Alice. One of the jurors had a pencil that squeaked. This of course, Alice could not think of any good reason, and as the monster crawls through the forest he seizes an animal with a leg and drags it to his ear. Alice considered a little, and then said 'The fourth.' 'Two days wrong!' sighed the Hatter. 'I deny it!' said the March Hare. 'Sixteenth,' added the Dormouse. 'Write that down,' the King replied. Here the other guinea-pig cheered, and was immediately suppressed by the officers of the court, all dressed in green clothes and had greenish skins. They looked at Dorothy again. Why should I do this for you? asked the Scarecrow. You are quite welcome to take my head off, as long as the tiger had said, and its body covered with coarse black hair. It had a great longing to have for her own the Silver Shoes had fallen off in her flight through the air, and on the morning of the second day I awoke and found the oil-can, and then she had to stop and untwist it. After a
```



## Go


```Go
package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"math/rand"
	"os"
	"strings"
	"time"
	"unicode"
	"unicode/utf8"
)

func main() {
	log.SetFlags(0)
	log.SetPrefix("markov: ")
	input := flag.String("in", "alice_oz.txt", "input file")
	n := flag.Int("n", 2, "number of words to use as prefix")
	runs := flag.Int("runs", 1, "number of runs to generate")
	wordsPerRun := flag.Int("words", 300, "number of words per run")
	startOnCapital := flag.Bool("capital", false, "start output with a capitalized prefix")
	stopAtSentence := flag.Bool("sentence", false, "end output at a sentence ending punctuation mark (after n words)")
	flag.Parse()

	rand.Seed(time.Now().UnixNano())

	m, err := NewMarkovFromFile(*input, *n)
	if err != nil {
		log.Fatal(err)
	}

	for i := 0; i < *runs; i++ {
		err = m.Output(os.Stdout, *wordsPerRun, *startOnCapital, *stopAtSentence)
		if err != nil {
			log.Fatal(err)
		}
		fmt.Println()
	}
}

// We'd like to use a map of []string -> []string (i.e. list of prefix
// words -> list of possible next words) but Go doesn't allow slices to be
// map keys.
//
// We could use arrays, e.g. map of [2]string -> []string, but array lengths
// are fixed at compile time. To work around that we could set a maximum value
// for n, say 8 or 16, and waste the extra array slots for smaller n.
//
// Or we could make the suffix map key just be the full prefix string. Then
// to get the words within the prefix we could either have a separate map
// (i.e. map of string -> []string) for the full prefix string -> the list
// of the prefix words. Or we could use strings.Fields() and strings.Join() to
// go back and forth (trading more runtime for less memory use).

// Markov is a Markov chain text generator.
type Markov struct {
	n           int
	capitalized int // number of suffix keys that start capitalized
	suffix      map[string][]string
}

// NewMarkovFromFile initializes the Markov text generator
// with window `n` from the contents of `filename`.
func NewMarkovFromFile(filename string, n int) (*Markov, error) {
	f, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer f.Close() // nolint: errcheck
	return NewMarkov(f, n)
}

// NewMarkov initializes the Markov text generator
// with window `n` from the contents of `r`.
func NewMarkov(r io.Reader, n int) (*Markov, error) {
	m := &Markov{
		n:      n,
		suffix: make(map[string][]string),
	}
	sc := bufio.NewScanner(r)
	sc.Split(bufio.ScanWords)
	window := make([]string, 0, n)
	for sc.Scan() {
		word := sc.Text()
		if len(window) > 0 {
			prefix := strings.Join(window, " ")
			m.suffix[prefix] = append(m.suffix[prefix], word)
			//log.Printf("%20q -> %q", prefix, m.suffix[prefix])
			if isCapitalized(prefix) {
				m.capitalized++
			}
		}
		window = appendMax(n, window, word)
	}
	if err := sc.Err(); err != nil {
		return nil, err
	}
	return m, nil
}

// Output writes generated text of approximately `n` words to `w`.
// If `startCapital` is true it picks a starting prefix that is capitalized.
// If `stopSentence` is true it continues after `n` words until it finds
// a suffix ending with sentence ending punctuation ('.', '?', or '!').
func (m *Markov) Output(w io.Writer, n int, startCapital, stopSentence bool) error {
	// Use a bufio.Writer both for buffering and for simplified
	// error handling (it remembers any error and turns all future
	// writes/flushes into NOPs returning the same error).
	bw := bufio.NewWriter(w)

	var i int
	if startCapital {
		i = rand.Intn(m.capitalized)
	} else {
		i = rand.Intn(len(m.suffix))
	}
	var prefix string
	for prefix = range m.suffix {
		if startCapital && !isCapitalized(prefix) {
			continue
		}
		if i == 0 {
			break
		}
		i--
	}

	bw.WriteString(prefix) // nolint: errcheck
	prefixWords := strings.Fields(prefix)
	n -= len(prefixWords)

	for {
		suffixChoices := m.suffix[prefix]
		if len(suffixChoices) == 0 {
			break
		}
		i = rand.Intn(len(suffixChoices))
		suffix := suffixChoices[i]
		//log.Printf("prefix: %q, suffix: %q (from %q)", prefixWords, suffix, suffixChoices)
		bw.WriteByte(' ') // nolint: errcheck
		if _, err := bw.WriteString(suffix); err != nil {
			break
		}
		n--
		if n < 0 && (!stopSentence || isSentenceEnd(suffix)) {
			break
		}

		prefixWords = appendMax(m.n, prefixWords, suffix)
		prefix = strings.Join(prefixWords, " ")
	}
	return bw.Flush()
}

func isCapitalized(s string) bool {
	// We can't just look at s[0], which is the first *byte*,
	// if we want to support arbitrary Unicode input.
	// This still doesn't support combining runes :(.
	r, _ := utf8.DecodeRuneInString(s)
	return unicode.IsUpper(r)
}

func isSentenceEnd(s string) bool {
	r, _ := utf8.DecodeLastRuneInString(s)
	// Unfortunately, Unicode doesn't seem to provide
	// a test for sentence ending punctution :(.
	//return unicode.IsPunct(r)
	return r == '.' || r == '?' || r == '!'
}

func appendMax(max int, slice []string, value string) []string {
	// Often FIFO queues in Go are implemented via:
	//     fifo = append(fifo, newValues...)
	// and:
	//     fifo = fifo[numberOfValuesToRemove:]
	//
	// However, the append will periodically reallocate and copy. Since
	// we're dealing with a small number (usually two) of strings and we
	// only need to append a single new string it's better to (almost)
	// never reallocate the slice and just copy n-1 strings (which only
	// copies n-1 pointers, not the entire string contents) every time.
	if len(slice)+1 > max {
		n := copy(slice, slice[1:])
		slice = slice[:n]
	}
	return append(slice, value)
}
```

{{out|input=-n=3 -runs=3 -words=12 -capital -sentence}}

```txt
Alice could speak again. The Mock Turtle went on at last, more calmly, though still sobbing a little now and then, and holding it to his ear.
Don't you suppose we could rescue them? asked the girl anxiously. Oh, no.
City must wear spectacles night and day. Now they are all set free, and are grateful to you for help.

```



## J


This seems to be reasonably close to the specification:


```J
require'web/gethttp'

setstats=:dyad define
  'plen slen limit'=: x
  txt=. gethttp y
  letters=. (tolower~:toupper)txt
  NB. apostrophes have letters on both sides
  apostrophes=. (_1 |.!.0 letters)*(1 |.!.0 letters)*''''=txt
  parsed=. <;._1 ' ',deb ' ' (I.-.letters+apostrophes)} tolower txt
  words=: ~.parsed
  corpus=: words i.parsed
  prefixes=: ~.plen]\corpus
  suffixes=: ~.slen]\corpus
  ngrams=. (plen+slen)]\corpus
  pairs=. (prefixes i. plen{."1 ngrams),. suffixes i. plen}."1 ngrams
  stats=: (#/.~pairs) (<"1~.pairs)} (prefixes ,&# suffixes)$0
  weights=: +/\"1 stats
  totals=: (+/"1 stats),0 
  i.0 0
)

genphrase=:3 :0
  pren=. #prefixes
  sufn=. #suffixes
  phrase=. (?pren) { prefixes
  while. limit > #phrase do.
    p=. prefixes i. (-plen) {. phrase
    t=. p { totals
    if. 0=t do. break.end. NB. no valid matching suffix
    s=. (p { weights) I. ?t
    phrase=. phrase, s { suffixes
  end.
  ;:inv phrase { words
)
```


{{out}}
```J
   2 1 50 setstats 'http://paulo-jorente.de/text/alice_oz.txt'
   genphrase''
got in as alice alice
   genphrase''
perhaps even alice
   genphrase''
pretty milkmaid alice
```


And, using 8 word suffixes (but limiting results to a bit over 50 words):

{{out}}
```J
   2 8 50 setstats 'http://paulo-jorente.de/text/alice_oz.txt'
   genphrase''
added it alice was beginning to get very tired of this i vote the young lady tells us alice was beginning to get very tired of being such a tiny little thing it did not take her long to find the one paved with yellow bricks within a short time
   genphrase''
the raft through the water they got along quite well alice was beginning to get very tired of this i vote the young lady tells us alice was beginning to get very tired of being all alone here as she said this last word two or three times over to
   genphrase''
gown that alice was beginning to get very tired of sitting by her sister on the bank and alice was beginning to get very tired of being such a tiny little thing it did so indeed and much sooner than she had accidentally upset the week before oh i beg
```


(see talk page for discussion of odd line wrapping with some versions of Safari)


## Java

{{trans|Kotlin}}
{{works with|Java|8}}

```Java
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Random;

public class MarkovChain {
    private static Random r = new Random();

    private static String markov(String filePath, int keySize, int outputSize) throws IOException {
        if (keySize < 1) throw new IllegalArgumentException("Key size can't be less than 1");
        Path path = Paths.get(filePath);
        byte[] bytes = Files.readAllBytes(path);
        String[] words = new String(bytes).trim().split(" ");
        if (outputSize < keySize || outputSize >= words.length) {
            throw new IllegalArgumentException("Output size is out of range");
        }
        Map<String, List<String>> dict = new HashMap<>();

        for (int i = 0; i < (words.length - keySize); ++i) {
            StringBuilder key = new StringBuilder(words[i]);
            for (int j = i + 1; j < i + keySize; ++j) {
                key.append(' ').append(words[j]);
            }
            String value = (i + keySize < words.length) ? words[i + keySize] : "";
            if (!dict.containsKey(key.toString())) {
                ArrayList<String> list = new ArrayList<>();
                list.add(value);
                dict.put(key.toString(), list);
            } else {
                dict.get(key.toString()).add(value);
            }
        }

        int n = 0;
        int rn = r.nextInt(dict.size());
        String prefix = (String) dict.keySet().toArray()[rn];
        List<String> output = new ArrayList<>(Arrays.asList(prefix.split(" ")));

        while (true) {
            List<String> suffix = dict.get(prefix);
            if (suffix.size() == 1) {
                if (Objects.equals(suffix.get(0), "")) return output.stream().reduce("", (a, b) -> a + " " + b);
                output.add(suffix.get(0));
            } else {
                rn = r.nextInt(suffix.size());
                output.add(suffix.get(rn));
            }
            if (output.size() >= outputSize) return output.stream().limit(outputSize).reduce("", (a, b) -> a + " " + b);
            n++;
            prefix = output.stream().skip(n).limit(keySize).reduce("", (a, b) -> a + " " + b).trim();
        }
    }

    public static void main(String[] args) throws IOException {
        System.out.println(markov("alice_oz.txt", 3, 200));
    }
}
```

{{out}}

```txt
Emerald City is? Certainly, answered the Queen; but it is a lucky thing I am not, for my mouth is only painted, and if I ever get my heart? Or I my courage? asked the Lion in surprise, as he watched her pick up the Scarecrow and said: Come with me, for Oz has made me its ruler and the people gazed upon it with much curiosity. The Tin Woodman appeared to think deeply for a moment. Then he said, The Winkies were sorry to have them go, and they had grown so large in the last few minutes that she wasn't a bit afraid of interrupting him,) 'I'll give him sixpence. I don't believe there's an atom of meaning in it.' The jury all wrote down on their slates, and then added them up, and reduced the answer to it?' said the Mock Turtle. 'No, no! The adventures first,' said the Gryphon hastily. 'Go on with the next verse,' the Gryphon repeated impatiently: 'it begins I passed by his garden, and marked, with one eye,
```



## Julia

{{works with|Julia|0.6}}


```julia
function markovtext(txt::AbstractString, klen::Integer, maxchlen::Integer)
    words = matchall(r"\w+", txt)
    dict = Dict()
    for i in 1:length(words)-klen
        k = join(words[i:i+klen-1], " ")
        v = words[i+klen]
        if haskey(dict, k)
            dict[k] = push!(dict[k], v)
        else
            dict[k] = [v]
        end
    end
    keytext = rand(collect(keys(dict)))
    outtext = keytext
    lasttext = outtext
    while length(outtext) < maxchlen
        lasttext = outtext
        valtext = rand(dict[keytext])
        outtext = outtext * " " * valtext
        keytext = replace(keytext, r"^\w+\s+(.+)", s"\1") * " " * valtext
    end
    return lasttext
end

txt = readstring(download("http://paulo-jorente.de/text/alice_oz.txt"))
println(markovtext(txt, 3, 200))
```


{{out}}

```txt
Wizard could take upon himself and the Lion looking around him with joy Never have I seen a more
beautiful place It seems gloomy said the Scarecrow If it required brains to figure it out I never
```



## Kotlin


```scala
// version 1.3.10

import java.io.File

fun markov(filePath: String, keySize: Int, outputSize: Int): String {
    require(keySize >= 1) { "Key size can't be less than 1" }

    val words = File(filePath).readText().trimEnd().split(' ')
    require(outputSize in keySize..words.size) { "Output size is out of range" }

    val dict = mutableMapOf<String, MutableList<String>>()

    for (i in 0..(words.size - keySize)) {
        val prefix = words.subList(i, i + keySize).joinToString(" ")
        val suffix = if (i + keySize < words.size) words[i + keySize] else ""
        val suffixes = dict.getOrPut(prefix) { mutableListOf() }
        suffixes += suffix
    }

    val output = mutableListOf<String>()
    var prefix = dict.keys.random()
    output += prefix.split(' ')

    for (n in 1..words.size) {
        val nextWord = dict[prefix]!!.random()
        if (nextWord.isEmpty()) break

        output += nextWord
        if (output.size >= outputSize) break

        prefix = output.subList(n, n + keySize).joinToString(" ")
    }

    return output.take(outputSize).joinToString(" ")
}

fun main() {
    println(markov("alice_oz.txt", 3, 100))
}
```


Sample output:

```txt

jump? asked the Scarecrow. Never. He sits day after day in the great fields. They kept on walking, however, and at night the moon came out and shone brightly. So they lay down among the sweet smelling yellow flowers and slept soundly until morning. When it was daylight, the girl bathed her face in her hands, and she set to work in one of the shelves as she passed; it was labelled 'ORANGE MARMALADE', but to her great delight it fitted! Alice opened the door and closer to one another, for the stillness of the empty room was more dreadful

```



## Lua


Not sure whether this is correct, but I am sure it is quite inefficient. Also not written very nicely.

Computes keys of all lengths <= N. During text generation, if a key does not exist in the dictionary, the first (least recent) word is removed, until a key is found (if no key at all is found, the program terminates).


```Lua
local function pick(t)
  local i = math.ceil(math.random() * #t)
  return t[i]
end

local n_prevs = tonumber(arg[1]) or 2
local n_words = tonumber(arg[2]) or 8

local dict, wordset = {}, {}
local prevs, pidx = {}, 1

local function add(word)   -- add new word to dictionary
  local prev = ''
  local i, len = pidx, #prevs

  for _ = 1, len do
    i = i - 1
    if i == 0 then i = len end

    if prev ~= '' then prev = ' ' .. prev end
    prev = prevs[i] .. prev
    local t = dict[prev]
    if not t then
      t = {}
      dict[prev] = t
    end
    t[#t+1] = word
  end
end

for line in io.lines() do
  for word in line:gmatch("%S+") do
    wordset[word] = true
    add(word)
    prevs[pidx] = word
    pidx = pidx + 1; if pidx > n_prevs then pidx = 1 end
  end
end
add('')

local wordlist = {}
for word in pairs(wordset) do
  wordlist[#wordlist+1] = word
end
wordset = nil

math.randomseed(os.time())
math.randomseed(os.time() * math.random())
local word = pick(wordlist)
local prevs, cnt = '', 0

--[[  print the dictionary
for prevs, nexts in pairs(dict) do
  io.write(prevs, ': ')
  for _,word in ipairs(nexts) do
    io.write(word, ' ')
  end
  io.write('\n')
end
]]

for i = 1, n_words do
  io.write(word, ' ')

  if cnt < n_prevs then
    cnt = cnt + 1
  else
    local i = prevs:find(' ')
    if i then prevs = prevs:sub(i+1) end
  end
  if prevs ~= '' then prevs = prevs .. ' ' end
  prevs = prevs .. word

  local cprevs = ' ' .. prevs
  local nxt_words
  repeat
    local i = cprevs:find(' ')
    if not i then break end
    cprevs = cprevs:sub(i+1)
    if DBG then io.write('\x1b[2m', cprevs, '\x1b[m ') end
    nxt_words = dict[cprevs]
  until nxt_words

  if not nxt_words then break end
  word = pick(nxt_words)
end
io.write('\n')

```


{{out}}

```txt

> ./markov.lua <alice_oz.txt 3 200
hugged the soft, stuffed body of the Scarecrow in her arms instead of kissing his
painted face, and found she was crying herself at this sorrowful parting from her
loving comrades. Glinda the Good stepped down from her ruby throne to give the
prizes?' quite a chorus of voices asked. 'Why, she, of course,' said the Dodo,
pointing to Alice with one finger; and the whole party look so grave and
anxious.) Alice could think of nothing else to do, and perhaps after all it might
tell her something worth hearing. For some minutes it puffed away without
speaking, but at last it sat down a good way off, panting, with its tongue
hanging out of its mouth again, and said, 'So you think you're changed, do you?'
'I'm afraid I don't know one,' said Alice, rather alarmed at the proposal. 'Then
the Dormouse shall!' they both cried. 'Wake up, Dormouse!' And they pinched it
on both sides at once. The Dormouse slowly opened his eyes. 'I wasn't asleep,' he
said in a low voice, 'Why the fact is, you see, Miss, we're doing our best, afore
she comes, to-' At this moment Five, who had been greatly interested in

```



## Perl


```perl
use strict;
use warnings;

my $file = shift || 'alice_oz.txt';
my $n    = shift || 3;
my $max  = shift || 200;

sub build_dict {
    my ($n, @words) = @_;
    my %dict;
    for my $i (0 .. $#words - $n) {
        my @prefix = @words[$i .. $i+$n-1];
        push @{$dict{join ' ', @prefix}}, $words[$i+$n];
    }
    return %dict;
}

sub pick1 { $_[rand @_] }

my $text = do {
    open my $fh, '<', $file;
    local $/;
    <$fh>;
};

my @words = split ' ', $text;
push @words, @words[0..$n-1];
my %dict  = build_dict($n, @words);
my @rotor = @words[0..$n-1];
my @chain = @rotor;

for (1 .. $max) {
    my $new = pick1(@{$dict{join ' ', @rotor}});
    shift @rotor;
    push @rotor, $new;
    push @chain, $new;
}

print join(' ', @chain) . "\n";
```

{{out}}

```txt
Alice was thoroughly puzzled. 'Does the boots and shoes,' the Gryphon whispered in a fight with another hedgehog, which seemed to extend to the South Country? To see the Great Oz was ready to sink into the garden, where Alice could see or feel was the end of you, as she chose, she ran after her. 'I've something important to say!' This sounded promising, certainly: Alice turned and walked through the forest very thick on this side, and it seemed to Alice as she chose, she ran out and shone brightly. So they all spoke at once, I'll chop it down, and the Dormouse sulkily remarked, 'If you please, sir-' The Rabbit started violently, dropped the white kid gloves and the four travelers walked up to the Land of Oz in less than no time to think about stopping herself before she made a dreadfully ugly child: but it is a man,' said the Stork, as she spoke. 'I must be a person of authority among them, called out, 'Sit down, all of them expected to come out of breath, and till the Pigeon in a sorrowful tone; 'at least there's no use to me they flew away with me,' thought Alice,
```



## Perl 6

{{Works with|rakudo|2017-01}}

```perl6
unit sub MAIN ( :$text=$*IN, :$n=2, :$words=100, );

sub add-to-dict ( $text, :$n=2, ) {
    my @words = $text.words;
    my @prefix = @words.rotor: $n => -$n+1;

    (%).push: @prefix Z=> @words[$n .. *]
}

my %dict = add-to-dict $text, :$n;
my @start-words = %dict.keys.pick.words;
my @generated-text = lazy |@start-words, { %dict{ "@_[ *-$n .. * ]" }.pick } ...^ !*.defined;

put @generated-text.head: $words;

```


```txt
>perl6 markov.p6 <alice_oz.txt --n=3 --words=200
Scarecrow. He can't hurt the straw. Do let me carry that basket for you. I shall not mind it, for I can't get tired. I'll tell you what I think, said the little man. Give me two or three pairs of tiny white kid gloves: she took up the fan and gloves, and, as the Lory positively refused to tell its age, there was no use in saying anything more till the Pigeon had finished. 'As if it wasn't trouble enough hatching the eggs,' said the Pigeon; 'but I must be very careful. When Oz gives me a heart of course I needn't mind so much. They were obliged to camp out that night under a large tree in the wood,' continued the Pigeon, raising its voice to a whisper. He is more powerful than they themselves, they would surely have destroyed me. As it was, I lived in deadly fear of them for many years; so you can see for yourself. Indeed, a jolly little clown came walking toward them, and Dorothy could see that in spite of all her coaxing. Hardly knowing what she did, she picked up a little bit of stick, and held it out to

```



## Phix

This was fun! (easy, but fun)

```Phix
integer fn = open("alice_oz.txt","rb")
string text = get_text(fn)
close(fn)
sequence words = split(text)

function markov(integer n, m)
integer dict = new_dict(), ki
sequence key, data, res
string suffix
    for i=1 to length(words)-n do
        key = words[i..i+n-1]
        suffix = words[i+n]
        ki = getd_index(key,dict)
        if ki=0 then
            data = {}
        else
            data = getd_by_index(ki,dict)
        end if
        setd(key,append(data,suffix),dict)
    end for
    integer start = rand(length(words)-n)       
    key = words[start..start+n-1]
    res = key
    for i=1 to m do
        ki = getd_index(key,dict)
        if ki=0 then exit end if
        data = getd_by_index(ki,dict)
        suffix = data[rand(length(data))]
        res = append(res,suffix)
        key = append(key[2..$],suffix)
    end for
    return join(res)
end function

?markov(2,100)
```

{{out}}
from the alice_oz.txt file:

```txt

"serve me a heart, said the Gryphon. \'Then, you know,\' Alice gently remarked; \'they\'d have been ill.\' \'So they were,\' said the
Lion. One would almost suspect you had been running too long. They found the way to send me back to the imprisoned Lion; but every day 
she came upon a green velvet counterpane. There was a long sleep you\'ve had!\' \'Oh, I\'ve had such a capital one for catching mice-oh, 
I beg your pardon!\' cried Alice hastily, afraid that she was shrinking rapidly; so she felt lonely among all these strange people. Her 
tears seemed to Alice a good dinner."

```



## Python


### Procedural

Markov text generator - Python implementation.

Usage: markov.py source.txt context length


```python
#Import libraries.
import sys
import random


def readdata(file):
    '''Read file and return contents.'''
    with open(file) as f:
        contents = f.read()
    return contents


def makerule(data, context):
    '''Make a rule dict for given data.'''
    rule = {}
    words = data.split(' ')
    index = context
    
    for word in words[index:]:
        key = ' '.join(words[index-context:index])
        if key in rule:
            rule[key].append(word)
        else:
            rule[key] = [word]
        index += 1

    return rule


def makestring(rule, length):    
    '''Use a given rule to make a string.'''
    oldwords = random.choice(list(rule.keys())).split(' ') #random starting words
    string = ' '.join(oldwords) + ' '
    
    for i in range(length):
        try:
            key = ' '.join(oldwords)
            newword = random.choice(rule[key])
            string += newword + ' '

            for word in range(len(oldwords)):
                oldwords[word] = oldwords[(word + 1) % len(oldwords)]
            oldwords[-1] = newword

        except KeyError:
            return string
    return string


if __name__ == '__main__':
    data = readdata(sys.argv[1])
    rule = makerule(data, int(sys.argv[2]))
    string = makestring(rule, int(sys.argv[3]))
    print(string)
```


{{out}}

```txt
marry the pretty milkmaid was much pleased to have her little dog free. The other trees of
the Gates they had at the rapid flight of the castle to yourself. I have my shoulders got
to? And oh, I wish you were me?' 'Well, perhaps not,' said the Scarecrow fell off the
cake.'Curiouser and curiouser!' cried Alice (she was obliged to say to this: so she set to
work, so you have killed the Wicked Witch in all their simple joys, remembering her own
the Silver Shoes, began to work; and he did open his mouth, for his release, for he was
obliged to go on with the Wizard. What shall we do? asked the Witch, sinking her voice
sounded hoarse and strange, and the reason is-' here the country of the land of the court,
arm-in-arm with the name 'Alice!' 'Here!' cried Alice, quite forgetting that she could
remember them, all these strange people. Her tears seemed to be full of tears, until there
was no time to think about stopping herself before she gave her answer. 'They're done with
a deep groan near by. What was that? she asked the girl. Again the eyes move and the Tin
Woodman can chop it down, and felt quite strange at first; but she laughed heartily at the
Scarecrow. The Witch did not have lived much under the window, engaged in a grieved tone;
you're a humbug? asked Dorothy. A balloon, said Oz, for I have no right to command them
once
```



### Functional

Defining an '''nGramsFromWords''' function in terms of a generalized '''zipWithN''' wrapper.

{{Works with|Python|3.7}}

```python
'''Markov chain text generation'''

from os.path import (expanduser)
from os import (getcwd)

from itertools import (starmap)
from functools import (reduce)
from random import (choice)
from textwrap import (fill)


# markovText :: Dict -> [String] -> ([String] -> Bool) -> IO [String]
def markovText(dct):
    '''nGram-hashed word dict -> opening words -> end condition -> text
    '''
    # nGram length
    n = len(list(dct.keys())[0].split())

    # step :: [String] -> [String]
    def step(xs):
        return xs + [choice(dct[' '.join(xs[-n:])])]
    return lambda ws: lambda p: (
        until(p)(step)(ws)
    )


# markovRules :: Int -> [String] -> Dict
def markovRules(n):
    '''All words in ordered list hashed by
       preceding nGrams of length n.
    '''
    def nGramKey(dct, tpl):
        k = ' '.join(list(tpl[:-1]))
        dct[k] = (dct[k] if k in dct else []) + [tpl[-1]]
        return dct
    return lambda ws: reduce(
        nGramKey,
        nGramsFromWords(1 + n)(ws),
        {}
    )


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Text generation test.'''

    nGramLength = 3
    dctNGrams = markovRules(nGramLength)(
        readFile(getcwd() + '/' + 'alice_oz.txt').split()
    )
    print(__doc__ + ':\n')
    print(
        fill(
            ' '.join(
                markovText(dctNGrams)(
                    anyNGramWithInitialCap(dctNGrams)
                )(sentenceEndAfterMinWords(200))
            ),
            width=75
        )
    )


# HELPER FUNCTIONS ----------------------------------------

# nGramsFromWords :: Int -> [String] -> [Tuple]
def nGramsFromWords(n):
    '''List of nGrams, of length n, derived
       from ordered list of words ws.
    '''
    return lambda ws: zipWithN(lambda *xs: xs)(
        map(lambda i: ws[i:], range(0, n))
    )


# anyNGramWithInitialCap :: Dict -> [String]
def anyNGramWithInitialCap(dct):
    '''Random pick from nGrams which
       start with capital letters
    '''
    return choice(list(filter(
        lambda k: 1 < len(k) and k[0].isupper() and k[1].islower(),
        dct.keys()
    ))).split()


# sentenceEndAfterMinWords :: Int -> [String] -> Bool
def sentenceEndAfterMinWords(n):
    '''Predicate :: Sentence punctuation
       after minimum word count
    '''
    return lambda ws: n <= len(ws) and (
        ws[-1][-1] in ['.', "'", '!', '?']
    )


# GENERIC -------------------------------------------------

# readFile :: FilePath -> IO String
def readFile(fp):
    '''The contents of any file at the path
       derived by expanding any ~ in fp.'''
    with open(expanduser(fp), 'r', encoding='utf-8') as f:
        return f.read()


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of repeatedly applying f until p holds.
       The initial seed value is x.'''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


# zipWithN :: (a -> b -> ... -> c) -> ([a], [b] ...) -> [c]
def zipWithN(f):
    '''A new list constructed by the application of f
       to each tuple in the zip of an arbitrary
       number of existing lists.
    '''
    return lambda xs: list(
        starmap(f, zip(*xs))
    )


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Markov chain text generation:

Great Oz, said Dorothy. The man was so surprised at this answer that he sat
down to breakfast, after which they started again upon their journey. The
sun shone bright and the birds sang sweetly, and Dorothy did not know
anything about the charm of the Golden Cap and saw some words written upon
the lining. These, she thought, must be the right house, because the
chimneys were shaped like ears and the roof was thatched with fur. It was
so large a house, that she did not like the look of the creature, but on
the whole she thought it must be a very pretty dance,' said Alice timidly.
'Would you like cats if you were me?' 'Well, perhaps not,' said Alice in a
melancholy tone. 'Nobody seems to like her, down here, and I'm sure she's
the best cat in the world! Oh, my dear Dinah! I wonder if I've been changed
in the night? Let me think: was I the same when I got up this morning? I
almost think I can swim to the shore and pull the raft after me, if you
like. If Oz will not give you any brains you will be the wisest man in all
the Land of Oz, said Dorothy, and I am thankful I am made of straw and
cannot be easily damaged.
```



## Racket


```racket
#lang racket

(define (pick xs) (list-ref xs (random (length xs))))

(define (markov path N limit)
  (define xs (string-split (file->string path)))
  (define database (make-hash))
  (let loop ([xs xs])
    (define-values (prefix suffix*) (split-at xs N))
    (match suffix*
      [(cons suffix _)
       (hash-update! database prefix (curry cons suffix) '())
       (loop (rest xs))]
      [_ (void)]))

  (define prefix (pick (hash-keys database)))

  (for/fold ([prefix prefix]
             [out (reverse prefix)]
             #:result (string-join (reverse out) " "))
            ([i (in-range limit)])
    (define suffixes (hash-ref database prefix #f))
    #:break (not suffixes)
    (define suffix (pick suffixes))
    (values (append (rest prefix) (list suffix)) (cons suffix out))))

(markov "alice_oz.txt" 3 300)
```


{{out}}

```txt

"The Scarecrow, who was never tired, stood up in another corner and waited patiently until it chose to speak again. In a minute I shall melt away. I'm very sorry, indeed, said Dorothy, who was truly sorry for him. If you will help me sew the silk together, we will begin to work on our balloon. So Dorothy took a needle and thread, and as fast as Oz cut the strips of silk into proper shape the girl sewed them neatly together. First there was a body to cut it off from: that he had never had to do such a thing before, and he wasn't going to begin again, it was very like having a game of croquet she was playing against herself, for this curious child was very fond of pretending to be two people. 'But it's no use going back to yesterday, because I was a young man when the balloon brought me here, and I am only a little girl. But you were strong enough to kill the Wicked Witch of the East, and promised her two sheep and a cow if she would prevent the marriage. Thereupon the Wicked Witch and setting them free from bondage. So she chose a number of the Winkies she will find you, and make you all her slaves. Perhaps not, said the Scarecrow, in a grieved tone; you're a humbug. Exactly so! declared the little man, rubbing his hands together as if it were not one of the shoes she owned half the power of the Golden Cap; but if she could only get back to Kansas. The Silver Shoes, said the Good Witch, have wonderful powers. And one of the eyes slowly wink at her. She thought she must have a basket to ride in, he said. So he sent the"

```



## REXX


```rexx
/*REXX program produces a Markov chain text from a training text using a text generator.*/
parse arg ord fin iFID seed .                    /*obtain optional arguments from the CL*/
if  ord=='' | ord==","  then ord=   3            /*Not specified?  Then use the default.*/
if  fin=='' | fin==","  then fin= 300            /* "      "         "   "   "     "    */
if iFID=='' |iFID==","  then iFID='alice_oz.txt' /* "      "         "   "   "     "    */
if datatype(seed, 'W')  then call random ,,seed  /* "      "         "   "   "     "    */
sw = linesize() - 1                              /*get usable linesize  (screen width). */
$= space( linein(iFID) )                         /*elide any superfluous whitespace in $*/
say words($)  ' words read from input file: '  iFID
call gTab                                        /*generate the Markov chain text table.*/
call gTxt                                        /*generate the Markov chain text.      */
call show                                        /*display formatted output and a title.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gTab: @.=;   do j=1  for words($)-ord            /*keep processing until words exhausted*/
             p= subword($, j, ord)               /*get the appropriate number of words. */
             @.p= @.p  word($, j + ord)          /*get a prefix & 1 (of sev.?) suffixes.*/
             end   /*j*/
      #= j-1                                     /*define the number of prefixes.       */
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
gTxt: mc=;   do  until words(mc)>=fin            /*build Markov chain text until enough.*/
             y= subword($, random(1, #), ord)    /*obtain appropriate number of words.  */
             s= @.y;      w= words(s)            /*get a suffix for a word set; # wprds.*/
             if w>1  then s= word(s,random(1,w)) /*pick random word in the set of words.*/
             mc= mc y s                          /*add a prefix and suffix to the output*/
             end   /*until*/
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: say center('Markov chain text', sw, "═")   /*display the title for the output.    */
      g= word(mc, 1)                             /*generate lines of Markov chain text. */
             do k=2  to words(mc)                /*build output lines word by word.     */
             _= word(mc, k);    g_= g _          /*get a word; add it to a temp variable*/
             if length(g_)>=sw  then do;    say g;    g= _;    end    /*line too long ? */
                                else g= g_                            /*line OK so far. */
             end   /*k*/
      if g\==''  then say g                      /*There any residual?  Then display it.*/
      return                                     /* [↑]  limits   G   to terminal width.*/
```

{{out|output|text=  when using the default inputs:}}

```txt

════════════════════════════════════════════════════════Markov chain text════════════════════════════════════════════════════════
great Head? That was fell out of the the Caterpillar. 'Well, I've her crows and her us, he said, built eyes so often with quiet
till she got Tin Woodman were glad my body, splitting me said Alice, very much long arms growing out her and bit her before she
had drunk tremble. Quick! cried the a low, trembling voice. corner, but the Rabbit never heard it before,' small point a foot in
a little rippling the little girl was silver whistle twice. Straightway in groups. But the green leaves that lay well! It means
much who ran to Alice said the Cowardly Lion, Palace for several days, head and arms and say How doth the the effect: the next
her; so I set not doing these little saw no one at who played with Toto So the Scarecrow followed have witches and wizards
people to keep away ropes, and the balloon added to one of answered Oz. But I if you don't mind, for them. They came her in any
way. that it might happen Hatter said, turning to at the water's edge. anything to say, she Are you going with it a kind heart?
still in existence; 'and of speaking to a of tin. After this was as big as From the Land of it with hot air. The country here is
and frightening my cow? a coward. Have you upon it.) 'I'm glad creeping back, and Toto But Dorothy they did grin, and she said
for one so cowardly. but I think I dear! Let this be Dorothy and Toto and it's an arm, yer sent the soldier with what to do with
a mouse: she had not think of any their paws. 'And how I will fight them there were two little Making believe! cried Dorothy.

```



## Rust

{{trans|Python}}
{{works with|Rust|1.32 nightly (2018 edition)}}

```rust
use std::env;
use std::fs;

use std::collections::HashMap;

extern crate rand;
use rand::{seq::index::sample, thread_rng};

fn read_data(filename: &str) -> String {
    fs::read_to_string(filename).expect("Something went wrong reading the file")
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let key_size = args[2].parse::<usize>().expect("Invalid key_size!");
    let output_size = args[3].parse::<usize>().expect("Invalid output_size!");

    let data = read_data(filename);
    let rule = make_rule(&data, key_size);
    let result = make_string(&rule.unwrap(), output_size);
    println!("{}", result);
}

fn make_rule(content: &str, key_size: usize) -> Result<HashMap<String, Vec<String>>, &'static str> {
    if key_size < 1 {
        eprintln!();
        return Err("key_size may not be less than 1!");
    }
    let words: Vec<&str> = content.split(' ').collect();

    let mut dict: HashMap<String, Vec<String>> = HashMap::new();

    for i in 0..words.len() - key_size {
        let mut key = words[i].to_string();
        for word in words.iter().take(i + key_size).skip(i + 1) {
            key.push_str(" ");
            key.push_str(word);
        }
        let value = words[i + key_size];
        match dict.get_mut(&key) {
            Some(e) => {
                e.push(value.to_string());
            }
            None => {
                dict.insert(key, vec![value.to_string()]);
            }
        }
    }
    Ok(dict)
}

fn make_string(rule: &HashMap<String, Vec<String>>, length: usize) -> String {
    let keys: Vec<&String> = rule.keys().collect();
    let random_key = keys[get_random_index(keys.len())];
    let mut words = random_key.split(' ').collect::<Vec<&str>>();
    let mut buffer = words.clone().join(" ");
    buffer.push_str(" ");
    for _i in 0..length {
        let key = words.join(" ");
        let entry = match rule.get(&key) {
            None => continue,
            Some(e) => e,
        };
        let new = &entry[get_random_index(entry.len())];
        buffer.push_str(new);
        buffer.push_str(" ");
        let len = words.len();
        for j in 0..len - 1 {
            words[j] = words[j + 1];
        }
        words[len - 1] = new;
    }
    buffer
}

fn get_random_index(max: usize) -> usize {
    let mut rng = thread_rng();
    sample(&mut rng, max, 1).into_vec()[0]
}

```


{{out}}

```txt
$ cargo run "alice_oz.txt" 2 100
to her. The Cat seemed to think about stopping herself before she had never forgotten that, if you will be a coward, said the Caterpillar. Alice thought she had this fit) An obstacle that came between Him, and ourselves, and it. Don't let him know she liked them best, For this must be shutting up like a telescope.' And so she never knew so much at first, but he did was to get him up and said, 'It was the dark to see if the slightest breath of relief, I see we are going to give the Scarecrow and the voice said,
```



## Sidef

{{trans|Perl}}

```ruby
func build_dict (n, words) {
    var dict = Hash()
    words.each_cons(n+1, {|*prefix|
        var suffix = prefix.pop
        dict{prefix.join(' ')} := [] << suffix
    })
    return dict
}

var file = File(ARGV[0] || "alice_oz.txt")
var n    =  Num(ARGV[1] || 2)
var max  =  Num(ARGV[2] || 100)

var words = file.open_r.words
words << words.first(n)...

var dict = build_dict(n, words)
var rotor = words.first(n)
var chain = [rotor...]

max.times {
    var new = dict{rotor.join(' ')}.rand
    chain.push(new)
    rotor.shift
    rotor.push(new)
}

say chain.join(' ')
```


{{out}}

```txt

Alice was a large caterpillar, that was linked into hers began to cry a little startled when she knows such a nice little histories about children who had always been used to; but neither were they very small. In fact, they seemed about as she had not been lying on the rocks below. But if you were down here with me! There are wild beasts in the house, quite forgetting that she stamped her foot as far as they moved. The hats of the East and West. Fortunately, the Witches of the fact. 'I keep them back, so they might rest until

```



## Swift

{{trans|Python}}
{{works with|Swift|4.2}}

```swift
import Foundation

func makeRule(input: String, keyLength: Int) -> [String: [String]] {
  let words = input.components(separatedBy: " ")
  var rules = [String: [String]]()
  var i = keyLength
  
  for word in words[i...] {
    let key = words[i-keyLength..<i].joined(separator: " ")
    
    rules[key, default: []].append(word)
    
    i += 1
  }
  
  return rules
}

func makeString(rule: [String: [String]], length: Int) -> String {
  var oldWords = rule.keys.randomElement()!.components(separatedBy: " ")
  var string = oldWords.joined(separator: " ") + " "
  
  for _ in 0..<length {
    let key = oldWords.joined(separator: " ")
    guard let newWord = rule[key]?.randomElement() else { return string }
    
    string += newWord + " "
    
    for ii in 0..<oldWords.count {
      oldWords[ii] = oldWords[(ii + 1) % oldWords.count]
    }
    
    oldWords[oldWords.index(before: oldWords.endIndex)] = newWord
  }
  
  return string
}

let inputLoc = CommandLine.arguments.dropFirst().first!
let input = FileManager.default.contents(atPath: inputLoc)!
let inputStr = String(data: input, encoding: .utf8)!
let rule = makeRule(input: inputStr, keyLength: 3)
let str = makeString(rule: rule, length: 300)

print(str)
```


{{out}}

```txt
$ ./main /path/to/alice_oz.txt
with a crash, whereupon the Scarecrow's clothes fell out of the clouds to rule over us. Still, for many days they grieved over the loss of my heart. While I was in love I was the happiest man on earth; but no one came near them nor spoke to them because of the great beam the house rested on, two feet were sticking out, shod in silver shoes with pointed toes. Oh, dear! Oh, dear! cried Dorothy, clasping her hands together in dismay. The house must have fallen on her. Whatever shall we do? There is nothing to be done, I wonder?' As she said this, she looked up, but it was the first to break the silence. 'What day of the month is it?' he said, turning to Alice as it spoke. 'As wet as ever,' said Alice in a piteous tone. And she thought of herself, 'I wish the creatures wouldn't be so stingy about it, you know-' She had quite forgotten the Duchess by this time, and was going to dive in among the leaves, which she found to be nothing but the stars over them; and they rested very well indeed. In the morning they traveled on until they came to the great Throne Room, where he saw, sitting in the emerald throne, a most lovely Lady. She was dressed in a green uniform and wearing a long green beard. Here are strangers, said the Guardian of the Gates lived. This officer unlocked their spectacles to put them back in his great box, and then he struck at the Tin Woodman passed safely under it. Come on! he shouted to the others. These she also led to rooms, and each one of them can explain it,' said the King, 'and don't look at me like that!' He got behind
```

