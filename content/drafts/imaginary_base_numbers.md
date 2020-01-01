+++
title = "Imaginary base numbers"
description = ""
date = 2019-10-12T00:51:38Z
aliases = []
[extra]
id = 21308
[taxonomies]
categories = []
tags = []
+++

{{task}}

Imaginary base numbers are a non-standard positional numeral system which uses an imaginary number as its radix. The most common is quater-imaginary with radix 2i.

''The quater-imaginary numeral system was first proposed by [https://en.wikipedia.org/wiki/Donald_Knuth Donald Knuth] in 1955 as a submission for a high school science talent search. [http://www.fact-index.com/q/qu/quater_imaginary_base.html  [Ref.]]''

Other imaginary bases are possible too but are not as widely discussed and aren't specifically named.

'''Task:''' Write a set of procedures (functions, subroutines, however they are referred to in your language) to convert base 10 numbers to an imaginary base and back.

At a minimum, support quater-imaginary (base 2i).

For extra kudos, support positive or negative bases 2i through 6i (or higher).

As a stretch goal, support converting non-integer numbers ( E.G. 227.65625+10.859375i ) to an imaginary base.

See [https://en.wikipedia.org/wiki/Quater-imaginary_base Wikipedia: Quater-imaginary_base] for more details.

For reference, here are some some decimal and complex numbers converted to quater-imaginary.
<table class="multicol" role="presentation" style="border-collapse: collapse; padding: 0; border: 0; background:transparent; width:100%;">
<tr>
<td style="text-align: left; vertical-align: top;">
<table class="wikitable" style="text-align:right">
<tr>
<th>Base 10</th>
<th>Base 2<i>i</i></th>
</tr>
<tr>
<td>1</td>
<td><i> </i>1</td>
</tr>
<tr>
<td>2</td>
<td><i> </i>2</td>
</tr>
<tr>
<td>3</td>
<td><i> </i>3</td>
</tr>
<tr>
<td>4</td>
<td><i> </i>10300</td>
</tr>
<tr>
<td>5</td>
<td><i> </i>10301</td>
</tr>
<tr>
<td>6</td>
<td><i> </i>10302</td>
</tr>
<tr>
<td>7</td>
<td><i> </i>10303</td>
</tr>
<tr>
<td>8</td>
<td><i> </i>10200</td>
</tr>
<tr>
<td>9</td>
<td><i> </i>10201</td>
</tr>
<tr>
<td>10</td>
<td><i> </i>10202</td>
</tr>
<tr>
<td>11</td>
<td><i> </i>10203</td>
</tr>
<tr>
<td>12</td>
<td><i> </i>10100</td>
</tr>
<tr>
<td>13</td>
<td><i> </i>10101</td>
</tr>
<tr>
<td>14</td>
<td><i> </i>10102</td>
</tr>
<tr>
<td>15</td>
<td><i> </i>10103</td>
</tr>
<tr>
<td>16</td>
<td><i> </i>10000</td>
</tr>
</table>
</td>
<td style="text-align: left; vertical-align: top;">
<table class="wikitable" style="text-align:right">
<tr>
<th>Base 10</th>
<th>Base 2<i>i</i></th>
</tr>
<tr>
<td>−1</td>
<td><i> </i>103</td>
</tr>
<tr>
<td>−2</td>
<td><i> </i>102</td>
</tr>
<tr>
<td>−3</td>
<td><i> </i>101</td>
</tr>
<tr>
<td>−4</td>
<td><i> </i>100</td>
</tr>
<tr>
<td>−5</td>
<td><i> </i>203</td>
</tr>
<tr>
<td>−6</td>
<td><i> </i>202</td>
</tr>
<tr>
<td>−7</td>
<td><i> </i>201</td>
</tr>
<tr>
<td>−8</td>
<td><i> </i>200</td>
</tr>
<tr>
<td>−9</td>
<td><i> </i>303</td>
</tr>
<tr>
<td>−10</td>
<td><i> </i>302</td>
</tr>
<tr>
<td>−11</td>
<td><i> </i>301</td>
</tr>
<tr>
<td>−12</td>
<td><i> </i>300</td>
</tr>
<tr>
<td>−13</td>
<td><i> </i>1030003</td>
</tr>
<tr>
<td>−14</td>
<td><i> </i>1030002</td>
</tr>
<tr>
<td>−15</td>
<td><i> </i>1030001</td>
</tr>
<tr>
<td>−16</td>
<td><i> </i>1030000</td>
</tr>
</table>
</td>
<td style="text-align: left; vertical-align: top;">
<table class="wikitable" style="text-align:right">
<tr>
<th>Base 10</th>
<th>Base 2<i>i</i></th>
</tr>
<tr>
<td>1<i>i</i></td>
<td>10.2</td>
</tr>
<tr>
<td>2<i>i</i></td>
<td>10.0</td>
</tr>
<tr>
<td>3<i>i</i></td>
<td>20.2</td>
</tr>
<tr>
<td>4<i>i</i></td>
<td>20.0</td>
</tr>
<tr>
<td>5<i>i</i></td>
<td>30.2</td>
</tr>
<tr>
<td>6<i>i</i></td>
<td>30.0</td>
</tr>
<tr>
<td>7<i>i</i></td>
<td>103000.2</td>
</tr>
<tr>
<td>8<i>i</i></td>
<td>103000.0</td>
</tr>
<tr>
<td>9<i>i</i></td>
<td>103010.2</td>
</tr>
<tr>
<td>10<i>i</i></td>
<td>103010.0</td>
</tr>
<tr>
<td>11<i>i</i></td>
<td>103020.2</td>
</tr>
<tr>
<td>12<i>i</i></td>
<td>103020.0</td>
</tr>
<tr>
<td>13<i>i</i></td>
<td>103030.2</td>
</tr>
<tr>
<td>14<i>i</i></td>
<td>103030.0</td>
</tr>
<tr>
<td>15<i>i</i></td>
<td>102000.2</td>
</tr>
<tr>
<td>16<i>i</i></td>
<td>102000.0</td>
</tr>
</table>
</td>
<td style="text-align: left; vertical-align: top;">
<table class="wikitable" style="text-align:right">
<tr>
<th>Base 10</th>
<th>Base 2<i>i</i></th>
</tr>
<tr>
<td>−1<i>i</i></td>
<td>0.2</td>
</tr>
<tr>
<td>−2<i>i</i></td>
<td>1030.0</td>
</tr>
<tr>
<td>−3<i>i</i></td>
<td>1030.2</td>
</tr>
<tr>
<td>−4<i>i</i></td>
<td>1020.0</td>
</tr>
<tr>
<td>−5<i>i</i></td>
<td>1020.2</td>
</tr>
<tr>
<td>−6<i>i</i></td>
<td>1010.0</td>
</tr>
<tr>
<td>−7<i>i</i></td>
<td>1010.2</td>
</tr>
<tr>
<td>−8<i>i</i></td>
<td>1000.0</td>
</tr>
<tr>
<td>−9<i>i</i></td>
<td>1000.2</td>
</tr>
<tr>
<td>−10<i>i</i></td>
<td>2030.0</td>
</tr>
<tr>
<td>−11<i>i</i></td>
<td>2030.2</td>
</tr>
<tr>
<td>−12<i>i</i></td>
<td>2020.0</td>
</tr>
<tr>
<td>−13<i>i</i></td>
<td>2020.2</td>
</tr>
<tr>
<td>−14<i>i</i></td>
<td>2010.0</td>
</tr>
<tr>
<td>−15<i>i</i></td>
<td>2010.2</td>
</tr>
<tr>
<td>−16<i>i</i></td>
<td>2000.0</td>
</tr>
</table>
</td>
</tr>
</table>


## C

{{trans|C++}}

```c
#include <math.h>
#include <stdio.h>
#include <string.h>

int find(char *s, char c) {
    for (char *i = s; *i != 0; i++) {
        if (*i == c) {
            return i - s;
        }
    }
    return -1;
}

void reverse(char *b, char *e) {
    for (e--; b < e; b++, e--) {
        char t = *b;
        *b = *e;
        *e = t;
    }
}

//////////////////////////////////////////////////////

struct Complex {
    double rel, img;
};

void printComplex(struct Complex c) {
    printf("(%3.0f + %3.0fi)", c.rel, c.img);
}

struct Complex makeComplex(double rel, double img) {
    struct Complex c = { rel, img };
    return c;
}

struct Complex addComplex(struct Complex a, struct Complex b) {
    struct Complex c = { a.rel + b.rel, a.img + b.img };
    return c;
}

struct Complex mulComplex(struct Complex a, struct Complex b) {
    struct Complex c = { a.rel * b.rel - a.img * b.img, a.rel * b.img - a.img * b.rel };
    return c;
}

struct Complex mulComplexD(struct Complex a, double b) {
    struct Complex c = { a.rel * b, a.img * b };
    return c;
}

struct Complex negComplex(struct Complex a) {
    return mulComplexD(a, -1.0);
}

struct Complex divComplex(struct Complex a, struct Complex b) {
    double re = a.rel * b.rel + a.img * b.img;
    double im = a.img * b.rel - a.rel * b.img;
    double d = b.rel * b.rel + b.img * b.img;
    struct Complex c = { re / d, im / d };
    return c;
}

struct Complex inv(struct Complex c) {
    double d = c.rel * c.rel + c.img * c.img;
    struct Complex i = { c.rel / d, -c.img / d };
    return i;
}

const struct Complex TWO_I = { 0.0, 2.0 };
const struct Complex INV_TWO_I = { 0.0, -0.5 };

//////////////////////////////////////////////////////

struct QuaterImaginary {
    char *b2i;
    int valid;
};

struct QuaterImaginary makeQuaterImaginary(char *s) {
    struct QuaterImaginary qi = { s, 0 }; // assume invalid until tested
    size_t i, valid = 1, cnt = 0;

    if (*s != 0) {
        for (i = 0; s[i] != 0; i++) {
            if (s[i] < '0' || '3' < s[i]) {
                if (s[i] == '.') {
                    cnt++;
                } else {
                    valid = 0;
                    break;
                }
            }
        }
        if (valid && cnt > 1) {
            valid = 0;
        }
    }

    qi.valid = valid;
    return qi;
}

void printQuaterImaginary(struct QuaterImaginary qi) {
    if (qi.valid) {
        printf("%8s", qi.b2i);
    } else {
        printf(" ERROR  ");
    }
}

//////////////////////////////////////////////////////

struct Complex qi2c(struct QuaterImaginary qi) {
    size_t len = strlen(qi.b2i);
    int pointPos = find(qi.b2i, '.');
    size_t posLen = (pointPos > 0) ? pointPos : len;
    struct Complex sum = makeComplex(0.0, 0.0);
    struct Complex prod = makeComplex(1.0, 0.0);
    size_t j;

    for (j = 0; j < posLen; j++) {
        double k = qi.b2i[posLen - 1 - j] - '0';
        if (k > 0.0) {
            sum = addComplex(sum, mulComplexD(prod, k));
        }
        prod = mulComplex(prod, TWO_I);
    }
    if (pointPos != -1) {
        prod = INV_TWO_I;
        for (j = posLen + 1; j < len; j++) {
            double k = qi.b2i[j] - '0';
            if (k > 0.0) {
                sum = addComplex(sum, mulComplexD(prod, k));
            }
            prod = mulComplex(prod, INV_TWO_I);
        }
    }
    return sum;
}

// only works properly if the real and imaginary parts are integral
struct QuaterImaginary c2qi(struct Complex c, char *out) {
    char *p = out;
    int re, im, fi;

    *p = 0;
    if (c.rel == 0.0 && c.img == 0.0) {
        return makeQuaterImaginary("0");
    }

    re = (int)c.rel;
    im = (int)c.img;
    fi = -1;
    while (re != 0) {
        int rem = re % -4;
        re /= -4;
        if (rem < 0) {
            rem += 4;
            re++;
        }
        *p++ = rem + '0';
        *p++ = '0';
        *p = 0;
    }
    if (im != 0) {
        size_t index = 1;
        struct Complex fc = divComplex((struct Complex) { 0.0, c.img }, (struct Complex) { 0.0, 2.0 });
        double f = fc.rel;
        im = (int)ceil(f);
        f = -4.0 * (f - im);
        while (im != 0) {
            int rem = im % -4;
            im /= -4;
            if (rem < 0) {
                rem += 4;
                im++;
            }
            if (index < (p - out)) {
                out[index] = rem + '0';
            } else {
                *p++ = '0';
                *p++ = rem + '0';
                *p = 0;
            }
            index += 2;
        }
        fi = (int)f;
    }

    reverse(out, p);
    if (fi != -1) {
        *p++ = '.';
        *p++ = fi + '0';
        *p = 0;
    }
    while (out[0] == '0' && out[1] != '.') {
        size_t i;
        for (i = 0; out[i] != 0; i++) {
            out[i] = out[i + 1];
        }
    }
    if (*out == '.') {
        reverse(out, p);
        *p++ = '0';
        *p = 0;
        reverse(out, p);
    }
    return makeQuaterImaginary(out);
}

//////////////////////////////////////////////////////

int main() {
    char buffer[16];
    int i;

    for (i = 1; i <= 16; i++) {
        struct Complex c1 = { i, 0.0 };
        struct QuaterImaginary qi = c2qi(c1, buffer);
        struct Complex c2 = qi2c(qi);
        printComplex(c1);
        printf(" -> ");
        printQuaterImaginary(qi);
        printf(" -> ");
        printComplex(c2);

        printf("     ");

        c1 = negComplex(c1);
        qi = c2qi(c1, buffer);
        c2 = qi2c(qi);
        printComplex(c1);
        printf(" -> ");
        printQuaterImaginary(qi);
        printf(" -> ");
        printComplex(c2);

        printf("\n");
    }

    printf("\n");

    for (i = 1; i <= 16; i++) {
        struct Complex c1 = { 0.0, i };
        struct QuaterImaginary qi = c2qi(c1, buffer);
        struct Complex c2 = qi2c(qi);
        printComplex(c1);
        printf(" -> ");
        printQuaterImaginary(qi);
        printf(" -> ");
        printComplex(c2);

        printf("     ");

        c1 = negComplex(c1);
        qi = c2qi(c1, buffer);
        c2 = qi2c(qi);
        printComplex(c1);
        printf(" -> ");
        printQuaterImaginary(qi);
        printf(" -> ");
        printComplex(c2);

        printf("\n");
    }

    return 0;
}
```

{{out}}

```txt
(  1 +   0i) ->        1 -> (  1 +   0i)     ( -1 +  -0i) ->      103 -> ( -1 +   0i)
(  2 +   0i) ->        2 -> (  2 +   0i)     ( -2 +  -0i) ->      102 -> ( -2 +   0i)
(  3 +   0i) ->        3 -> (  3 +   0i)     ( -3 +  -0i) ->      101 -> ( -3 +   0i)
(  4 +   0i) ->    10300 -> (  4 +   0i)     ( -4 +  -0i) ->      100 -> ( -4 +   0i)
(  5 +   0i) ->    10301 -> (  5 +   0i)     ( -5 +  -0i) ->      203 -> ( -5 +   0i)
(  6 +   0i) ->    10302 -> (  6 +   0i)     ( -6 +  -0i) ->      202 -> ( -6 +   0i)
(  7 +   0i) ->    10303 -> (  7 +   0i)     ( -7 +  -0i) ->      201 -> ( -7 +   0i)
(  8 +   0i) ->    10200 -> (  8 +   0i)     ( -8 +  -0i) ->      200 -> ( -8 +   0i)
(  9 +   0i) ->    10201 -> (  9 +   0i)     ( -9 +  -0i) ->      303 -> ( -9 +   0i)
( 10 +   0i) ->    10202 -> ( 10 +   0i)     (-10 +  -0i) ->      302 -> (-10 +   0i)
( 11 +   0i) ->    10203 -> ( 11 +   0i)     (-11 +  -0i) ->      301 -> (-11 +   0i)
( 12 +   0i) ->    10100 -> ( 12 +   0i)     (-12 +  -0i) ->      300 -> (-12 +   0i)
( 13 +   0i) ->    10101 -> ( 13 +   0i)     (-13 +  -0i) ->  1030003 -> (-13 +   0i)
( 14 +   0i) ->    10102 -> ( 14 +   0i)     (-14 +  -0i) ->  1030002 -> (-14 +   0i)
( 15 +   0i) ->    10103 -> ( 15 +   0i)     (-15 +  -0i) ->  1030001 -> (-15 +   0i)
( 16 +   0i) ->    10000 -> ( 16 +   0i)     (-16 +  -0i) ->  1030000 -> (-16 +   0i)

(  0 +   1i) ->     10.2 -> (  0 +   1i)     ( -0 +  -1i) ->      0.2 -> (  0 +  -1i)
(  0 +   2i) ->     10.0 -> (  0 +   2i)     ( -0 +  -2i) ->   1030.0 -> (  0 +  -2i)
(  0 +   3i) ->     20.2 -> (  0 +   3i)     ( -0 +  -3i) ->   1030.2 -> (  0 +  -3i)
(  0 +   4i) ->     20.0 -> (  0 +   4i)     ( -0 +  -4i) ->   1020.0 -> (  0 +  -4i)
(  0 +   5i) ->     30.2 -> (  0 +   5i)     ( -0 +  -5i) ->   1020.2 -> (  0 +  -5i)
(  0 +   6i) ->     30.0 -> (  0 +   6i)     ( -0 +  -6i) ->   1010.0 -> (  0 +  -6i)
(  0 +   7i) -> 103000.2 -> (  0 +   7i)     ( -0 +  -7i) ->   1010.2 -> (  0 +  -7i)
(  0 +   8i) -> 103000.0 -> (  0 +   8i)     ( -0 +  -8i) ->   1000.0 -> (  0 +  -8i)
(  0 +   9i) -> 103010.2 -> (  0 +   9i)     ( -0 +  -9i) ->   1000.2 -> (  0 +  -9i)
(  0 +  10i) -> 103010.0 -> (  0 +  10i)     ( -0 + -10i) ->   2030.0 -> (  0 + -10i)
(  0 +  11i) -> 103020.2 -> (  0 +  11i)     ( -0 + -11i) ->   2030.2 -> (  0 + -11i)
(  0 +  12i) -> 103020.0 -> (  0 +  12i)     ( -0 + -12i) ->   2020.0 -> (  0 + -12i)
(  0 +  13i) -> 103030.2 -> (  0 +  13i)     ( -0 + -13i) ->   2020.2 -> (  0 + -13i)
(  0 +  14i) -> 103030.0 -> (  0 +  14i)     ( -0 + -14i) ->   2010.0 -> (  0 + -14i)
(  0 +  15i) -> 102000.2 -> (  0 +  15i)     ( -0 + -15i) ->   2010.2 -> (  0 + -15i)
(  0 +  16i) -> 102000.0 -> (  0 +  16i)     ( -0 + -16i) ->   2000.0 -> (  0 + -16i)
```



## C++

{{trans|C#}}

```cpp
#include <algorithm>
#include <complex>
#include <iomanip>
#include <iostream>

std::complex<double> inv(const std::complex<double>& c) {
    double denom = c.real() * c.real() + c.imag() * c.imag();
    return std::complex<double>(c.real() / denom, -c.imag() / denom);
}

class QuaterImaginary {
public:
    QuaterImaginary(const std::string& s) : b2i(s) {
        static std::string base("0123.");

        if (b2i.empty()
            || std::any_of(s.cbegin(), s.cend(), [](char c) { return base.find(c) == std::string::npos; })
            || std::count(s.cbegin(), s.cend(), '.') > 1) {
            throw std::runtime_error("Invalid base 2i number");
        }
    }

    QuaterImaginary& operator=(const QuaterImaginary& q) {
        b2i = q.b2i;
        return *this;
    }

    std::complex<double> toComplex() const {
        int pointPos = b2i.find('.');
        int posLen = (pointPos != std::string::npos) ? pointPos : b2i.length();
        std::complex<double> sum(0.0, 0.0);
        std::complex<double> prod(1.0, 0.0);
        for (int j = 0; j < posLen; j++) {
            double k = (b2i[posLen - 1 - j] - '0');
            if (k > 0.0) {
                sum += prod * k;
            }
            prod *= twoI;
        }
        if (pointPos != -1) {
            prod = invTwoI;
            for (size_t j = posLen + 1; j < b2i.length(); j++) {
                double k = (b2i[j] - '0');
                if (k > 0.0) {
                    sum += prod * k;
                }
                prod *= invTwoI;
            }
        }

        return sum;
    }

    friend std::ostream& operator<<(std::ostream&, const QuaterImaginary&);

private:
    const std::complex<double> twoI{ 0.0, 2.0 };
    const std::complex<double> invTwoI = inv(twoI);

    std::string b2i;
};

std::ostream& operator<<(std::ostream& os, const QuaterImaginary& q) {
    return os << q.b2i;
}

// only works properly if 'real' and 'imag' are both integral
QuaterImaginary toQuaterImaginary(const std::complex<double>& c) {
    if (c.real() == 0.0 && c.imag() == 0.0) return QuaterImaginary("0");

    int re = (int)c.real();
    int im = (int)c.imag();
    int fi = -1;
    std::stringstream ss;
    while (re != 0) {
        int rem = re % -4;
        re /= -4;
        if (rem < 0) {
            rem = 4 + rem;
            re++;
        }
        ss << rem << 0;
    }
    if (im != 0) {
        double f = (std::complex<double>(0.0, c.imag()) / std::complex<double>(0.0, 2.0)).real();
        im = (int)ceil(f);
        f = -4.0 * (f - im);
        size_t index = 1;
        while (im != 0) {
            int rem = im % -4;
            im /= -4;
            if (rem < 0) {
                rem = 4 + rem;
                im++;
            }
            if (index < ss.str().length()) {
                ss.str()[index] = (char)(rem + 48);
            } else {
                ss << 0 << rem;
            }
            index += 2;
        }
        fi = (int)f;
    }

    auto r = ss.str();
    std::reverse(r.begin(), r.end());
    ss.str("");
    ss.clear();
    ss << r;
    if (fi != -1) ss << '.' << fi;
    r = ss.str();
    r.erase(r.begin(), std::find_if(r.begin(), r.end(), [](char c) { return c != '0'; }));
    if (r[0] == '.')r = "0" + r;
    return QuaterImaginary(r);
}

int main() {
    using namespace std;

    for (int i = 1; i <= 16; i++) {
        complex<double> c1(i, 0);
        QuaterImaginary qi = toQuaterImaginary(c1);
        complex<double> c2 = qi.toComplex();
        cout << setw(8) << c1 << " -> " << setw(8) << qi << " -> " << setw(8) << c2 << "     ";
        c1 = -c1;
        qi = toQuaterImaginary(c1);
        c2 = qi.toComplex();
        cout << setw(8) << c1 << " -> " << setw(8) << qi << " -> " << setw(8) << c2 << endl;
    }
    cout << endl;

    for (int i = 1; i <= 16; i++) {
        complex<double> c1(0, i);
        QuaterImaginary qi = toQuaterImaginary(c1);
        complex<double> c2 = qi.toComplex();
        cout << setw(8) << c1 << " -> " << setw(8) << qi << " -> " << setw(8) << c2 << "     ";
        c1 = -c1;
        qi = toQuaterImaginary(c1);
        c2 = qi.toComplex();
        cout << setw(8) << c1 << " -> " << setw(8) << qi << " -> " << setw(8) << c2 << endl;
    }

    return 0;
}
```

{{out}}

```txt
   (1,0) ->        1 ->    (1,0)      (-1,-0) ->      103 ->   (-1,0)
   (2,0) ->        2 ->    (2,0)      (-2,-0) ->      102 ->   (-2,0)
   (3,0) ->        3 ->    (3,0)      (-3,-0) ->      101 ->   (-3,0)
   (4,0) ->    10300 ->    (4,0)      (-4,-0) ->      100 ->   (-4,0)
   (5,0) ->    10301 ->    (5,0)      (-5,-0) ->      203 ->   (-5,0)
   (6,0) ->    10302 ->    (6,0)      (-6,-0) ->      202 ->   (-6,0)
   (7,0) ->    10303 ->    (7,0)      (-7,-0) ->      201 ->   (-7,0)
   (8,0) ->    10200 ->    (8,0)      (-8,-0) ->      200 ->   (-8,0)
   (9,0) ->    10201 ->    (9,0)      (-9,-0) ->      303 ->   (-9,0)
  (10,0) ->    10202 ->   (10,0)     (-10,-0) ->      302 ->  (-10,0)
  (11,0) ->    10203 ->   (11,0)     (-11,-0) ->      301 ->  (-11,0)
  (12,0) ->    10100 ->   (12,0)     (-12,-0) ->      300 ->  (-12,0)
  (13,0) ->    10101 ->   (13,0)     (-13,-0) ->  1030003 ->  (-13,0)
  (14,0) ->    10102 ->   (14,0)     (-14,-0) ->  1030002 ->  (-14,0)
  (15,0) ->    10103 ->   (15,0)     (-15,-0) ->  1030001 ->  (-15,0)
  (16,0) ->    10000 ->   (16,0)     (-16,-0) ->  1030000 ->  (-16,0)

   (0,1) ->     10.2 ->    (0,1)      (-0,-1) ->      0.2 ->   (0,-1)
   (0,2) ->     10.0 ->    (0,2)      (-0,-2) ->   1030.0 ->   (0,-2)
   (0,3) ->     20.2 ->    (0,3)      (-0,-3) ->   1030.2 ->   (0,-3)
   (0,4) ->     20.0 ->    (0,4)      (-0,-4) ->   1020.0 ->   (0,-4)
   (0,5) ->     30.2 ->    (0,5)      (-0,-5) ->   1020.2 ->   (0,-5)
   (0,6) ->     30.0 ->    (0,6)      (-0,-6) ->   1010.0 ->   (0,-6)
   (0,7) -> 103000.2 ->    (0,7)      (-0,-7) ->   1010.2 ->   (0,-7)
   (0,8) -> 103000.0 ->    (0,8)      (-0,-8) ->   1000.0 ->   (0,-8)
   (0,9) -> 103010.2 ->    (0,9)      (-0,-9) ->   1000.2 ->   (0,-9)
  (0,10) -> 103010.0 ->   (0,10)     (-0,-10) ->   2030.0 ->  (0,-10)
  (0,11) -> 103020.2 ->   (0,11)     (-0,-11) ->   2030.2 ->  (0,-11)
  (0,12) -> 103020.0 ->   (0,12)     (-0,-12) ->   2020.0 ->  (0,-12)
  (0,13) -> 103030.2 ->   (0,13)     (-0,-13) ->   2020.2 ->  (0,-13)
  (0,14) -> 103030.0 ->   (0,14)     (-0,-14) ->   2010.0 ->  (0,-14)
  (0,15) -> 102000.2 ->   (0,15)     (-0,-15) ->   2010.2 ->  (0,-15)
  (0,16) -> 102000.0 ->   (0,16)     (-0,-16) ->   2000.0 ->  (0,-16)
```


## C#

```c#
using System;
using System.Linq;
using System.Text;

namespace ImaginaryBaseNumbers {
    class Complex {
        private double real, imag;

        public Complex(int r, int i) {
            real = r;
            imag = i;
        }

        public Complex(double r, double i) {
            real = r;
            imag = i;
        }

        public static Complex operator -(Complex self) =>
            new Complex(-self.real, -self.imag);

        public static Complex operator +(Complex rhs, Complex lhs) =>
            new Complex(rhs.real + lhs.real, rhs.imag + lhs.imag);

        public static Complex operator -(Complex rhs, Complex lhs) =>
            new Complex(rhs.real - lhs.real, rhs.imag - lhs.imag);

        public static Complex operator *(Complex rhs, Complex lhs) =>
            new Complex(
                rhs.real * lhs.real - rhs.imag * lhs.imag,
                rhs.real * lhs.imag + rhs.imag * lhs.real
                );

        public static Complex operator *(Complex rhs, double lhs) =>
             new Complex(rhs.real * lhs, rhs.imag * lhs);

        public static Complex operator /(Complex rhs, Complex lhs) =>
            rhs * lhs.Inv();

        public Complex Inv() {
            double denom = real * real + imag * imag;
            return new Complex(real / denom, -imag / denom);
        }

        public QuaterImaginary ToQuaterImaginary() {
            if (real == 0.0 && imag == 0.0) return new QuaterImaginary("0");
            int re = (int)real;
            int im = (int)imag;
            int fi = -1;
            StringBuilder sb = new StringBuilder();
            while (re != 0) {
                int rem = re % -4;
                re /= -4;
                if (rem < 0) {
                    rem = 4 + rem;
                    re++;
                }
                sb.Append(rem);
                sb.Append(0);
            }
            if (im != 0) {
                double f = (new Complex(0.0, imag) / new Complex(0.0, 2.0)).real;
                im = (int)Math.Ceiling(f);
                f = -4.0 * (f - im);
                int index = 1;
                while (im != 0) {
                    int rem = im % -4;
                    im /= -4;
                    if (rem < 0) {
                        rem = 4 + rem;
                        im++;
                    }
                    if (index < sb.Length) {
                        sb[index] = (char)(rem + 48);
                    } else {
                        sb.Append(0);
                        sb.Append(rem);
                    }
                    index += 2;
                }
                fi = (int)f;
            }
            string reverse = new string(sb.ToString().Reverse().ToArray());
            sb.Length = 0;
            sb.Append(reverse);
            if (fi != -1) sb.AppendFormat(".{0}", fi);
            string s = sb.ToString().TrimStart('0');
            if (s[0] == '.') s = "0" + s;
            return new QuaterImaginary(s);
        }

        public override string ToString() {
            double real2 = (real == -0.0) ? 0.0 : real;  // get rid of negative zero
            double imag2 = (imag == -0.0) ? 0.0 : imag;  // ditto
            if (imag2 == 0.0) {
                return string.Format("{0}", real2);
            }
            if (real2 == 0.0) {
                return string.Format("{0}i", imag2);
            }
            if (imag2 > 0.0) {
                return string.Format("{0} + {1}i", real2, imag2);
            }
            return string.Format("{0} - {1}i", real2, -imag2);
        }
    }

    class QuaterImaginary {
        internal static Complex twoI = new Complex(0.0, 2.0);
        internal static Complex invTwoI = twoI.Inv();

        private string b2i;

        public QuaterImaginary(string b2i) {
            if (b2i == "" || !b2i.All(c => "0123.".IndexOf(c) > -1) || b2i.Count(c => c == '.') > 1) {
                throw new Exception("Invalid Base 2i number");
            }
            this.b2i = b2i;
        }

        public Complex ToComplex() {
            int pointPos = b2i.IndexOf(".");
            int posLen = (pointPos != -1) ? pointPos : b2i.Length;
            Complex sum = new Complex(0.0, 0.0);
            Complex prod = new Complex(1.0, 0.0);
            for (int j = 0; j < posLen; j++) {
                double k = (b2i[posLen - 1 - j] - '0');
                if (k > 0.0) {
                    sum += prod * k;
                }
                prod *= twoI;
            }
            if (pointPos != -1) {
                prod = invTwoI;
                for (int j = posLen + 1; j < b2i.Length; j++) {
                    double k = (b2i[j] - '0');
                    if (k > 0.0) {
                        sum += prod * k;
                    }
                    prod *= invTwoI;
                }
            }

            return sum;
        }

        public override string ToString() {
            return b2i;
        }
    }

    class Program {
        static void Main(string[] args) {
            for (int i = 1; i <= 16; i++) {
                Complex c1 = new Complex(i, 0);
                QuaterImaginary qi = c1.ToQuaterImaginary();
                Complex c2 = qi.ToComplex();
                Console.Write("{0,4} -> {1,8} -> {2,4}     ", c1, qi, c2);
                c1 = -c1;
                qi = c1.ToQuaterImaginary();
                c2 = qi.ToComplex();
                Console.WriteLine("{0,4} -> {1,8} -> {2,4}", c1, qi, c2);
            }
            Console.WriteLine();
            for (int i = 1; i <= 16; i++) {
                Complex c1 = new Complex(0, i);
                QuaterImaginary qi = c1.ToQuaterImaginary();
                Complex c2 = qi.ToComplex();
                Console.Write("{0,4} -> {1,8} -> {2,4}     ", c1, qi, c2);
                c1 = -c1;
                qi = c1.ToQuaterImaginary();
                c2 = qi.ToComplex();
                Console.WriteLine("{0,4} -> {1,8} -> {2,4}", c1, qi, c2);
            }
        }
    }
}
```

{{out}}

```txt
   1 ->        1 ->    1       -1 ->      103 ->   -1
   2 ->        2 ->    2       -2 ->      102 ->   -2
   3 ->        3 ->    3       -3 ->      101 ->   -3
   4 ->    10300 ->    4       -4 ->      100 ->   -4
   5 ->    10301 ->    5       -5 ->      203 ->   -5
   6 ->    10302 ->    6       -6 ->      202 ->   -6
   7 ->    10303 ->    7       -7 ->      201 ->   -7
   8 ->    10200 ->    8       -8 ->      200 ->   -8
   9 ->    10201 ->    9       -9 ->      303 ->   -9
  10 ->    10202 ->   10      -10 ->      302 ->  -10
  11 ->    10203 ->   11      -11 ->      301 ->  -11
  12 ->    10100 ->   12      -12 ->      300 ->  -12
  13 ->    10101 ->   13      -13 ->  1030003 ->  -13
  14 ->    10102 ->   14      -14 ->  1030002 ->  -14
  15 ->    10103 ->   15      -15 ->  1030001 ->  -15
  16 ->    10000 ->   16      -16 ->  1030000 ->  -16

  1i ->     10.2 ->   1i      -1i ->      0.2 ->  -1i
  2i ->     10.0 ->   2i      -2i ->   1030.0 ->  -2i
  3i ->     20.2 ->   3i      -3i ->   1030.2 ->  -3i
  4i ->     20.0 ->   4i      -4i ->   1020.0 ->  -4i
  5i ->     30.2 ->   5i      -5i ->   1020.2 ->  -5i
  6i ->     30.0 ->   6i      -6i ->   1010.0 ->  -6i
  7i -> 103000.2 ->   7i      -7i ->   1010.2 ->  -7i
  8i -> 103000.0 ->   8i      -8i ->   1000.0 ->  -8i
  9i -> 103010.2 ->   9i      -9i ->   1000.2 ->  -9i
 10i -> 103010.0 ->  10i     -10i ->   2030.0 -> -10i
 11i -> 103020.2 ->  11i     -11i ->   2030.2 -> -11i
 12i -> 103020.0 ->  12i     -12i ->   2020.0 -> -12i
 13i -> 103030.2 ->  13i     -13i ->   2020.2 -> -13i
 14i -> 103030.0 ->  14i     -14i ->   2010.0 -> -14i
 15i -> 102000.2 ->  15i     -15i ->   2010.2 -> -15i
 16i -> 102000.0 ->  16i     -16i ->   2000.0 -> -16i
```



## D

{{trans|Kotlin}}

```D
import std.algorithm;
import std.array;
import std.complex;
import std.conv;
import std.format;
import std.math;
import std.stdio;
import std.string;

Complex!double inv(Complex!double v) {
    auto denom = v.re*v.re + v.im*v.im;
    return v.conj / denom;
}

QuaterImaginary toQuaterImaginary(Complex!double v) {
    if (v.re == 0.0 && v.im == 0.0) return QuaterImaginary("0");
    auto re = v.re.to!int;
    auto im = v.im.to!int;
    auto fi = -1;
    auto sb = appender!(char[]);
    while (re != 0) {
        auto rem = re % -4;
        re /= -4;
        if (rem < 0) {
            rem = 4 + rem;
            re++;
        }
        sb.formattedWrite("%d", rem);
        sb.put("0");
    }
    if (im != 0) {
        auto f = (complex(0.0, v.im) / complex(0.0, 2.0)).re;
        im = f.ceil.to!int;
        f = -4.0 * (f - im.to!double);
        auto index = 1;
        while (im != 0) {
            auto rem = im % -4;
            im /= -4;
            if (rem < 0) {
                rem = 4 + rem;
                im++;
            }
            if (index < sb.data.length) {
                sb.data[index] = cast(char)(rem + '0');
            } else {
                sb.put("0");
                sb.formattedWrite("%d", rem);
            }
            index += 2;
        }
        fi = f.to!int;
    }
    sb.data.reverse;
    if (fi != -1) sb.formattedWrite(".%d", fi);
    int i;
    while (i < sb.data.length && sb.data[i] == '0') {
        i++;
    }
    auto s = sb.data[i..$].idup;
    if (s[0] == '.') s = "0" ~ s;
    return QuaterImaginary(s);
}

struct QuaterImaginary {
    private string b2i;

    this(string b2i) {
        if (b2i == "" || b2i.count('.') > 1) {
            throw new Exception("Invalid Base 2i number");
        }
        foreach (c; b2i) {
            if (!canFind("0123.", c)) {
                throw new Exception("Invalid Base 2i number");
            }
        }
        this.b2i = b2i;
    }

    T opCast(T : Complex!double)() {
        auto pointPos = b2i.indexOf('.');
        size_t posLen;
        if (pointPos != -1) {
            posLen = pointPos;
        } else {
            posLen = b2i.length;
        }
        auto sum = complex(0.0, 0.0);
        auto prod = complex(1.0, 0.0);
        foreach (j; 0..posLen) {
            auto k = (b2i[posLen - 1 - j] - '0').to!double;
            if (k > 0.0) {
                sum += prod * k;
            }
            prod *= twoI;
        }
        if (pointPos != -1) {
            prod = invTwoI;
            foreach (j; posLen+1..b2i.length) {
                auto k = (b2i[j] - '0').to!double;
                if (k > 0.0) {
                    sum += prod * k;
                }
                prod *= invTwoI;
            }
        }
        return sum;
    }

    void toString(scope void delegate(const(char)[]) sink, FormatSpec!char fmt) const {
        if (fmt.spec == 's') {
            for (int i=0; i<fmt.width-b2i.length; ++i) {
                sink(" ");
            }
        }
        sink(b2i);
    }

    enum twoI = complex(0.0, 2.0);
    enum invTwoI = twoI.inv;
}

unittest {
    import std.exception;
    assertThrown!Exception(QuaterImaginary(""));
    assertThrown!Exception(QuaterImaginary("1.2.3"));
    assertThrown!Exception(QuaterImaginary("a"));
    assertThrown!Exception(QuaterImaginary("4"));
    assertThrown!Exception(QuaterImaginary(" "));
}

void main() {
    foreach (i; 1..17) {
        auto c1 = complex(i, 0);
        auto qi = c1.toQuaterImaginary;
        auto c2 = cast(Complex!double) qi;
        writef("%4s -> %8s -> %4s     ", c1.re, qi, c2.re);
        c1 = -c1;
        qi = c1.toQuaterImaginary();
        c2 = cast(Complex!double) qi;
        writefln("%4s -> %8s -> %4s", c1.re, qi, c2.re);
    }
    writeln;
    foreach (i; 1..17) {
        auto c1 = complex(0, i);
        auto qi = c1.toQuaterImaginary;
        auto c2 = qi.to!(Complex!double);
        writef("%4si -> %8s -> %4si     ", c1.im, qi, c2.im);
        c1 = -c1;
        qi = c1.toQuaterImaginary();
        c2 = cast(Complex!double) qi;
        writefln("%4si -> %8s -> %4si", c1.im, qi, c2.im);
    }
}
```

{{out}}

```txt
   1 ->        1 ->    1       -1 ->      103 ->   -1
   2 ->        2 ->    2       -2 ->      102 ->   -2
   3 ->        3 ->    3       -3 ->      101 ->   -3
   4 ->    10300 ->    4       -4 ->      100 ->   -4
   5 ->    10301 ->    5       -5 ->      203 ->   -5
   6 ->    10302 ->    6       -6 ->      202 ->   -6
   7 ->    10303 ->    7       -7 ->      201 ->   -7
   8 ->    10200 ->    8       -8 ->      200 ->   -8
   9 ->    10201 ->    9       -9 ->      303 ->   -9
  10 ->    10202 ->   10      -10 ->      302 ->  -10
  11 ->    10203 ->   11      -11 ->      301 ->  -11
  12 ->    10100 ->   12      -12 ->      300 ->  -12
  13 ->    10101 ->   13      -13 ->  1030003 ->  -13
  14 ->    10102 ->   14      -14 ->  1030002 ->  -14
  15 ->    10103 ->   15      -15 ->  1030001 ->  -15
  16 ->    10000 ->   16      -16 ->  1030000 ->  -16

  1i ->     10.2 ->   1i      -1i ->      0.2 ->  -1i
  2i ->     10.0 ->   2i      -2i ->   1030.0 ->  -2i
  3i ->     20.2 ->   3i      -3i ->   1030.2 ->  -3i
  4i ->     20.0 ->   4i      -4i ->   1020.0 ->  -4i
  5i ->     30.2 ->   5i      -5i ->   1020.2 ->  -5i
  6i ->     30.0 ->   6i      -6i ->   1010.0 ->  -6i
  7i -> 103000.2 ->   7i      -7i ->   1010.2 ->  -7i
  8i -> 103000.0 ->   8i      -8i ->   1000.0 ->  -8i
  9i -> 103010.2 ->   9i      -9i ->   1000.2 ->  -9i
 10i -> 103010.0 ->  10i     -10i ->   2030.0 -> -10i
 11i -> 103020.2 ->  11i     -11i ->   2030.2 -> -11i
 12i -> 103020.0 ->  12i     -12i ->   2020.0 -> -12i
 13i -> 103030.2 ->  13i     -13i ->   2020.2 -> -13i
 14i -> 103030.0 ->  14i     -14i ->   2010.0 -> -14i
 15i -> 102000.2 ->  15i     -15i ->   2010.2 -> -15i
 16i -> 102000.0 ->  16i     -16i ->   2000.0 -> -16i
```



## Go

{{trans|Kotlin}}
... though a bit shorter as Go has support for complex numbers built into the language.

```go
package main

import (
    "fmt"
    "math"
    "strconv"
    "strings"
)

const (
    twoI    = 2.0i
    invTwoI = 1.0 / twoI
)

type quaterImaginary struct {
    b2i string
}

func reverse(s string) string {
    r := []rune(s)
    for i, j := 0, len(r)-1; i < len(r)/2; i, j = i+1, j-1 {
        r[i], r[j] = r[j], r[i]
    }
    return string(r)
}

func newQuaterImaginary(b2i string) quaterImaginary {
    b2i = strings.TrimSpace(b2i)
    _, err := strconv.ParseFloat(b2i, 64)
    if err != nil {
        panic("invalid Base 2i number")
    }
    return quaterImaginary{b2i}
}

func toComplex(q quaterImaginary) complex128 {
    pointPos := strings.Index(q.b2i, ".")
    var posLen int
    if pointPos != -1 {
        posLen = pointPos
    } else {
        posLen = len(q.b2i)
    }
    sum := 0.0i
    prod := complex(1.0, 0.0)
    for j := 0; j < posLen; j++ {
        k := float64(q.b2i[posLen-1-j] - '0')
        if k > 0.0 {
            sum += prod * complex(k, 0.0)
        }
        prod *= twoI
    }
    if pointPos != -1 {
        prod = invTwoI
        for j := posLen + 1; j < len(q.b2i); j++ {
            k := float64(q.b2i[j] - '0')
            if k > 0.0 {
                sum += prod * complex(k, 0.0)
            }
            prod *= invTwoI
        }
    }
    return sum
}

func (q quaterImaginary) String() string {
    return q.b2i
}

// only works properly if 'real' and 'imag' are both integral
func toQuaterImaginary(c complex128) quaterImaginary {
    if c == 0i {
        return quaterImaginary{"0"}
    }
    re := int(real(c))
    im := int(imag(c))
    fi := -1
    var sb strings.Builder
    for re != 0 {
        rem := re % -4
        re /= -4
        if rem < 0 {
            rem += 4
            re++
        }
        sb.WriteString(strconv.Itoa(rem))
        sb.WriteString("0")
    }
    if im != 0 {
        f := real(complex(0.0, imag(c)) / 2.0i)
        im = int(math.Ceil(f))
        f = -4.0 * (f - float64(im))
        index := 1
        for im != 0 {
            rem := im % -4
            im /= -4
            if rem < 0 {
                rem += 4
                im++
            }
            if index < sb.Len() {
                bs := []byte(sb.String())
                bs[index] = byte(rem + 48)
                sb.Reset()
                sb.Write(bs)
            } else {
                sb.WriteString("0")
                sb.WriteString(strconv.Itoa(rem))
            }
            index += 2
        }
        fi = int(f)
    }
    s := reverse(sb.String())
    if fi != -1 {
        s = fmt.Sprintf("%s.%d", s, fi)
    }
    s = strings.TrimLeft(s, "0")
    if s[0] == '.' {
        s = "0" + s
    }
    return newQuaterImaginary(s)
}

func main() {
    for i := 1; i <= 16; i++ {
        c1 := complex(float64(i), 0.0)
        qi := toQuaterImaginary(c1)
        c2 := toComplex(qi)
        fmt.Printf("%4.0f -> %8s -> %4.0f     ", real(c1), qi, real(c2))
        c1 = -c1
        qi = toQuaterImaginary(c1)
        c2 = toComplex(qi)
        fmt.Printf("%4.0f -> %8s -> %4.0f\n", real(c1), qi, real(c2))
    }
    fmt.Println()
    for i := 1; i <= 16; i++ {
        c1 := complex(0.0, float64(i))
        qi := toQuaterImaginary(c1)
        c2 := toComplex(qi)
        fmt.Printf("%3.0fi -> %8s -> %3.0fi     ", imag(c1), qi, imag(c2))
        c1 = -c1
        qi = toQuaterImaginary(c1)
        c2 = toComplex(qi)
        fmt.Printf("%3.0fi -> %8s -> %3.0fi\n", imag(c1), qi, imag(c2))
    }
}
```


{{out}}

```txt

   1 ->        1 ->    1       -1 ->      103 ->   -1
   2 ->        2 ->    2       -2 ->      102 ->   -2
   3 ->        3 ->    3       -3 ->      101 ->   -3
   4 ->    10300 ->    4       -4 ->      100 ->   -4
   5 ->    10301 ->    5       -5 ->      203 ->   -5
   6 ->    10302 ->    6       -6 ->      202 ->   -6
   7 ->    10303 ->    7       -7 ->      201 ->   -7
   8 ->    10200 ->    8       -8 ->      200 ->   -8
   9 ->    10201 ->    9       -9 ->      303 ->   -9
  10 ->    10202 ->   10      -10 ->      302 ->  -10
  11 ->    10203 ->   11      -11 ->      301 ->  -11
  12 ->    10100 ->   12      -12 ->      300 ->  -12
  13 ->    10101 ->   13      -13 ->  1030003 ->  -13
  14 ->    10102 ->   14      -14 ->  1030002 ->  -14
  15 ->    10103 ->   15      -15 ->  1030001 ->  -15
  16 ->    10000 ->   16      -16 ->  1030000 ->  -16

  1i ->     10.2 ->   1i      -1i ->      0.2 ->  -1i
  2i ->     10.0 ->   2i      -2i ->   1030.0 ->  -2i
  3i ->     20.2 ->   3i      -3i ->   1030.2 ->  -3i
  4i ->     20.0 ->   4i      -4i ->   1020.0 ->  -4i
  5i ->     30.2 ->   5i      -5i ->   1020.2 ->  -5i
  6i ->     30.0 ->   6i      -6i ->   1010.0 ->  -6i
  7i -> 103000.2 ->   7i      -7i ->   1010.2 ->  -7i
  8i -> 103000.0 ->   8i      -8i ->   1000.0 ->  -8i
  9i -> 103010.2 ->   9i      -9i ->   1000.2 ->  -9i
 10i -> 103010.0 ->  10i     -10i ->   2030.0 -> -10i
 11i -> 103020.2 ->  11i     -11i ->   2030.2 -> -11i
 12i -> 103020.0 ->  12i     -12i ->   2020.0 -> -12i
 13i -> 103030.2 ->  13i     -13i ->   2020.2 -> -13i
 14i -> 103030.0 ->  14i     -14i ->   2010.0 -> -14i
 15i -> 102000.2 ->  15i     -15i ->   2010.2 -> -15i
 16i -> 102000.0 ->  16i     -16i ->   2000.0 -> -16i

```



## Haskell


```Haskell
import Data.Char (chr, digitToInt, intToDigit, isDigit, ord)
import Data.Complex (Complex(..), imagPart, realPart)
import Data.List (elemIndex, delete)
import Data.Maybe (fromMaybe)


base :: Complex Float
base = 0 :+ 2

quotRemPositive :: Int -> Int -> (Int, Int)
quotRemPositive a b
  | r < 0 = (1 + q, floor (realPart (-base ^^ 2)) + r)
  | otherwise = (q, r)
  where
    (q, r) = quotRem a b

digitToIntQI :: Char -> Int
digitToIntQI c
  | isDigit c = digitToInt c
  | otherwise = ord c - ord 'a' + 10

shiftRight :: String -> String
shiftRight n
  | l == '0' = h
  | otherwise = h ++ "." ++ [l]
  where
    (l, h) = (last n, init n)

intToDigitQI :: Int -> Char
intToDigitQI i
  | i `elem` [0 .. 9] = intToDigit i
  | otherwise = chr (i - 10 + ord 'a')

fromQItoComplex :: String -> Complex Float -> Complex Float
fromQItoComplex num b =
  let dot = fromMaybe (length num) (elemIndex '.' num)
  in fst $
     foldl
       (\(a, indx) x ->
           (a + fromIntegral (digitToIntQI x) * (b ^^ (dot - indx)), indx + 1))
       (0, 1)
       (delete '.' num)

euclidEr :: Int -> Int -> [Int] -> [Int]
euclidEr a b l
  | a == 0 = l
  | otherwise =
    let (q, r) = quotRemPositive a b
    in euclidEr q b (0 : r : l)

fromIntToQI :: Int -> [Int]
fromIntToQI 0 = [0]
fromIntToQI x = tail (euclidEr x (floor $ realPart (base ^^ 2)) [])

getCuid :: Complex Int -> Int
getCuid c = imagPart c * floor (imagPart (-base))

qizip :: Complex Int -> [Int]
qizip c =
  let (r, i) = (fromIntToQI (realPart c) ++ [0], fromIntToQI (getCuid c))
  in let m = min (length r) (length i)
     in take (length r - m) r ++
        take (length i - m) i ++
        reverse (zipWith (+) (take m (reverse r)) (take m (reverse i)))

fromComplexToQI :: Complex Int -> String
fromComplexToQI = shiftRight . fmap intToDigitQI . qizip

main :: IO ()
main = print (fromComplexToQI (35 :+ 23)) >> print (fromQItoComplex "10.2" base)
```

{{out}}

```txt
"121003.2"
0.0 :+ 1.0

```

With base = 8i (you may choose any base):

```txt

"3z.8"
0.0 :+ 7.75
```



## Java

{{trans|Kotlin}}

```Java
public class ImaginaryBaseNumber {
    private static class Complex {
        private Double real, imag;

        public Complex(double r, double i) {
            this.real = r;
            this.imag = i;
        }

        public Complex(int r, int i) {
            this.real = (double) r;
            this.imag = (double) i;
        }

        public Complex add(Complex rhs) {
            return new Complex(
                real + rhs.real,
                imag + rhs.imag
            );
        }

        public Complex times(Complex rhs) {
            return new Complex(
                real * rhs.real - imag * rhs.imag,
                real * rhs.imag + imag * rhs.real
            );
        }

        public Complex times(double rhs) {
            return new Complex(
                real * rhs,
                imag * rhs
            );
        }

        public Complex inv() {
            double denom = real * real + imag * imag;
            return new Complex(
                real / denom,
                -imag / denom
            );
        }

        public Complex unaryMinus() {
            return new Complex(-real, -imag);
        }

        public Complex divide(Complex rhs) {
            return this.times(rhs.inv());
        }

        // only works properly if 'real' and 'imag' are both integral
        public QuaterImaginary toQuaterImaginary() {
            if (real == 0.0 && imag == 0.0) return new QuaterImaginary("0");
            int re = real.intValue();
            int im = imag.intValue();
            int fi = -1;
            StringBuilder sb = new StringBuilder();
            while (re != 0) {
                int rem = re % -4;
                re /= -4;
                if (rem < 0) {
                    rem += 4;
                    re++;
                }
                sb.append(rem);
                sb.append(0);
            }
            if (im != 0) {
                Double f = new Complex(0.0, imag).divide(new Complex(0.0, 2.0)).real;
                im = ((Double) Math.ceil(f)).intValue();
                f = -4.0 * (f - im);
                int index = 1;
                while (im != 0) {
                    int rem = im % -4;
                    im /= -4;
                    if (rem < 0) {
                        rem += 4;
                        im++;
                    }
                    if (index < sb.length()) {
                        sb.setCharAt(index, (char) (rem + 48));
                    } else {
                        sb.append(0);
                        sb.append(rem);
                    }
                    index += 2;
                }
                fi = f.intValue();
            }
            sb.reverse();
            if (fi != -1) sb.append(".").append(fi);
            while (sb.charAt(0) == '0') sb.deleteCharAt(0);
            if (sb.charAt(0) == '.') sb.insert(0, '0');
            return new QuaterImaginary(sb.toString());
        }

        @Override
        public String toString() {
            double real2 = real == -0.0 ? 0.0 : real;  // get rid of negative zero
            double imag2 = imag == -0.0 ? 0.0 : imag;  // ditto
            String result = imag2 >= 0.0 ? String.format("%.0f + %.0fi", real2, imag2) : String.format("%.0f - %.0fi", real2, -imag2);
            result = result.replace(".0 ", " ").replace(".0i", "i").replace(" + 0i", "");
            if (result.startsWith("0 + ")) result = result.substring(4);
            if (result.startsWith("0 - ")) result = result.substring(4);
            return result;
        }
    }

    private static class QuaterImaginary {
        private static final Complex TWOI = new Complex(0.0, 2.0);
        private static final Complex INVTWOI = TWOI.inv();

        private String b2i;

        public QuaterImaginary(String b2i) {
            if (b2i.equals("") || !b2i.chars().allMatch(c -> "0123.".indexOf(c) > -1) || b2i.chars().filter(c -> c == '.').count() > 1) {
                throw new RuntimeException("Invalid Base 2i number");
            }
            this.b2i = b2i;
        }

        public Complex toComplex() {
            int pointPos = b2i.indexOf(".");
            int posLen = pointPos != -1 ? pointPos : b2i.length();
            Complex sum = new Complex(0, 0);
            Complex prod = new Complex(1, 0);

            for (int j = 0; j < posLen; ++j) {
                double k = b2i.charAt(posLen - 1 - j) - '0';
                if (k > 0.0) sum = sum.add(prod.times(k));
                prod = prod.times(TWOI);
            }
            if (pointPos != -1) {
                prod = INVTWOI;
                for (int j = posLen + 1; j < b2i.length(); ++j) {
                    double k = b2i.charAt(j) - '0';
                    if (k > 0.0) sum = sum.add(prod.times(k));
                    prod = prod.times(INVTWOI);
                }
            }

            return sum;
        }

        @Override
        public String toString() {
            return b2i;
        }
    }

    public static void main(String[] args) {
        String fmt = "%4s -> %8s -> %4s";
        for (int i = 1; i <= 16; ++i) {
            Complex c1 = new Complex(i, 0);
            QuaterImaginary qi = c1.toQuaterImaginary();
            Complex c2 = qi.toComplex();
            System.out.printf(fmt + "     ", c1, qi, c2);
            c1 = c2.unaryMinus();
            qi = c1.toQuaterImaginary();
            c2 = qi.toComplex();
            System.out.printf(fmt, c1, qi, c2);
            System.out.println();
        }
        System.out.println();
        for (int i = 1; i <= 16; ++i) {
            Complex c1 = new Complex(0, i);
            QuaterImaginary qi = c1.toQuaterImaginary();
            Complex c2 = qi.toComplex();
            System.out.printf(fmt + "     ", c1, qi, c2);
            c1 = c2.unaryMinus();
            qi = c1.toQuaterImaginary();
            c2 = qi.toComplex();
            System.out.printf(fmt, c1, qi, c2);
            System.out.println();
        }
    }
}
```

{{out}}

```txt
   1 ->        1 ->    1       -1 ->      103 ->   -1
   2 ->        2 ->    2       -2 ->      102 ->   -2
   3 ->        3 ->    3       -3 ->      101 ->   -3
   4 ->    10300 ->    4       -4 ->      100 ->   -4
   5 ->    10301 ->    5       -5 ->      203 ->   -5
   6 ->    10302 ->    6       -6 ->      202 ->   -6
   7 ->    10303 ->    7       -7 ->      201 ->   -7
   8 ->    10200 ->    8       -8 ->      200 ->   -8
   9 ->    10201 ->    9       -9 ->      303 ->   -9
  10 ->    10202 ->   10      -10 ->      302 ->  -10
  11 ->    10203 ->   11      -11 ->      301 ->  -11
  12 ->    10100 ->   12      -12 ->      300 ->  -12
  13 ->    10101 ->   13      -13 ->  1030003 ->  -13
  14 ->    10102 ->   14      -14 ->  1030002 ->  -14
  15 ->    10103 ->   15      -15 ->  1030001 ->  -15
  16 ->    10000 ->   16      -16 ->  1030000 ->  -16

  1i ->     10.2 ->   1i       1i ->      0.2 ->   1i
  2i ->     10.0 ->   2i       2i ->   1030.0 ->   2i
  3i ->     20.2 ->   3i       3i ->   1030.2 ->   3i
  4i ->     20.0 ->   4i       4i ->   1020.0 ->   4i
  5i ->     30.2 ->   5i       5i ->   1020.2 ->   5i
  6i ->     30.0 ->   6i       6i ->   1010.0 ->   6i
  7i -> 103000.2 ->   7i       7i ->   1010.2 ->   7i
  8i -> 103000.0 ->   8i       8i ->   1000.0 ->   8i
  9i -> 103010.2 ->   9i       9i ->   1000.2 ->   9i
 10i -> 103010.0 ->  10i      10i ->   2030.0 ->  10i
 11i -> 103020.2 ->  11i      11i ->   2030.2 ->  11i
 12i -> 103020.0 ->  12i      12i ->   2020.0 ->  12i
 13i -> 103030.2 ->  13i      13i ->   2020.2 ->  13i
 14i -> 103030.0 ->  14i      14i ->   2010.0 ->  14i
 15i -> 102000.2 ->  15i      15i ->   2010.2 ->  15i
 16i -> 102000.0 ->  16i      16i ->   2000.0 ->  16i
```



## Julia

{{trans|C#}}

```julia
import Base.show, Base.parse, Base.+, Base.-, Base.*, Base./, Base.^

function inbase4(charvec::Vector)
    if (!all(x -> x in ['-', '0', '1', '2', '3', '.'], charvec)) ||
        ((x = findlast(x -> x == '-', charvec)) != nothing && x > findfirst(x -> x != '-', charvec)) ||
        ((x = findall(x -> x == '.', charvec)) != nothing && length(x) > 1)
        return false
    end
    true
end
inbase4(s::String) = inbase4(split(s, ""))

abstract type ImaginaryBaseNumber <: Number end

struct QuaterImaginary <: ImaginaryBaseNumber
    cvector::Vector{Char}
    isnegative::Bool
end

function QuaterImaginary(charvec::Vector{Char})
    isneg = false
    if !inbase4(charvec)
        throw("Constructor vector for QuaterImaginary ($charvec) is not base 2i")
    elseif (i = length(findall(x -> x == '-', charvec))) > 0
        isneg = (-1) ^ i == -1
    end
    while length(charvec) > 1 && charvec[1] == '0' && charvec[2] != '.'
        popfirst!(charvec)
    end
    if (i = findfirst(x -> x == '.', charvec)) != nothing
        while length(charvec) > 3 && charvec[end] == '0' && charvec[end-1] != '.'
            pop!(charvec)
        end
    end
    if charvec[1] == '.'
            pushfirst!(charvec, '0')
    end
    if charvec[end] == '.'
        pop!(charvec)
    end
    QuaterImaginary(filter!(x -> x in ['0', '1', '2', '3', '.'], charvec), isneg)
end

function QuaterImaginary(s::String = "0")
    if match(r"^-?[0123\.]+$", s) == nothing
        throw("String constructor argument <$s> for QuaterImaginary is not base 2i")
    end
    QuaterImaginary([s[i] for i in 1:length(s)])
end

show(io::IO, qim::QuaterImaginary) = print(io, qim.isnegative ? "-" : "", join(qim.cvector, ""))

function parse(QuaterImaginary, x::Complex)
    sb = Vector{Char}()
    rea, ima = Int(floor(real(x))), Int(floor(imag(x)))
    if floor(real(x)) != rea || floor(imag(x)) != ima
        throw("Non-integer real and complex portions of complex numbers are not supported for QuaterImaginary")
    elseif rea == 0 == ima
        return QuaterImaginary(['0'])
    else
        fi = -1
        while rea != 0
            rea, rem = divrem(rea, -4)
            if rem < 0
                rem += 4
                rea += 1
            end
            push!(sb, Char(rem + '0'), '0')
        end
        if ima != 0
            f = real((ima * im)/(2im))
            ima = Int(ceil(f))
            f = -4.0 * (f - ima)
            idx = 1
            while ima != 0
                ima, rem = divrem(ima, -4)
                if rem < 0
                    rem += 4
                    ima += 1
                end
                if idx < length(sb)
                    sb[idx + 1] = Char(rem + '0')
                else
                    push!(sb, '0', Char(rem + '0'))
                end
                idx += 2
            end
            fi = Int(floor(f))
        end
        sb = reverse(sb)
        if fi != -1
            push!(sb, '.')
            append!(sb, map(x -> x[1], split(string(fi), "")))
        end
    end
    QuaterImaginary(sb)
end

function parse(Complex, qim::QuaterImaginary)
    pointpos = ((x = indexin('.', qim.cvector))[1] == nothing) ? -1 : x[1]
    poslen = (pointpos != -1) ? pointpos : length(qim.cvector) + 1
    qsum = 0.0 + 0.0im
    prod = 1.0 + 0.0im
    for j in 1:poslen-1
        k = Float64(qim.cvector[poslen - j] - '0')
        if k > 0.0
            qsum += prod * k
        end
        prod *= 2im
    end
    if pointpos != -1
        prod = inv(2im)
        for j in poslen+1:length(qim.cvector)
            k = Float64(qim.cvector[j] - '0')
            if k > 0.0
                qsum += prod * k
            end
            prod *= inv(2im)
        end
    end
    qsum
end

function testquim()
    function printcqc(c)
        q = parse(QuaterImaginary, Complex(c))
        c2 = parse(Complex, q)
        if imag(c2) == 0
            c2 = Int(c2)
        end
        print(lpad(c, 10), " -> ", lpad(q, 10), " -> ", lpad(c2, 12))
    end
    for i in 1:16
        printcqc(i)
        print("       ")
        printcqc(-i)
        println()
    end
    println()
    for i in 1:16
        c1 = Complex(0, i)
        printcqc(c1)
        print("       ")
        printcqc(-c1)
        println()
    end
end

QuaterImaginary(c::Complex) = parse(QuaterImaginary, c)
Complex(q::QuaterImaginary) = parse(Complex, q)

+(q1::QuaterImaginary, q2::QuaterImaginary) = QuaterImaginary(Complex(q1) + Complex(q2))
+(q1::Complex, q2::QuaterImaginary) = q1 + Complex(q2)
+(q1::QuaterImaginary, q2::Complex) = Complex(q1) + q2
-(q1::QuaterImaginary, q2::QuaterImaginary) = QuaterImaginary(Complex(q1) - Complex(q2))
-(q1::Complex, q2::QuaterImaginary) = q1 - Complex(q2)
-(q1::QuaterImaginary, q2::Complex) = Complex(q1) - q2
*(q1::QuaterImaginary, q2::QuaterImaginary) = QuaterImaginary(Complex(q1) * Complex(q2))
*(q1::Complex, q2::QuaterImaginary) = q1 * Complex(q2)
*(q1::QuaterImaginary, q2::Complex) = Complex(q1) * q2
/(q1::QuaterImaginary, q2::QuaterImaginary) = QuaterImaginary(Complex(q1) / Complex(q2))
/(q1::Complex, q2::QuaterImaginary) = q1 / Complex(q2)
/(q1::QuaterImaginary, q2::Complex) = Complex(q1) / q2
^(q1::QuaterImaginary, q2::QuaterImaginary) = QuaterImaginary(Complex(q1) ^ Complex(q2))
^(q1::Complex, q2::QuaterImaginary) = q1 ^ Complex(q2)
^(q1::QuaterImaginary, q2::Complex) = Complex(q1) ^ q2

testquim()

```
{{output}}
```txt

         1 ->          1 ->            1               -1 ->        103 ->           -1
         2 ->          2 ->            2               -2 ->        102 ->           -2
         3 ->          3 ->            3               -3 ->        101 ->           -3
         4 ->      10300 ->            4               -4 ->        100 ->           -4
         5 ->      10301 ->            5               -5 ->        203 ->           -5
         6 ->      10302 ->            6               -6 ->        202 ->           -6
         7 ->      10303 ->            7               -7 ->        201 ->           -7
         8 ->      10200 ->            8               -8 ->        200 ->           -8
         9 ->      10201 ->            9               -9 ->        303 ->           -9
        10 ->      10202 ->           10              -10 ->        302 ->          -10
        11 ->      10203 ->           11              -11 ->        301 ->          -11
        12 ->      10100 ->           12              -12 ->        300 ->          -12
        13 ->      10101 ->           13              -13 ->    1030003 ->          -13
        14 ->      10102 ->           14              -14 ->    1030002 ->          -14
        15 ->      10103 ->           15              -15 ->    1030001 ->          -15
        16 ->      10000 ->           16              -16 ->    1030000 ->          -16

   0 + 1im ->       10.2 ->  0.0 + 1.0im          0 - 1im ->        0.2 ->  0.0 - 1.0im
   0 + 2im ->       10.0 ->  0.0 + 2.0im          0 - 2im ->     1030.0 ->  0.0 - 2.0im
   0 + 3im ->       20.2 ->  0.0 + 3.0im          0 - 3im ->     1030.2 ->  0.0 - 3.0im
   0 + 4im ->       20.0 ->  0.0 + 4.0im          0 - 4im ->     1020.0 ->  0.0 - 4.0im
   0 + 5im ->       30.2 ->  0.0 + 5.0im          0 - 5im ->     1020.2 ->  0.0 - 5.0im
   0 + 6im ->       30.0 ->  0.0 + 6.0im          0 - 6im ->     1010.0 ->  0.0 - 6.0im
   0 + 7im ->   103000.2 ->  0.0 + 7.0im          0 - 7im ->     1010.2 ->  0.0 - 7.0im
   0 + 8im ->   103000.0 ->  0.0 + 8.0im          0 - 8im ->     1000.0 ->  0.0 - 8.0im
   0 + 9im ->   103010.2 ->  0.0 + 9.0im          0 - 9im ->     1000.2 ->  0.0 - 9.0im
  0 + 10im ->   103010.0 -> 0.0 + 10.0im         0 - 10im ->     2030.0 -> 0.0 - 10.0im
  0 + 11im ->   103020.2 -> 0.0 + 11.0im         0 - 11im ->     2030.2 -> 0.0 - 11.0im
  0 + 12im ->   103020.0 -> 0.0 + 12.0im         0 - 12im ->     2020.0 -> 0.0 - 12.0im
  0 + 13im ->   103030.2 -> 0.0 + 13.0im         0 - 13im ->     2020.2 -> 0.0 - 13.0im
  0 + 14im ->   103030.0 -> 0.0 + 14.0im         0 - 14im ->     2010.0 -> 0.0 - 14.0im
  0 + 15im ->   102000.2 -> 0.0 + 15.0im         0 - 15im ->     2010.2 -> 0.0 - 15.0im
  0 + 16im ->   102000.0 -> 0.0 + 16.0im         0 - 16im ->     2000.0 -> 0.0 - 16.0im

```



## Kotlin

The following deals with conversions to and from quater-imaginary only.

As the JDK lacks a complex number class, I've included a very basic one in the program.

```scala
// version 1.2.10

import kotlin.math.ceil

class Complex(val real: Double, val imag: Double) {

    constructor(r: Int, i: Int) : this(r.toDouble(), i.toDouble())

    operator fun plus(other: Complex) = Complex(real + other.real, imag + other.imag)

    operator fun times(other: Complex) = Complex(
        real * other.real - imag * other.imag,
        real * other.imag + imag * other.real
    )

    operator fun times(other: Double) = Complex(real * other, imag * other)

    fun inv(): Complex {
        val denom = real * real + imag * imag
        return Complex(real / denom, -imag / denom)
    }

    operator fun unaryMinus() = Complex(-real, -imag)

    operator fun minus(other: Complex) = this + (-other)

    operator fun div(other: Complex) = this * other.inv()

    // only works properly if 'real' and 'imag' are both integral
    fun toQuaterImaginary(): QuaterImaginary {
        if (real == 0.0 && imag == 0.0) return QuaterImaginary("0")
        var re = real.toInt()
        var im = imag.toInt()
        var fi = -1
        val sb = StringBuilder()
        while (re != 0) {
            var rem = re % -4
            re /= -4
            if (rem < 0) {
                rem = 4 + rem
                re++
            }
            sb.append(rem)
            sb.append(0)
        }
        if (im != 0) {
            var f = (Complex(0.0, imag) / Complex(0.0, 2.0)).real
            im = ceil(f).toInt()
            f = -4.0 * (f - im.toDouble())
            var index = 1
            while (im != 0) {
                var rem = im % -4
                im /= -4
                if (rem < 0) {
                    rem = 4 + rem
                    im++
                }
                if (index < sb.length) {
                    sb[index] = (rem + 48).toChar()
                }
                else {
                    sb.append(0)
                    sb.append(rem)
                }
                index += 2
            }
            fi = f.toInt()
        }
        sb.reverse()
        if (fi != -1) sb.append(".$fi")
        var s = sb.toString().trimStart('0')
        if (s.startsWith(".")) s = "0$s"
        return QuaterImaginary(s)
    }

    override fun toString(): String {
        val real2 = if (real == -0.0) 0.0 else real  // get rid of negative zero
        val imag2 = if (imag == -0.0) 0.0 else imag  // ditto
        var result = if (imag2 >= 0.0) "$real2 + ${imag2}i" else "$real2 - ${-imag2}i"
        result = result.replace(".0 ", " ").replace(".0i", "i").replace(" + 0i", "")
        if (result.startsWith("0 + ")) result = result.drop(4)
        if (result.startsWith("0 - ")) result = "-" + result.drop(4)
        return result
    }
}

class QuaterImaginary(val b2i: String) {

    init {
        if (b2i == "" || !b2i.all { it in "0123." } || b2i.count { it == '.'} > 1 )
            throw RuntimeException("Invalid Base 2i number")
    }

    fun toComplex(): Complex {
        val pointPos = b2i.indexOf(".")
        var posLen = if (pointPos != -1) pointPos else b2i.length
        var sum = Complex(0.0, 0.0)
        var prod = Complex(1.0, 0.0)
        for (j in 0 until posLen) {
            val k = (b2i[posLen - 1 - j] - '0').toDouble()
            if (k > 0.0) sum += prod * k
            prod *= twoI
        }
        if (pointPos != -1) {
            prod = invTwoI
            for (j in posLen + 1 until b2i.length) {
                val k = (b2i[j] - '0').toDouble()
                if (k > 0.0) sum += prod * k
                prod *= invTwoI
            }
        }
        return sum
    }

    override fun toString() = b2i

    companion object {
        val twoI = Complex(0.0, 2.0)
        val invTwoI = twoI.inv()
    }
}

fun main(args: Array<String>) {
    val fmt = "%4s -> %8s -> %4s"
    for (i in 1..16) {
        var c1 = Complex(i, 0)
        var qi = c1.toQuaterImaginary()
        var c2 = qi.toComplex()
        print("$fmt     ".format(c1, qi, c2))
        c1 = -c1
        qi = c1.toQuaterImaginary()
        c2 = qi.toComplex()
        println(fmt.format(c1, qi, c2))
    }
    println()
    for (i in 1..16) {
        var c1 = Complex(0, i)
        var qi = c1.toQuaterImaginary()
        var c2 = qi.toComplex()
        print("$fmt     ".format(c1, qi, c2))
        c1 = -c1
        qi = c1.toQuaterImaginary()
        c2 = qi.toComplex()
        println(fmt.format(c1, qi, c2))
    }
}
```


{{out}}

```txt

   1 ->        1 ->    1       -1 ->      103 ->   -1
   2 ->        2 ->    2       -2 ->      102 ->   -2
   3 ->        3 ->    3       -3 ->      101 ->   -3
   4 ->    10300 ->    4       -4 ->      100 ->   -4
   5 ->    10301 ->    5       -5 ->      203 ->   -5
   6 ->    10302 ->    6       -6 ->      202 ->   -6
   7 ->    10303 ->    7       -7 ->      201 ->   -7
   8 ->    10200 ->    8       -8 ->      200 ->   -8
   9 ->    10201 ->    9       -9 ->      303 ->   -9
  10 ->    10202 ->   10      -10 ->      302 ->  -10
  11 ->    10203 ->   11      -11 ->      301 ->  -11
  12 ->    10100 ->   12      -12 ->      300 ->  -12
  13 ->    10101 ->   13      -13 ->  1030003 ->  -13
  14 ->    10102 ->   14      -14 ->  1030002 ->  -14
  15 ->    10103 ->   15      -15 ->  1030001 ->  -15
  16 ->    10000 ->   16      -16 ->  1030000 ->  -16

  1i ->     10.2 ->   1i      -1i ->      0.2 ->  -1i
  2i ->     10.0 ->   2i      -2i ->   1030.0 ->  -2i
  3i ->     20.2 ->   3i      -3i ->   1030.2 ->  -3i
  4i ->     20.0 ->   4i      -4i ->   1020.0 ->  -4i
  5i ->     30.2 ->   5i      -5i ->   1020.2 ->  -5i
  6i ->     30.0 ->   6i      -6i ->   1010.0 ->  -6i
  7i -> 103000.2 ->   7i      -7i ->   1010.2 ->  -7i
  8i -> 103000.0 ->   8i      -8i ->   1000.0 ->  -8i
  9i -> 103010.2 ->   9i      -9i ->   1000.2 ->  -9i
 10i -> 103010.0 ->  10i     -10i ->   2030.0 -> -10i
 11i -> 103020.2 ->  11i     -11i ->   2030.2 -> -11i
 12i -> 103020.0 ->  12i     -12i ->   2020.0 -> -12i
 13i -> 103030.2 ->  13i     -13i ->   2020.2 -> -13i
 14i -> 103030.0 ->  14i     -14i ->   2010.0 -> -14i
 15i -> 102000.2 ->  15i     -15i ->   2010.2 -> -15i
 16i -> 102000.0 ->  16i     -16i ->   2000.0 -> -16i

```


=={{header|Modula-2}}==
{{trans|C#}}

```modula2
MODULE ImaginaryBase;
FROM FormatString IMPORT FormatString;
FROM RealMath IMPORT round;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

(* Helper *)
TYPE
    String = ARRAY[0..10] OF CHAR;
    StringBuilder = RECORD
        buf : String;
        ptr : CARDINAL;
    END;

PROCEDURE ToChar(n : INTEGER) : CHAR;
BEGIN
    CASE n OF
        0 : RETURN '0' |
        1 : RETURN '1' |
        2 : RETURN '2' |
        3 : RETURN '3' |
        4 : RETURN '4' |
        5 : RETURN '5' |
        6 : RETURN '6' |
        7 : RETURN '7' |
        8 : RETURN '8' |
        9 : RETURN '9'
    ELSE
        RETURN '-'
    END
END ToChar;

PROCEDURE AppendChar(VAR sb : StringBuilder; c : CHAR);
BEGIN
    sb.buf[sb.ptr] := c;
    INC(sb.ptr);
    sb.buf[sb.ptr] := 0C
END AppendChar;

PROCEDURE AppendInt(VAR sb : StringBuilder; n : INTEGER);
BEGIN
    sb.buf[sb.ptr] := ToChar(n);
    INC(sb.ptr);
    sb.buf[sb.ptr] := 0C
END AppendInt;

PROCEDURE Ceil(r : REAL) : REAL;
VAR t : REAL;
BEGIN
    t := FLOAT(INT(r));
    IF r - t > 0.0 THEN
        t := t + 1.0
    END;
    RETURN t
END Ceil;

PROCEDURE Modulus(q,d : INTEGER) : INTEGER;
VAR t : INTEGER;
BEGIN
    t := q / d;
    RETURN q - d * t
END Modulus;

PROCEDURE PrependInt(VAR sb : StringBuilder; n : INTEGER);
VAR i : CARDINAL;
BEGIN
    i := sb.ptr;
    INC(sb.ptr);
    sb.buf[sb.ptr] := 0C;
    WHILE i > 0 DO
        sb.buf[i] := sb.buf[i-1];
        DEC(i)
    END;
    sb.buf[0] := ToChar(n)
END PrependInt;

PROCEDURE Reverse(VAR str : String);
VAR
    i,j : CARDINAL;
    c : CHAR;
BEGIN
    IF str[0] = 0C THEN RETURN END;
    i := 0;
    WHILE str[i] # 0C DO INC(i) END;
    DEC(i);
    j := 0;
    WHILE i > j DO
        c := str[i];
        str[i] := str[j];
        str[j] := c;

        DEC(i);
        INC(j)
    END
END Reverse;

PROCEDURE TrimStart(VAR str : String; c : CHAR);
VAR i : CARDINAL;
BEGIN
    WHILE str[0] = c DO
        i := 0;
        WHILE str[i] # 0C DO
            str[i] := str[i+1];
            INC(i)
        END
    END
END TrimStart;

PROCEDURE WriteInteger(n : INTEGER);
VAR buf : ARRAY[0..15] OF CHAR;
BEGIN
    FormatString("%i", buf, n);
    WriteString(buf)
END WriteInteger;

(* Imaginary *)
TYPE
    Complex = RECORD
        real,imag : REAL;
    END;
    QuaterImaginary = RECORD
        b2i : String;
    END;

PROCEDURE ComplexMul(lhs,rhs : Complex) : Complex;
BEGIN
    RETURN Complex{
        rhs.real * lhs.real - rhs.imag * lhs.imag,
        rhs.real * lhs.imag + rhs.imag * lhs.real
    }
END ComplexMul;

PROCEDURE ComplexMulR(lhs : Complex; rhs : REAL) : Complex;
BEGIN
    RETURN Complex{lhs.real * rhs, lhs.imag * rhs}
END ComplexMulR;

PROCEDURE ComplexInv(c : Complex) : Complex;
VAR denom : REAL;
BEGIN
    denom := c.real * c.real + c.imag * c.imag;
    RETURN Complex{c.real / denom, -c.imag / denom}
END ComplexInv;

PROCEDURE ComplexDiv(lhs,rhs : Complex) : Complex;
BEGIN
    RETURN ComplexMul(lhs, ComplexInv(rhs))
END ComplexDiv;

PROCEDURE ComplexNeg(c : Complex) : Complex;
BEGIN
    RETURN Complex{-c.real, -c.imag}
END ComplexNeg;

PROCEDURE ComplexSum(lhs,rhs : Complex) : Complex;
BEGIN
    RETURN Complex{lhs.real + rhs.real, lhs.imag + rhs.imag}
END ComplexSum;

PROCEDURE WriteComplex(c : Complex);
VAR buf : ARRAY[0..15] OF CHAR;
BEGIN
    IF c.imag = 0.0 THEN
        WriteInteger(INT(c.real))
    ELSIF c.real = 0.0 THEN
        WriteInteger(INT(c.imag));
        WriteString("i")
    ELSIF c.imag > 0.0 THEN
        WriteInteger(INT(c.real));
        WriteString(" + ");
        WriteInteger(INT(c.imag));
        WriteString("i")
    ELSE
        WriteInteger(INT(c.real));
        WriteString(" - ");
        WriteInteger(INT(-c.imag));
        WriteString("i")
    END
END WriteComplex;

PROCEDURE ToQuaterImaginary(c : Complex) : QuaterImaginary;
VAR
    re,im,fi,rem,index : INTEGER;
    f : REAL;
    t : Complex;
    sb : StringBuilder;
BEGIN
    IF (c.real = 0.0) AND (c.imag = 0.0) THEN RETURN QuaterImaginary{"0"} END;
    re := INT(c.real);
    im := INT(c.imag);
    fi := -1;
    sb := StringBuilder{"", 0};
    WHILE re # 0 DO
        rem := Modulus(re, -4);
        re := re / (-4);
        IF rem < 0 THEN
            rem := 4 + rem;
            INC(re)
        END;
        AppendInt(sb, rem);
        AppendInt(sb, 0)
    END;
    IF im # 0 THEN
        t := ComplexDiv(Complex{0.0, c.imag}, Complex{0.0, 2.0});
        f := t.real;
        im := INT(Ceil(f));
        f := -4.0 * (f - FLOAT(im));
        index := 1;
        WHILE im # 0 DO
            rem := Modulus(im, -4);
            im := im / (-4);
            IF rem < 0 THEN
                rem := 4 + rem;
                INC(im)
            END;
            IF index < INT(sb.ptr) THEN
                sb.buf[index] := ToChar(rem)
            ELSE
                AppendInt(sb, 0);
                AppendInt(sb, rem)
            END;
            index := index + 2;
        END;
        fi := INT(f)
    END;
    Reverse(sb.buf);
    IF fi # -1 THEN
        AppendChar(sb, '.');
        AppendInt(sb, fi)
    END;
    TrimStart(sb.buf, '0');
    IF sb.buf[0] = '.' THEN
        PrependInt(sb, 0)
    END;

    RETURN QuaterImaginary{sb.buf}
END ToQuaterImaginary;

PROCEDURE ToComplex(qi : QuaterImaginary) : Complex;
VAR
    j,pointPos,posLen,b2iLen : INTEGER;
    k : REAL;
    sum,prod : Complex;
BEGIN
    pointPos := 0;
    WHILE (qi.b2i[pointPos] # 0C) AND (qi.b2i[pointPos] # '.') DO
        INC(pointPos)
    END;
    IF qi.b2i[pointPos] # '.' THEN
        pointPos := -1;
        posLen := 0;
        WHILE qi.b2i[posLen] # 0C DO
            INC(posLen)
        END
    ELSE
        posLen := pointPos
    END;

    sum := Complex{0.0, 0.0};
    prod := Complex{1.0, 0.0};

    FOR j:=0 TO posLen - 1 DO
        k := FLOAT(ORD(qi.b2i[posLen - 1 - j]) - ORD('0'));
        IF k > 0.0 THEN
            sum := ComplexSum(sum, ComplexMulR(prod, k))
        END;
        prod := ComplexMul(prod, Complex{0.0, 2.0})
    END;

    IF pointPos # -1 THEN
        prod := ComplexInv(Complex{0.0, 2.0});
        b2iLen := 0;
        WHILE qi.b2i[b2iLen] # 0C DO INC(b2iLen) END;
        FOR j:=posLen + 1 TO b2iLen - 1 DO
            k := FLOAT(ORD(qi.b2i[j]) - ORD('0'));
            IF k > 0.0 THEN
                sum := ComplexSum(sum, ComplexMulR(prod, k))
            END;
            prod := ComplexMul(prod, ComplexInv(Complex{0.0, 2.0}))
        END
    END;

    RETURN sum
END ToComplex;

(* Main *)
VAR
    c1,c2 : Complex;
    qi : QuaterImaginary;
    i : INTEGER;
BEGIN
    FOR i:=1 TO 16 DO
        c1 := Complex{FLOAT(i), 0.0};
        WriteComplex(c1);
        WriteString(" -> ");
        qi := ToQuaterImaginary(c1);
        WriteString(qi.b2i);
        WriteString(" -> ");
        c2 := ToComplex(qi);
        WriteComplex(c2);
        WriteString("   ");

        c1 := ComplexNeg(c1);
        WriteComplex(c1);
        WriteString(" -> ");
        qi := ToQuaterImaginary(c1);
        WriteString(qi.b2i);
        WriteString(" -> ");
        c2 := ToComplex(qi);
        WriteComplex(c2);
        WriteLn
    END;
    WriteLn;

    FOR i:=1 TO 16 DO
        c1 := Complex{0.0, FLOAT(i)};
        WriteComplex(c1);
        WriteString(" -> ");
        qi := ToQuaterImaginary(c1);
        WriteString(qi.b2i);
        WriteString(" -> ");
        c2 := ToComplex(qi);
        WriteComplex(c2);
        WriteString("   ");

        c1 := ComplexNeg(c1);
        WriteComplex(c1);
        WriteString(" -> ");
        qi := ToQuaterImaginary(c1);
        WriteString(qi.b2i);
        WriteString(" -> ");
        c2 := ToComplex(qi);
        WriteComplex(c2);
        WriteLn
    END;

    ReadChar
END ImaginaryBase.
```

{{out}}

```txt
1 -> 1 -> 1   -1 -> 103 -> -1
2 -> 2 -> 2   -2 -> 102 -> -2
3 -> 3 -> 3   -3 -> 101 -> -3
4 -> 10300 -> 4   -4 -> 100 -> -4
5 -> 10301 -> 5   -5 -> 203 -> -5
6 -> 10302 -> 6   -6 -> 202 -> -6
7 -> 10303 -> 7   -7 -> 201 -> -7
8 -> 10200 -> 8   -8 -> 200 -> -8
9 -> 10201 -> 9   -9 -> 303 -> -9
10 -> 10202 -> 10   -10 -> 302 -> -10
11 -> 10203 -> 11   -11 -> 301 -> -11
12 -> 10100 -> 12   -12 -> 300 -> -12
13 -> 10101 -> 13   -13 -> 1030003 -> -13
14 -> 10102 -> 14   -14 -> 1030002 -> -14
15 -> 10103 -> 15   -15 -> 1030001 -> -15
16 -> 10000 -> 16   -16 -> 1030000 -> -16

1i -> 10.2 -> 1i   -1i -> 0.2 -> -1i
2i -> 10.0 -> 2i   -2i -> 1030.0 -> -2i
3i -> 20.2 -> 3i   -3i -> 1030.2 -> -3i
4i -> 20.0 -> 4i   -4i -> 1020.0 -> -4i
5i -> 30.2 -> 5i   -5i -> 1020.2 -> -5i
6i -> 30.0 -> 6i   -6i -> 1010.0 -> -6i
7i -> 103000.2 -> 7i   -7i -> 1010.2 -> -7i
8i -> 103000.0 -> 8i   -8i -> 1000.0 -> -8i
9i -> 103010.2 -> 9i   -9i -> 1000.2 -> -9i
10i -> 103010.0 -> 10i   -10i -> 2030.0 -> -10i
11i -> 103020.2 -> 11i   -11i -> 2030.2 -> -11i
12i -> 103020.0 -> 12i   -12i -> 2020.0 -> -12i
13i -> 103030.2 -> 13i   -13i -> 2020.2 -> -13i
14i -> 103030.0 -> 14i   -14i -> 2010.0 -> -14i
15i -> 102000.2 -> 15i   -15i -> 2010.2 -> -15i
16i -> 102000.0 -> 16i   -16i -> 2000.0 -> -16i
```



## Perl 6

{{works with|Rakudo|2017.01}}
These are generalized imaginary-base conversion routines. They only work for imaginary bases, not complex. (Any real portion of the radix must be zero.) Theoretically they could be made to work for any imaginary base; in practice, they are limited to integer bases from -6i to -2i and 2i to 6i. Bases -1i and 1i exist but require special handling and are not supported. Bases larger than 6i (or -6i) require digits outside of base 36 to express them, so aren't as standardized, are implementation dependent and are not supported. Note that imaginary number coefficients are stored as floating point numbers in Perl 6 so some rounding error may creep in during calculations. The precision these conversion routines use is configurable; we are using 6 <strike>decimal</strike>, um... radicimal(?) places of precision here.

Implements minimum, extra kudos and stretch goals.


```perl6
multi sub base ( Real $num, Int $radix where -37 < * < -1, :$precision = -15 ) {
    return '0' unless $num;
    my $value  = $num;
    my $result = '';
    my $place  = 0;
    my $upper-bound = 1 / (-$radix + 1);
    my $lower-bound = $radix * $upper-bound;

    $value = $num / $radix ** ++$place until $lower-bound <= $value < $upper-bound;

    while ($value or $place > 0) and $place > $precision {
        my $digit = ($radix * $value - $lower-bound).Int;
        $value    =  $radix * $value - $digit;
        $result ~= '.' unless $place or $result.contains: '.';
        $result ~= $digit == -$radix ?? ($digit-1).base(-$radix)~'0' !! $digit.base(-$radix);
        $place--
    }
    $result
}

multi sub base (Numeric $num, Complex $radix where *.re == 0, :$precision = -8 ) {
    die "Base $radix out of range" unless -6 <= $radix.im <= -2 or 2 <= $radix.im <= 6;
    my ($re, $im) = $num.Complex.reals;
    my ($re-wh, $re-fr) =             $re.&base( -$radix.im².Int, :precision($precision) ).split: '.';
    my ($im-wh, $im-fr) = ($im/$radix.im).&base( -$radix.im².Int, :precision($precision) ).split: '.';
    $_ //= '' for $re-fr, $im-fr;

    sub zip (Str $a, Str $b) {
        my $l = '0' x ($a.chars - $b.chars).abs;
        ([~] flat ($a~$l).comb Z flat ($b~$l).comb).subst(/ '0'+ $ /, '') || '0'
    }

    my $whole = flip zip $re-wh.flip, $im-wh.flip;
    my $fraction = zip $im-fr, $re-fr;
    $fraction eq 0 ?? "$whole" !! "$whole.$fraction"
}

multi sub parse-base (Str $str, Complex $radix where *.re == 0) {
    return -1 * $str.substr(1).&parse-base($radix) if $str.substr(0,1) eq '-';
    my ($whole, $frac) = $str.split: '.';
    my $fraction = 0;
    $fraction = [+] $frac.comb.kv.map: { $^v.parse-base($radix.im².Int) * $radix ** -($^k+1) } if $frac;
    $fraction + [+] $whole.flip.comb.kv.map: { $^v.parse-base($radix.im².Int) * $radix ** $^k }
}

# TESTING
for 0, 2i, 1, 2i, 5, 2i, -13, 2i, 9i, 2i, -3i, 2i, 7.75-7.5i, 2i, .25, 2i, # base 2i tests
    5+5i,  2i, 5+5i,  3i, 5+5i,  4i, 5+5i,  5i, 5+5i,  6i, # same value, positive imaginary bases
    5+5i, -2i, 5+5i, -3i, 5+5i, -4i, 5+5i, -5i, 5+5i, -6i, # same value, negative imaginary bases
    227.65625+10.859375i, 4i, # larger test value
    31433.3487654321-2902.4480452675i, 6i # heh
  -> $v, $r {
my $ibase = $v.&base($r, :precision(-6));
printf "%33s.&base\(%2si\) = %-11s : %13s.&parse-base\(%2si\) = %s\n",
  $v, $r.im, $ibase, "'$ibase'", $r.im, $ibase.&parse-base($r).round(1e-10).narrow;
}
```

{{out}}

```txt
                                0.&base( 2i) = 0           :           '0'.&parse-base( 2i) = 0
                                1.&base( 2i) = 1           :           '1'.&parse-base( 2i) = 1
                                5.&base( 2i) = 10301       :       '10301'.&parse-base( 2i) = 5
                              -13.&base( 2i) = 1030003     :     '1030003'.&parse-base( 2i) = -13
                             0+9i.&base( 2i) = 103010.2    :    '103010.2'.&parse-base( 2i) = 0+9i
                            -0-3i.&base( 2i) = 1030.2      :      '1030.2'.&parse-base( 2i) = 0-3i
                        7.75-7.5i.&base( 2i) = 11210.31    :    '11210.31'.&parse-base( 2i) = 7.75-7.5i
                             0.25.&base( 2i) = 1.03        :        '1.03'.&parse-base( 2i) = 0.25
                             5+5i.&base( 2i) = 10331.2     :     '10331.2'.&parse-base( 2i) = 5+5i
                             5+5i.&base( 3i) = 25.3        :        '25.3'.&parse-base( 3i) = 5+5i
                             5+5i.&base( 4i) = 25.C        :        '25.C'.&parse-base( 4i) = 5+5i
                             5+5i.&base( 5i) = 15          :          '15'.&parse-base( 5i) = 5+5i
                             5+5i.&base( 6i) = 15.6        :        '15.6'.&parse-base( 6i) = 5+5i
                             5+5i.&base(-2i) = 11321.2     :     '11321.2'.&parse-base(-2i) = 5+5i
                             5+5i.&base(-3i) = 1085.6      :      '1085.6'.&parse-base(-3i) = 5+5i
                             5+5i.&base(-4i) = 10F5.4      :      '10F5.4'.&parse-base(-4i) = 5+5i
                             5+5i.&base(-5i) = 10O5        :        '10O5'.&parse-base(-5i) = 5+5i
                             5+5i.&base(-6i) = 5.U         :         '5.U'.&parse-base(-6i) = 5+5i
             227.65625+10.859375i.&base( 4i) = 10234.5678  :  '10234.5678'.&parse-base( 4i) = 227.65625+10.859375i
31433.3487654321-2902.4480452675i.&base( 6i) = PERL6.ROCKS : 'PERL6.ROCKS'.&parse-base( 6i) = 31433.3487654321-2902.4480452675i
```



## Phix

{{trans|Sidef}}

```Phix
include complex.e

function base2(atom num, integer radix, precision = -8)
    if radix<-36 or radix>-2 then throw("radix out of range (-2..-36)") end if
    sequence result
    if num=0 then
        result = {"0",""}
    else
        integer place = 0
        result = ""
        atom v = num
        atom upper_bound = 1/(1-radix),
             lower_bound = radix*upper_bound
        while not(lower_bound <= v) or not(v < upper_bound) do
            place += 1
            v = num/power(radix,place)
        end while

        while (v or place > 0) and (place > precision) do
            integer digit = floor(radix*v - lower_bound)
            v = (radix*v - digit)
            if place=0 and not find('.',result) then result &= '.' end if
            result &= digit+iff(digit>9?'a'-10:'0')
            place -= 1
        end while
        integer dot = find('.',result)
        if dot then
            result = trim_tail(result,'0')
            result = {result[1..dot-1],result[dot+1..$]}
        else
            result = {result,""}
        end if
    end if
    return result
end function

function zip(string a, string b)
    integer ld = length(a)-length(b)
    if ld!=0 then
        if ld>0 then
            b &= repeat('0',ld)
        else
            a &= repeat('0',abs(ld))
        end if
    end if
    string res = ""
    for i=1 to length(a) do
        res &= a[i]&b[i]
    end for
    res = trim_tail(res,'0')
    if res="" then res = "0" end if
    return res
end function

function base(complexn num, integer radix, precision = -8)

    integer absrad = abs(radix),
            radix2 = -power(radix,2)
    if absrad<2 or absrad>6 then throw("base radix out of range") end if

    atom {re, im}         = {complex_real(num), complex_imag(num)}
    string {re_wh, re_fr} = base2(re,       radix2, precision),
           {im_wh, im_fr} = base2(im/radix, radix2, precision)

    string whole = reverse(zip(reverse(re_wh), reverse(im_wh))),
           fraction = zip(im_fr, re_fr)
    if fraction!="0" then whole &= '.'&fraction end if
    return whole
end function

function parse_base(string str, integer radix)

    complexn fraction = 0

    integer dot = find('.',str)
    if dot then
        string fr = str[dot+1..$]
        for i=1 to length(fr) do
            integer c = fr[i]
            c -= iff(c>='a'?'a'-10:'0')
            fraction = complex_add(fraction,complex_mul(c,complex_power({0,radix},-i)))
        end for
        str = str[1..dot-1]
    end if

    str = reverse(str)
    for i=1 to length(str) do
        integer c = str[i]
        c -= iff(c>='a'?'a'-10:'0')
        fraction = complex_add(fraction,complex_mul(c,complex_power({0,radix},(i-1))))
    end for

    return fraction
end function

constant tests = {{0,2},{1,2},{5,2},{-13,2},{{0,9},2},{{0,-3},2},{{7.75,-7.5}, 2},{.25, 2}, -- base 2i tests
                  {{5,5}, 2},{{5,5}, 3},{{5,5}, 4},{{5,5}, 5},{{5,5}, 6}, -- same value, positive imaginary bases
                  {{5,5},-2},{{5,5},-3},{{5,5},-4},{{5,5},-5},{{5,5},-6}, -- same value, negative imaginary bases
                  {{227.65625,10.859375},4}, -- larger test value
                  {{-579.8225308641975744,-5296.406378600824},6}}   -- phix.rules

-- matches output of Sidef and Perl6:
for t=1 to length(tests) do
    {complexn v, integer r} = tests[t]
    string ibase = base(v,r),
           strv = complex_sprint(v),
           strb = complex_sprint(parse_base(ibase, r))
    printf(1,"base(%20s, %2di) = %-10s : parse_base(%12s, %2di) = %s\n",
                  {strv,  r,     ibase,    '"'&ibase&'"', r,     strb})
end for

-- matches output of Kotlin, Java, Go, D, and C#:
for ri=1 to 2 do    -- real then imag
    for i=1 to 16 do
        complexn c = iff(ri=1?i:{0,i}),
                nc = complex_neg(c)
        string sc = complex_sprint(c),
              snc = complex_sprint(nc),
               ib = base(c,2),
              inb = base(nc,2),
               rc = complex_sprint(parse_base(ib,2)),
              rnc = complex_sprint(parse_base(inb,2))
        printf(1,"%4s -> %8s -> %4s     %4s -> %8s -> %4s\n",
                 {sc,    ib,    rc,     snc,   inb,   rnc })
    end for
    puts(1,"\n")
end for
```

{{out}}
Matches the output of Sidef and Perl6, except for the final line:

```txt

base(   -579.823-5296.41i,  6i) = phix.rules : parse_base("phix.rules",  6i) = -579.823-5296.41i

```

Also matches the output of Kotlin, Java, Go, D, and C#, except the even entries in the second half, eg:

```txt

  2i ->       10 ->   2i      -2i ->     1030 ->  -2i

```

instead of

```txt

  2i ->     10.0 ->   2i      -2i ->   1030.0 ->  -2i

```

ie the unnecessary trailing ".0" are trimmed. (see talk page)


## Python

{{trans|C++}}

```python
import math
import re

def inv(c):
    denom = c.real * c.real + c.imag * c.imag
    return complex(c.real / denom, -c.imag / denom)

class QuaterImaginary:
    twoI = complex(0, 2)
    invTwoI = inv(twoI)

    def __init__(self, str):
        if not re.match("^[0123.]+$", str) or str.count('.') > 1:
            raise Exception('Invalid base 2i number')
        self.b2i = str

    def toComplex(self):
        pointPos = self.b2i.find('.')
        posLen = len(self.b2i) if (pointPos < 0) else pointPos
        sum = complex(0, 0)
        prod = complex(1, 0)
        for j in xrange(0, posLen):
            k = (ord(self.b2i[posLen - 1 - j]) - ord('0'))
            if k > 0:
                sum = sum + prod * k
            prod = prod * QuaterImaginary.twoI
        if pointPos != -1:
            prod = QuaterImaginary.invTwoI
            for j in xrange(posLen + 1, len(self.b2i)):
                k = (ord(self.b2i[j]) - ord('0'))
                if k > 0:
                    sum = sum + prod * k
                prod = prod * QuaterImaginary.invTwoI
        return sum

    def __str__(self):
        return str(self.b2i)

def toQuaterImaginary(c):
    if c.real == 0.0 and c.imag == 0.0:
        return QuaterImaginary("0")

    re = int(c.real)
    im = int(c.imag)
    fi = -1
    ss = ""
    while re != 0:
        rem = re % -4
        re = re / -4
        if rem < 0:
            rem = 4 + rem
            re = re + 1
        ss = ss + str(rem) + '0'
    if im != 0:
        f = (complex(0, c.imag) / complex(0, 2)).real
        im = int(math.ceil(f))
        f = -4 * (f - im)
        index = 1
        while im != 0:
            rem = im % -4
            im = im / -4
            if rem < 0:
                rem = 4 + rem
                im = im + 1
            if index < len(ss):
                ss[index] = chr(rem + 48)
            else:
                ss = ss + '0' + str(rem)
            index = index + 2
        fi = int(f)
    ss = ss[::-1]
    if fi != -1:
        ss = ss + '.' + str(fi)
    ss = ss.lstrip('0')
    if ss[0] == '.':
        ss = '0' + ss
    return QuaterImaginary(ss)

for i in xrange(1,17):
    c1 = complex(i, 0)
    qi = toQuaterImaginary(c1)
    c2 = qi.toComplex()
    print "{0:8} -> {1:>8} -> {2:8}     ".format(c1, qi, c2),

    c1 = -c1
    qi = toQuaterImaginary(c1)
    c2 = qi.toComplex()
    print "{0:8} -> {1:>8} -> {2:8}".format(c1, qi, c2)
print

for i in xrange(1,17):
    c1 = complex(0, i)
    qi = toQuaterImaginary(c1)
    c2 = qi.toComplex()
    print "{0:8} -> {1:>8} -> {2:8}     ".format(c1, qi, c2),

    c1 = -c1
    qi = toQuaterImaginary(c1)
    c2 = qi.toComplex()
    print "{0:8} -> {1:>8} -> {2:8}".format(c1, qi, c2)

print "done"

```

{{out}}

```txt
  (1+0j) ->        1 ->   (1+0j)       (-1-0j) ->      103 ->  (-1+0j)
  (2+0j) ->        2 ->   (2+0j)       (-2-0j) ->      102 ->  (-2+0j)
  (3+0j) ->        3 ->   (3+0j)       (-3-0j) ->      101 ->  (-3+0j)
  (4+0j) ->    10300 ->   (4+0j)       (-4-0j) ->      100 ->  (-4+0j)
  (5+0j) ->    10301 ->   (5+0j)       (-5-0j) ->      203 ->  (-5+0j)
  (6+0j) ->    10302 ->   (6+0j)       (-6-0j) ->      202 ->  (-6+0j)
  (7+0j) ->    10303 ->   (7+0j)       (-7-0j) ->      201 ->  (-7+0j)
  (8+0j) ->    10200 ->   (8+0j)       (-8-0j) ->      200 ->  (-8+0j)
  (9+0j) ->    10201 ->   (9+0j)       (-9-0j) ->      303 ->  (-9+0j)
 (10+0j) ->    10202 ->  (10+0j)      (-10-0j) ->      302 -> (-10+0j)
 (11+0j) ->    10203 ->  (11+0j)      (-11-0j) ->      301 -> (-11+0j)
 (12+0j) ->    10100 ->  (12+0j)      (-12-0j) ->      300 -> (-12+0j)
 (13+0j) ->    10101 ->  (13+0j)      (-13-0j) ->  1030003 -> (-13+0j)
 (14+0j) ->    10102 ->  (14+0j)      (-14-0j) ->  1030002 -> (-14+0j)
 (15+0j) ->    10103 ->  (15+0j)      (-15-0j) ->  1030001 -> (-15+0j)
 (16+0j) ->    10000 ->  (16+0j)      (-16-0j) ->  1030000 -> (-16+0j)

      1j ->     10.2 ->       1j       (-0-1j) ->      0.2 ->      -1j
      2j ->     10.0 ->       2j       (-0-2j) ->   1030.0 ->      -2j
      3j ->     20.2 ->       3j       (-0-3j) ->   1030.2 ->      -3j
      4j ->     20.0 ->       4j       (-0-4j) ->   1020.0 ->      -4j
      5j ->     30.2 ->       5j       (-0-5j) ->   1020.2 ->      -5j
      6j ->     30.0 ->       6j       (-0-6j) ->   1010.0 ->      -6j
      7j -> 103000.2 ->       7j       (-0-7j) ->   1010.2 ->      -7j
      8j -> 103000.0 ->       8j       (-0-8j) ->   1000.0 ->      -8j
      9j -> 103010.2 ->       9j       (-0-9j) ->   1000.2 ->      -9j
     10j -> 103010.0 ->      10j      (-0-10j) ->   2030.0 ->     -10j
     11j -> 103020.2 ->      11j      (-0-11j) ->   2030.2 ->     -11j
     12j -> 103020.0 ->      12j      (-0-12j) ->   2020.0 ->     -12j
     13j -> 103030.2 ->      13j      (-0-13j) ->   2020.2 ->     -13j
     14j -> 103030.0 ->      14j      (-0-14j) ->   2010.0 ->     -14j
     15j -> 102000.2 ->      15j      (-0-15j) ->   2010.2 ->     -15j
     16j -> 102000.0 ->      16j      (-0-16j) ->   2000.0 ->     -16j
done
```



## Sidef

{{trans|Perl 6}}

```ruby
func base (Number num, Number radix { _ ~~ (-36 .. -2) }, precision = -15) -> String {
    num || return '0'

    var place  = 0
    var result = ''
    var value  = num
    var upper_bound = 1/(-radix + 1)
    var lower_bound = radix*upper_bound

    while (!(lower_bound <= value) || !(value < upper_bound)) {
        value = num/(radix**++place)
    }

    while ((value || (place > 0)) && (place > precision)) {
        var digit = (radix*value - lower_bound -> int)
        value    =  (radix*value - digit)
        result += '.' if (!place && !result.contains('.'))
        result += ((digit == -radix) ? (digit-1 -> base(-radix) + '0') : digit.base(-radix))
        place--
    }

    return result
}

func base (Number num, Number radix { .re == 0 }, precision = -8) -> String {

    (radix.im.abs ~~ 2..6) || die "Base #{radix} out of range"

    var (re, im)          = (num.re, num.im)
    var (re_wh, re_fr='') = base(re,          -radix.im**2, precision).split('.')...
    var (im_wh, im_fr='') = base(im/radix.im, -radix.im**2, precision).split('.')...

    func zip (String a, String b) {
        var l = ('0' * abs(a.len - b.len))
        chars(a+l) ~Z chars(b+l) -> flat.join.sub(/0+\z/, '') || '0'
    }

    var whole = zip(re_wh.flip, im_wh.flip).flip
    var fraction = zip(im_fr, re_fr)
    fraction == '0' ? whole : "#{whole}.#{fraction}"
}

func parse_base (String str, Number radix { .re == 0 }) -> Number {

    if (str.char(0) == '-') {
        return (-1 * parse_base(str.substr(1), radix))
    }

    var (whole, frac='') = str.split('.')...

    var fraction = frac.chars.map_kv {|k,v|
        Number(v, radix.im**2) * radix**-(k+1)
    }.sum

    fraction += whole.flip.chars.map_kv {|k,v|
        Number(v, radix.im**2) * radix**k
    }.sum

    return fraction
}

var tests = [0, 2i, 1, 2i, 5, 2i, -13, 2i, 9i, 2i, -3i, 2i, 7.75-7.5i, 2i, .25, 2i, # base 2i tests
    5+5i,  2i, 5+5i,  3i, 5+5i,  4i, 5+5i,  5i, 5+5i,  6i, # same value, positive imaginary bases
    5+5i, -2i, 5+5i, -3i, 5+5i, -4i, 5+5i, -5i, 5+5i, -6i, # same value, negative imaginary bases
    227.65625+10.859375i, 4i] # larger test value

tests.each_slice(2, {|v,r|
    var ibase = base(v, r)
    printf("base(%20s, %2si) = %-10s : parse_base(%12s, %2si) = %s\n",
        v, r.im, ibase, "'#{ibase}'", r.im, parse_base(ibase, r).round(-8))
})
```

{{out}}

```txt

base(                   0,  2i) = 0          : parse_base(         '0',  2i) = 0
base(                   1,  2i) = 1          : parse_base(         '1',  2i) = 1
base(                   5,  2i) = 10301      : parse_base(     '10301',  2i) = 5
base(                 -13,  2i) = 1030003    : parse_base(   '1030003',  2i) = -13
base(                  9i,  2i) = 103010.2   : parse_base(  '103010.2',  2i) = 9i
base(                 -3i,  2i) = 1030.2     : parse_base(    '1030.2',  2i) = -3i
base(           7.75-7.5i,  2i) = 11210.31   : parse_base(  '11210.31',  2i) = 7.75-7.5i
base(                0.25,  2i) = 1.03       : parse_base(      '1.03',  2i) = 0.25
base(                5+5i,  2i) = 10331.2    : parse_base(   '10331.2',  2i) = 5+5i
base(                5+5i,  3i) = 25.3       : parse_base(      '25.3',  3i) = 5+5i
base(                5+5i,  4i) = 25.c       : parse_base(      '25.c',  4i) = 5+5i
base(                5+5i,  5i) = 15         : parse_base(        '15',  5i) = 5+5i
base(                5+5i,  6i) = 15.6       : parse_base(      '15.6',  6i) = 5+5i
base(                5+5i, -2i) = 11321.2    : parse_base(   '11321.2', -2i) = 5+5i
base(                5+5i, -3i) = 1085.6     : parse_base(    '1085.6', -3i) = 5+5i
base(                5+5i, -4i) = 10f5.4     : parse_base(    '10f5.4', -4i) = 5+5i
base(                5+5i, -5i) = 10o5       : parse_base(      '10o5', -5i) = 5+5i
base(                5+5i, -6i) = 5.u        : parse_base(       '5.u', -6i) = 5+5i
base(227.65625+10.859375i,  4i) = 10234.5678 : parse_base('10234.5678',  4i) = 227.65625+10.859375i

```

