+++
title = "Align columns/C++"
description = ""
date = 2010-02-06T12:45:47Z
aliases = []
[extra]
id = 5296
[taxonomies]
categories = []
tags = []
+++

{{collection|Column Aligner}}

The following code fragments are all in one source file for this example (and in this order), but broken up here for clarity.

A reusable template function that handles the tokenizing, and is independent of any work that might wish to be done with the results:


```cpp>#include <vector

#include <string>       // for getline etc.
#include <iostream>
#include <sstream>      // for istringstream
#include <algorithm>    // for max
#include <iomanip>      // for setw
#include <fstream>      // for ofstream

using namespace std;

template< typename C >
void enumerateFields( const string& strInput, char chDelim, C callback )
{
    istringstream issFile( strInput );
    string strLine;
    string strField;
    size_t nColIndex;

    while ( getline( issFile, strLine ) )
    {
        istringstream issLine( strLine );
        nColIndex = 0;

        while ( getline( issLine, strField, chDelim ) )
        {
            callback( nColIndex, strField );

            nColIndex++;
        }
    }
}
```


A function object that fills an array with column widths:


```cpp
typedef vector< size_t > ColWidths;

struct MaxColWidthsDeterminer
{
    explicit MaxColWidthsDeterminer( ColWidths& colWidths )
    : m_colWidths( colWidths ) {}

    void operator()( size_t nColIndex, const string& strField );

    ColWidths& m_colWidths;
};

void MaxColWidthsDeterminer::operator()( size_t nColIndex,
                                         const string& strField )
{
    size_t nWidth = strField.length();

    if ( nColIndex >= m_colWidths.size() )
        m_colWidths.push_back( nWidth );
    else
        m_colWidths[ nColIndex ] = max( m_colWidths[ nColIndex ], nWidth );
}
```


A function object that outputs fields formatted in columns:


```cpp
struct FormattedLister
{
    enum Alignment { eLeft, eRight, eCenter };

    FormattedLister( const ColWidths& colWidths, ostream& os,
                     Alignment alignment = eLeft )
    : m_colWidths( colWidths ), m_os( os ), m_alignment( alignment ),
      m_nPrevColIndex( 0 )
    {
        m_savedStreamFlags = os.flags();
        m_os.setf( ( m_alignment == eRight ) ? ios::right : ios::left,
                   ios::adjustfield );
    }

    ~FormattedLister()
    {
        m_os.flags( m_savedStreamFlags );
    }

    void operator()( size_t nColIndex, const string& strField );

    const ColWidths& m_colWidths;
    ostream&         m_os;
    Alignment        m_alignment;
    size_t           m_nPrevColIndex;
    ios::fmtflags    m_savedStreamFlags;
};

void FormattedLister::operator()( size_t nColIndex, const string& strField )
{
    if ( nColIndex < m_nPrevColIndex )
        m_os << '\n';

    if ( m_alignment == eCenter )
    {
        size_t nSpacesBefore = ( m_colWidths[ nColIndex ] - strField.length() )
                               / 2;
        size_t nSpacesAfter = m_colWidths[ nColIndex ] - strField.length()
                              - nSpacesBefore + 1;

        m_os << string( nSpacesBefore, ' ' ) << strField
             << string( nSpacesAfter, ' ' );
    }
    else
    {
        m_os << setw( static_cast< streamsize >( m_colWidths[ nColIndex ] ) )
             << strField << ' ';
    }

    m_nPrevColIndex = nColIndex;
}
```


The test program, that makes a pass through the data to determine the column widths, and then three more for outputting in each of the three column alignments:


```cpp
int main()
{
    const string strInput( 
        "Given$a$text$file$of$many$lines,$where$fields$within$a$line$\n"
        "are$delineated$by$a$single$'dollar'$character,$write$a$program\n"
        "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$\n"
        "column$are$separated$by$at$least$one$space.\n"
        "Further,$allow$for$each$word$in$a$column$to$be$either$left$\n"
        "justified,$right$justified,$or$center$justified$within$its$column." );

    const char chDelim = '$';

    ColWidths colWidths;

    enumerateFields( strInput, chDelim, MaxColWidthsDeterminer( colWidths ) );

    ofstream outFile( "ColumnAligner.txt" );
    if ( outFile )
    {
        enumerateFields( strInput, chDelim,
                         FormattedLister( colWidths, outFile ) );
        outFile << '\n';

        enumerateFields( strInput, chDelim,
                         FormattedLister( colWidths, outFile,
                                          FormattedLister::eRight ) );
        outFile << '\n';

        enumerateFields( strInput, chDelim,
                         FormattedLister( colWidths, outFile,
                                          FormattedLister::eCenter ) );
        outFile << endl;
    }
}
```

